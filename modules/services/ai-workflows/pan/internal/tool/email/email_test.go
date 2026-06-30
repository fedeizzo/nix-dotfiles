package email

import (
	"context"
	"testing"
	"time"

	"github.com/samber/ro"

	"pan/internal/fastmail"
	"pan/internal/fastmail/mocks"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/mock"
	"github.com/stretchr/testify/require"
	"google.golang.org/adk/memory"
	"google.golang.org/adk/tool"
)

type fakeToolContext struct {
	tool.Context
	ctx     context.Context
	memResp *memory.SearchResponse
	memErr  error
}

func (f *fakeToolContext) Value(key any) any           { return f.ctx.Value(key) }
func (f *fakeToolContext) Done() <-chan struct{}       { return f.ctx.Done() }
func (f *fakeToolContext) Err() error                  { return f.ctx.Err() }
func (f *fakeToolContext) Deadline() (time.Time, bool) { return f.ctx.Deadline() }
func (f *fakeToolContext) SearchMemory(ctx context.Context, query string) (*memory.SearchResponse, error) {
	return f.memResp, f.memErr
}

func TestSuggestTriage(t *testing.T) {
	tests := []struct {
		name         string
		input        SuggestTriageInput
		setupContext func() tool.Context
		expectedMsg  string
	}{
		{
			name:  "returns default success message when memory is empty",
			input: SuggestTriageInput{Reason: "Checking email"},
			setupContext: func() tool.Context {
				return &fakeToolContext{
					ctx: t.Context(),
				}
			},
			expectedMsg: "Email successfully triaged and validated. You may now proceed to add tags or mark as seen.",
		},
		{
			name:  "appends memory context when available",
			input: SuggestTriageInput{Reason: "Checking urgent email"},
			setupContext: func() tool.Context {
				return &fakeToolContext{
					ctx: t.Context(),
					memResp: &memory.SearchResponse{
						Memories: []memory.Entry{
							{}, // Empty memory entry mocking the genai.Content
						},
					},
				}
			},
			expectedMsg: "Email successfully triaged and validated. You may now proceed to add tags or mark as seen.",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			m := mocks.NewMockService(t)
			toolset, err := New(m, ro.NewPublishSubject[any]())
			require.NoError(t, err)

			out, err := toolset.SuggestTriage(tc.setupContext(), tc.input)
			assert.NoError(t, err)

			// We only assert the prefix for the memory case since we can't cleanly mock *genai.Content
			if tc.name == "appends memory context when available" {
				assert.Contains(t, out.Result, tc.expectedMsg)
			} else {
				assert.Equal(t, tc.expectedMsg, out.Result)
			}
		})
	}
}

func TestGetUnreadEmail(t *testing.T) {
	tests := []struct {
		name          string
		input         GetUnreadEmailInput
		setupMock     func(m *mocks.MockService)
		setupContext  func() tool.Context
		expectedError string
		expectCached  bool
	}{
		{
			name:  "successfully fetches unread email and caches it",
			input: GetUnreadEmailInput{TagName: "Inbox"},
			setupMock: func(m *mocks.MockService) {
				m.EXPECT().GetTags(mock.Anything).Return([]fastmail.Tag{
					{ID: "tag-1", Name: "Inbox"},
				}, nil)
				m.EXPECT().GetUnreadEmailForTag(mock.Anything, fastmail.Tag{ID: "tag-1", Name: "Inbox"}).Return(&fastmail.Email{
					ID: "email-123",
				}, nil)
			},
			setupContext: func() tool.Context {
				return &fakeToolContext{ctx: context.WithValue(t.Context(), "matrix_thread_id", "thread1")}
			},
			expectedError: "",
			expectCached:  true,
		},
		{
			name:  "fails when tag is not found",
			input: GetUnreadEmailInput{TagName: "Unknown"},
			setupMock: func(m *mocks.MockService) {
				m.EXPECT().GetTags(mock.Anything).Return([]fastmail.Tag{
					{ID: "tag-1", Name: "Inbox"},
				}, nil)
			},
			setupContext: func() tool.Context {
				return &fakeToolContext{ctx: context.WithValue(t.Context(), "matrix_thread_id", "thread1")}
			},
			expectedError: `tag "Unknown" not found`,
			expectCached:  false,
		},
		{
			name:  "fails when matrix_thread_id is missing",
			input: GetUnreadEmailInput{TagName: "Inbox"},
			setupMock: func(m *mocks.MockService) {
				m.EXPECT().GetTags(mock.Anything).Return([]fastmail.Tag{
					{ID: "tag-1", Name: "Inbox"},
				}, nil)
				m.EXPECT().GetUnreadEmailForTag(mock.Anything, fastmail.Tag{ID: "tag-1", Name: "Inbox"}).Return(&fastmail.Email{
					ID: "email-123",
				}, nil)
			},
			setupContext: func() tool.Context {
				return &fakeToolContext{ctx: t.Context()}
			},
			expectedError: "failed to extract matrix_thread_id from context",
			expectCached:  false,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			m := mocks.NewMockService(t)
			tc.setupMock(m)

			toolset, err := New(m, ro.NewPublishSubject[any]())
			require.NoError(t, err)

			out, err := toolset.GetUnreadEmail(tc.setupContext(), tc.input)

			if tc.expectedError != "" {
				assert.EqualError(t, err, tc.expectedError)
				assert.Nil(t, out.Email)
			} else {
				assert.NoError(t, err)
				assert.NotNil(t, out.Email)
			}

			if tc.expectCached {
				toolset.cache.Wait()
				val, found := toolset.cache.Get("active_email_thread1")
				assert.True(t, found)
				assert.Equal(t, "email-123", val)
			}
		})
	}
}

func TestMarkEmailAsSeen(t *testing.T) {
	tests := []struct {
		name          string
		setupMock     func(m *mocks.MockService)
		setupContext  func() tool.Context
		populateCache func(toolset EmailToolset)
		expectedError string
	}{
		{
			name: "successfully marks email as seen and removes from cache",
			setupMock: func(m *mocks.MockService) {
				m.EXPECT().MarkEmailAsSeen(mock.Anything, "email-123").Return(nil)
			},
			setupContext: func() tool.Context {
				return &fakeToolContext{ctx: context.WithValue(t.Context(), "matrix_thread_id", "thread1")}
			},
			populateCache: func(toolset EmailToolset) {
				toolset.cache.SetWithTTL("active_email_thread1", "email-123", 1, time.Hour)
				toolset.cache.Wait()
			},
			expectedError: "",
		},
		{
			name: "fails when email ID is not in cache",
			setupMock: func(m *mocks.MockService) {
			},
			setupContext: func() tool.Context {
				return &fakeToolContext{ctx: context.WithValue(t.Context(), "matrix_thread_id", "thread1")}
			},
			populateCache: func(toolset EmailToolset) {},
			expectedError: "no email id found in cache",
		},
		{
			name: "fails when matrix_thread_id is missing",
			setupMock: func(m *mocks.MockService) {
			},
			setupContext: func() tool.Context {
				return &fakeToolContext{ctx: t.Context()}
			},
			populateCache: func(toolset EmailToolset) {},
			expectedError: "failed to extract matrix_thread_id from context",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			m := mocks.NewMockService(t)
			tc.setupMock(m)

			toolset, err := New(m, ro.NewPublishSubject[any]())
			require.NoError(t, err)

			tc.populateCache(toolset)

			out, err := toolset.MarkEmailAsSeen(tc.setupContext(), MarkEmailAsSeenInput{})

			if tc.expectedError != "" {
				assert.EqualError(t, err, tc.expectedError)
				assert.False(t, out.Success)
			} else {
				assert.NoError(t, err)
				assert.True(t, out.Success)

				// Verify cache eviction
				_, found := toolset.cache.Get("active_email_thread1")
				assert.False(t, found)
			}
		})
	}
}

func TestAddTagToEmail(t *testing.T) {
	tests := []struct {
		name          string
		input         AddTagToEmailInput
		setupMock     func(m *mocks.MockService)
		setupContext  func() tool.Context
		populateCache func(toolset EmailToolset)
		expectedError string
	}{
		{
			name:  "successfully adds tag to email",
			input: AddTagToEmailInput{TagName: "Urgent"},
			setupMock: func(m *mocks.MockService) {
				m.EXPECT().GetTags(mock.Anything).Return([]fastmail.Tag{
					{ID: "tag-2", Name: "Urgent"},
				}, nil)
				m.EXPECT().AddTagToEmail(mock.Anything, "email-123", fastmail.Tag{ID: "tag-2", Name: "Urgent"}).Return(nil)
			},
			setupContext: func() tool.Context {
				return &fakeToolContext{ctx: context.WithValue(t.Context(), "matrix_thread_id", "thread1")}
			},
			populateCache: func(toolset EmailToolset) {
				toolset.cache.SetWithTTL("active_email_thread1", "email-123", 1, time.Hour)
				toolset.cache.Wait()
			},
			expectedError: "",
		},
		{
			name:  "fails when tag is not found",
			input: AddTagToEmailInput{TagName: "Unknown"},
			setupMock: func(m *mocks.MockService) {
				m.EXPECT().GetTags(mock.Anything).Return([]fastmail.Tag{
					{ID: "tag-2", Name: "Urgent"},
				}, nil)
			},
			setupContext: func() tool.Context {
				return &fakeToolContext{ctx: context.WithValue(t.Context(), "matrix_thread_id", "thread1")}
			},
			populateCache: func(toolset EmailToolset) {
				toolset.cache.SetWithTTL("active_email_thread1", "email-123", 1, time.Hour)
				toolset.cache.Wait()
			},
			expectedError: `tag "Unknown" not found`,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			m := mocks.NewMockService(t)
			tc.setupMock(m)

			toolset, err := New(m, ro.NewPublishSubject[any]())
			require.NoError(t, err)

			tc.populateCache(toolset)

			out, err := toolset.AddTagToEmail(tc.setupContext(), tc.input)

			if tc.expectedError != "" {
				assert.EqualError(t, err, tc.expectedError)
				assert.False(t, out.Success)
			} else {
				assert.NoError(t, err)
				assert.True(t, out.Success)
			}
		})
	}
}
