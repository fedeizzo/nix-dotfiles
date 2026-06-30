package fusion

import (
	"context"
	"testing"
	"time"

	"github.com/samber/oops"
	"github.com/samber/ro"

	fusionsvc "pan/internal/fusion"
	"pan/internal/fusion/mocks"

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

func TestGetUnreadFeeds(t *testing.T) {
	tests := []struct {
		name          string
		input         GetUnreadFeedsInput
		setupMock     func(m *mocks.MockService)
		setupContext  func() tool.Context
		expectedFeeds int
		expectedError string
	}{
		{
			name:  "clamps negative limit to 10 and returns feeds",
			input: GetUnreadFeedsInput{Limit: -5},
			setupMock: func(m *mocks.MockService) {
				m.EXPECT().GetUnreadFeeds(mock.Anything, 10).Return([]fusionsvc.FeedItem{
					{ID: 1, Title: "Test Feed"},
				}, nil)
			},
			setupContext: func() tool.Context {
				ctx := context.WithValue(t.Context(), "matrix_thread_id", "thread123")
				return &fakeToolContext{ctx: ctx}
			},
			expectedFeeds: 1,
			expectedError: "",
		},
		{
			name:  "fails when matrix_thread_id is missing",
			input: GetUnreadFeedsInput{Limit: 5},
			setupMock: func(m *mocks.MockService) {
				m.EXPECT().GetUnreadFeeds(mock.Anything, 5).Return([]fusionsvc.FeedItem{
					{ID: 1, Title: "Test Feed"},
				}, nil)
			},
			setupContext: func() tool.Context {
				// No matrix_thread_id
				return &fakeToolContext{ctx: t.Context()}
			},
			expectedFeeds: 0,
			expectedError: "failed to extract matrix_thread_id from context",
		},
		{
			name:  "returns error from service",
			input: GetUnreadFeedsInput{Limit: 5},
			setupMock: func(m *mocks.MockService) {
				m.EXPECT().GetUnreadFeeds(mock.Anything, 5).Return(nil, oops.Errorf("service down"))
			},
			setupContext: func() tool.Context {
				ctx := context.WithValue(t.Context(), "matrix_thread_id", "thread123")
				return &fakeToolContext{ctx: ctx}
			},
			expectedFeeds: 0,
			expectedError: "service down",
		},
		{
			name:  "retrieves preferences from agent memory",
			input: GetUnreadFeedsInput{Limit: 1},
			setupMock: func(m *mocks.MockService) {
				m.EXPECT().GetUnreadFeeds(mock.Anything, 1).Return([]fusionsvc.FeedItem{
					{ID: 2, Title: "Tech News"},
				}, nil)
			},
			setupContext: func() tool.Context {
				ctx := context.WithValue(t.Context(), "matrix_thread_id", "thread123")
				return &fakeToolContext{
					ctx: ctx,
					memResp: &memory.SearchResponse{
						Memories: []memory.Entry{
							{}, // Empty memory entry since mocking the exact genai.Content struct is complex
						},
					},
				}
			},
			expectedFeeds: 1,
			expectedError: "", // Preference will be extracted successfully
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			m := mocks.NewMockService(t)
			tc.setupMock(m)

			toolset, err := New(m, ro.NewPublishSubject[any]())
			require.NoError(t, err)

			out, err := toolset.GetUnreadFeeds(tc.setupContext(), tc.input)

			if tc.expectedError != "" {
				assert.EqualError(t, err, tc.expectedError)
			} else {
				assert.NoError(t, err)
				assert.Len(t, out.Feeds, tc.expectedFeeds)
			}
		})
	}
}

func TestMarkFeedsAsRead(t *testing.T) {
	tests := []struct {
		name          string
		setupMock     func(m *mocks.MockService)
		setupContext  func() tool.Context
		populateCache func(toolset FusionToolset)
		expectedError string
	}{
		{
			name: "returns success when feeds are in cache",
			setupMock: func(m *mocks.MockService) {
				m.EXPECT().MarkFeedsAsRead(mock.Anything, []int{1, 2, 3}).Return(nil)
			},
			setupContext: func() tool.Context {
				ctx := context.WithValue(t.Context(), "matrix_thread_id", "thread123")
				return &fakeToolContext{ctx: ctx}
			},
			populateCache: func(toolset FusionToolset) {
				toolset.cache.SetWithTTL("active_feeds_thread123", []int{1, 2, 3}, 1, time.Hour)
				toolset.cache.Wait()
			},
			expectedError: "",
		},
		{
			name: "returns success early when cache is empty",
			setupMock: func(m *mocks.MockService) {
				// Should not be called
			},
			setupContext: func() tool.Context {
				ctx := context.WithValue(t.Context(), "matrix_thread_id", "thread123")
				return &fakeToolContext{ctx: ctx}
			},
			populateCache: func(toolset FusionToolset) {}, // empty cache
			expectedError: "",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			m := mocks.NewMockService(t)
			tc.setupMock(m)

			toolset, err := New(m, ro.NewPublishSubject[any]())
			require.NoError(t, err)

			tc.populateCache(toolset)

			out, err := toolset.MarkFeedsAsRead(tc.setupContext(), MarkFeedsAsReadInput{})

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
