package fastmail_test

import (
	"testing"

	"pan/internal/fastmail"
	"pan/internal/fastmail/mocks"

	"git.sr.ht/~rockorager/go-jmap"
	"git.sr.ht/~rockorager/go-jmap/mail/email"
	"git.sr.ht/~rockorager/go-jmap/mail/mailbox"

	"github.com/samber/oops"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/mock"
)

type testSetup struct {
	mockClient *mocks.MockJMAPClient
}

func setupTest(t *testing.T) (*testSetup, fastmail.Service) {
	t.Helper()
	ts := &testSetup{
		mockClient: mocks.NewMockJMAPClient(t),
	}
	fm := fastmail.NewForTest(ts.mockClient, jmap.ID("test-account-id"))
	return ts, fm
}

func mockJMAPResponse(name string, args any) *jmap.Response {
	return &jmap.Response{
		Responses: []*jmap.Invocation{
			{
				Name: name,
				Args: args,
			},
		},
	}
}

func TestGetTags(t *testing.T) {
	tests := []struct {
		name          string
		mockSetup     func(ts *testSetup)
		expectedTags  []fastmail.Tag
		expectedError string
	}{
		{
			name: "returns a list of tags on success",
			mockSetup: func(ts *testSetup) {
				args := &mailbox.GetResponse{
					List: []*mailbox.Mailbox{
						{ID: "m1", Name: "Inbox", TotalEmails: 5, UnreadEmails: 1},
						{ID: "m2", Name: "Archive", TotalEmails: 10, UnreadEmails: 0},
					},
				}
				mockResp := mockJMAPResponse("Mailbox/get", args)
				ts.mockClient.EXPECT().Do(mock.AnythingOfType("*jmap.Request")).Return(mockResp, nil).Once()
			},
			expectedTags: []fastmail.Tag{
				{ID: "m1", Name: "Inbox", Emails: 5, Unread: 1},
				{ID: "m2", Name: "Archive", Emails: 10, Unread: 0},
			},
			expectedError: "",
		},
		{
			name: "returns an error when the API request fails",
			mockSetup: func(ts *testSetup) {
				ts.mockClient.EXPECT().Do(mock.AnythingOfType("*jmap.Request")).Return(nil, oops.Errorf("network error")).Once()
			},
			expectedTags:  nil,
			expectedError: "error getting tags from fastmail: network error",
		},
		{
			name: "returns an error when the response type is unexpected",
			mockSetup: func(ts *testSetup) {
				mockResp := mockJMAPResponse("Mailbox/get", "unexpected string instead of GetResponse")
				ts.mockClient.EXPECT().Do(mock.AnythingOfType("*jmap.Request")).Return(mockResp, nil).Once()
			},
			expectedTags:  nil,
			expectedError: "error while casting response from get tags",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			ts, fm := setupTest(t)
			tc.mockSetup(ts)

			tags, err := fm.GetTags(t.Context())

			if tc.expectedError != "" {
				assert.EqualError(t, err, tc.expectedError)
			} else {
				assert.NoError(t, err)
				assert.Equal(t, tc.expectedTags, tags)
			}
		})
	}
}

func TestGetUnreadEmailForTag(t *testing.T) {
	testTag := fastmail.Tag{ID: "tag1", Name: "Inbox"}

	tests := []struct {
		name          string
		mockSetup     func(ts *testSetup)
		expectedEmail *fastmail.Email
		expectedError string
	}{
		{
			name: "fetches the full email when an unread email ID is found",
			mockSetup: func(ts *testSetup) {
				queryResp := mockJMAPResponse("Email/query", &email.QueryResponse{IDs: []jmap.ID{"email-123"}})
				ts.mockClient.EXPECT().Do(mock.MatchedBy(func(req *jmap.Request) bool {
					return len(req.Calls) > 0 && req.Calls[0].Name == "Email/query"
				})).Return(queryResp, nil).Once()

				getResp := mockJMAPResponse("Email/get", &email.GetResponse{
					List: []*email.Email{
						{ID: "email-123", Subject: "Hello Test", Preview: "Preview text"},
					},
				})
				ts.mockClient.EXPECT().Do(mock.MatchedBy(func(req *jmap.Request) bool {
					return len(req.Calls) > 0 && req.Calls[0].Name == "Email/get"
				})).Return(getResp, nil).Once()
			},
			expectedEmail: &fastmail.Email{
				ID:      "email-123",
				Subject: "Hello Test",
				From:    "", // Not mocked for simplicity
				To:      "",
				Preview: "Preview text",
			},
			expectedError: "",
		},
		{
			name: "returns nil when no unread emails are found",
			mockSetup: func(ts *testSetup) {
				queryResp := mockJMAPResponse("Email/query", &email.QueryResponse{IDs: []jmap.ID{}})
				ts.mockClient.EXPECT().Do(mock.Anything).Return(queryResp, nil).Once()
			},
			expectedEmail: nil,
			expectedError: "",
		},
		{
			name: "returns an error when the initial query fails",
			mockSetup: func(ts *testSetup) {
				ts.mockClient.EXPECT().Do(mock.Anything).Return(nil, oops.Errorf("api error")).Once()
			},
			expectedEmail: nil,
			expectedError: "error querying emails: api error",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			ts, fm := setupTest(t)
			tc.mockSetup(ts)

			emailResult, err := fm.GetUnreadEmailForTag(t.Context(), testTag)

			if tc.expectedError != "" {
				assert.EqualError(t, err, tc.expectedError)
			} else {
				assert.NoError(t, err)
				assert.Equal(t, tc.expectedEmail, emailResult)
			}
		})
	}
}

func TestAddTagToEmail(t *testing.T) {
	tests := []struct {
		name          string
		mockSetup     func(ts *testSetup)
		expectedError string
	}{
		{
			name: "completes without error on success",
			mockSetup: func(ts *testSetup) {
				mockResp := mockJMAPResponse("Email/set", &email.SetResponse{NotUpdated: nil})
				ts.mockClient.EXPECT().Do(mock.Anything).Return(mockResp, nil).Once()
			},
			expectedError: "",
		},
		{
			name: "returns an error when the API returns a NotUpdated failure",
			mockSetup: func(ts *testSetup) {
				desc := "mailbox not found"
				args := &email.SetResponse{
					NotUpdated: map[jmap.ID]*jmap.SetError{
						"email-1": {
							Type:        "invalidProperties",
							Description: &desc,
						},
					},
				}
				mockResp := mockJMAPResponse("Email/set", args)
				ts.mockClient.EXPECT().Do(mock.Anything).Return(mockResp, nil).Once()
			},
			expectedError: "failed to add tag: invalidProperties - mailbox not found",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			ts, fm := setupTest(t)
			tc.mockSetup(ts)

			err := fm.AddTagToEmail(t.Context(), "email-1", fastmail.Tag{ID: "m1", Name: "Inbox"})

			if tc.expectedError != "" {
				assert.EqualError(t, err, tc.expectedError)
			} else {
				assert.NoError(t, err)
			}
		})
	}
}

func TestMarkEmailAsSeen(t *testing.T) {
	tests := []struct {
		name          string
		mockSetup     func(ts *testSetup)
		expectedError string
	}{
		{
			name: "completes without error on success",
			mockSetup: func(ts *testSetup) {
				mockResp := mockJMAPResponse("Email/set", &email.SetResponse{NotUpdated: nil})
				ts.mockClient.EXPECT().Do(mock.Anything).Return(mockResp, nil).Once()
			},
			expectedError: "",
		},
		{
			name: "returns an error on network failure",
			mockSetup: func(ts *testSetup) {
				ts.mockClient.EXPECT().Do(mock.Anything).Return(nil, oops.Errorf("timeout")).Once()
			},
			expectedError: "error marking email as seen: timeout",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			ts, fm := setupTest(t)
			tc.mockSetup(ts)

			err := fm.MarkEmailAsSeen(t.Context(), "email-2")

			if tc.expectedError != "" {
				assert.EqualError(t, err, tc.expectedError)
			} else {
				assert.NoError(t, err)
			}
		})
	}
}
