package fusion

import (
	"encoding/json"
	"net/http"
	"net/http/httptest"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestGetUnreadFeeds(t *testing.T) {
	tests := []struct {
		name          string
		handler       http.HandlerFunc
		expectedFeeds []FeedItem
		expectedError string
	}{
		{
			name: "returns unread feeds on success",
			handler: func(w http.ResponseWriter, r *http.Request) {
				if r.URL.Path == "/sessions" {
					http.SetCookie(w, &http.Cookie{Name: "session", Value: "123"})
					w.WriteHeader(http.StatusOK)
					return
				}
				if r.URL.Path == "/items" {
					res := struct {
						Data  []FeedItem `json:"data"`
						Total int        `json:"total"`
					}{
						Data:  []FeedItem{{ID: 1, Title: "Test Feed", Unread: true}},
						Total: 1,
					}
					json.NewEncoder(w).Encode(res)
					return
				}
			},
			expectedFeeds: []FeedItem{{ID: 1, Title: "Test Feed", Unread: true}},
			expectedError: "",
		},
		{
			name: "returns error on authentication failure",
			handler: func(w http.ResponseWriter, r *http.Request) {
				if r.URL.Path == "/sessions" {
					w.WriteHeader(http.StatusUnauthorized)
					return
				}
			},
			expectedFeeds: nil,
			expectedError: "auth error: failed to login: status 401",
		},
		{
			name: "returns error on missing session cookie",
			handler: func(w http.ResponseWriter, r *http.Request) {
				if r.URL.Path == "/sessions" {
					// Returns 200 but no session cookie
					w.WriteHeader(http.StatusOK)
					return
				}
			},
			expectedFeeds: nil,
			expectedError: "auth error: fusion: session cookie not found in response",
		},
		{
			name: "returns error on items api failure",
			handler: func(w http.ResponseWriter, r *http.Request) {
				if r.URL.Path == "/sessions" {
					http.SetCookie(w, &http.Cookie{Name: "session", Value: "123"})
					w.WriteHeader(http.StatusOK)
					return
				}
				if r.URL.Path == "/items" {
					w.WriteHeader(http.StatusInternalServerError)
					return
				}
			},
			expectedFeeds: nil,
			expectedError: "failed to get items: status 500",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			ts := httptest.NewServer(tc.handler)
			defer ts.Close()

			c := New(ts.URL, "pass")
			feeds, err := c.GetUnreadFeeds(t.Context(), 10)

			if tc.expectedError != "" {
				assert.EqualError(t, err, tc.expectedError)
			} else {
				assert.NoError(t, err)
				assert.Equal(t, tc.expectedFeeds, feeds)
			}
		})
	}
}

func TestMarkFeedsAsRead(t *testing.T) {
	tests := []struct {
		name          string
		ids           []int
		handler       http.HandlerFunc
		expectedError string
	}{
		{
			name: "returns early with no error if ids is empty",
			ids:  []int{},
			handler: func(w http.ResponseWriter, r *http.Request) {
				// Should not be called
				w.WriteHeader(http.StatusInternalServerError)
			},
			expectedError: "",
		},
		{
			name: "completes without error on success",
			ids:  []int{1, 2, 3},
			handler: func(w http.ResponseWriter, r *http.Request) {
				if r.URL.Path == "/sessions" {
					http.SetCookie(w, &http.Cookie{Name: "session", Value: "123"})
					w.WriteHeader(http.StatusOK)
					return
				}
				if r.URL.Path == "/items/-/read" {
					w.WriteHeader(http.StatusNoContent)
					return
				}
			},
			expectedError: "",
		},
		{
			name: "returns error on api failure",
			ids:  []int{1, 2, 3},
			handler: func(w http.ResponseWriter, r *http.Request) {
				if r.URL.Path == "/sessions" {
					http.SetCookie(w, &http.Cookie{Name: "session", Value: "123"})
					w.WriteHeader(http.StatusOK)
					return
				}
				if r.URL.Path == "/items/-/read" {
					w.WriteHeader(http.StatusBadRequest)
					return
				}
			},
			expectedError: "failed to mark feeds as read: status 400",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			ts := httptest.NewServer(tc.handler)
			defer ts.Close()

			c := New(ts.URL, "pass")
			err := c.MarkFeedsAsRead(t.Context(), tc.ids)

			if tc.expectedError != "" {
				assert.EqualError(t, err, tc.expectedError)
			} else {
				assert.NoError(t, err)
			}
		})
	}
}
