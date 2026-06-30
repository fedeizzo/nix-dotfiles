package hindsight

import (
	"context"
	"net/http"
	"net/http/httptest"
	"testing"

	"github.com/stretchr/testify/assert"
	hindsightSDK "github.com/vectorize-io/hindsight/hindsight-clients/go"
	"google.golang.org/adk/memory"
)

func TestService_AddFact(t *testing.T) {
	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(http.StatusOK)
		w.Write([]byte(`{"success":true, "bank_id":"test-bank", "items_count": 1, "async": false}`))
	}))
	defer ts.Close()

	cfg := hindsightSDK.NewConfiguration()
	cfg.Servers = hindsightSDK.ServerConfigurations{{URL: ts.URL}}
	client := hindsightSDK.NewAPIClient(cfg)

	tests := []struct {
		name          string
		bankID        string
		fact          string
		expectedError string
	}{
		{
			name:          "adds fact successfully",
			bankID:        "test-bank",
			fact:          "test fact",
			expectedError: "",
		},
		{
			name:          "skips if no bank ID",
			bankID:        "",
			fact:          "test fact",
			expectedError: "",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			svc := NewService(client, tc.bankID)
			err := svc.AddFact(context.Background(), tc.fact)

			if tc.expectedError != "" {
				assert.EqualError(t, err, tc.expectedError)
			} else {
				assert.NoError(t, err)
			}
		})
	}
}

func TestService_SearchMemory(t *testing.T) {
	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(http.StatusOK)
		w.Write([]byte(`{"results":[{"id":"1","text":"test fact"}]}`))
	}))
	defer ts.Close()

	cfg := hindsightSDK.NewConfiguration()
	cfg.Servers = hindsightSDK.ServerConfigurations{{URL: ts.URL}}
	client := hindsightSDK.NewAPIClient(cfg)

	tests := []struct {
		name          string
		bankID        string
		req           *memory.SearchRequest
		expectedCount int
		expectedError string
	}{
		{
			name:          "searches memory successfully",
			bankID:        "test-bank",
			req:           &memory.SearchRequest{Query: "test"},
			expectedCount: 1,
			expectedError: "",
		},
		{
			name:          "uses user ID if bank ID is empty",
			bankID:        "",
			req:           &memory.SearchRequest{Query: "test", UserID: "u1"},
			expectedCount: 1,
			expectedError: "",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			svc := NewService(client, tc.bankID)
			resp, err := svc.SearchMemory(context.Background(), tc.req)

			if tc.expectedError != "" {
				assert.EqualError(t, err, tc.expectedError)
			} else {
				assert.NoError(t, err)
				assert.Len(t, resp.Memories, tc.expectedCount)
			}
		})
	}
}
