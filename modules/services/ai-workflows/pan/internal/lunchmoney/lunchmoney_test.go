package lunchmoney_test

import (
	"context"
	"encoding/json"
	"net/http"
	"net/http/httptest"
	"testing"

	lm "github.com/Cidan/lunchmoney-go"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"pan/internal/lunchmoney"
)

type testSetup struct {
	server *httptest.Server
	mux    *http.ServeMux
}

func setupTest(t *testing.T) (*testSetup, lunchmoney.Service) {
	t.Helper()
	mux := http.NewServeMux()
	server := httptest.NewServer(mux)

	lmClient, err := lm.NewClient(
		lm.WithStaticToken("test-token"),
		lm.WithBaseURL(server.URL),
		lm.WithHTTPClient(server.Client()),
	)
	require.NoError(t, err)

	svc := lunchmoney.New(lmClient)
	return &testSetup{
		server: server,
		mux:    mux,
	}, svc
}

func TestListTransactions(t *testing.T) {
	ts, svc := setupTest(t)
	defer ts.server.Close()

	ts.mux.HandleFunc("/transactions", func(w http.ResponseWriter, r *http.Request) {
		assert.Equal(t, "GET", r.Method)
		assert.Equal(t, "Bearer test-token", r.Header.Get("Authorization"))

		resp := map[string]any{
			"transactions": []map[string]any{
				{"id": 1, "amount": "10.00", "payee": "Test Store"},
				{"id": 2, "amount": "-5.00", "payee": "Refund"},
			},
		}
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(resp)
	})

	resp, err := svc.ListTransactions(context.Background(), nil)
	require.NoError(t, err)
	assert.NotNil(t, resp)
	assert.Len(t, resp.Transactions, 2)
}

func TestGetCategory(t *testing.T) {
	ts, svc := setupTest(t)
	defer ts.server.Close()

	ts.mux.HandleFunc("/categories/42", func(w http.ResponseWriter, r *http.Request) {
		assert.Equal(t, "GET", r.Method)
		
		cat := map[string]any{
			"id": 42,
			"name": "Food",
		}
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(cat)
	})

	cat, err := svc.GetCategory(context.Background(), 42)
	require.NoError(t, err)
	assert.NotNil(t, cat)
	assert.Equal(t, "Food", cat.Name)
}

func TestListCategories(t *testing.T) {
	ts, svc := setupTest(t)
	defer ts.server.Close()

	ts.mux.HandleFunc("/categories", func(w http.ResponseWriter, r *http.Request) {
		assert.Equal(t, "GET", r.Method)
		
		resp := map[string]any{
			"categories": []map[string]any{
				{"id": 1, "name": "Food"},
				{"id": 2, "name": "Travel"},
			},
		}
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(resp)
	})

	resp, err := svc.ListCategories(context.Background(), nil)
	require.NoError(t, err)
	assert.NotNil(t, resp)
	assert.Len(t, resp.Categories, 2)
}

func TestListTags(t *testing.T) {
	ts, svc := setupTest(t)
	defer ts.server.Close()

	ts.mux.HandleFunc("/tags", func(w http.ResponseWriter, r *http.Request) {
		assert.Equal(t, "GET", r.Method)
		
		resp := map[string]any{
			"tags": []map[string]any{
				{"id": 10, "name": "Vacation"},
				{"id": 11, "name": "Business"},
			},
		}
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(resp)
	})

	resp, err := svc.ListTags(context.Background())
	require.NoError(t, err)
	assert.NotNil(t, resp)
}

func TestUpdateTransaction(t *testing.T) {
	ts, svc := setupTest(t)
	defer ts.server.Close()

	ts.mux.HandleFunc("/transactions/123", func(w http.ResponseWriter, r *http.Request) {
		assert.Equal(t, "PUT", r.Method)
		
		resp := map[string]any{
			"id": 123,
			"payee": "Updated Store",
		}
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(resp)
	})

	body := lm.UpdateTransactionBody{}
	
	resp, err := svc.UpdateTransaction(context.Background(), 123, body)
	require.NoError(t, err)
	assert.NotNil(t, resp)
}
