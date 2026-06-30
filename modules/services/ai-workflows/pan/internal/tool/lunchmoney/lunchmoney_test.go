package lunchmoney

import (
	"context"
	"testing"
	"time"

	"github.com/samber/oops"
	"github.com/samber/ro"

	"pan/internal/lunchmoney/mocks"

	lm "github.com/Cidan/lunchmoney-go"

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

func TestGetLatestUnreviewedTransaction(t *testing.T) {
	testDate := lm.Date{Time: time.Date(2023, 10, 27, 0, 0, 0, 0, time.UTC)}
	catID := int32(42)
	notes := "Test notes"

	tests := []struct {
		name          string
		setupMock     func(m *mocks.MockService)
		setupContext  func() tool.Context
		expectedError string
		expectedTxn   *TransactionSummary
	}{
		{
			name: "successfully fetches unreviewed transaction and category",
			setupMock: func(m *mocks.MockService) {
				m.EXPECT().ListTransactions(mock.Anything, mock.MatchedBy(func(p *lm.ListTransactionsParams) bool {
					return p.Status != nil && *p.Status == "unreviewed" && p.Limit != nil && *p.Limit == 1
				})).Return(&lm.TransactionsListResponse{
					Transactions: []lm.Transaction{
						{
							Id:         100,
							Date:       testDate,
							Amount:     "12.34",
							Payee:      "Coffee Shop",
							Notes:      &notes,
							Status:     "unreviewed",
							CategoryId: &catID,
						},
					},
				}, nil)

				m.EXPECT().GetCategory(mock.Anything, int32(42)).Return(&lm.Category{
					Id:   42,
					Name: "Food & Dining",
				}, nil)
			},
			setupContext: func() tool.Context {
				return &fakeToolContext{ctx: context.WithValue(t.Context(), "matrix_thread_id", "thread1")}
			},
			expectedError: "",
			expectedTxn: &TransactionSummary{
				ID:       100,
				Date:     testDate.String(),
				Amount:   "12.34",
				Payee:    "Coffee Shop",
				Notes:    "Test notes",
				Status:   "unreviewed",
				Category: func() *string { s := "Food & Dining"; return &s }(),
			},
		},
		{
			name: "returns no transaction when list is empty",
			setupMock: func(m *mocks.MockService) {
				m.EXPECT().ListTransactions(mock.Anything, mock.Anything).Return(&lm.TransactionsListResponse{
					Transactions: []lm.Transaction{},
				}, nil)
			},
			setupContext: func() tool.Context {
				return &fakeToolContext{ctx: context.WithValue(t.Context(), "matrix_thread_id", "thread1")}
			},
			expectedError: "",
			expectedTxn:   nil,
		},
		{
			name: "fails when list API fails",
			setupMock: func(m *mocks.MockService) {
				m.EXPECT().ListTransactions(mock.Anything, mock.Anything).Return(nil, oops.Errorf("api error"))
			},
			setupContext: func() tool.Context {
				return &fakeToolContext{ctx: context.WithValue(t.Context(), "matrix_thread_id", "thread1")}
			},
			expectedError: "failed to list unreviewed transactions: api error",
			expectedTxn:   nil,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			m := mocks.NewMockService(t)
			tc.setupMock(m)

			toolset, err := New(m, ro.NewPublishSubject[any]())
			require.NoError(t, err)

			out, err := toolset.GetLatestUnreviewedTransaction(tc.setupContext(), GetLatestUnreviewedTransactionInput{})

			if tc.expectedError != "" {
				assert.EqualError(t, err, tc.expectedError)
				assert.Nil(t, out.Transaction)
			} else {
				assert.NoError(t, err)
				if tc.expectedTxn != nil {
					assert.NotNil(t, out.Transaction)
					assert.Equal(t, tc.expectedTxn.ID, out.Transaction.ID)
					assert.Equal(t, tc.expectedTxn.Amount, out.Transaction.Amount)
					assert.Equal(t, tc.expectedTxn.Payee, out.Transaction.Payee)
					if tc.expectedTxn.Category != nil {
						assert.Equal(t, *tc.expectedTxn.Category, *out.Transaction.Category)
					}

					// verify cache
					toolset.cache.Wait()
					val, found := toolset.cache.Get("active_txn_thread1")
					assert.True(t, found)
					assert.Equal(t, tc.expectedTxn.ID, val)
				} else {
					assert.Nil(t, out.Transaction)
				}
			}
		})
	}
}

func TestGetCategories(t *testing.T) {
	t.Run("successfully fetches categories", func(t *testing.T) {
		m := mocks.NewMockService(t)
		desc := "Test category"
		m.EXPECT().ListCategories(mock.Anything, (*lm.ListCategoriesParams)(nil)).Return(&lm.CategoriesListResponse{
			Categories: []lm.Category{
				{Id: 1, Name: "Food", Description: &desc, IsGroup: false},
				{Id: 2, Name: "Bills", IsGroup: true},
			},
		}, nil)

		toolset, err := New(m, ro.NewPublishSubject[any]())
		require.NoError(t, err)

		out, err := toolset.GetCategories(&fakeToolContext{ctx: t.Context()}, GetCategoriesInput{})
		assert.NoError(t, err)
		assert.Len(t, out.Categories, 2)
		assert.Equal(t, "Food", out.Categories[0].Name)
		assert.Equal(t, "Test category", out.Categories[0].Description)
		assert.Equal(t, "Bills", out.Categories[1].Name)
		assert.Equal(t, "", out.Categories[1].Description)
	})
}

func TestGetTags(t *testing.T) {
	t.Run("successfully fetches tags", func(t *testing.T) {
		m := mocks.NewMockService(t)
		desc := "Tax related"
		m.EXPECT().ListTags(mock.Anything).Return(&lm.TagsListResponse{
			Tags: []lm.Tag{
				{Id: 1, Name: "taxes", Description: &desc},
				{Id: 2, Name: "urgent"},
			},
		}, nil)

		toolset, err := New(m, ro.NewPublishSubject[any]())
		require.NoError(t, err)

		out, err := toolset.GetTags(&fakeToolContext{ctx: t.Context()}, GetTagsInput{})
		assert.NoError(t, err)
		assert.Len(t, out.Tags, 2)
		assert.Equal(t, "taxes", out.Tags[0].Name)
		assert.Equal(t, "Tax related", out.Tags[0].Description)
		assert.Equal(t, "urgent", out.Tags[1].Name)
		assert.Equal(t, "", out.Tags[1].Description)
	})
}

func TestUpdateTransaction(t *testing.T) {
	tests := []struct {
		name          string
		input         UpdateTransactionInput
		setupMock     func(m *mocks.MockService)
		setupContext  func() tool.Context
		populateCache func(t *LunchMoneyToolset)
		expectedError string
	}{
		{
			name: "successfully updates transaction",
			input: UpdateTransactionInput{
				CategoryID: func() *int32 { i := int32(42); return &i }(),
				Status:     func() *string { s := "reviewed"; return &s }(),
			},
			setupMock: func(m *mocks.MockService) {
				m.EXPECT().UpdateTransaction(mock.Anything, int64(100), mock.MatchedBy(func(b lm.UpdateTransactionBody) bool {
					// We just care that it receives an update body since it's transformed to JSON internally
					return true
				})).Return(&lm.Transaction{Id: 100}, nil)
			},
			setupContext: func() tool.Context {
				return &fakeToolContext{ctx: context.WithValue(t.Context(), "matrix_thread_id", "thread1")}
			},
			populateCache: func(toolset *LunchMoneyToolset) {
				toolset.cache.SetWithTTL("active_txn_thread1", int64(100), 1, time.Hour)
				toolset.cache.Wait()
			},
			expectedError: "",
		},
		{
			name:      "fails when not in cache",
			input:     UpdateTransactionInput{},
			setupMock: func(m *mocks.MockService) {},
			setupContext: func() tool.Context {
				return &fakeToolContext{ctx: context.WithValue(t.Context(), "matrix_thread_id", "thread1")}
			},
			populateCache: func(toolset *LunchMoneyToolset) {},
			expectedError: "no transaction id found in cache",
		},
		{
			name:      "fails when thread ID missing",
			input:     UpdateTransactionInput{},
			setupMock: func(m *mocks.MockService) {},
			setupContext: func() tool.Context {
				return &fakeToolContext{ctx: t.Context()}
			},
			populateCache: func(toolset *LunchMoneyToolset) {},
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

			out, err := toolset.UpdateTransaction(tc.setupContext(), tc.input)

			if tc.expectedError != "" {
				assert.EqualError(t, err, tc.expectedError)
				assert.False(t, out.Success)
			} else {
				assert.NoError(t, err)
				assert.True(t, out.Success)

				toolset.cache.Wait()
				_, found := toolset.cache.Get("active_txn_thread1")
				assert.False(t, found)
			}
		})
	}
}
