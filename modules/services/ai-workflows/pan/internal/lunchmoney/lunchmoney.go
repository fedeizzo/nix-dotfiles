package lunchmoney

import (
	"context"

	lm "github.com/Cidan/lunchmoney-go"
)

type Service interface {
	ListTransactions(ctx context.Context, params *lm.ListTransactionsParams) (*lm.TransactionsListResponse, error)
	GetCategory(ctx context.Context, id int32) (*lm.Category, error)
	ListCategories(ctx context.Context, params *lm.ListCategoriesParams) (*lm.CategoriesListResponse, error)
	ListTags(ctx context.Context) (*lm.TagsListResponse, error)
	UpdateTransaction(ctx context.Context, id int64, body lm.UpdateTransactionBody) (*lm.Transaction, error)
}

type client struct {
	lmClient *lm.Client
}

func New(lmClient *lm.Client) Service {
	return &client{lmClient: lmClient}
}

func (c *client) ListTransactions(ctx context.Context, params *lm.ListTransactionsParams) (*lm.TransactionsListResponse, error) {
	return c.lmClient.Transactions.List(ctx, params)
}

func (c *client) GetCategory(ctx context.Context, id int32) (*lm.Category, error) {
	return c.lmClient.Categories.Get(ctx, id)
}

func (c *client) ListCategories(ctx context.Context, params *lm.ListCategoriesParams) (*lm.CategoriesListResponse, error) {
	return c.lmClient.Categories.List(ctx, params)
}

func (c *client) ListTags(ctx context.Context) (*lm.TagsListResponse, error) {
	return c.lmClient.Tags.List(ctx)
}

func (c *client) UpdateTransaction(ctx context.Context, id int64, body lm.UpdateTransactionBody) (*lm.Transaction, error) {
	return c.lmClient.Transactions.UpdateTransaction(ctx, id, body)
}
