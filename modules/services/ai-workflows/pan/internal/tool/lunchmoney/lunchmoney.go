package lunchmoney

import (
	"encoding/json"
	"fmt"
	"time"

	"github.com/dgraph-io/ristretto"
	"go.opentelemetry.io/otel"
	"go.opentelemetry.io/otel/attribute"
	"google.golang.org/adk/tool"

	"pan/internal/events"

	lm "github.com/Cidan/lunchmoney-go"
)

type LunchMoneyToolset struct {
	client *lm.Client
	cache  *ristretto.Cache
}

func New(client *lm.Client) (LunchMoneyToolset, error) {
	cache, err := ristretto.NewCache(&ristretto.Config{
		NumCounters: 1e7,
		MaxCost:     1 << 30,
		BufferItems: 64,
	})
	if err != nil {
		return LunchMoneyToolset{}, fmt.Errorf("failed to initialize ristretto cache: %w", err)
	}
	return LunchMoneyToolset{client: client, cache: cache}, nil
}

type GetLatestUnreviewedTransactionInput struct{}

type TransactionSummary struct {
	ID       int64   `json:"id" jsonschema:"The transaction ID"`
	Date     string  `json:"date" jsonschema:"The transaction date"`
	Amount   string  `json:"amount" jsonschema:"The transaction amount"`
	Payee    string  `json:"payee" jsonschema:"The payee name"`
	Category *string `json:"category" jsonschema:"The category name"`
	Notes    string  `json:"notes" jsonschema:"Current notes"`
	Status   string  `json:"status" jsonschema:"Current status"`
}

type GetLatestUnreviewedTransactionOutput struct {
	Transaction *TransactionSummary `json:"transaction,omitempty"`
}

func (t *LunchMoneyToolset) GetLatestUnreviewedTransaction(ctx tool.Context, input GetLatestUnreviewedTransactionInput) (GetLatestUnreviewedTransactionOutput, error) {
	ctxTr, span := otel.Tracer("pan.agent.lunchmoney").Start(ctx, "GetLatestUnreviewedTransaction")
	defer span.End()

	events.Publish(events.ToolInvokedEvent{
		ToolName:   "GetLatestUnreviewedTransaction",
		Attributes: map[string]any{"component": "AgentTool"},
	})

	st := lm.TransactionStatus("unreviewed")
	limit := int32(1)
	resp, err := t.client.Transactions.List(ctxTr, &lm.ListTransactionsParams{
		Status: &st,
		Limit:  &limit,
	})

	if err != nil {
		events.Publish(events.ToolCompletedEvent{ToolName: "GetLatestUnreviewedTransaction", Success: false, Error: err})
		return GetLatestUnreviewedTransactionOutput{}, err
	}

	var txnSummary *TransactionSummary
	if len(resp.Transactions) > 0 {
		txn := &resp.Transactions[0]

		var category *string
		if txn.CategoryId != nil {
			categoryResp, err := t.client.Categories.Get(ctx, *txn.CategoryId)
			if err != nil {
				events.Publish(events.ToolCompletedEvent{ToolName: "GetLatestUnreviewedTransaction", Success: false, Error: err})
				return GetLatestUnreviewedTransactionOutput{}, err
			}
			category = &categoryResp.Name
		}

		threadID, ok := ctx.Value("matrix_thread_id").(string)
		if ok && threadID != "" {
			cacheKey := fmt.Sprintf("active_txn_%s", threadID)
			t.cache.SetWithTTL(cacheKey, txn.Id, 1, 24*time.Hour)
		}

		payee := txn.Payee
		notes := ""
		if txn.Notes != nil {
			notes = *txn.Notes
		}
		status := string(txn.Status)
		txnSummary = &TransactionSummary{
			ID:       txn.Id,
			Date:     txn.Date.String(),
			Amount:   txn.Amount,
			Payee:    payee,
			Notes:    notes,
			Category: category,
			Status:   status,
		}
	}

	events.Publish(events.ToolCompletedEvent{
		ToolName:   "GetLatestUnreviewedTransaction",
		Success:    true,
		Attributes: map[string]any{"found": txnSummary != nil, "component": "AgentTool"},
	})

	return GetLatestUnreviewedTransactionOutput{Transaction: txnSummary}, nil
}

type CategorySummary struct {
	ID          int32  `json:"id" jsonschema:"The category ID"`
	Name        string `json:"name" jsonschema:"The category name"`
	Description string `json:"description" jsonschema:"The category description"`
	IsGroup     bool   `json:"is_group" jsonschema:"Whether it's a category group"`
}

type GetCategoriesInput struct{}

type GetCategoriesOutput struct {
	Categories []CategorySummary `json:"categories"`
}

func (t *LunchMoneyToolset) GetCategories(ctx tool.Context, input GetCategoriesInput) (GetCategoriesOutput, error) {
	ctxTr, span := otel.Tracer("pan.agent.lunchmoney").Start(ctx, "GetCategories")
	defer span.End()

	events.Publish(events.ToolInvokedEvent{
		ToolName:   "GetCategories",
		Attributes: map[string]any{"component": "AgentTool"},
	})

	resp, err := t.client.Categories.List(ctxTr, nil)

	events.Publish(events.ToolCompletedEvent{
		ToolName:   "GetCategories",
		Success:    err == nil,
		Error:      err,
		Attributes: map[string]any{"component": "AgentTool"},
	})

	if err != nil {
		return GetCategoriesOutput{}, err
	}

	var summaries []CategorySummary
	for _, c := range resp.Categories {
		desc := ""
		if c.Description != nil {
			desc = *c.Description
		}
		summaries = append(summaries, CategorySummary{
			ID:          c.Id,
			Name:        c.Name,
			Description: desc,
			IsGroup:     c.IsGroup,
		})
	}

	return GetCategoriesOutput{Categories: summaries}, nil
}

type TagSummary struct {
	ID          int32  `json:"id" jsonschema:"The tag ID"`
	Name        string `json:"name" jsonschema:"The tag name"`
	Description string `json:"description" jsonschema:"The tag description"`
}

type GetTagsInput struct{}

type GetTagsOutput struct {
	Tags []TagSummary `json:"tags"`
}

func (t *LunchMoneyToolset) GetTags(ctx tool.Context, input GetTagsInput) (GetTagsOutput, error) {
	ctxTr, span := otel.Tracer("pan.agent.lunchmoney").Start(ctx, "GetTags")
	defer span.End()

	events.Publish(events.ToolInvokedEvent{
		ToolName:   "GetTags",
		Attributes: map[string]any{"component": "AgentTool"},
	})

	resp, err := t.client.Tags.List(ctxTr)

	events.Publish(events.ToolCompletedEvent{
		ToolName:   "GetTags",
		Success:    err == nil,
		Error:      err,
		Attributes: map[string]any{"component": "AgentTool"},
	})

	if err != nil {
		return GetTagsOutput{}, err
	}

	var summaries []TagSummary
	for _, tg := range resp.Tags {
		desc := ""
		if tg.Description != nil {
			desc = *tg.Description
		}
		summaries = append(summaries, TagSummary{
			ID:          tg.Id,
			Name:        tg.Name,
			Description: desc,
		})
	}

	return GetTagsOutput{Tags: summaries}, nil
}

type UpdateTransactionInput struct {
	CategoryID *int32  `json:"category_id" jsonschema:"The ID of the category to assign"`
	TagIDs     []int32 `json:"tag_ids" jsonschema:"A list of tag IDs to assign"`
	Notes      *string `json:"notes" jsonschema:"A note to attach to the transaction"`
	Status     *string `json:"status" jsonschema:"The new status for the transaction (e.g. reviewed, unreviewed)"`
}

type UpdateTransactionOutput struct {
	Success bool   `json:"success"`
	Message string `json:"message"`
}

func (t *LunchMoneyToolset) UpdateTransaction(ctx tool.Context, input UpdateTransactionInput) (UpdateTransactionOutput, error) {
	ctxTr, span := otel.Tracer("pan.agent.lunchmoney").Start(ctx, "UpdateTransaction")
	defer span.End()

	threadID, ok := ctx.Value("matrix_thread_id").(string)
	if !ok || threadID == "" {
		return UpdateTransactionOutput{Success: false, Message: "failed to extract matrix_thread_id from context"}, fmt.Errorf("failed to extract matrix_thread_id from context")
	}

	var txnID int64
	cacheKey := fmt.Sprintf("active_txn_%s", threadID)
	if val, found := t.cache.Get(cacheKey); found && val != nil {
		txnID = val.(int64)
	}

	if txnID == 0 {
		return UpdateTransactionOutput{Success: false, Message: "no active transaction found. Please call GetLatestUnreviewedTransaction first."}, fmt.Errorf("no transaction id found in cache")
	}

	span.SetAttributes(attribute.Int64("transactionID", txnID))
	events.Publish(events.ToolInvokedEvent{
		ToolName:   "UpdateTransaction",
		Attributes: map[string]any{"transactionID": txnID, "component": "AgentTool"},
	})

	type proxyBody struct {
		CategoryId *int32   `json:"category_id,omitempty"`
		TagIds     *[]int32 `json:"tag_ids,omitempty"`
		Notes      *string  `json:"notes,omitempty"`
		Status     *string  `json:"status,omitempty"`
	}

	pb := proxyBody{
		CategoryId: input.CategoryID,
		Notes:      input.Notes,
		Status:     input.Status,
	}

	if len(input.TagIDs) > 0 {
		pb.TagIds = &input.TagIDs
	}

	var body lm.UpdateTransactionBody
	b, _ := json.Marshal(pb)
	_ = json.Unmarshal(b, &body)

	_, err := t.client.Transactions.UpdateTransaction(ctxTr, txnID, body)

	events.Publish(events.ToolCompletedEvent{
		ToolName:   "UpdateTransaction",
		Success:    err == nil,
		Error:      err,
		Attributes: map[string]any{"transactionID": txnID, "component": "AgentTool"},
	})

	if err != nil {
		return UpdateTransactionOutput{Success: false, Message: err.Error()}, err
	}

	t.cache.Del(cacheKey)

	return UpdateTransactionOutput{Success: true, Message: "Transaction successfully updated"}, nil
}
