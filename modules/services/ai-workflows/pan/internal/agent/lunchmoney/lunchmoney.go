package lunchmoney

import (
	_ "embed"

	"github.com/samber/oops"
	"google.golang.org/adk/agent"
	"google.golang.org/adk/agent/llmagent"
	"google.golang.org/adk/model"
	"google.golang.org/adk/tool"
	"google.golang.org/adk/tool/agenttool"
	"google.golang.org/adk/tool/functiontool"

	lmtool "pan/internal/tool/lunchmoney"
)

//go:embed prompt.md
var prompt string

type LunchMoneyToolset interface {
	GetLatestUnreviewedTransaction(ctx tool.Context, input lmtool.GetLatestUnreviewedTransactionInput) (lmtool.GetLatestUnreviewedTransactionOutput, error)
	GetCategories(ctx tool.Context, input lmtool.GetCategoriesInput) (lmtool.GetCategoriesOutput, error)
	GetTags(ctx tool.Context, input lmtool.GetTagsInput) (lmtool.GetTagsOutput, error)
	UpdateTransaction(ctx tool.Context, input lmtool.UpdateTransactionInput) (lmtool.UpdateTransactionOutput, error)
}

func New(llmModel model.LLM, lmToolset LunchMoneyToolset) (agent.Agent, error) {
	getTxnTool, err := functiontool.New(functiontool.Config{
		Name:        "get_latest_unreviewed_transaction",
		Description: "Fetches the most recent unreviewed transaction from LunchMoney.",
	}, lmToolset.GetLatestUnreviewedTransaction)
	if err != nil {
		return nil, oops.In("agent").Wrapf(err, "failed to create get_latest_unreviewed_transaction tool")
	}

	getCategoriesTool, err := functiontool.New(functiontool.Config{
		Name:        "get_categories",
		Description: "Fetches all available categories in LunchMoney.",
	}, lmToolset.GetCategories)
	if err != nil {
		return nil, oops.In("agent").Wrapf(err, "failed to create get_categories tool")
	}

	getTagsTool, err := functiontool.New(functiontool.Config{
		Name:        "get_tags",
		Description: "Fetches all available tags in LunchMoney.",
	}, lmToolset.GetTags)
	if err != nil {
		return nil, oops.In("agent").Wrapf(err, "failed to create get_tags tool")
	}

	updateTxnTool, err := functiontool.New(functiontool.Config{
		Name:                "update_transaction",
		Description:         "Updates the currently active transaction with a new category, tags, notes, or status. The active transaction ID is remembered automatically.",
		RequireConfirmation: true,
	}, lmToolset.UpdateTransaction)
	if err != nil {
		return nil, oops.In("agent").Wrapf(err, "failed to create update_transaction tool")
	}

	a, err := llmagent.New(llmagent.Config{
		Name:        "lunchmoney_assistant",
		Model:       llmModel,
		Description: "Delegates tasks related to reviewing and categorizing transactions in LunchMoney.",
		Instruction: prompt,
		Tools: []tool.Tool{
			getTxnTool,
			getCategoriesTool,
			getTagsTool,
			updateTxnTool,
		},
	})
	if err != nil {
		return nil, oops.In("agent").Wrapf(err, "failed to create agent")
	}

	return a, nil
}

func NewLunchMoneyTool(llmModel model.LLM, lmToolset LunchMoneyToolset) (tool.Tool, error) {
	a, err := New(llmModel, lmToolset)
	if err != nil {
		return nil, err
	}

	return agenttool.New(a, nil), nil
}
