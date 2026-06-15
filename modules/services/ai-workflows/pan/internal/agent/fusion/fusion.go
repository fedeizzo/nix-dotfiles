package fusion

import (
	_ "embed"
	"fmt"

	"google.golang.org/adk/agent"
	"google.golang.org/adk/agent/llmagent"
	"google.golang.org/adk/model"
	"google.golang.org/adk/tool"
	"google.golang.org/adk/tool/agenttool"
	"google.golang.org/adk/tool/functiontool"

	fusiontool "pan/internal/tool/fusion"
)

//go:embed prompt.md
var prompt string

func New(llmModel model.LLM, fusionToolset fusiontool.FusionToolset) (agent.Agent, error) {
	getUnreadFeedsTool, err := functiontool.New(functiontool.Config{
		Name:        "GetUnreadFeeds",
		Description: "Fetches a batch of unread RSS feeds from Fusion.",
	}, fusionToolset.GetUnreadFeeds)
	if err != nil {
		return nil, fmt.Errorf("failed to create get_unread_feeds tool: %w", err)
	}

	markFeedsAsReadTool, err := functiontool.New(functiontool.Config{
		Name:                "MarkFeedsAsRead",
		Description:         "Marks the currently active batch of RSS feeds as read. Requires confirmation.",
		RequireConfirmation: true,
	}, fusionToolset.MarkFeedsAsRead)
	if err != nil {
		return nil, fmt.Errorf("failed to create mark_feeds_as_read tool: %w", err)
	}

	a, err := llmagent.New(llmagent.Config{
		Name:        "fusion_assistant",
		Model:       llmModel,
		Description: "Delegates tasks related to reading, triaging, and managing RSS feeds via Fusion.",
		Instruction: prompt,
		Tools: []tool.Tool{
			getUnreadFeedsTool,
			markFeedsAsReadTool,
		},
	})
	if err != nil {
		return nil, fmt.Errorf("failed to create agent: %w", err)
	}

	return a, nil
}

func NewFusionTool(llmModel model.LLM, fusionToolset fusiontool.FusionToolset) (tool.Tool, error) {
	a, err := New(llmModel, fusionToolset)
	if err != nil {
		return nil, err
	}

	return agenttool.New(a, nil), nil
}
