package email

import (
	_ "embed"
	"fmt"

	"google.golang.org/adk/agent"
	"google.golang.org/adk/agent/llmagent"
	"google.golang.org/adk/model"
	"google.golang.org/adk/tool"
	"google.golang.org/adk/tool/agenttool"
	"google.golang.org/adk/tool/functiontool"

	emailtool "pan/internal/tool/email"
)

//go:embed prompt.md
var prompt string

func New(llmModel model.LLM, emailToolset emailtool.EmailToolset) (agent.Agent, error) {
	triageTool, err := functiontool.New(functiontool.Config{
		Name:        "suggest_triage",
		Description: "Suggests the triage evaluation for an email based on its content.",
	}, emailToolset.SuggestTriage)
	if err != nil {
		return nil, fmt.Errorf("failed to create triage tool: %w", err)
	}

	getUnreadTool, err := functiontool.New(functiontool.Config{
		Name:        "get_unread_email",
		Description: "Fetches one unread email for a given tag/mailbox (e.g., 'Inbox')",
	}, emailToolset.GetUnreadEmail)
	if err != nil {
		return nil, fmt.Errorf("failed to create get_unread_email tool: %w", err)
	}

	markSeenTool, err := functiontool.New(functiontool.Config{
		Name:                "mark_email_as_seen",
		Description:         "Marks the currently active email as seen/read. Takes no arguments because the system remembers the active email ID automatically.",
		RequireConfirmation: true,
	}, emailToolset.MarkEmailAsSeen)
	if err != nil {
		return nil, fmt.Errorf("failed to create mark_email_as_seen tool: %w", err)
	}

	addTagTool, err := functiontool.New(functiontool.Config{
		Name:                "add_tag_to_email",
		Description:         "Adds a specific tag or mailbox (like 'Urgent', 'Receipts', or 'Archive') to the currently active email. Do not provide the email ID, the system remembers it automatically.",
		RequireConfirmation: true,
	}, emailToolset.AddTagToEmail)
	if err != nil {
		return nil, fmt.Errorf("failed to create add_tag_to_email tool: %w", err)
	}

	a, err := llmagent.New(llmagent.Config{
		Name:        "email_assistant",
		Model:       llmModel,
		Description: "Delegates tasks related to reading, triaging, tagging, and managing emails via Fastmail.",
		Instruction: prompt,
		Tools: []tool.Tool{
			triageTool,
			getUnreadTool,
			markSeenTool,
			addTagTool,
		},
	})
	if err != nil {
		return nil, fmt.Errorf("failed to create agent: %w", err)
	}

	return a, nil
}

func NewEmailTool(llmModel model.LLM, emailToolset emailtool.EmailToolset) (tool.Tool, error) {
	a, err := New(llmModel, emailToolset)
	if err != nil {
		return nil, err
	}

	return agenttool.New(a, nil), nil
}
