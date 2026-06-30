package email

import (
	_ "embed"

	"github.com/samber/oops"
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
		return nil, oops.In("agent").Wrapf(err, "failed to create triage tool")
	}

	getUnreadTool, err := functiontool.New(functiontool.Config{
		Name:        "get_unread_email",
		Description: "Fetches one unread email for a given tag/mailbox (e.g., 'Inbox')",
	}, emailToolset.GetUnreadEmail)
	if err != nil {
		return nil, oops.In("agent").Wrapf(err, "failed to create get_unread_email tool")
	}

	markSeenTool, err := functiontool.New(functiontool.Config{
		Name:                "mark_email_as_seen",
		Description:         "Marks the currently active email as seen/read. Takes no arguments because the system remembers the active email ID automatically.",
		RequireConfirmation: true,
	}, emailToolset.MarkEmailAsSeen)
	if err != nil {
		return nil, oops.In("agent").Wrapf(err, "failed to create mark_email_as_seen tool")
	}

	addTagTool, err := functiontool.New(functiontool.Config{
		Name:                "add_tag_to_email",
		Description:         "Adds a specific tag or mailbox (like 'Urgent', 'Receipts', or 'Archive') to the currently active email. Do not provide the email ID, the system remembers it automatically.",
		RequireConfirmation: true,
	}, emailToolset.AddTagToEmail)
	if err != nil {
		return nil, oops.In("agent").Wrapf(err, "failed to create add_tag_to_email tool")
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
		return nil, oops.In("agent").Wrapf(err, "failed to create agent")
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
