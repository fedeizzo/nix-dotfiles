package email

import (
	"fmt"
	"strings"
	"time"

	pantool "pan/internal/tool"

	"github.com/samber/oops"
	"github.com/samber/ro"

	"github.com/dgraph-io/ristretto"
	"go.opentelemetry.io/otel"
	"go.opentelemetry.io/otel/attribute"
	"google.golang.org/adk/tool"

	"pan/internal/fastmail"
)

type EmailToolset struct {
	service fastmail.Service
	cache   *ristretto.Cache
	stream  ro.Subject[any]
}

func New(service fastmail.Service, stream ro.Subject[any]) (EmailToolset, error) {
	cache, err := ristretto.NewCache(&ristretto.Config{
		NumCounters: 1e7,     // number of keys to track frequency of (10M).
		MaxCost:     1 << 30, // maximum cost of cache (1GB).
		BufferItems: 64,      // number of keys per Get buffer.
	})
	if err != nil {
		return EmailToolset{}, oops.In("email-toolset").Wrapf(err, "failed to initialize ristretto cache")
	}
	return EmailToolset{service: service, cache: cache, stream: stream}, nil
}

type SuggestTriageInput struct {
	ImmediateAction    bool     `json:"immediate_action" jsonschema:"Email needs response within 1 hour."`
	ActionWithDeadline bool     `json:"action_with_deadline" jsonschema:"Future action with specific deadline (meeting RSVP due date)."`
	ShouldBeATodoItem  bool     `json:"should_be_a_todo_item" jsonschema:"Actionable task without fixed time."`
	Start              *string  `json:"start,omitempty" jsonschema:"Calendar event start (ISO 8601: YYYY-MM-DDTHH:MM:SSZ). Only if action_with_deadline is true."`
	End                *string  `json:"end,omitempty" jsonschema:"Calendar event end (ISO 8601: YYYY-MM-DDTHH:MM:SSZ). Only if action_with_deadline is true."`
	Location           *string  `json:"location,omitempty" jsonschema:"Location for calendar event. Only if info exists in email."`
	TodoDate           *string  `json:"todo_date,omitempty" jsonschema:"Target date for todo item (YYYY-MM-DD). Only if should_be_a_todo_item is true."`
	EventName          *string  `json:"event_name,omitempty" jsonschema:"Short name for calendar event or task. Maximum length 100 characters."`
	Description        *string  `json:"description,omitempty" jsonschema:"Details for the event/task. Maximum length 500 characters."`
	Reason             string   `json:"reason" jsonschema:"Short explanation for mobile notification. Maximum length 200 characters."`
	Priority           int      `json:"priority" jsonschema:"5 is urgent, 4 is high, 3 is default, 2 is low, 1 is min. Must be between 1 and 5."`
	Tags               []string `json:"tags" jsonschema:"Categorized tags. Use format category:specific. Provide 1 to 5 unique tags."`
}

type SuggestTriageOutput struct {
	Result string `json:"result" jsonschema:"The result of the triage operation"`
}

type GetUnreadEmailInput struct {
	TagName string `json:"tag_name" jsonschema:"The name of the tag/mailbox to get unread email for (e.g. Inbox)"`
}

type GetUnreadEmailOutput struct {
	Email *fastmail.Email `json:"email,omitempty" jsonschema:"The unread email, or null if none found"`
}

func (e *EmailToolset) SuggestTriage(ctx tool.Context, input SuggestTriageInput) (SuggestTriageOutput, error) {
	_, span := otel.Tracer("pan.agent.email").Start(ctx, "SuggestTriage")
	defer span.End()

	e.stream.Next(pantool.InvokedEvent{
		ToolName:   "SuggestTriage",
		Attributes: map[string]any{"component": "AgentTool"},
	})

	resultMsg := "Email successfully triaged and validated. You may now proceed to add tags or mark as seen."

	if memResp, err := ctx.SearchMemory(ctx, input.Reason); err == nil && memResp != nil {
		if len(memResp.Memories) > 0 {
			if txt := memResp.Memories[0].Content; txt != nil {
				resultMsg += fmt.Sprintf(" Context from memory: %v", txt)
			}
		}
	}

	e.stream.Next(pantool.CompletedEvent{
		ToolName:   "SuggestTriage",
		Success:    true,
		Attributes: map[string]any{"priority": input.Priority, "component": "AgentTool"},
	})

	return SuggestTriageOutput{
		Result: resultMsg,
	}, nil
}

func (e *EmailToolset) GetUnreadEmail(ctx tool.Context, input GetUnreadEmailInput) (GetUnreadEmailOutput, error) {
	ctxTr, span := otel.Tracer("pan.agent.email").Start(ctx, "GetUnreadEmail")
	span.SetAttributes(attribute.String("mailbox", input.TagName))
	defer span.End()

	e.stream.Next(pantool.InvokedEvent{
		ToolName:   "GetUnreadEmail",
		Attributes: map[string]any{"mailbox": input.TagName, "component": "AgentTool"},
	})

	tags, err := e.service.GetTags(ctxTr)
	if err != nil {
		e.stream.Next(pantool.CompletedEvent{ToolName: "GetUnreadEmail", Success: false, Error: err})
		return GetUnreadEmailOutput{}, err
	}
	var targetTag fastmail.Tag
	for _, t := range tags {
		if strings.EqualFold(t.Name, input.TagName) {
			targetTag = t
			break
		}
	}
	if targetTag.ID == "" {
		err := oops.In("email-toolset").Errorf("tag %q not found", input.TagName)
		e.stream.Next(pantool.CompletedEvent{ToolName: "GetUnreadEmail", Success: false, Error: err})
		return GetUnreadEmailOutput{}, err
	}
	mail, err := e.service.GetUnreadEmailForTag(ctxTr, targetTag)
	if err == nil && mail != nil {
		threadID, ok := ctx.Value("matrix_thread_id").(string)
		if !ok || threadID == "" {
			return GetUnreadEmailOutput{}, oops.In("email-toolset").Errorf("failed to extract matrix_thread_id from context")
		}
		sessionID := threadID
		cacheKey := fmt.Sprintf("active_email_%s", sessionID)
		e.cache.SetWithTTL(cacheKey, mail.ID, 1, 24*time.Hour)
	}

	e.stream.Next(pantool.CompletedEvent{
		ToolName:   "GetUnreadEmail",
		Success:    err == nil,
		Error:      err,
		Attributes: map[string]any{"found": mail != nil, "component": "AgentTool"},
	})

	return GetUnreadEmailOutput{Email: mail}, err
}

type MarkEmailAsSeenInput struct {
}

type MarkEmailAsSeenOutput struct {
	Success bool
	Message string
}

func (e *EmailToolset) MarkEmailAsSeen(ctx tool.Context, input MarkEmailAsSeenInput) (MarkEmailAsSeenOutput, error) {
	ctxTr, span := otel.Tracer("pan.agent.email").Start(ctx, "MarkEmailAsSeen")
	defer span.End()

	threadID, ok := ctx.Value("matrix_thread_id").(string)
	if !ok || threadID == "" {
		return MarkEmailAsSeenOutput{Success: false, Message: "failed to extract matrix_thread_id from context"}, oops.In("email-toolset").Errorf("failed to extract matrix_thread_id from context")
	}
	sessionID := threadID

	var emailID string
	cacheKey := fmt.Sprintf("active_email_%s", sessionID)
	if val, found := e.cache.Get(cacheKey); found && val != nil {
		emailID = val.(string)
	}

	if emailID == "" {
		return MarkEmailAsSeenOutput{Success: false, Message: "no email id found in cache"}, oops.In("email-toolset").Errorf("no email id found in cache")
	}

	span.SetAttributes(attribute.String("emailID", emailID))
	e.stream.Next(pantool.InvokedEvent{
		ToolName:   "MarkEmailAsSeen",
		Attributes: map[string]any{"emailID": emailID, "component": "AgentTool"},
	})

	err := e.service.MarkEmailAsSeen(ctxTr, emailID)

	e.stream.Next(pantool.CompletedEvent{
		ToolName:   "MarkEmailAsSeen",
		Success:    err == nil,
		Error:      err,
		Attributes: map[string]any{"emailID": emailID, "component": "AgentTool"},
	})

	if err != nil {
		return MarkEmailAsSeenOutput{Success: false, Message: err.Error()}, err
	}

	e.cache.Del(cacheKey)

	return MarkEmailAsSeenOutput{Success: true, Message: "Email successfully marked as seen"}, nil
}

type AddTagToEmailInput struct {
	TagName string `json:"tag_name" jsonschema:"Name of the tag to add (e.g. Urgent)"`
}

type AddTagToEmailOutput struct {
	Success bool
	Message string
}

func (e *EmailToolset) AddTagToEmail(ctx tool.Context, input AddTagToEmailInput) (AddTagToEmailOutput, error) {
	ctxTr, span := otel.Tracer("pan.agent.email").Start(ctx, "AddTagToEmail")
	span.SetAttributes(attribute.String("tagName", input.TagName))
	defer span.End()

	threadID, ok := ctx.Value("matrix_thread_id").(string)
	if !ok || threadID == "" {
		return AddTagToEmailOutput{Success: false, Message: "failed to extract matrix_thread_id from context"}, oops.In("email-toolset").Errorf("failed to extract matrix_thread_id from context")
	}
	sessionID := threadID

	var emailID string
	cacheKey := fmt.Sprintf("active_email_%s", sessionID)
	if val, found := e.cache.Get(cacheKey); found && val != nil {
		emailID = val.(string)
	}

	if emailID == "" {
		return AddTagToEmailOutput{Success: false, Message: "no email id found in cache"}, oops.In("email-toolset").Errorf("no email id found in cache")
	}

	span.SetAttributes(attribute.String("emailID", emailID))
	e.stream.Next(pantool.InvokedEvent{
		ToolName:   "AddTagToEmail",
		Attributes: map[string]any{"emailID": emailID, "tagName": input.TagName, "component": "AgentTool"},
	})

	tags, err := e.service.GetTags(ctxTr)
	if err != nil {
		e.stream.Next(pantool.CompletedEvent{ToolName: "AddTagToEmail", Success: false, Error: err})
		return AddTagToEmailOutput{Success: false, Message: "Failed to fetch tags"}, err
	}

	var targetTag fastmail.Tag
	for _, t := range tags {
		if strings.EqualFold(t.Name, input.TagName) {
			targetTag = t
			break
		}
	}

	if targetTag.ID == "" {
		err := oops.In("email-toolset").Errorf("tag %q not found", input.TagName)
		e.stream.Next(pantool.CompletedEvent{ToolName: "AddTagToEmail", Success: false, Error: err})
		return AddTagToEmailOutput{Success: false, Message: err.Error()}, err
	}

	err = e.service.AddTagToEmail(ctxTr, emailID, targetTag)

	e.stream.Next(pantool.CompletedEvent{
		ToolName:   "AddTagToEmail",
		Success:    err == nil,
		Error:      err,
		Attributes: map[string]any{"emailID": emailID, "tagName": input.TagName, "component": "AgentTool"},
	})

	if err != nil {
		return AddTagToEmailOutput{Success: false, Message: err.Error()}, err
	}

	return AddTagToEmailOutput{Success: true, Message: fmt.Sprintf("Tag %q successfully added to email", input.TagName)}, nil
}
