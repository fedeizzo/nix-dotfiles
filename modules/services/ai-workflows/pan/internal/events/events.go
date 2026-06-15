package events

// --- Define your domain events here ---

// EmailProcessedEvent is fired when an email is processed.
type EmailProcessedEvent struct {
	Success bool
}

func (e EmailProcessedEvent) Name() string { return "EmailProcessedEvent" }

// AgentActionCompletedEvent is fired when the agent finishes a thought/action.
type AgentActionCompletedEvent struct {
	ActionName string
	Success    bool
}

func (e AgentActionCompletedEvent) Name() string { return "AgentActionCompletedEvent" }

// SystemErrorEvent is fired on global unhandled or significant errors.
type SystemErrorEvent struct {
	Component string
	Error     error
}

func (e SystemErrorEvent) Name() string { return "SystemErrorEvent" }

// ToolInvokedEvent is fired when an agent tool starts.
type ToolInvokedEvent struct {
	ToolName   string
	Attributes map[string]any
}
func (e ToolInvokedEvent) Name() string { return "ToolInvokedEvent" }

// ToolCompletedEvent is fired when an agent tool finishes.
type ToolCompletedEvent struct {
	ToolName   string
	Success    bool
	Error      error
	Attributes map[string]any
}
func (e ToolCompletedEvent) Name() string { return "ToolCompletedEvent" }
