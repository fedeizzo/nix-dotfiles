package tool

// InvokedEvent is fired when an agent tool starts.
type InvokedEvent struct {
	ToolName   string
	Attributes map[string]any
}

func (e InvokedEvent) Name() string { return "ToolInvokedEvent" }

// CompletedEvent is fired when an agent tool finishes.
type CompletedEvent struct {
	ToolName   string
	Success    bool
	Error      error
	Attributes map[string]any
}

func (e CompletedEvent) Name() string { return "ToolCompletedEvent" }
