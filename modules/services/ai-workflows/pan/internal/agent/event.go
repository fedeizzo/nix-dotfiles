package agent

// ActionCompletedEvent is fired when the agent finishes a thought/action.
type ActionCompletedEvent struct {
	ActionName string
	Success    bool
}

func (e ActionCompletedEvent) Name() string { return "AgentActionCompletedEvent" }
