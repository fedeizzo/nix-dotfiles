package telemetry

// SystemErrorEvent is fired on global unhandled or significant errors.
type SystemErrorEvent struct {
	Component string
	Error     error
}

func (e SystemErrorEvent) Name() string { return "SystemErrorEvent" }
