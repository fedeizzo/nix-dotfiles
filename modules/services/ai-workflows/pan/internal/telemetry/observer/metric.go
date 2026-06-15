package observer

import (
	"pan/internal/events"
	"pan/internal/telemetry"
)

// SetupMetrics registers the metric counters to the event bus.
func SetupMetrics(bus *events.Bus) {
	events.Subscribe(bus, func(e events.SystemErrorEvent) {
		telemetry.IncCounter("system.error."+e.Component, 1)
	})

	events.Subscribe(bus, func(e events.ToolInvokedEvent) {
		telemetry.IncCounter("tool.invoked."+e.ToolName, 1)
	})

	events.Subscribe(bus, func(e events.ToolCompletedEvent) {
		telemetry.IncCounter("tool.completed."+e.ToolName, 1)
		if !e.Success {
			telemetry.IncCounter("tool.error."+e.ToolName, 1)
		}
	})
}
