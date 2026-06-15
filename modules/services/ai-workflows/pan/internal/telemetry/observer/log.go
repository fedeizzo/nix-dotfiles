package observer

import (
	"log/slog"
	"pan/internal/events"
)

// SetupLogging registers structured logging to the event bus.
func SetupLogging(bus *events.Bus) {
	events.Subscribe(bus, func(e events.SystemErrorEvent) {
		slog.Error("System error occurred", "component", e.Component, "error", e.Error)
	})

	events.Subscribe(bus, func(e events.ToolInvokedEvent) {
		args := []any{"tool", e.ToolName}
		for k, v := range e.Attributes {
			args = append(args, k, v)
		}
		slog.Info("Tool invoked", args...)
	})

	events.Subscribe(bus, func(e events.ToolCompletedEvent) {
		args := []any{"tool", e.ToolName, "success", e.Success}
		if e.Error != nil {
			args = append(args, "error", e.Error)
		}
		for k, v := range e.Attributes {
			args = append(args, k, v)
		}
		if e.Success {
			slog.Info("Tool completed", args...)
		} else {
			slog.Error("Tool failed", args...)
		}
	})
}
