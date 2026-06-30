package observer

import (
	"log/slog"
	"pan/internal/fastmail"
	"pan/internal/stream"
	"pan/internal/telemetry"
	pantool "pan/internal/tool"

	"github.com/samber/ro"
)

// SetupLogging registers structured logging to the app event stream.
func SetupLogging(appStream ro.Observable[any]) {
	logger := slog.Default()

	stream.OfType[telemetry.SystemErrorEvent](appStream).Subscribe(ro.OnNext(
		func(e telemetry.SystemErrorEvent) {
			logger.Error("System error", "component", e.Component, "error", e.Error)
		},
	))

	stream.OfType[pantool.InvokedEvent](appStream).Subscribe(ro.OnNext(
		func(e pantool.InvokedEvent) {
			args := []any{"tool", e.ToolName}
			for k, v := range e.Attributes {
				args = append(args, k, v)
			}
			logger.Info("Tool invoked", args...)
		},
	))

	stream.OfType[pantool.CompletedEvent](appStream).Subscribe(ro.OnNext(
		func(e pantool.CompletedEvent) {
			args := []any{"tool", e.ToolName, "success", e.Success}
			for k, v := range e.Attributes {
				args = append(args, k, v)
			}
			logger.Info("Tool completed", args...)
		},
	))

	stream.OfType[fastmail.EmailProcessedEvent](appStream).Subscribe(ro.OnNext(
		func(e fastmail.EmailProcessedEvent) {
			logger.Info("Email processed", "success", e.Success)
		},
	))
}
