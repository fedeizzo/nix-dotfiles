package observer

import (
	"pan/internal/stream"
	"pan/internal/telemetry"
	pantool "pan/internal/tool"

	"github.com/samber/ro"
)

// SetupMetrics registers the metric counters to the app event stream.
func SetupMetrics(appStream ro.Observable[any]) {
	stream.OfType[telemetry.SystemErrorEvent](appStream).Subscribe(ro.OnNext(
		func(e telemetry.SystemErrorEvent) {
			telemetry.IncCounter("system.error."+e.Component, 1)
		},
	))

	stream.OfType[pantool.InvokedEvent](appStream).Subscribe(ro.OnNext(
		func(e pantool.InvokedEvent) {
			telemetry.IncCounter("tool.invoked."+e.ToolName, 1)
		},
	))

	stream.OfType[pantool.CompletedEvent](appStream).Subscribe(ro.OnNext(
		func(e pantool.CompletedEvent) {
			telemetry.IncCounter("tool.completed."+e.ToolName, 1)
			if !e.Success {
				telemetry.IncCounter("tool.error."+e.ToolName, 1)
			}
		},
	))
}
