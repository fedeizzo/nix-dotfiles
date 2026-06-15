package telemetry

// IncCounter increments a counter metric by the given value.
// It is non-blocking and fire-and-forget.
func IncCounter(name string, value float64) {
	select {
	case metricsChan <- MetricEvent{Name: name, Value: value, Type: "c"}:
		// Successfully buffered
	default:
		// Channel is full, drop the metric to prevent blocking
	}
}

// SetGauge sets a gauge metric to the given value.
// It is non-blocking and fire-and-forget.
func SetGauge(name string, value float64) {
	select {
	case metricsChan <- MetricEvent{Name: name, Value: value, Type: "g"}:
		// Successfully buffered
	default:
		// Channel is full, drop the metric to prevent blocking
	}
}
