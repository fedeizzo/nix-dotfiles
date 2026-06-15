package telemetry

// MetricEvent represents a single telemetry event.
type MetricEvent struct {
	Name  string
	Value float64
	Type  string // "c" for counter, "g" for gauge
}

// metricsChan acts as our fire-and-forget buffer
var metricsChan = make(chan MetricEvent, 1000)

// StartProcessor listens on the channel and updates the store.
// It should be run as a goroutine.
func StartProcessor() {
	for event := range metricsChan {
		store.mu.Lock()
		if event.Type == "c" {
			store.Counters[event.Name] += event.Value
		} else if event.Type == "g" {
			store.Gauges[event.Name] = event.Value
		}
		store.mu.Unlock()
	}
}
