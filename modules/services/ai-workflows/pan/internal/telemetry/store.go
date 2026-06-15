package telemetry

import "sync"

// MetricsStore holds all in-memory metrics safely.
type MetricsStore struct {
	mu       sync.RWMutex
	Counters map[string]float64 `json:"counters"`
	Gauges   map[string]float64 `json:"gauges"`
}

// Global instance of the store
var store = MetricsStore{
	Counters: make(map[string]float64),
	Gauges:   make(map[string]float64),
}
