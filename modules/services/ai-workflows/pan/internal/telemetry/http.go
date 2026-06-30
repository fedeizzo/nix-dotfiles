package telemetry

import (
	"encoding/json"
	"log/slog"
	"net/http"
)

// StartHTTP starts a simple HTTP server to expose the metrics.
// It should be run as a goroutine.
func StartHTTP(addr string) {
	mux := http.NewServeMux()

	mux.HandleFunc("/health", func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(http.StatusOK)
		w.Write([]byte(`{"status":"ok"}`))
	})

	mux.HandleFunc("/metrics", func(w http.ResponseWriter, r *http.Request) {
		store.mu.RLock()
		defer store.mu.RUnlock()

		w.Header().Set("Content-Type", "application/json")
		if err := json.NewEncoder(w).Encode(store); err != nil {
			slog.Error("Failed to encode metrics", "error", err)
		}
	})

	slog.Info("Starting telemetry HTTP server", "addr", addr)
	if err := http.ListenAndServe(addr, mux); err != nil {
		slog.Error("Telemetry HTTP server failed", "error", err)
	}
}
