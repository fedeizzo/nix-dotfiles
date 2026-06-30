package telemetry

import (
	"encoding/json"
	"errors"
	"net/http"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestTelemetry(t *testing.T) {
	// Reset global state
	store.mu.Lock()
	store.Counters = make(map[string]float64)
	store.Gauges = make(map[string]float64)
	store.mu.Unlock()

	// Drain the channel if any
	for len(metricsChan) > 0 {
		<-metricsChan
	}

	go StartProcessor()

	t.Run("IncCounter", func(t *testing.T) {
		IncCounter("test_counter", 1)
		IncCounter("test_counter", 2)
		
		// Give processor time to process
		time.Sleep(100 * time.Millisecond)

		store.mu.RLock()
		val := store.Counters["test_counter"]
		store.mu.RUnlock()

		assert.Equal(t, float64(3), val)
	})

	t.Run("SetGauge", func(t *testing.T) {
		SetGauge("test_gauge", 42.5)
		SetGauge("test_gauge", 10.1)

		time.Sleep(100 * time.Millisecond)

		store.mu.RLock()
		val := store.Gauges["test_gauge"]
		store.mu.RUnlock()

		assert.Equal(t, float64(10.1), val)
	})

	t.Run("HTTP Health and Metrics", func(t *testing.T) {
		go StartHTTP("127.0.0.1:19099")
		time.Sleep(100 * time.Millisecond)

		resp, err := http.Get("http://127.0.0.1:19099/health")
		require.NoError(t, err)
		defer resp.Body.Close()
		assert.Equal(t, http.StatusOK, resp.StatusCode)

		resp2, err := http.Get("http://127.0.0.1:19099/metrics")
		require.NoError(t, err)
		defer resp2.Body.Close()
		assert.Equal(t, http.StatusOK, resp2.StatusCode)
		
		var res map[string]map[string]float64
		require.NoError(t, json.NewDecoder(resp2.Body).Decode(&res))
		assert.Equal(t, float64(3), res["counters"]["test_counter"])
		assert.Equal(t, float64(10.1), res["gauges"]["test_gauge"])
	})
}

func TestSystemErrorEvent(t *testing.T) {
	ev := SystemErrorEvent{Component: "test", Error: errors.New("err")}
	assert.Equal(t, "SystemErrorEvent", ev.Name())
	assert.Equal(t, "test", ev.Component)
	assert.EqualError(t, ev.Error, "err")
}
