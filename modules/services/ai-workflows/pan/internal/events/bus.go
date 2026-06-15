package events

import (
	"sync"
)

// Event is the interface all domain events must implement.
type Event interface {
	Name() string
}

// Handler is a function that processes an event.
type Handler func(Event)

// Bus manages subscriptions and publishing of events.
type Bus struct {
	mu       sync.RWMutex
	handlers map[string][]Handler
}

// NewBus creates a new EventBus.
func NewBus() *Bus {
	return &Bus{
		handlers: make(map[string][]Handler),
	}
}

// Subscribe registers a strongly-typed handler for a specific event type.
// It avoids reflection by using the event's Name() method as the routing key.
func Subscribe[T Event](bus *Bus, handler func(T)) {
	bus.mu.Lock()
	defer bus.mu.Unlock()

	var zero T
	eventName := zero.Name()

	// We use a small wrapper so your handler stays strongly typed.
	// This hides the interface type assertion from your observer code.
	wrapper := func(e Event) {
		handler(e.(T))
	}

	bus.handlers[eventName] = append(bus.handlers[eventName], wrapper)
}

// Publish broadcasts an event to all registered handlers for that event type.
func (b *Bus) Publish(e Event) {
	b.mu.RLock()
	defer b.mu.RUnlock()

	eventName := e.Name()
	for _, handler := range b.handlers[eventName] {
		// Run asynchronously so publishers are never blocked
		go handler(e)
	}
}

// DefaultBus is a global bus for convenience.
var DefaultBus = NewBus()

// Publish is a convenience wrapper around DefaultBus.Publish
func Publish(e Event) {
	DefaultBus.Publish(e)
}
