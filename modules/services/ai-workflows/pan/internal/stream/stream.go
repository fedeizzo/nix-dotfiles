package stream

import "github.com/samber/ro"

// OfType filters an any-stream for a specific type T and casts it to T.
func OfType[T any](stream ro.Observable[any]) ro.Observable[T] {
	return ro.Pipe2(
		stream,
		ro.Filter(func(e any) bool { _, ok := e.(T); return ok }),
		ro.Map(func(e any) T { return e.(T) }),
	)
}
