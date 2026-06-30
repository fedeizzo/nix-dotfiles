package stream

import (
	"context"
	"testing"

	"github.com/samber/ro"
	"github.com/stretchr/testify/assert"
)

type observer struct {
	next func(int)
}
func (o observer) Next(v int) { o.next(v) }
func (o observer) Error(err error) {}
func (o observer) Complete() {}
func (o observer) NextWithContext(ctx context.Context, v int) { o.next(v) }
func (o observer) ErrorWithContext(ctx context.Context, err error) {}
func (o observer) CompleteWithContext(ctx context.Context) {}
func (o observer) HasThrown() bool { return false }
func (o observer) IsClosed() bool { return false }
func (o observer) IsCompleted() bool { return false }

func TestOfType(t *testing.T) {
	subj := ro.NewPublishSubject[any]()
	
	// Create the filtered stream
	intStream := OfType[int](subj)
	
	// Record elements emitted by the filtered stream
	var results []int
	intStream.Subscribe(observer{
		next: func(v int) {
			results = append(results, v)
		},
	})

	// Emit mixed types
	subj.Next(1)
	subj.Next("hello")
	subj.Next(2)
	subj.Next(3.14)
	subj.Next(3)
	
	subj.Complete()

	assert.Equal(t, []int{1, 2, 3}, results, "should only contain ints emitted in order")
}
