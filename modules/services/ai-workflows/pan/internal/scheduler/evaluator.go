package scheduler

import (
	"context"
	"fmt"
)

type ConditionFunc func(ctx context.Context) (bool, error)

type EvaluatorRegistry struct {
	evaluators map[string]ConditionFunc
}

func NewEvaluatorRegistry() *EvaluatorRegistry {
	return &EvaluatorRegistry{
		evaluators: make(map[string]ConditionFunc),
	}
}

func (r *EvaluatorRegistry) Register(name string, fn ConditionFunc) {
	r.evaluators[name] = fn
}

func (r *EvaluatorRegistry) Evaluate(ctx context.Context, condition string) (bool, error) {
	fn, ok := r.evaluators[condition]
	if !ok {
		return false, fmt.Errorf("unknown condition: %s", condition)
	}
	return fn(ctx)
}
