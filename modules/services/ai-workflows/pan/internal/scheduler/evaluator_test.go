package scheduler

import (
	"context"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestEvaluatorRegistry(t *testing.T) {
	registry := NewEvaluatorRegistry()

	registry.Register("alwaysTrue", func(ctx context.Context) (bool, error) {
		return true, nil
	})
	
	registry.Register("alwaysFalse", func(ctx context.Context) (bool, error) {
		return false, nil
	})

	tests := []struct {
		name          string
		condition     string
		expectedBool  bool
		expectedError string
	}{
		{
			name:          "returns true for alwaysTrue",
			condition:     "alwaysTrue",
			expectedBool:  true,
			expectedError: "",
		},
		{
			name:          "returns false for alwaysFalse",
			condition:     "alwaysFalse",
			expectedBool:  false,
			expectedError: "",
		},
		{
			name:          "returns error for unknown condition",
			condition:     "unknown",
			expectedBool:  false,
			expectedError: "unknown condition: unknown",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			res, err := registry.Evaluate(context.Background(), tc.condition)

			if tc.expectedError != "" {
				assert.EqualError(t, err, tc.expectedError)
			} else {
				assert.NoError(t, err)
			}

			assert.Equal(t, tc.expectedBool, res)
		})
	}
}
