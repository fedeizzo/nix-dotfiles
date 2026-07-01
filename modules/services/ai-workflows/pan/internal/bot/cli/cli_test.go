package cli

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"google.golang.org/adk/runner"
	"google.golang.org/genai"
)

func TestExtractToolName(t *testing.T) {
	c := New(nil, &runner.Runner{}, "")

	tests := []struct {
		name     string
		origCall any
		expected string
	}{
		{
			name:     "nil input",
			origCall: nil,
			expected: "an action",
		},
		{
			name:     "map with lowercase name",
			origCall: map[string]any{"name": "my_tool"},
			expected: "'my_tool'",
		},
		{
			name:     "map without name",
			origCall: map[string]any{"other": "value"},
			expected: "an action",
		},
		{
			name: "struct with lowercase name tag",
			origCall: struct {
				Name string `json:"name"`
			}{Name: "struct_tool"},
			expected: "'struct_tool'",
		},
		{
			name: "struct with uppercase Name tag",
			origCall: struct {
				Name string `json:"Name"`
			}{Name: "struct_tool2"},
			expected: "'struct_tool2'",
		},
		{
			name: "unmarshalable struct",
			origCall: struct {
				Channel chan int
			}{Channel: make(chan int)},
			expected: "an action",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			result := c.extractToolName(tc.origCall)
			assert.Equal(t, tc.expected, result)
		})
	}
}

func TestBuildMessage(t *testing.T) {
	c := New(nil, &runner.Runner{}, "")

	t.Run("normal message", func(t *testing.T) {
		msg := c.buildMessage("session-1", "hello world")
		assert.Equal(t, "user", msg.Role)
		assert.Len(t, msg.Parts, 1)
		assert.Equal(t, "hello world", msg.Parts[0].Text)
	})

	t.Run("pending confirmation - approved", func(t *testing.T) {
		c.pendingConfirmations["session-2"] = "call-123"
		msg := c.buildMessage("session-2", "yes go ahead")

		assert.Empty(t, c.pendingConfirmations["session-2"])
		assert.Equal(t, "user", msg.Role)
		assert.Len(t, msg.Parts, 1)
		assert.NotNil(t, msg.Parts[0].FunctionResponse)
		assert.Equal(t, "call-123", msg.Parts[0].FunctionResponse.ID)
		assert.Equal(t, "adk_request_confirmation", msg.Parts[0].FunctionResponse.Name)

		respMap := msg.Parts[0].FunctionResponse.Response
		assert.True(t, respMap["confirmed"].(bool))

		payload := respMap["payload"].(map[string]any)
		assert.True(t, payload["approved"].(bool))
		assert.Equal(t, "yes go ahead", payload["feedback"].(string))
	})

	t.Run("pending confirmation - rejected", func(t *testing.T) {
		c.pendingConfirmations["session-3"] = "call-456"
		msg := c.buildMessage("session-3", "no stop")

		assert.Empty(t, c.pendingConfirmations["session-3"])
		assert.Equal(t, "user", msg.Role)
		assert.Len(t, msg.Parts, 1)

		respMap := msg.Parts[0].FunctionResponse.Response
		payload := respMap["payload"].(map[string]any)
		assert.False(t, payload["approved"].(bool))
		assert.Equal(t, "no stop", payload["feedback"].(string))
	})
}

func TestHandleToolConfirmation(t *testing.T) {
	c := New(nil, &runner.Runner{}, "")

	funcCall := &genai.FunctionCall{
		ID:   "call-789",
		Name: "adk_request_confirmation",
		Args: map[string]any{
			"originalFunctionCall": map[string]any{
				"name": "dangerous_action",
			},
		},
	}

	c.handleToolConfirmation("session-4", funcCall)
	assert.Equal(t, "call-789", c.pendingConfirmations["session-4"])

	// Try again, shouldn't overwrite
	funcCall2 := &genai.FunctionCall{
		ID: "call-999",
	}
	c.handleToolConfirmation("session-4", funcCall2)
	assert.Equal(t, "call-789", c.pendingConfirmations["session-4"])
}
