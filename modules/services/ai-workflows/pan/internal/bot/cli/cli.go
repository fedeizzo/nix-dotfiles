package cli

import (
	"bufio"
	"context"
	"encoding/json"
	"fmt"
	"log/slog"
	"os"
	"strings"

	"github.com/samber/oops"
	"google.golang.org/adk/agent"
	"google.golang.org/adk/runner"
	"google.golang.org/adk/session"
	"google.golang.org/genai"
)

type cliBot struct {
	r                    *runner.Runner
	pendingConfirmations map[string]string // sessionID -> funcCallID
}

func New(r *runner.Runner) *cliBot {
	return &cliBot{
		r:                    r,
		pendingConfirmations: make(map[string]string),
	}
}

func (c *cliBot) Start() error {
	reader := bufio.NewReader(os.Stdin)
	sessionID := "cli-session"
	userID := "user"
	ctx := context.Background()

	fmt.Println("CLI interface started. Type your message below (type 'exit' to quit):")

	for {
		input, err := c.readInput(reader)
		if err != nil {
			return oops.In("bot").Wrapf(err, "failed to read from stdin")
		}

		if input == "" {
			continue
		}
		if input == "exit" || input == "quit" {
			fmt.Println("Goodbye!")
			break
		}

		// Prepare the message (handling standard input or confirmation responses)
		msg := c.buildMessage(sessionID, input)

		// Run the agent
		stream := c.r.Run(ctx, userID, sessionID, msg, agent.RunConfig{})

		// Process the event stream
		for ev, err := range stream {
			if err != nil {
				slog.Error("Runner error processing stream", "error", err)
				continue
			}
			c.handleAgentEvent(sessionID, ev)
		}
	}

	return nil
}

// readInput handles the terminal prompt and reads a trimmed string.
func (c *cliBot) readInput(reader *bufio.Reader) (string, error) {
	fmt.Print("\n> ")
	input, err := reader.ReadString('\n')
	if err != nil {
		return "", err
	}
	return strings.TrimSpace(input), nil
}

// buildMessage creates the proper genai.Content based on whether we are
// waiting for a tool confirmation or just sending a normal message.
func (c *cliBot) buildMessage(sessionID, input string) *genai.Content {
	if funcCallID, hasConf := c.pendingConfirmations[sessionID]; hasConf {
		delete(c.pendingConfirmations, sessionID)

		approved := true
		lower := strings.ToLower(input)
		if lower == "no" || strings.HasPrefix(lower, "no ") {
			approved = false
		}

		return &genai.Content{
			Role: "user",
			Parts: []*genai.Part{
				{
					FunctionResponse: &genai.FunctionResponse{
						ID:   funcCallID,
						Name: "adk_request_confirmation",
						Response: map[string]any{
							"confirmed": true,
							"payload": map[string]any{
								"approved": approved,
								"feedback": input,
							},
						},
					},
				},
			},
		}
	}

	return &genai.Content{
		Role:  "user",
		Parts: []*genai.Part{{Text: input}},
	}
}

// handleAgentEvent parses a single event from the Runner's stream,
// printing textual responses and catching tool confirmation requests.
func (c *cliBot) handleAgentEvent(sessionID string, ev *session.Event) {
	if ev.Author == "user" || ev.Content == nil {
		return
	}

	for _, part := range ev.Content.Parts {
		if part.FunctionCall != nil && part.FunctionCall.Name == "adk_request_confirmation" {
			c.handleToolConfirmation(sessionID, part.FunctionCall)
		}

		if part.Text != "" {
			c.printText(part)
		}
	}

	if ev.IsFinalResponse() {
		fmt.Println()
	}
}

// handleToolConfirmation processes an adk_request_confirmation tool call
// and prompts the user for approval.
func (c *cliBot) handleToolConfirmation(sessionID string, funcCall *genai.FunctionCall) {
	if _, exists := c.pendingConfirmations[sessionID]; exists {
		return // Already pending
	}

	c.pendingConfirmations[sessionID] = funcCall.ID
	toolName := c.extractToolName(funcCall.Args["originalFunctionCall"])
	fmt.Printf("\n[Agent requires confirmation to proceed with %s. Type 'yes' to allow or 'no' to reject]", toolName)
}

// extractToolName attempts to extract the original tool name from the confirmation args
func (c *cliBot) extractToolName(origCall any) string {
	toolName := "an action"
	if origCall == nil {
		return toolName
	}

	if m, ok := origCall.(map[string]any); ok {
		if name, ok := m["name"].(string); ok && name != "" {
			return fmt.Sprintf("'%s'", name)
		}
		return toolName
	}

	// Fallback for strongly typed structs marshaled to JSON
	b, err := json.Marshal(origCall)
	if err == nil {
		var extracted struct {
			Name string `json:"name"`
		}
		if json.Unmarshal(b, &extracted) == nil && extracted.Name != "" {
			return fmt.Sprintf("'%s'", extracted.Name)
		}

		// Also try capitalized Name just in case
		var extracted2 struct {
			Name string `json:"Name"`
		}
		if json.Unmarshal(b, &extracted2) == nil && extracted2.Name != "" {
			return fmt.Sprintf("'%s'", extracted2.Name)
		}
	}

	return toolName
}

// printText formats and prints text and thought parts
func (c *cliBot) printText(part *genai.Part) {
	if part.Thought {
		fmt.Printf("\n[Agent is thinking...]\n%s\n", part.Text)
	} else {
		fmt.Printf("\nAgent: %s", part.Text)
	}
}
