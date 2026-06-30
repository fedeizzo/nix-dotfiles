//go:build e2e

package email_test

import (
	"context"
	"net/http"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/mock"
	"go.uber.org/goleak"

	"github.com/achetronic/adk-utils-go/genai/openai"
	adkagent "google.golang.org/adk/agent"
	"google.golang.org/adk/runner"
	"google.golang.org/adk/session"

	"github.com/samber/ro"

	agentemail "pan/internal/agent/email"
	"pan/internal/config"
	"pan/internal/fastmail"
	fastmailmocks "pan/internal/fastmail/mocks"
	emailtool "pan/internal/tool/email"

	genai "google.golang.org/genai"
)

func TestMain(m *testing.M) {
	goleak.VerifyTestMain(m, goleak.IgnoreCurrent(),
		goleak.IgnoreTopFunction("net/http.(*http2clientConnReadLoop).run"),
		goleak.IgnoreTopFunction("github.com/dgraph-io/ristretto.(*defaultPolicy).processItems"),
		goleak.IgnoreTopFunction("github.com/dgraph-io/ristretto.(*Cache).processItems"),
	)
}

func runAgent(t *testing.T, r *runner.Runner, sessionID, input string) string {
	msg := &genai.Content{
		Role: "user",
		Parts: []*genai.Part{
			{Text: input},
		},
	}

	var fullText string

	// Safety timeout for E2E tests to prevent infinite hang
	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Minute)
	defer cancel()
	ctx = context.WithValue(ctx, "matrix_thread_id", sessionID)

	for {
		stream := r.Run(ctx, "user", sessionID, msg, adkagent.RunConfig{})

		var funcCallID string
		for ev, err := range stream {
			assert.NoError(t, err)
			if err != nil || ev == nil {
				break
			}

			if ev.Content != nil {
				for _, p := range ev.Content.Parts {
					if p.Text != "" {
						fullText += p.Text
					}
					if p.FunctionCall != nil && p.FunctionCall.Name == "adk_request_confirmation" {
						funcCallID = p.FunctionCall.ID
					}
				}
			}
		}

		if funcCallID != "" {
			msg = &genai.Content{
				Role: "user",
				Parts: []*genai.Part{
					{
						FunctionResponse: &genai.FunctionResponse{
							ID:   funcCallID,
							Name: "adk_request_confirmation",
							Response: map[string]any{
								"confirmed": true,
								"payload": map[string]any{
									"approved": true,
									"feedback": "yes",
								},
							},
						},
					},
				},
			}
		} else {
			break
		}
	}

	return fullText
}

func TestEmailAgent(t *testing.T) {
	t.Parallel()
	t.Cleanup(func() {
		http.DefaultClient.CloseIdleConnections()
	})

	cfg, err := config.Load("")
	if err != nil {
		t.Fatalf("Failed to load config: %v", err)
	}
	if cfg.Models.OpenAIBase == "" {
		t.Skip("Skipping E2E test; local model URL not configured")
	}

	llmModel := openai.New(openai.Config{
		APIKey:    cfg.Models.OpenAIKey,
		BaseURL:   cfg.Models.OpenAIBase,
		ModelName: cfg.Models.Name,
	})

	type turn struct {
		input  string
		verify func(t *testing.T, output string)
	}

	tests := []struct {
		name       string
		setupMocks func(m *fastmailmocks.MockService)
		turns      []turn
	}{
		{
			name: "get unread email",
			setupMocks: func(m *fastmailmocks.MockService) {
				m.On("GetTags", mock.Anything).Return([]fastmail.Tag{
					{ID: "tag_inbox", Name: "Inbox"},
				}, nil)

				m.On("GetUnreadEmailForTag", mock.Anything, mock.Anything).Return(&fastmail.Email{
					ID:      "email_123",
					Subject: "Hello from Alice",
					Preview: "Are we still on for tomorrow?",
				}, nil)
			},
			turns: []turn{
				{
					input: "Can you get my unread emails from Inbox?",
					verify: func(t *testing.T, output string) {
						assert.Contains(t, output, "Hello from Alice")
					},
				},
			},
		},
	}

	for _, tt := range tests {
		tt := tt
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			mockService := fastmailmocks.NewMockService(t)
			tt.setupMocks(mockService)

			stream := ro.NewSubject[any]()
			toolset, err := emailtool.New(mockService, stream)
			assert.NoError(t, err)

			agent, err := agentemail.New(llmModel, toolset)
			assert.NoError(t, err)

			sessSvc := session.InMemoryService()
			r, err := runner.New(runner.Config{
				AppName:        "test",
				Agent:          agent,
				SessionService: sessSvc,
			})
			assert.NoError(t, err)

			sess, err := sessSvc.Create(context.Background(), &session.CreateRequest{AppName: "test", UserID: "user"})
			assert.NoError(t, err)

			for _, turn := range tt.turns {
				output := runAgent(t, r, sess.Session.ID(), turn.input)
				if turn.verify != nil {
					turn.verify(t, output)
				}
			}
		})
	}
}
