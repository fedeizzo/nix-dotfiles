//go:build e2e

package fusion_test

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

	agentfusion "pan/internal/agent/fusion"
	"pan/internal/config"
	"pan/internal/fusion"
	fusionmocks "pan/internal/fusion/mocks"
	fusiontool "pan/internal/tool/fusion"

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

func TestFusionAgent(t *testing.T) {
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
		setupMocks func(m *fusionmocks.MockService)
		turns      []turn
	}{
		{
			name: "get unread feeds",
			setupMocks: func(m *fusionmocks.MockService) {
				m.On("GetUnreadFeeds", mock.Anything, mock.Anything, mock.Anything).Return([]fusion.FeedItem{
					{
						ID:    1,
						Title: "New AI Model Released",
					},
				}, nil)
			},
			turns: []turn{
				{
					input: "Can you get my latest unread RSS feeds?",
					verify: func(t *testing.T, output string) {
						assert.Contains(t, output, "New AI Model Released")
					},
				},
			},
		},
	}

	for _, tt := range tests {
		tt := tt
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			mockService := fusionmocks.NewMockService(t)
			tt.setupMocks(mockService)

			stream := ro.NewSubject[any]()
			toolset, err := fusiontool.New(mockService, stream)
			assert.NoError(t, err)

			agent, err := agentfusion.New(llmModel, toolset)
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
