//go:build e2e

package lunchmoney_test

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

	agentlm "pan/internal/agent/lunchmoney"
	"pan/internal/agent/lunchmoney/mocks"
	"pan/internal/config"
	lmtool "pan/internal/tool/lunchmoney"

	genai "google.golang.org/genai"
)

func TestMain(m *testing.M) {
	goleak.VerifyTestMain(m,
		goleak.IgnoreCurrent(),
		goleak.IgnoreTopFunction("net/http.(*http2clientConnReadLoop).run"),
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

func TestLunchMoneyAgent(t *testing.T) {
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
		setupMocks func(m *mocks.MockLunchMoneyToolset)
		turns      []turn
	}{
		{
			name: "present transaction",
			setupMocks: func(m *mocks.MockLunchMoneyToolset) {
				mockCategory := "Entertainment"
				m.On("GetLatestUnreviewedTransaction", mock.Anything, mock.Anything).Return(lmtool.GetLatestUnreviewedTransactionOutput{
					Transaction: &lmtool.TransactionSummary{
						ID:       123,
						Date:     "2023-10-01",
						Amount:   "15.99",
						Payee:    "Netflix",
						Category: &mockCategory,
						Status:   "unreviewed",
					},
				}, nil).Once()
			},
			turns: []turn{
				{
					input: "Can you help me review my transactions?",
					verify: func(t *testing.T, output string) {
						assert.Contains(t, output, "**Transaction to review**")
						assert.Contains(t, output, "Netflix")
					},
				},
			},
		},
		{
			name: "update transaction",
			setupMocks: func(m *mocks.MockLunchMoneyToolset) {
				mockCategory := "Entertainment"
				m.On("GetLatestUnreviewedTransaction", mock.Anything, mock.Anything).Return(lmtool.GetLatestUnreviewedTransactionOutput{
					Transaction: &lmtool.TransactionSummary{
						ID:       123,
						Date:     "2023-10-01",
						Amount:   "15.99",
						Payee:    "Netflix",
						Category: &mockCategory,
						Status:   "unreviewed",
					},
				}, nil).Once()

				m.On("GetCategories", mock.Anything, mock.Anything).Return(lmtool.GetCategoriesOutput{
					Categories: []lmtool.CategorySummary{
						{ID: 1, Name: "Entertainment"},
						{ID: 2, Name: "Groceries"},
					},
				}, nil).Once()

				m.On("GetTags", mock.Anything, mock.Anything).Return(lmtool.GetTagsOutput{
					Tags: []lmtool.TagSummary{
						{ID: 1, Name: "Subscription"},
					},
				}, nil).Once()

				m.On("UpdateTransaction", mock.Anything, mock.MatchedBy(func(input lmtool.UpdateTransactionInput) bool {
					return input.CategoryID != nil && *input.CategoryID == 1 && input.Status != nil && *input.Status == "reviewed"
				})).Return(lmtool.UpdateTransactionOutput{Success: true}, nil).Once()
			},
			turns: []turn{
				{
					input: "Can you help me review my transactions?",
				},
				{
					input: "It's my monthly streaming subscription.",
					verify: func(t *testing.T, output string) {
						assert.Contains(t, output, "Entertainment")
					},
				},
			},
		},
	}

	for _, tt := range tests {
		tt := tt // capture loop variable
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			mockToolset := mocks.NewMockLunchMoneyToolset(t)
			tt.setupMocks(mockToolset)

			lmAgent, err := agentlm.New(llmModel, mockToolset)
			assert.NoError(t, err)

			sessSvc := session.InMemoryService()
			r, err := runner.New(runner.Config{
				AppName:        "test",
				Agent:          lmAgent,
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
