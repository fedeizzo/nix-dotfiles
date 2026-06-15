package hindsight

import (
	"context"
	"fmt"
	"regexp"
	"strings"

	hindsightSDK "github.com/vectorize-io/hindsight/hindsight-clients/go"
	"google.golang.org/adk/memory"
	"google.golang.org/adk/session"
	"google.golang.org/genai"
)

type Service struct {
	client *hindsightSDK.APIClient
	bankID string
}

func NewService(client *hindsightSDK.APIClient, bankID string) *Service {
	return &Service{
		client: client,
		bankID: bankID,
	}
}

// AddSessionToMemory passes episodic session history to Hindsight for consolidation.
func (s *Service) AddSessionToMemory(ctx context.Context, sess session.Session) error {
	var items []hindsightSDK.MemoryItem

	for ev := range sess.Events().All() {
		if ev != nil && ev.Content != nil {
			var texts []string
			for _, part := range ev.Content.Parts {
				if part == nil {
					continue
				}
				if part.Text != "" {
					// Remove <think> blocks if present
					cleanText := regexp.MustCompile(`(?s)<think>.*?</think>`).ReplaceAllString(part.Text, "")
					cleanText = strings.TrimSpace(cleanText)
					if cleanText != "" {
						texts = append(texts, cleanText)
					}
				}
			}

			contentStr := strings.TrimSpace(strings.Join(texts, " "))
			if contentStr != "" {
				role := ev.Content.Role
				if role == "" {
					role = "system"
				}
				// Skip logging pure tool outputs to avoid bloat
				if role == "tool" {
					continue
				}
				items = append(items, hindsightSDK.MemoryItem{
					Content: fmt.Sprintf("[%s]: %s", role, contentStr),
				})
			}
		}
	}

	if len(items) == 0 {
		return nil
	}

	req := hindsightSDK.NewRetainRequest(items)

	bank := s.bankID
	if bank == "" {
		bank = sess.UserID()
	}

	_, _, err := s.client.MemoryAPI.RetainMemories(ctx, bank).RetainRequest(*req).Execute()
	return err
}

// AddFact explicitly saves a focused, distilled fact to Hindsight without episodic noise.
func (s *Service) AddFact(ctx context.Context, fact string) error {
	req := hindsightSDK.NewRetainRequest([]hindsightSDK.MemoryItem{
		{Content: fact},
	})

	bank := s.bankID
	if bank == "" {
		return nil // Silently skip if no bank ID is set
	}

	_, _, err := s.client.MemoryAPI.RetainMemories(ctx, bank).RetainRequest(*req).Execute()
	return err
}

// SearchMemory queries Hindsight's long-term memory banks for relevant facts.
func (s *Service) SearchMemory(ctx context.Context, req *memory.SearchRequest) (*memory.SearchResponse, error) {
	recallReq := hindsightSDK.NewRecallRequest(req.Query)

	bank := s.bankID
	if bank == "" {
		bank = req.UserID
	}

	resp, _, err := s.client.MemoryAPI.RecallMemories(ctx, bank).RecallRequest(*recallReq).Execute()
	if err != nil {
		return nil, err
	}

	var entries []memory.Entry
	if resp != nil {
		for _, res := range resp.Results {
			entries = append(entries, memory.Entry{
				ID:      res.Id,
				Content: genai.NewContentFromText(res.Text, ""),
			})
		}
	}

	return &memory.SearchResponse{Memories: entries}, nil
}
