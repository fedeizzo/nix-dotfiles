package optimizer

import (
	"context"
	_ "embed"
	"fmt"
	"log/slog"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"github.com/achetronic/adk-utils-go/genai/openai"
	"google.golang.org/adk/model"
	"google.golang.org/genai"
	"pan/internal/config"
)

//go:embed prompt.md
var sysPrompt string

func Run(ctx context.Context, cfg *config.Config, agentName string) error {
	slog.Info("Starting optimization loop", "agent", agentName)

	llmModel := openai.New(openai.Config{
		APIKey:    cfg.Models.OpenAIKey,
		BaseURL:   cfg.Models.OpenAIBase,
		ModelName: cfg.Models.Name, // We use the same configured model to optimize
	})

	maxIterations := 5
	promptPath := filepath.Join("internal", "agent", agentName, "prompt.md")

	for i := 1; i <= maxIterations; i++ {
		slog.Info("Running tests", "iteration", i, "agent", agentName)

		// Run the test suite for the specific agent
		cmd := exec.CommandContext(ctx, "go", "test", "-tags", "e2e", "./internal/agent/"+agentName)
		outputBytes, err := cmd.CombinedOutput()
		output := string(outputBytes)

		if err == nil {
			slog.Info("Tests passed! Prompt is fully optimized.", "iteration", i)
			return nil
		}

		slog.Info("Tests failed. Engaging meta-agent to rewrite prompt...", "iteration", i)
		slog.Debug("Test Output", "output", output)

		currentPromptBytes, readErr := os.ReadFile(promptPath)
		if readErr != nil {
			return fmt.Errorf("failed to read prompt file %s: %w", promptPath, readErr)
		}
		currentPrompt := string(currentPromptBytes)

		userPrompt := fmt.Sprintf("CURRENT PROMPT:\n%s\n\nTEST FAILURES:\n%s\n\nPlease provide the updated prompt.md content.", currentPrompt, output)

		req := &model.LLMRequest{
			Contents: []*genai.Content{
				{Role: "system", Parts: []*genai.Part{{Text: sysPrompt}}},
				{Role: "user", Parts: []*genai.Part{{Text: userPrompt}}},
			},
		}

		var newPrompt string
		for resp, genErr := range llmModel.GenerateContent(ctx, req, false) {
			if genErr != nil {
				return fmt.Errorf("failed to generate new prompt: %w", genErr)
			}
			if resp != nil && resp.Content != nil && len(resp.Content.Parts) > 0 {
				newPrompt += resp.Content.Parts[0].Text
			}
		}

		if newPrompt == "" {
			return fmt.Errorf("no response from LLM")
		}
		newPrompt = strings.TrimPrefix(newPrompt, "```markdown\n")
		newPrompt = strings.TrimPrefix(newPrompt, "```\n")
		newPrompt = strings.TrimSuffix(newPrompt, "```")

		// Write the updated prompt back to disk
		if writeErr := os.WriteFile(promptPath, []byte(newPrompt), 0644); writeErr != nil {
			return fmt.Errorf("failed to write new prompt: %w", writeErr)
		}

		slog.Info("Prompt updated. Re-running tests...", "iteration", i)
	}

	return fmt.Errorf("optimization failed to pass tests after %d iterations", maxIterations)
}
