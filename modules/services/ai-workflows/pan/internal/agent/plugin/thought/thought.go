package thought

import (
	"log/slog"
	"strings"

	"google.golang.org/adk/agent"
	"google.golang.org/adk/model"
	"google.golang.org/adk/plugin"
	"google.golang.org/genai"
)

func New() (*plugin.Plugin, error) {
	return plugin.New(plugin.Config{
		Name:               "hide_thoughts",
		AfterModelCallback: modelCallback,
	})
}

func modelCallback(cc agent.CallbackContext, resp *model.LLMResponse, respErr error) (*model.LLMResponse, error) {
	slog.Info("AfterModelCallback", "err", respErr, "hasResp", resp != nil)
	if resp == nil || resp.Content == nil {
		return resp, respErr
	}

	var newParts []*genai.Part
	for _, p := range resp.Content.Parts {
		if p.Thought {
			continue
		}

		p.Text = removeThinkingBlock(p.Text)

		// KEEP the part if it has text OR if it's a tool call!
		if p.Text != "" || p.FunctionCall != nil {
			newParts = append(newParts, p)
		}
	}
	resp.Content.Parts = newParts
	return resp, respErr
}

func removeThinkingBlock(text string) string {
	if text == "" {
		return text
	}

	var cleaned string
	if strings.Contains(text, "<think>") {
		idx := strings.Index(text, "</think>")
		if idx != -1 {
			cleaned = strings.TrimSpace(text[idx+len("</think>"):])
		} else {
			// The <think> block hasn't closed yet
			cleaned = ""
		}
	}

	return cleaned
}
