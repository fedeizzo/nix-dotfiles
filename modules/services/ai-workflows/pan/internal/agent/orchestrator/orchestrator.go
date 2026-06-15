package orchestrator

import (
	_ "embed"

	"google.golang.org/adk/agent"
	"google.golang.org/adk/agent/llmagent"
	"google.golang.org/adk/model"
)

//go:embed prompt.md
var prompt string

func New(llmModel model.LLM, emailAgent agent.Agent, lunchMoneyAgent agent.Agent, fusionAgent agent.Agent) (agent.Agent, error) {
	return llmagent.New(llmagent.Config{
		Name:        "orchestrator",
		Model:       llmModel,
		Instruction: prompt,
		SubAgents:   []agent.Agent{emailAgent, lunchMoneyAgent, fusionAgent},
	})
}
