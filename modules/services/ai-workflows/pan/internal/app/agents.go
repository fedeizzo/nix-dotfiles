package app

import (
	"fmt"

	genaiopenai "github.com/achetronic/adk-utils-go/genai/openai"
	"github.com/samber/ro"
	"google.golang.org/adk/agent"

	emailagent "pan/internal/agent/email"
	fusionagent "pan/internal/agent/fusion"
	lmagent "pan/internal/agent/lunchmoney"
	orchestratorAgent "pan/internal/agent/orchestrator"
	"pan/internal/config"
	"pan/internal/lunchmoney"
	"pan/internal/tool/email"
	fusiontool "pan/internal/tool/fusion"
	lmtool "pan/internal/tool/lunchmoney"
)

type Agents struct {
	Email        agent.Agent
	LunchMoney   agent.Agent
	Fusion       agent.Agent
	Orchestrator agent.Agent
}

func buildAgents(cfg *config.Config, svcs *Services, appStream ro.Subject[any]) (*Agents, error) {
	llmModel := genaiopenai.New(genaiopenai.Config{
		APIKey:    cfg.Models.OpenAIKey,
		BaseURL:   cfg.Models.OpenAIBase,
		ModelName: cfg.Models.Name,
	})

	emailToolset, err := email.New(svcs.Fastmail, appStream)
	if err != nil {
		return nil, fmt.Errorf("email toolset: %w", err)
	}
	emailAgent, err := emailagent.New(llmModel, emailToolset)
	if err != nil {
		return nil, fmt.Errorf("email agent: %w", err)
	}

	lmService := lunchmoney.New(svcs.LunchMoney)
	lunchmoneyToolset, err := lmtool.New(lmService, appStream)
	if err != nil {
		return nil, fmt.Errorf("lunchmoney toolset: %w", err)
	}
	lunchmoneyAgent, err := lmagent.New(llmModel, lunchmoneyToolset)
	if err != nil {
		return nil, fmt.Errorf("lunchmoney agent: %w", err)
	}

	fusionToolset, err := fusiontool.New(svcs.Fusion, appStream)
	if err != nil {
		return nil, fmt.Errorf("fusion toolset: %w", err)
	}
	fusionAgent, err := fusionagent.New(llmModel, fusionToolset)
	if err != nil {
		return nil, fmt.Errorf("fusion agent: %w", err)
	}

	orch, err := orchestratorAgent.New(llmModel, emailAgent, lunchmoneyAgent, fusionAgent)
	if err != nil {
		return nil, fmt.Errorf("orchestrator agent: %w", err)
	}

	return &Agents{
		Email:        emailAgent,
		LunchMoney:   lunchmoneyAgent,
		Fusion:       fusionAgent,
		Orchestrator: orch,
	}, nil
}
