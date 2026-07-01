package app

import (
	"context"
	"fmt"
	"log/slog"

	"google.golang.org/adk/agent"
	"google.golang.org/adk/plugin"
	"google.golang.org/adk/runner"
	"google.golang.org/adk/session"
)

type Runners struct {
	SessionSvc session.Service
	Map        map[string]*runner.Runner
}

func buildRunners(ctx context.Context, svcs *Services, agts *Agents) (*Runners, error) {
	s := session.InMemoryService()
	_, err := s.Create(ctx, &session.CreateRequest{AppName: "test", UserID: "name"})
	if err != nil {
		return nil, fmt.Errorf("session create: %w", err)
	}

	hindsightPlugin, err := plugin.New(plugin.Config{
		Name: "hindsight_consolidation",
		AfterRunCallback: func(ic agent.InvocationContext) {
			sess := ic.Session()
			if sess != nil {
				_ = svcs.Memory.AddSessionToMemory(ctx, sess)
			}
		},
	})
	if err != nil {
		slog.Error("Failed to create hindsight plugin", "error", err)
	}

	rConfigOrch := runner.Config{
		AppName:           "pan",
		AutoCreateSession: true,
		SessionService:    s,
		MemoryService:     svcs.Memory,
		Agent:             agts.Orchestrator,
		PluginConfig: runner.PluginConfig{
			Plugins: []*plugin.Plugin{hindsightPlugin},
		},
	}
	rOrch, err := runner.New(rConfigOrch)
	if err != nil {
		return nil, fmt.Errorf("orch runner: %w", err)
	}

	rConfigEmail := rConfigOrch
	rConfigEmail.Agent = agts.Email
	rEmail, err := runner.New(rConfigEmail)
	if err != nil {
		return nil, fmt.Errorf("email runner: %w", err)
	}

	rConfigLunch := rConfigOrch
	rConfigLunch.Agent = agts.LunchMoney
	rLunch, err := runner.New(rConfigLunch)
	if err != nil {
		return nil, fmt.Errorf("lunchmoney runner: %w", err)
	}

	rConfigFusion := rConfigOrch
	rConfigFusion.Agent = agts.Fusion
	rFusion, err := runner.New(rConfigFusion)
	if err != nil {
		return nil, fmt.Errorf("fusion runner: %w", err)
	}

	runnersMap := map[string]*runner.Runner{
		"orchestrator": rOrch,
		"email":        rEmail,
		"lunchmoney":   rLunch,
		"fusion":       rFusion,
	}

	return &Runners{
		SessionSvc: s,
		Map:        runnersMap,
	}, nil
}
