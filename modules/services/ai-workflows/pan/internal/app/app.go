package app

import (
	"context"

	"github.com/samber/ro"
	"google.golang.org/adk/agent"
	"google.golang.org/adk/runner"
	"google.golang.org/adk/session"

	"pan/internal/bot"
	"pan/internal/config"
)

type App struct {
	Config     *config.Config
	SessionSvc session.Service
	Runners    map[string]*runner.Runner
	Bot        bot.Bot
	OrchAgent  agent.Agent
}

func Build(ctx context.Context, cfg *config.Config, appStream ro.Subject[any]) (*App, error) {
	svcs, err := buildServices(cfg)
	if err != nil {
		return nil, err
	}

	evalRegistry := buildEvaluators(svcs)

	agts, err := buildAgents(cfg, svcs, appStream)
	if err != nil {
		return nil, err
	}

	runs, err := buildRunners(ctx, svcs, agts)
	if err != nil {
		return nil, err
	}

	b, err := buildBot(cfg, runs, evalRegistry)
	if err != nil {
		return nil, err
	}

	return &App{
		Config:     cfg,
		SessionSvc: runs.SessionSvc,
		Runners:    runs.Map,
		Bot:        b,
		OrchAgent:  agts.Orchestrator,
	}, nil
}
