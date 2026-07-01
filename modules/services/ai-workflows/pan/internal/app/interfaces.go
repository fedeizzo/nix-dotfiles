package app

import (
	"fmt"

	"maunium.net/go/mautrix/id"
	"pan/internal/bot"
	"pan/internal/bot/cli"
	"pan/internal/bot/matrix"
	"pan/internal/config"
	"pan/internal/scheduler"
)

func buildBot(cfg *config.Config, runs *Runners, evalRegistry *scheduler.EvaluatorRegistry) (bot.Bot, error) {
	switch cfg.Interface.Type {
	case "matrix":
		matrixBot, err := matrix.New(cfg, runs.Map["orchestrator"])
		if err != nil {
			return nil, fmt.Errorf("matrix bot: %w", err)
		}
		if matrixBot != nil && cfg.Matrix.NotificationRoom != "" {
			cronScheduler := scheduler.New(runs.Map, matrixBot, id.RoomID(cfg.Matrix.NotificationRoom), cfg.Jobs, evalRegistry)
			cronScheduler.Start()
		}
		return matrixBot, nil
	case "cli":
		return cli.New(runs.SessionSvc, runs.Map["orchestrator"], cfg.CLI.ConversationPath), nil
	default:
		return nil, fmt.Errorf("no bot selected")
	}
}
