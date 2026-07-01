package main

import (
	"context"
	"fmt"
	"io"
	"log/slog"
	"os"

	"github.com/joho/godotenv"
	"github.com/samber/ro"
	"github.com/spf13/cobra"
	"google.golang.org/adk/agent"
	"google.golang.org/adk/cmd/launcher"
	"google.golang.org/adk/cmd/launcher/full"

	"pan/internal/app"
	"pan/internal/config"
	"pan/internal/telemetry"
	"pan/internal/telemetry/observer"
)

var cfgFile string
var rootCfg *config.Config

var rootCmd = &cobra.Command{
	Use:           "pan",
	Short:         "Pan is an AI agent orchestrator",
	SilenceUsage:  true,
	SilenceErrors: true,
	PersistentPreRunE: func(cmd *cobra.Command, args []string) error {
		_ = godotenv.Load()
		cfg, err := config.Load(cfgFile)
		if err != nil {
			return fmt.Errorf("failed to load configuration: %w", err)
		}
		rootCfg = cfg

		logFile, err := os.OpenFile(cfg.Log.Path, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0644)
		if err != nil {
			return fmt.Errorf("failed to open log file %s: %w", cfg.Log.Path, err)
		}

		var multiWriter io.Writer
		if cfg.Interface.Type == "cli" {
			multiWriter = io.MultiWriter(logFile)
		} else {
			multiWriter = io.MultiWriter(logFile, os.Stdout)
		}
		var level slog.Level
		switch cfg.Log.Level {
		case "debug":
			level = slog.LevelDebug
		case "info":
			level = slog.LevelInfo
		case "warn":
			level = slog.LevelWarn
		case "error":
			level = slog.LevelError
		default:
			level = slog.LevelInfo
		}

		logger := slog.New(slog.NewTextHandler(multiWriter, &slog.HandlerOptions{
			Level: level,
		}))
		slog.SetDefault(logger)

		return nil
	},
	RunE: func(cmd *cobra.Command, args []string) error {
		ctx := cmd.Context()

		telemetry.InitTracer("pan-agent")
		go telemetry.StartProcessor()
		go telemetry.StartHTTP(":" + rootCfg.Telemetry.Port)

		appStream := ro.NewPublishSubject[any]()
		observer.SetupMetrics(appStream)
		observer.SetupLogging(appStream)

		application, err := app.Build(ctx, rootCfg, appStream)
		if err != nil {
			return fmt.Errorf("failed to build application dependencies: %w", err)
		}

		if application.Bot != nil {
			if err := application.Bot.Start(); err != nil {
				return fmt.Errorf("failed to start the bot: %w", err)
			}
			return nil
		}

		launcherConfig := &launcher.Config{
			SessionService: application.SessionSvc,
			AgentLoader:    agent.NewSingleLoader(application.OrchAgent),
		}

		l := full.NewLauncher()
		if err = l.Execute(ctx, launcherConfig, args); err != nil {
			return fmt.Errorf("run failed (%s): %w", l.CommandLineSyntax(), err)
		}
		return nil
	},
}

func init() {
	rootCmd.PersistentFlags().StringVarP(&cfgFile, "config", "c", "", "config file path")
}

func Execute() {
	if err := rootCmd.ExecuteContext(context.Background()); err != nil {
		slog.Error("Execution failed", "error", err)
		os.Exit(1)
	}
}
