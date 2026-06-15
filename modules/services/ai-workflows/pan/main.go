package main

import (
	"context"
	"flag"
	"io"
	"log/slog"
	"os"
	emailagent "pan/internal/agent/email"
	fusionagent "pan/internal/agent/fusion"
	lmagent "pan/internal/agent/lunchmoney"
	orchestratorAgent "pan/internal/agent/orchestrator"
	"pan/internal/bot"
	"pan/internal/bot/cli"
	"pan/internal/bot/matrix"
	"pan/internal/config"
	"pan/internal/events"
	"pan/internal/fastmail"
	"pan/internal/memory/hindsight"
	"pan/internal/scheduler"
	"pan/internal/telemetry"
	"pan/internal/telemetry/observer"
	"pan/internal/tool/email"
	"pan/internal/fusion"
	fusiontool "pan/internal/tool/fusion"
	lmtool "pan/internal/tool/lunchmoney"

	"git.sr.ht/~rockorager/go-jmap"
	lm "github.com/Cidan/lunchmoney-go"
	genaiopenai "github.com/achetronic/adk-utils-go/genai/openai"
	"github.com/joho/godotenv"
	hindsightSDK "github.com/vectorize-io/hindsight/hindsight-clients/go"
	"google.golang.org/adk/agent"
	"google.golang.org/adk/cmd/launcher"
	"google.golang.org/adk/cmd/launcher/full"
	"google.golang.org/adk/plugin"
	"google.golang.org/adk/runner"
	"google.golang.org/adk/session"

	"go.opentelemetry.io/otel"
	"go.opentelemetry.io/otel/exporters/otlp/otlptrace/otlptracehttp"
	"go.opentelemetry.io/otel/sdk/resource"
	sdktrace "go.opentelemetry.io/otel/sdk/trace"
	semconv "go.opentelemetry.io/otel/semconv/v1.24.0"
	"maunium.net/go/mautrix/id"
)

func initTracer() *sdktrace.TracerProvider {
	exporter, err := otlptracehttp.New(context.Background(), otlptracehttp.WithInsecure())
	if err != nil {
		slog.Error("Failed to create trace exporter", "error", err)
	}

	tp := sdktrace.NewTracerProvider(
		sdktrace.WithBatcher(exporter),
		sdktrace.WithResource(resource.NewWithAttributes(
			semconv.SchemaURL,
			semconv.ServiceName("pan-email-agent"),
		)),
	)

	otel.SetTracerProvider(tp)
	return tp
}

func main() {
	configPath := flag.String("config", "", "Path to the configuration file")
	flag.Parse()

	ctx := context.Background()
	_ = godotenv.Load()

	cfg := config.Load(*configPath)

	// Start telemetry background services
	go telemetry.StartProcessor()
	go telemetry.StartHTTP(":" + cfg.Telemetry.Port)

	logFile, err := os.OpenFile(cfg.Log.Path, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0644)
	if err != nil {
		slog.Error("Failed to open log file", "error", err, "path", cfg.Log.Path)
		os.Exit(1)
	}
	defer logFile.Close()

	multiWriter := io.MultiWriter(logFile, os.Stdout)

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

	observer.SetupMetrics(events.DefaultBus)
	observer.SetupLogging(events.DefaultBus)

	client := &jmap.Client{
		SessionEndpoint: "https://api.fastmail.com/jmap/session",
	}
	client.WithAccessToken(cfg.Fastmail.API)

	if err := client.Authenticate(); err != nil {
		slog.Error("Failed to authenticate to fastmail", "error", err)
		os.Exit(1)
	}
	fastmailService := fastmail.New(client)
	lmClient, err := lm.NewClient(lm.WithStaticToken(cfg.LunchMoney.API))
	if err != nil {
		slog.Error("Failed to initialize lunchmoney client", "error", err)
		os.Exit(1)
	}

	evalRegistry := scheduler.NewEvaluatorRegistry()
	evalRegistry.Register("fastmail:has_unread", func(ctx context.Context) (bool, error) {
		tags, err := fastmailService.GetTags(ctx)
		if err != nil {
			return false, err
		}
		for _, tag := range tags {
			if tag.Name != "Inbox" {
				continue
			}
			if tag.Unread > 0 {
				return true, nil
			}
		}
		return false, nil
	})
	evalRegistry.Register("lunchmoney:has_unreviewed", func(ctx context.Context) (bool, error) {
		st := lm.TransactionStatus("unreviewed")
		limit := int32(1)
		resp, err := lmClient.Transactions.List(ctx, &lm.ListTransactionsParams{
			Status: &st,
			Limit:  &limit,
		})
		if err != nil {
			return false, err
		}
		if len(resp.Transactions) > 0 {
			return true, nil
		}

		return false, nil
	})

	fusionSvc := fusion.NewFusionService(cfg.Fusion.Endpoint, cfg.Fusion.Password)
	evalRegistry.Register("fusion:has_unread", func(ctx context.Context) (bool, error) {
		feeds, err := fusionSvc.GetUnreadFeeds(ctx, 1)
		if err != nil {
			return false, err
		}
		return len(feeds) > 0, nil
	})

	configuration := hindsightSDK.NewConfiguration()
	if cfg.Hindsight.URL != "" {
		configuration.Servers = hindsightSDK.ServerConfigurations{
			{URL: cfg.Hindsight.URL},
		}
	}
	configuration.AddDefaultHeader("Authorization", "Bearer "+cfg.Hindsight.APIKey)
	hsClient := hindsightSDK.NewAPIClient(configuration)
	m := hindsight.NewService(hsClient, cfg.Hindsight.BankID)
	emailToolset, err := email.New(&fastmailService)
	if err != nil {
		slog.Error("Failed to initialize email toolset", "error", err)
		os.Exit(1)
	}

	llmModel := genaiopenai.New(genaiopenai.Config{
		APIKey:    cfg.Models.OpenAIKey,
		BaseURL:   cfg.Models.OpenAIBase,
		ModelName: cfg.Models.Name,
	})

	emailAgent, err := emailagent.New(llmModel, emailToolset)
	if err != nil {
		slog.Error("Failed to create email agent", "error", err)
		os.Exit(1)
	}

	lunchmoneyToolset, err := lmtool.New(lmClient)
	if err != nil {
		slog.Error("Failed to initialize lunchmoney toolset", "error", err)
		os.Exit(1)
	}

	lunchmoneyAgent, err := lmagent.New(llmModel, lunchmoneyToolset)
	if err != nil {
		slog.Error("Failed to create lunchmoney agent", "error", err)
		os.Exit(1)
	}

	fusionToolset, err := fusiontool.New(fusionSvc)
	if err != nil {
		slog.Error("Failed to create fusion toolset", "error", err)
		os.Exit(1)
	}

	fusionAgent, err := fusionagent.New(llmModel, fusionToolset)
	if err != nil {
		slog.Error("Failed to create fusion agent", "error", err)
		os.Exit(1)
	}

	a, err := orchestratorAgent.New(llmModel, emailAgent, lunchmoneyAgent, fusionAgent)
	if err != nil {
		slog.Error("Failed to create orchestrator agent", "error", err)
		os.Exit(1)
	}

	s := session.InMemoryService()
	_, err = s.Create(ctx, &session.CreateRequest{
		AppName: "test",
		UserID:  "name",
	})
	if err != nil {
		slog.Error("Failed to create session", "error", err)
		os.Exit(1)
	}

	hindsightPlugin, err := plugin.New(plugin.Config{
		Name: "hindsight_consolidation",
		AfterRunCallback: func(ic agent.InvocationContext) {
			sess := ic.Session()
			if sess != nil {
				_ = m.AddSessionToMemory(ctx, sess)
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
		MemoryService:     m,
		Agent:             a, // orchestrator
		PluginConfig: runner.PluginConfig{
			Plugins: []*plugin.Plugin{hindsightPlugin},
		},
	}
	rOrch, err := runner.New(rConfigOrch)
	if err != nil {
		slog.Error("Failed to initialize orchestrator runner", "error", err)
		os.Exit(1)
	}

	rConfigEmail := rConfigOrch
	rConfigEmail.Agent = emailAgent
	rEmail, err := runner.New(rConfigEmail)
	if err != nil {
		slog.Error("Failed to initialize email runner", "error", err)
		os.Exit(1)
	}

	rConfigLunch := rConfigOrch
	rConfigLunch.Agent = lunchmoneyAgent
	rLunch, err := runner.New(rConfigLunch)
	if err != nil {
		slog.Error("Failed to initialize lunchmoney runner", "error", err)
		os.Exit(1)
	}

	rConfigFusion := rConfigOrch
	rConfigFusion.Agent = fusionAgent
	rFusion, err := runner.New(rConfigFusion)
	if err != nil {
		slog.Error("Failed to initialize fusion runner", "error", err)
		os.Exit(1)
	}

	runnersMap := map[string]*runner.Runner{
		"orchestrator": rOrch,
		"email":        rEmail,
		"lunchmoney":   rLunch,
		"fusion":       rFusion,
	}

	var b bot.Bot
	switch cfg.Interface.Type {
	case "matrix":
		matrixBot, err := matrix.New(cfg, rOrch)
		if err != nil {
			slog.Error("Failed to initialize matrix bot", "error", err)
			os.Exit(1)
		}

		if matrixBot != nil && cfg.Matrix.NotificationRoom != "" {
			cronScheduler := scheduler.New(runnersMap, matrixBot, id.RoomID(cfg.Matrix.NotificationRoom), cfg.Jobs, evalRegistry)
			cronScheduler.Start()
		}

		if matrixBot != nil {
			b = matrixBot
		}
	case "cli":
		b = cli.New(rOrch)
	default:
		slog.Error("no bot selected")
		os.Exit(1)
	}

	if b != nil {
		if err := b.Start(); err != nil {
			slog.Error("failed to start the bot", "error", err)
			os.Exit(1)
		}
		return
	}

	config := &launcher.Config{
		SessionService: s,
		AgentLoader:    agent.NewSingleLoader(a),
		PluginConfig:   runner.PluginConfig{
			// Plugins: []*plugin.Plugin{thoughtPlugin},
		},
	}

	l := full.NewLauncher()
	if err = l.Execute(ctx, config, flag.Args()); err != nil {
		slog.Error("Run failed", "error", err, "syntax", l.CommandLineSyntax())
		os.Exit(1)
	}
}
