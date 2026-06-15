package scheduler

import (
	"context"
	"fmt"
	"iter"
	"log/slog"
	"time"

	"pan/internal/config"

	"github.com/robfig/cron/v3"
	"google.golang.org/adk/agent"
	"google.golang.org/adk/runner"
	"google.golang.org/adk/session"
	"google.golang.org/genai"
	"maunium.net/go/mautrix/id"
)

type Broadcaster interface {
	BroadcastStream(ctx context.Context, roomID id.RoomID, eventID id.EventID, sessionID string, stream iter.Seq2[*session.Event, error])
	StartJobSession(ctx context.Context, roomID id.RoomID, message string) (id.EventID, error)
}

type Scheduler struct {
	cron      *cron.Cron
	runners   map[string]*runner.Runner
	bot       Broadcaster
	roomID    id.RoomID
	jobs      []config.JobConfig
	evaluator *EvaluatorRegistry
}

func New(runners map[string]*runner.Runner, bot Broadcaster, roomID id.RoomID, jobs []config.JobConfig, evaluator *EvaluatorRegistry) *Scheduler {
	return &Scheduler{
		cron:      cron.New(),
		runners:   runners,
		bot:       bot,
		roomID:    roomID,
		jobs:      jobs,
		evaluator: evaluator,
	}
}

func (s *Scheduler) Start() {
	slog.Info("Starting scheduler")
	s.ScheduleJobs()
	s.cron.Start()
}

func (s *Scheduler) ScheduleJobs() {
	for _, job := range s.jobs {
		job := job // capture loop variable
		slog.Info("Scheduling job", "name", job.Name, "spec", job.Spec, "runner", job.Runner)
		_, err := s.cron.AddFunc(job.Spec, func() {
			ctx := context.Background()

			if job.Condition != "" && s.evaluator != nil {
				slog.Info("Evaluating condition for job", "name", job.Name, "spec", job.Spec, "condition", job.Condition)
				shouldRun, evalErr := s.evaluator.Evaluate(ctx, job.Condition)
				if evalErr != nil {
					slog.Error("Failed to evaluate condition", "name", job.Name, "spec", job.Spec, "condition", job.Condition, "error", evalErr)
					return
				}
				if !shouldRun {
					slog.Info("Condition not met, skipping job", "name", job.Name, "spec", job.Spec, "condition", job.Condition)
					return
				}
			}

			slog.Info("Executing scheduled job", "name", job.Name, "spec", job.Spec, "runner", job.Runner)

			var sessionID string
			var eventID id.EventID
			if s.bot != nil {
				if eID, err := s.bot.StartJobSession(ctx, s.roomID, fmt.Sprintf("🤖 Running %s job: %s", job.Name, job.Spec)); err == nil {
					sessionID = string(eID)
					eventID = eID
				}
			}

			if sessionID == "" {
				sessionID = fmt.Sprintf("cron-job-%d", time.Now().Unix())
				eventID = id.EventID(sessionID)
			}

			runCtx := context.WithValue(ctx, "matrix_thread_id", sessionID)

			msg := &genai.Content{
				Role:  "user",
				Parts: []*genai.Part{{Text: job.Prompt}},
			}

			targetRunner, ok := s.runners[job.Runner]
			if !ok {
				if defRunner, hasDef := s.runners["orchestrator"]; hasDef {
					targetRunner = defRunner
				} else {
					slog.Error("Runner not found for job and no orchestrator default available", "runner", job.Runner, "job", job.Name)
					return
				}
			}

			stream := targetRunner.Run(runCtx, "user", sessionID, msg, agent.RunConfig{})

			if s.bot != nil {
				slog.Debug("Broadcasting job output to matrix", "session_id", sessionID, "room_id", s.roomID)
				s.bot.BroadcastStream(ctx, s.roomID, eventID, sessionID, stream)
			}
		})
		if err != nil {
			slog.Error("Failed to schedule job", "name", job.Name, "spec", job.Spec, "error", err)
		}
	}
}
