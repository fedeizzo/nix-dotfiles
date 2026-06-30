package scheduler

import (
	"testing"

	"pan/internal/config"
	mocks "pan/internal/scheduler/mocks"

	"github.com/stretchr/testify/assert"
	"google.golang.org/adk/runner"
	"maunium.net/go/mautrix/id"
)

func TestScheduler(t *testing.T) {
	tests := []struct {
		name          string
		jobs          []config.JobConfig
		setupMock     func(m *mocks.MockBroadcaster)
		expectedJobs  int
	}{
		{
			name: "schedules simple job",
			jobs: []config.JobConfig{
				{
					Name:   "test-job",
					Spec:   "@every 1h",
					Runner: "orchestrator",
				},
			},
			setupMock: func(m *mocks.MockBroadcaster) {},
			expectedJobs: 1,
		},
		{
			name: "schedules multiple jobs",
			jobs: []config.JobConfig{
				{
					Name:   "test-job-1",
					Spec:   "@every 1h",
					Runner: "orchestrator",
				},
				{
					Name:   "test-job-2",
					Spec:   "@every 2h",
					Runner: "orchestrator",
				},
			},
			setupMock: func(m *mocks.MockBroadcaster) {},
			expectedJobs: 2,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			m := mocks.NewMockBroadcaster(t)
			tc.setupMock(m)

			runners := map[string]*runner.Runner{
				"orchestrator": {},
			}
            
			evaluator := NewEvaluatorRegistry()

			scheduler := New(runners, m, id.RoomID("room1"), tc.jobs, evaluator)

			// Start will call ScheduleJobs and start cron
			scheduler.Start()

			// Check that job was added to cron
			assert.Len(t, scheduler.cron.Entries(), tc.expectedJobs)
			
			// Stop the cron to clean up
			scheduler.cron.Stop()
		})
	}
}
