use std::{collections::BTreeSet, path::Path, sync::Arc, time::Duration};

use crate::domain::development::{
    DevelopmentExecutor, DevelopmentStore, Error, JobState, Result, WorkerCommand, WorkerResponse,
};

pub struct DevelopmentService<S> {
    store: Arc<S>,
    repositories: BTreeSet<String>,
}

impl<S: DevelopmentStore> DevelopmentService<S> {
    pub fn new(store: Arc<S>, repositories: BTreeSet<String>) -> Self {
        Self {
            store,
            repositories,
        }
    }

    #[must_use]
    pub fn handle(&self, command: WorkerCommand) -> WorkerResponse {
        let result = match command {
            WorkerCommand::PendingNotifications => self
                .store
                .pending_notifications()
                .map(|jobs| WorkerResponse::Notifications { jobs }),
            WorkerCommand::AcknowledgeNotification { id } => self
                .store
                .acknowledge_notification(&id)
                .and_then(|()| self.store.get(&id))
                .map(|job| WorkerResponse::Job { job: Box::new(job) }),
            command => self
                .dispatch(command)
                .map(|job| WorkerResponse::Job { job: Box::new(job) }),
        };
        match result {
            Ok(response) => response,
            Err(error) => WorkerResponse::Error {
                message: error.to_string(),
            },
        }
    }

    fn dispatch(
        &self,
        command: WorkerCommand,
    ) -> Result<crate::domain::development::DevelopmentJob> {
        match command {
            WorkerCommand::Submit { request } => {
                if !self.repositories.contains(&request.repository) {
                    return Err(Error::Invalid("repository is not allowlisted".into()));
                }
                if request.instruction.trim().is_empty()
                    || request.instruction.len() > 16_384
                    || request.request_id.is_empty()
                    || request.request_id.len() > 512
                    || request.origin.is_empty()
                    || request.origin.len() > 1024
                {
                    return Err(Error::Invalid("missing or oversized request fields".into()));
                }
                if request.conversation.as_ref().is_some_and(|binding| {
                    [&binding.room_id, &binding.thread_id, &binding.user_id]
                        .iter()
                        .any(|field| field.is_empty() || field.len() > 512)
                }) {
                    return Err(Error::Invalid("invalid development conversation".into()));
                }
                self.store.submit(&request)
            }
            WorkerCommand::Status { id } => self.store.get(&id),
            WorkerCommand::Cancel { id } => {
                self.store
                    .finish(&id, JobState::Cancelled, "Cancelled by the requester.")?;
                self.store.get(&id)
            }
            _ => Err(Error::Invalid("unsupported development command".into())),
        }
    }

    /// Executes one job at a time. Dropping an execution must stop its process tree.
    ///
    /// # Errors
    /// Returns storage errors; individual execution failures are recorded on the job.
    pub async fn run(
        &self,
        executor: &impl DevelopmentExecutor,
        root: &Path,
        timeout: Duration,
    ) -> Result<()> {
        self.store.recover()?;
        loop {
            let Some(job) = self.store.next()? else {
                tokio::time::sleep(Duration::from_millis(250)).await;
                continue;
            };
            let directory = root.join(&job.id);
            tracing::info!(job_id = %job.id, repository = %job.request.repository, "development job started");
            let execution = executor.execute(&job, &directory);
            tokio::pin!(execution);
            let cancellation = async {
                loop {
                    tokio::time::sleep(Duration::from_millis(100)).await;
                    if self.store.get(&job.id)?.state == JobState::Cancelled {
                        return Ok::<_, Error>(());
                    }
                }
            };
            let result = tokio::select! {
                result = &mut execution => result,
                result = cancellation => { result?; continue; }
                () = tokio::time::sleep(timeout) => Err(Error::Execution("job deadline exceeded".into())),
            };
            let (state, message) = match result {
                Ok(message) => (JobState::Completed, message),
                Err(error) => (JobState::Failed, error.to_string()),
            };
            self.store.finish(&job.id, state, &message)?;
            tracing::info!(job_id = %job.id, ?state, "development job finished");
        }
    }
}

#[cfg(test)]
mod tests {
    use std::sync::atomic::{AtomicBool, Ordering};

    use super::*;
    use crate::{
        domain::development::{DevelopmentJob, DevelopmentRequest},
        infrastructure::development_store::SqliteDevelopmentStore,
    };

    fn request() -> DevelopmentRequest {
        DevelopmentRequest {
            request_id: "event".into(),
            repository: "pan".into(),
            instruction: "Add a feature".into(),
            origin: "matrix/room/thread/user".into(),
            conversation: None,
        }
    }

    #[test]
    fn submission_requires_allowlisted_repository_and_bounded_nonempty_instruction() {
        let store = Arc::new(SqliteDevelopmentStore::open(Path::new(":memory:")).unwrap());
        let service = DevelopmentService::new(store, ["pan".into()].into());
        for (repository, instruction) in [
            ("other", "feature"),
            ("pan", " "),
            ("pan", &"x".repeat(16_385)),
        ] {
            let mut request = request();
            request.repository = repository.into();
            request.instruction = instruction.into();
            assert!(matches!(
                service.handle(WorkerCommand::Submit { request }),
                WorkerResponse::Error { .. }
            ));
        }
    }

    struct StalledExecutor(Arc<AtomicBool>);
    struct RunningGuard(Arc<AtomicBool>);
    impl Drop for RunningGuard {
        fn drop(&mut self) {
            self.0.store(false, Ordering::SeqCst);
        }
    }
    impl DevelopmentExecutor for StalledExecutor {
        async fn execute(&self, _: &DevelopmentJob, _: &Path) -> Result<String> {
            self.0.store(true, Ordering::SeqCst);
            let _guard = RunningGuard(Arc::clone(&self.0));
            std::future::pending().await
        }
    }

    #[tokio::test(start_paused = true)]
    async fn cancellation_and_deadline_drop_running_execution() {
        for cancel in [false, true] {
            let store = Arc::new(SqliteDevelopmentStore::open(Path::new(":memory:")).unwrap());
            let job = store.submit(&request()).unwrap();
            let service = DevelopmentService::new(Arc::clone(&store), ["pan".into()].into());
            let running = Arc::new(AtomicBool::new(false));
            let running_in_task = Arc::clone(&running);
            let task = tokio::spawn(async move {
                service
                    .run(
                        &StalledExecutor(running_in_task),
                        Path::new("/unused"),
                        Duration::from_secs(5),
                    )
                    .await
            });
            tokio::task::yield_now().await;
            assert!(running.load(Ordering::SeqCst));
            if cancel {
                store
                    .finish(&job.id, JobState::Cancelled, "cancel")
                    .unwrap();
            }
            tokio::time::advance(Duration::from_secs(6)).await;
            tokio::task::yield_now().await;
            assert!(!running.load(Ordering::SeqCst));
            assert_eq!(
                store.get(&job.id).unwrap().state,
                if cancel {
                    JobState::Cancelled
                } else {
                    JobState::Failed
                }
            );
            task.abort();
        }
    }
}
