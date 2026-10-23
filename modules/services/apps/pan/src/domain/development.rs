use std::{future::Future, path::Path};

use serde::{Deserialize, Serialize};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("invalid development request: {0}")]
    Invalid(String),
    #[error("development storage failed: {0}")]
    Storage(String),
    #[error("development execution failed: {0}")]
    Execution(String),
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct DevelopmentRequest {
    /// Stable delivery ID, used to deduplicate retransmitted requests.
    pub request_id: String,
    pub repository: String,
    pub instruction: String,
    pub origin: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub conversation: Option<DevelopmentConversation>,
}

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct DevelopmentConversation {
    pub room_id: String,
    pub thread_id: String,
    pub user_id: String,
}

#[derive(Clone, Copy, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum JobState {
    Queued,
    Running,
    Completed,
    Failed,
    Cancelled,
    Interrupted,
}

impl JobState {
    #[must_use]
    pub const fn is_terminal(self) -> bool {
        !matches!(self, Self::Queued | Self::Running)
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct DevelopmentJob {
    pub id: String,
    pub request: DevelopmentRequest,
    pub state: JobState,
    pub created_at: String,
    pub updated_at: String,
    pub result: String,
}

pub trait DevelopmentStore: Send + Sync {
    /// # Errors
    /// Returns storage, queue capacity, or conflicting delivery errors.
    fn submit(&self, request: &DevelopmentRequest) -> Result<DevelopmentJob>;
    /// # Errors
    /// Returns storage errors or an unknown job error.
    fn get(&self, id: &str) -> Result<DevelopmentJob>;
    /// # Errors
    /// Returns an error when the next job cannot be claimed durably.
    fn next(&self) -> Result<Option<DevelopmentJob>>;
    /// Only queued/running jobs may transition; cancellation wins completion races.
    ///
    /// # Errors
    /// Returns storage errors or an unknown job error.
    fn finish(&self, id: &str, state: JobState, result: &str) -> Result<()>;
    /// # Errors
    /// Returns an error when interrupted jobs cannot be reconciled.
    fn recover(&self) -> Result<()>;
    /// # Errors
    /// Returns an error when undelivered terminal jobs cannot be loaded.
    fn pending_notifications(&self) -> Result<Vec<DevelopmentJob>>;
    /// # Errors
    /// Returns an error for unknown/nonterminal jobs or storage failures.
    fn acknowledge_notification(&self, id: &str) -> Result<()>;
}

pub trait DevelopmentExecutor: Send + Sync {
    fn execute(
        &self,
        job: &DevelopmentJob,
        directory: &Path,
    ) -> impl Future<Output = Result<String>> + Send;
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(tag = "type", rename_all = "snake_case", deny_unknown_fields)]
pub enum WorkerCommand {
    Submit { request: DevelopmentRequest },
    Status { id: String },
    Cancel { id: String },
    PendingNotifications,
    AcknowledgeNotification { id: String },
    Patch { id: String, offset: u64 },
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum WorkerResponse {
    Job { job: Box<DevelopmentJob> },
    Notifications { jobs: Vec<DevelopmentJob> },
    Patch { chunk: PatchChunk },
    Error { message: String },
}

#[derive(Debug, Deserialize, Serialize)]
pub struct PatchChunk {
    pub offset: u64,
    pub total: u64,
    pub bytes: Vec<u8>,
}
