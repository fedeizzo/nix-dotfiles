use std::{future::Future, pin::Pin};

use chrono::{DateTime, Utc};

pub type RepositoryFuture<'a, T> = Pin<Box<dyn Future<Output = Result<T, Error>> + Send + 'a>>;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum WorkflowKind {
    LunchMoneyTransaction,
    FastmailEmail,
}

impl WorkflowKind {
    #[must_use]
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::LunchMoneyTransaction => "lunchmoney_transaction",
            Self::FastmailEmail => "fastmail_email",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum WorkflowState {
    Reserved,
    Delivered,
    Applying,
    Completed,
    Failed,
}

impl WorkflowState {
    #[must_use]
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Reserved => "reserved",
            Self::Delivered => "delivered",
            Self::Applying => "applying",
            Self::Completed => "completed",
            Self::Failed => "failed",
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WorkflowClaim {
    pub kind: WorkflowKind,
    pub resource_id: String,
    pub state: WorkflowState,
    pub room_id: Option<String>,
    pub thread_id: Option<String>,
    pub expires_at: DateTime<Utc>,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
    pub operation_id: Option<String>,
    pub action_payload: Option<String>,
    pub attempts: u32,
    pub last_error: Option<String>,
    pub next_attempt_at: Option<DateTime<Utc>>,
}

pub trait WorkflowRepository: Send + Sync {
    fn claim(
        &self,
        kind: WorkflowKind,
        resource_id: String,
        expires_at: DateTime<Utc>,
    ) -> RepositoryFuture<'_, bool>;

    fn bind_conversation(
        &self,
        kind: WorkflowKind,
        resource_id: String,
        room_id: String,
        thread_id: String,
    ) -> RepositoryFuture<'_, ()>;

    fn find_by_conversation(
        &self,
        kind: WorkflowKind,
        room_id: String,
        thread_id: String,
    ) -> RepositoryFuture<'_, Option<WorkflowClaim>>;

    fn release(&self, kind: WorkflowKind, resource_id: String) -> RepositoryFuture<'_, ()>;

    fn begin_operation(
        &self,
        kind: WorkflowKind,
        resource_id: String,
        operation_id: String,
        action_payload: String,
    ) -> RepositoryFuture<'_, WorkflowClaim>;

    fn record_failure(
        &self,
        kind: WorkflowKind,
        resource_id: String,
        error: String,
        next_attempt_at: Option<DateTime<Utc>>,
    ) -> RepositoryFuture<'_, ()>;

    fn complete(&self, kind: WorkflowKind, resource_id: String) -> RepositoryFuture<'_, ()>;

    fn list_recoverable(&self, kind: WorkflowKind) -> RepositoryFuture<'_, Vec<WorkflowClaim>>;
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("workflow storage failed: {0}")]
    Storage(String),
    #[error("workflow claim was not found")]
    ClaimNotFound,
    #[error("invalid workflow transition from {0}")]
    InvalidTransition(String),
}
