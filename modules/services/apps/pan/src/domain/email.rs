use std::future::Future;

use serde::Serialize;

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub struct Mailbox {
    pub id: String,
    pub name: String,
    pub total_emails: u64,
    pub unread_emails: u64,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub struct Email {
    pub id: String,
    pub subject: String,
    pub from: String,
    pub to: String,
    pub preview: String,
}

pub trait EmailProvider: Send + Sync {
    fn get_mailboxes(&self) -> impl Future<Output = Result<Vec<Mailbox>, Error>> + Send;

    fn get_unread_email(
        &self,
        mailbox_id: &str,
    ) -> impl Future<Output = Result<Option<Email>, Error>> + Send;

    fn get_email(&self, email_id: &str) -> impl Future<Output = Result<Email, Error>> + Send;

    fn add_mailbox_to_email(
        &self,
        email_id: &str,
        mailbox_id: &str,
        dry_run: bool,
    ) -> impl Future<Output = Result<(), Error>> + Send;

    fn mark_email_as_seen(
        &self,
        email_id: &str,
        dry_run: bool,
    ) -> impl Future<Output = Result<(), Error>> + Send;
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Fastmail authentication failed")]
    Unauthorized,
    #[error("Fastmail rate limit exceeded")]
    TooManyRequests,
    #[error("Fastmail returned HTTP {0}")]
    HttpStatus(u16),
    #[error("Fastmail response was invalid: {0}")]
    InvalidResponse(String),
    #[error("Fastmail request failed: {0}")]
    Network(String),
    #[error("Fastmail rejected the change: {0}")]
    MutationRejected(String),
}
