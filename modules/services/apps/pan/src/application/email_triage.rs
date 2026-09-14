use std::{collections::HashMap, sync::Arc};

use chrono::Utc;
use serde::{Deserialize, Serialize};
use tokio::sync::Mutex;

use crate::domain::email::{Email, EmailProvider, Mailbox, TriageSuggestion};
use crate::domain::workflow::{WorkflowKind, WorkflowRepository, WorkflowState};

const WORKFLOW_KIND: WorkflowKind = WorkflowKind::FastmailEmail;
const MAX_TRIAGE_FIELD_CHARS: usize = 512;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Fastmail request failed: {0}")]
    Provider(#[from] crate::domain::email::Error),
    #[error("workflow state failed: {0}")]
    Repository(#[from] crate::domain::workflow::Error),
    #[error("email reply is invalid: {0}")]
    InvalidReply(String),
    #[error("failed to encode confirmed email action: {0}")]
    ActionEncoding(String),
}

#[derive(Debug, PartialEq, Eq)]
pub enum StartOutcome {
    NoUnread(String),
    AlreadyActive(String),
    Started(String),
}

#[derive(Debug, PartialEq, Eq)]
pub enum ReplyOutcome {
    NotPending,
    NeedsConfirmation(String),
    Updated(String),
}

#[derive(Clone)]
struct ActiveEmail {
    email: Email,
    mailboxes: Vec<Mailbox>,
}

/// An unread message claimed for a deterministic triage workflow.
#[derive(Clone)]
pub struct PreparedEmail {
    email: Email,
    mailboxes: Vec<Mailbox>,
    mailbox_name: String,
}

impl PreparedEmail {
    #[must_use]
    pub fn message(&self) -> String {
        format_email(&self.email, &self.mailbox_name)
    }

    #[must_use]
    pub fn email_id(&self) -> &str {
        &self.email.id
    }

    #[must_use]
    pub fn suggestions(&self) -> Vec<TriageSuggestion> {
        vec![TriageSuggestion {
            kind: "mark_seen".to_owned(),
            value: "yes".to_owned(),
            requires_confirmation: true,
        }]
    }
}

pub struct EmailTriageService<P> {
    provider: Arc<P>,
    repository: Arc<dyn WorkflowRepository>,
    active: Mutex<HashMap<(String, String), ActiveEmail>>,
}

impl<P: EmailProvider> EmailTriageService<P> {
    #[must_use]
    pub fn new(provider: Arc<P>, repository: Arc<dyn WorkflowRepository>) -> Self {
        Self {
            provider,
            repository,
            active: Mutex::new(HashMap::new()),
        }
    }

    /// Opens an unread Fastmail message for a Matrix thread.
    ///
    /// # Errors
    ///
    /// Returns an error when the mailbox lookup or JMAP query fails.
    pub async fn start(
        &self,
        room_id: String,
        thread_id: String,
        mailbox_name: &str,
    ) -> Result<StartOutcome, Error> {
        if self
            .repository
            .find_by_conversation(WORKFLOW_KIND, room_id.clone(), thread_id.clone())
            .await?
            .is_some()
        {
            return Ok(StartOutcome::AlreadyActive(
                "This Matrix thread already has an active email.".to_owned(),
            ));
        }
        let Some(prepared) = self.prepare(mailbox_name).await? else {
            let mailboxes = self.provider.get_mailboxes().await?;
            let mailbox = unique_mailbox(mailbox_name, &mailboxes)?;
            if self.provider.get_unread_email(&mailbox.id).await?.is_some() {
                return Ok(StartOutcome::AlreadyActive(
                    "All unread emails in that mailbox are already being handled.".to_owned(),
                ));
            }
            return Ok(StartOutcome::NoUnread(format!(
                "No unreserved unread email was found in {mailbox_name}."
            )));
        };
        let message = prepared.message();
        self.track(room_id, thread_id, prepared).await?;

        Ok(StartOutcome::Started(message))
    }

    /// Claims the next available unread message in a mailbox without delivering it.
    ///
    /// # Errors
    ///
    /// Returns an error when Fastmail or durable workflow state cannot be read.
    pub async fn prepare(&self, mailbox_name: &str) -> Result<Option<PreparedEmail>, Error> {
        let mailboxes = self.provider.get_mailboxes().await?;
        let mailbox = unique_mailbox(mailbox_name, &mailboxes)?;
        let mailbox_name = mailbox.name.clone();
        for email in self.provider.get_unread_emails(&mailbox.id, 25).await? {
            let claimed = self
                .repository
                .claim(
                    WORKFLOW_KIND,
                    email.id.clone(),
                    Utc::now() + chrono::Duration::hours(24),
                )
                .await?;
            if claimed {
                return Ok(Some(PreparedEmail {
                    email,
                    mailboxes,
                    mailbox_name,
                }));
            }
        }
        Ok(None)
    }

    /// Binds a claimed email to the Matrix root event that delivered it.
    ///
    /// # Errors
    ///
    /// Returns an error when the durable conversation association cannot be stored.
    pub async fn track(
        &self,
        room_id: String,
        thread_id: String,
        prepared: PreparedEmail,
    ) -> Result<(), Error> {
        if let Err(error) = self
            .repository
            .bind_conversation(
                WORKFLOW_KIND,
                prepared.email.id.clone(),
                room_id.clone(),
                thread_id.clone(),
            )
            .await
        {
            self.repository
                .release(WORKFLOW_KIND, prepared.email.id)
                .await?;
            return Err(error.into());
        }
        self.active.lock().await.insert(
            (room_id, thread_id),
            ActiveEmail {
                email: prepared.email,
                mailboxes: prepared.mailboxes,
            },
        );
        Ok(())
    }

    /// Releases a claimed email when it could not be delivered.
    ///
    /// # Errors
    ///
    /// Returns an error when durable workflow state cannot be updated.
    pub async fn release(&self, email_id: &str) -> Result<(), Error> {
        self.repository
            .release(WORKFLOW_KIND, email_id.to_owned())
            .await
            .map_err(Error::from)
    }

    /// Applies explicitly confirmed Fastmail actions for an active Matrix thread.
    ///
    /// # Errors
    ///
    /// Returns an error for invalid actions or rejected JMAP mutations.
    pub async fn handle_reply(
        &self,
        room_id: &str,
        thread_id: &str,
        reply: &str,
    ) -> Result<ReplyOutcome, Error> {
        let Some(active) = self.load_active(room_id, thread_id).await? else {
            return Ok(ReplyOutcome::NotPending);
        };
        if reply
            .lines()
            .any(|line| line.trim().eq_ignore_ascii_case("cancel"))
        {
            self.active
                .lock()
                .await
                .remove(&(room_id.to_owned(), thread_id.to_owned()));
            self.repository
                .release(WORKFLOW_KIND, active.email.id)
                .await?;
            return Ok(ReplyOutcome::Updated(
                "Fastmail triage was cancelled without changes.".to_owned(),
            ));
        }
        if !reply
            .lines()
            .any(|line| line.trim().eq_ignore_ascii_case("confirm"))
        {
            return Ok(ReplyOutcome::NeedsConfirmation(
                "No email changes were made. Include a line containing `confirm`.".to_owned(),
            ));
        }

        let actions = parse_actions(reply, &active.mailboxes)?;
        if actions.mailbox_id.is_none() && !actions.mark_seen {
            return Err(Error::InvalidReply(
                "specify `mailbox: NAME` and/or `mark seen: yes`".to_owned(),
            ));
        }
        self.repository
            .begin_operation(
                WORKFLOW_KIND,
                active.email.id.clone(),
                uuid::Uuid::new_v4().to_string(),
                serde_json::to_string(&actions)
                    .map_err(|error| Error::ActionEncoding(error.to_string()))?,
            )
            .await?;
        let result = async {
            if let Some(mailbox_id) = actions.mailbox_id.as_deref() {
                add_mailbox_with_retry(self.provider.as_ref(), &active.email.id, mailbox_id)
                    .await?;
            }
            if actions.mark_seen {
                mark_seen_with_retry(self.provider.as_ref(), &active.email.id).await?;
            }
            Ok::<_, crate::domain::email::Error>(())
        }
        .await;
        if let Err(error) = result {
            let next_attempt_at =
                is_transient_email_error(&error).then(|| Utc::now() + chrono::Duration::minutes(1));
            self.repository
                .record_failure(
                    WORKFLOW_KIND,
                    active.email.id.clone(),
                    error.to_string(),
                    next_attempt_at,
                )
                .await?;
            return Err(error.into());
        }

        self.active
            .lock()
            .await
            .remove(&(room_id.to_owned(), thread_id.to_owned()));
        self.repository
            .complete(WORKFLOW_KIND, active.email.id.clone())
            .await?;
        Ok(ReplyOutcome::Updated(format!(
            "Fastmail message `{}` was updated.",
            active.email.subject
        )))
    }

    /// Converts ambiguous in-flight Fastmail mutations into explicit manual-retry failures.
    ///
    /// Fastmail data currently exposed by the provider cannot prove both mailbox and seen state,
    /// so recovery deliberately never replays a mutation blindly.
    ///
    /// # Errors
    ///
    /// Returns an error when durable workflow state cannot be read or updated.
    pub async fn recover(&self) -> Result<usize, Error> {
        let claims = self.repository.list_recoverable(WORKFLOW_KIND).await?;
        let applying = claims
            .into_iter()
            .filter(|claim| claim.state == WorkflowState::Applying)
            .collect::<Vec<_>>();
        for claim in &applying {
            self.repository
                .record_failure(
                    WORKFLOW_KIND,
                    claim.resource_id.clone(),
                    "Fastmail mutation outcome is ambiguous; manual confirmation required"
                        .to_owned(),
                    None,
                )
                .await?;
        }
        Ok(applying.len())
    }

    async fn load_active(
        &self,
        room_id: &str,
        thread_id: &str,
    ) -> Result<Option<ActiveEmail>, Error> {
        let key = (room_id.to_owned(), thread_id.to_owned());
        if let Some(active) = self.active.lock().await.get(&key).cloned() {
            return Ok(Some(active));
        }
        let Some(claim) = self
            .repository
            .find_by_conversation(WORKFLOW_KIND, room_id.to_owned(), thread_id.to_owned())
            .await?
        else {
            return Ok(None);
        };
        let (email, mailboxes) = tokio::try_join!(
            self.provider.get_email(&claim.resource_id),
            self.provider.get_mailboxes(),
        )?;
        let active = ActiveEmail { email, mailboxes };
        self.active.lock().await.insert(key, active.clone());
        Ok(Some(active))
    }
}

async fn add_mailbox_with_retry<P: EmailProvider>(
    provider: &P,
    email_id: &str,
    mailbox_id: &str,
) -> Result<(), crate::domain::email::Error> {
    let mut delay = std::time::Duration::from_millis(100);
    for attempt in 0..3 {
        match provider
            .add_mailbox_to_email(email_id, mailbox_id, false)
            .await
        {
            Ok(()) => return Ok(()),
            Err(error) if attempt < 2 && is_transient_email_error(&error) => {
                tokio::time::sleep(delay).await;
                delay *= 2;
            }
            Err(error) => return Err(error),
        }
    }
    unreachable!("bounded retry loop always returns")
}

async fn mark_seen_with_retry<P: EmailProvider>(
    provider: &P,
    email_id: &str,
) -> Result<(), crate::domain::email::Error> {
    let mut delay = std::time::Duration::from_millis(100);
    for attempt in 0..3 {
        match provider.mark_email_as_seen(email_id, false).await {
            Ok(()) => return Ok(()),
            Err(error) if attempt < 2 && is_transient_email_error(&error) => {
                tokio::time::sleep(delay).await;
                delay *= 2;
            }
            Err(error) => return Err(error),
        }
    }
    unreachable!("bounded retry loop always returns")
}

fn is_transient_email_error(error: &crate::domain::email::Error) -> bool {
    matches!(
        error,
        crate::domain::email::Error::TooManyRequests
            | crate::domain::email::Error::Network(_)
            | crate::domain::email::Error::HttpStatus(500..=599)
    )
}

#[derive(Deserialize, Serialize)]
struct EmailActions {
    mailbox_id: Option<String>,
    mark_seen: bool,
}

fn parse_actions(reply: &str, mailboxes: &[Mailbox]) -> Result<EmailActions, Error> {
    let mut actions = EmailActions {
        mailbox_id: None,
        mark_seen: false,
    };
    for line in reply.lines().map(str::trim).filter(|line| !line.is_empty()) {
        if line.eq_ignore_ascii_case("confirm") {
            continue;
        }
        let Some((field, value)) = line.split_once(':') else {
            return Err(Error::InvalidReply(format!(
                "expected `field: value`, received `{line}`"
            )));
        };
        match field.trim().to_ascii_lowercase().as_str() {
            "mailbox" | "tag" => {
                actions.mailbox_id = Some(unique_mailbox(value.trim(), mailboxes)?.id.clone());
            }
            "mark seen" => {
                actions.mark_seen = match value.trim().to_ascii_lowercase().as_str() {
                    "yes" | "true" => true,
                    "no" | "false" => false,
                    _ => {
                        return Err(Error::InvalidReply(
                            "`mark seen` must be `yes` or `no`".to_owned(),
                        ));
                    }
                };
            }
            field => return Err(Error::InvalidReply(format!("unknown field `{field}`"))),
        }
    }
    Ok(actions)
}

fn unique_mailbox<'a>(name: &str, mailboxes: &'a [Mailbox]) -> Result<&'a Mailbox, Error> {
    let matches = mailboxes
        .iter()
        .filter(|mailbox| mailbox.name.eq_ignore_ascii_case(name.trim()))
        .collect::<Vec<_>>();
    match matches.as_slice() {
        [mailbox] => Ok(mailbox),
        [] => Err(Error::InvalidReply(format!(
            "mailbox/tag `{name}` was not found"
        ))),
        _ => Err(Error::InvalidReply(format!(
            "mailbox/tag `{name}` is ambiguous"
        ))),
    }
}

fn format_email(email: &Email, mailbox: &str) -> String {
    let suggestions = serde_json::to_string(&[TriageSuggestion {
        kind: "mark_seen".to_owned(),
        value: "yes".to_owned(),
        requires_confirmation: true,
    }])
    .expect("triage suggestions are serializable");
    format!(
        "## Fastmail triage\n\n\
         - **Mailbox:** {mailbox}\n\
         - **From:** {}\n\
         - **To:** {}\n\
         - **Subject:** {}\n\
         - **Preview:** {}\n\n\
         Suggestions (not applied): `{suggestions}`\n\n\
         Reply in this thread with explicit actions:\n\n\
         ```text\n\
         confirm\n\
         mailbox: Archive\n\
         mark seen: yes\n\
         ```\n\n\
         Omit either action if it should not be applied, or reply `cancel` to abandon the triage. This request expires after 24 hours.",
        limit_field(&email.from),
        limit_field(&email.to),
        limit_field(&email.subject),
        limit_field(&email.preview)
    )
}

fn limit_field(value: &str) -> String {
    let mut characters = value.chars();
    let limited = characters
        .by_ref()
        .take(MAX_TRIAGE_FIELD_CHARS)
        .collect::<String>();
    if characters.next().is_some() {
        format!("{limited}…")
    } else {
        limited
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Mutex as StdMutex;

    use super::*;
    use crate::infrastructure::workflow_sqlite::SqliteWorkflowRepository;

    async fn repository() -> (tempfile::TempDir, Arc<dyn WorkflowRepository>) {
        let directory = tempfile::tempdir().unwrap();
        let repository = SqliteWorkflowRepository::open(directory.path().join("workflows.sqlite3"))
            .await
            .unwrap();
        (directory, Arc::new(repository))
    }

    #[derive(Default)]
    struct FakeProvider {
        calls: StdMutex<Vec<String>>,
    }

    impl EmailProvider for FakeProvider {
        async fn get_mailboxes(&self) -> Result<Vec<Mailbox>, crate::domain::email::Error> {
            Ok(vec![
                Mailbox {
                    id: "inbox".to_owned(),
                    name: "Inbox".to_owned(),
                    total_emails: 2,
                    unread_emails: 1,
                },
                Mailbox {
                    id: "archive".to_owned(),
                    name: "Archive".to_owned(),
                    total_emails: 0,
                    unread_emails: 0,
                },
            ])
        }

        async fn get_unread_email(
            &self,
            _mailbox_id: &str,
        ) -> Result<Option<Email>, crate::domain::email::Error> {
            Ok(Some(Email {
                id: "email-1".to_owned(),
                subject: "Receipt".to_owned(),
                from: "shop@example.com".to_owned(),
                to: "me@example.com".to_owned(),
                preview: "Thank you".to_owned(),
            }))
        }

        async fn get_unread_emails(
            &self,
            _mailbox_id: &str,
            _limit: u32,
        ) -> Result<Vec<Email>, crate::domain::email::Error> {
            Ok(["email-1", "email-2"]
                .into_iter()
                .map(|id| Email {
                    id: id.to_owned(),
                    subject: "Receipt".to_owned(),
                    from: "shop@example.com".to_owned(),
                    to: "me@example.com".to_owned(),
                    preview: "Thank you".to_owned(),
                })
                .collect())
        }

        async fn get_email(&self, email_id: &str) -> Result<Email, crate::domain::email::Error> {
            let mut email = self.get_unread_email("inbox").await?.unwrap();
            email.id = email_id.to_owned();
            Ok(email)
        }

        async fn add_mailbox_to_email(
            &self,
            email_id: &str,
            mailbox_id: &str,
            dry_run: bool,
        ) -> Result<(), crate::domain::email::Error> {
            self.calls
                .lock()
                .unwrap()
                .push(format!("mailbox:{email_id}:{mailbox_id}:{dry_run}"));
            Ok(())
        }

        async fn mark_email_as_seen(
            &self,
            email_id: &str,
            dry_run: bool,
        ) -> Result<(), crate::domain::email::Error> {
            self.calls
                .lock()
                .unwrap()
                .push(format!("seen:{email_id}:{dry_run}"));
            Ok(())
        }
    }

    #[tokio::test]
    async fn confirmation_applies_mailbox_and_seen_actions() {
        let provider = Arc::new(FakeProvider::default());
        let (_directory, repository) = repository().await;
        let service = EmailTriageService::new(Arc::clone(&provider), repository);
        assert!(matches!(
            service
                .start("room".to_owned(), "thread".to_owned(), "Inbox")
                .await
                .unwrap(),
            StartOutcome::Started(_)
        ));

        let outcome = service
            .handle_reply(
                "room",
                "thread",
                "confirm\nmailbox: Archive\nmark seen: yes",
            )
            .await
            .unwrap();

        assert!(matches!(outcome, ReplyOutcome::Updated(_)));
        assert_eq!(
            *provider.calls.lock().unwrap(),
            ["mailbox:email-1:archive:false", "seen:email-1:false"]
        );
    }

    #[tokio::test]
    async fn email_selection_advances_past_a_reserved_message() {
        let (_directory, repository) = repository().await;
        let service = EmailTriageService::new(Arc::new(FakeProvider::default()), repository);
        service
            .start("room".to_owned(), "first".to_owned(), "Inbox")
            .await
            .unwrap();

        let outcome = service
            .start("room".to_owned(), "second".to_owned(), "Inbox")
            .await
            .unwrap();

        assert!(matches!(outcome, StartOutcome::Started(_)));
    }

    #[tokio::test]
    async fn missing_confirmation_never_mutates_fastmail() {
        let provider = Arc::new(FakeProvider::default());
        let (_directory, repository) = repository().await;
        let service = EmailTriageService::new(Arc::clone(&provider), repository);
        service
            .start("room".to_owned(), "thread".to_owned(), "Inbox")
            .await
            .unwrap();

        let outcome = service
            .handle_reply("room", "thread", "mark seen: yes")
            .await
            .unwrap();

        assert!(matches!(outcome, ReplyOutcome::NeedsConfirmation(_)));
        assert!(provider.calls.lock().unwrap().is_empty());
    }

    #[tokio::test]
    async fn pending_email_can_be_confirmed_after_service_restart() {
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().join("workflows.sqlite3");
        let provider = Arc::new(FakeProvider::default());
        let repository: Arc<dyn WorkflowRepository> =
            Arc::new(SqliteWorkflowRepository::open(&path).await.unwrap());
        let service = EmailTriageService::new(Arc::clone(&provider), repository);
        service
            .start("room".to_owned(), "thread".to_owned(), "Inbox")
            .await
            .unwrap();
        drop(service);

        let reopened: Arc<dyn WorkflowRepository> =
            Arc::new(SqliteWorkflowRepository::open(path).await.unwrap());
        let restarted = EmailTriageService::new(Arc::clone(&provider), reopened);
        let outcome = restarted
            .handle_reply("room", "thread", "confirm\nmark seen: yes")
            .await
            .unwrap();

        assert!(matches!(outcome, ReplyOutcome::Updated(_)));
        assert_eq!(*provider.calls.lock().unwrap(), ["seen:email-1:false"]);
    }

    #[tokio::test]
    async fn recovery_marks_an_inflight_fastmail_mutation_for_manual_retry() {
        let provider = Arc::new(FakeProvider::default());
        let (_directory, repository) = repository().await;
        repository
            .claim(
                WORKFLOW_KIND,
                "email-1".to_owned(),
                Utc::now() + chrono::Duration::hours(1),
            )
            .await
            .unwrap();
        repository
            .bind_conversation(
                WORKFLOW_KIND,
                "email-1".to_owned(),
                "room".to_owned(),
                "thread".to_owned(),
            )
            .await
            .unwrap();
        repository
            .begin_operation(
                WORKFLOW_KIND,
                "email-1".to_owned(),
                "operation".to_owned(),
                r#"{"mailbox_id":"archive","mark_seen":true}"#.to_owned(),
            )
            .await
            .unwrap();
        let service = EmailTriageService::new(provider, Arc::clone(&repository));

        let recovered = service.recover().await.unwrap();
        let claim = repository
            .find_by_conversation(WORKFLOW_KIND, "room".to_owned(), "thread".to_owned())
            .await
            .unwrap()
            .unwrap();

        assert_eq!(recovered, 1);
        assert_eq!(claim.state, WorkflowState::Failed);
        assert_eq!(
            claim.last_error.as_deref(),
            Some("Fastmail mutation outcome is ambiguous; manual confirmation required")
        );
    }

    #[test]
    fn triage_rendering_limits_untrusted_message_fields() {
        let email = Email {
            id: "email-1".to_owned(),
            subject: "x".repeat(MAX_TRIAGE_FIELD_CHARS + 1),
            from: "from@example.com".to_owned(),
            to: "to@example.com".to_owned(),
            preview: "preview".to_owned(),
        };

        let message = format_email(&email, "Inbox");

        assert!(message.contains(&format!("{}…", "x".repeat(MAX_TRIAGE_FIELD_CHARS))));
        assert!(!message.contains(&"x".repeat(MAX_TRIAGE_FIELD_CHARS + 1)));
    }

    #[test]
    fn triage_suggestions_are_structured_and_require_confirmation() {
        let suggestion = TriageSuggestion {
            kind: "mark_seen".to_owned(),
            value: "yes".to_owned(),
            requires_confirmation: true,
        };

        assert_eq!(
            serde_json::to_string(&suggestion).unwrap(),
            r#"{"kind":"mark_seen","value":"yes","requires_confirmation":true}"#
        );
    }
}
