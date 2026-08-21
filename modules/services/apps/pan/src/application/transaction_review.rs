use std::{collections::HashMap, sync::Arc};

use chrono::Utc;
use tokio::sync::Mutex;

use crate::domain::finance::{
    Account, AccountSource, Category, FinanceProvider, RecurringItem, Tag, Transaction,
    TransactionUpdate,
};
use crate::domain::workflow::{WorkflowKind, WorkflowRepository, WorkflowState};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Lunch Money request failed: {0}")]
    Provider(#[from] crate::domain::finance::Error),
    #[error("workflow state failed: {0}")]
    Repository(#[from] crate::domain::workflow::Error),
    #[error("review reply is invalid: {0}")]
    InvalidReply(String),
    #[error("failed to encode confirmed transaction action: {0}")]
    ActionEncoding(String),
}

#[derive(Clone, Debug, PartialEq)]
pub struct PreparedReview {
    transaction: Transaction,
    categories: Vec<Category>,
    tags: Vec<Tag>,
    account: Option<String>,
    recurring: Option<RecurringItem>,
}

impl PreparedReview {
    #[must_use]
    pub fn transaction_id(&self) -> i64 {
        self.transaction.id
    }

    #[must_use]
    pub fn message(&self) -> String {
        let category = self
            .transaction
            .category_id
            .map_or_else(|| "None".to_owned(), |id| self.category_label(id));
        let tags = if self.transaction.tag_ids.is_empty() {
            "None".to_owned()
        } else {
            self.transaction
                .tag_ids
                .iter()
                .map(|id| self.tag_label(*id))
                .collect::<Vec<_>>()
                .join(", ")
        };
        let notes = self.transaction.notes.as_deref().unwrap_or("None");
        let account = self.account.as_deref().unwrap_or("Cash transaction");
        let recurring = self.recurring.as_ref().map_or_else(
            || "None".to_owned(),
            |item| format!("{} (ID {})", item.description, item.id),
        );

        format!(
            "## Lunch Money transaction review\n\n\
             - **Transaction ID:** {}\n\
             - **Date:** {}\n\
             - **Payee:** {}\n\
             - **Original name:** {}\n\
             - **Amount:** {:.2} {}\n\
             - **Account:** {}\n\
             - **Category:** {}\n\
             - **Tags:** {}\n\
             - **Notes:** {}\n\
             - **Recurring item:** {}\n\
             - **Source:** {}\n\n\
             Reply in this thread with `confirm` and any changes:\n\n\
             ```text\n\
             confirm\n\
             category: Groceries\n\
             notes: Dinner with Alice\n\
             tags: social, dining\n\
             ```\n\n\
             Omit a field to keep it unchanged, use `none` to clear it, or reply `cancel` to abandon the review. This request expires after 24 hours.",
            self.transaction.id,
            self.transaction.date,
            self.transaction.payee,
            self.transaction.original_name.as_deref().unwrap_or("None"),
            self.transaction.amount,
            self.transaction.currency.to_uppercase(),
            account,
            category,
            tags,
            notes,
            recurring,
            self.transaction.source.as_deref().unwrap_or("Unknown"),
        )
    }

    fn category_label(&self, id: i32) -> String {
        self.categories
            .iter()
            .find(|category| category.id == id)
            .map_or_else(
                || format!("Unknown category (ID {id})"),
                |category| format!("{} (ID {id})", category.name),
            )
    }

    fn tag_label(&self, id: i32) -> String {
        self.tags.iter().find(|tag| tag.id == id).map_or_else(
            || format!("Unknown tag (ID {id})"),
            |tag| format!("{} (ID {id})", tag.name),
        )
    }
}

#[derive(Debug, PartialEq)]
pub enum ReplyOutcome {
    NotPending,
    NeedsConfirmation(String),
    Updated(String),
}

const WORKFLOW_KIND: WorkflowKind = WorkflowKind::LunchMoneyTransaction;

pub struct TransactionReviewService<P> {
    provider: Arc<P>,
    repository: Arc<dyn WorkflowRepository>,
    pending: Mutex<HashMap<(String, String), PreparedReview>>,
}

impl<P: FinanceProvider> TransactionReviewService<P> {
    #[must_use]
    pub fn new(provider: Arc<P>, repository: Arc<dyn WorkflowRepository>) -> Self {
        Self {
            provider,
            repository,
            pending: Mutex::new(HashMap::new()),
        }
    }

    /// Fetches and resolves the most recent unreviewed transaction.
    ///
    /// # Errors
    ///
    /// Returns an error when any required Lunch Money lookup fails.
    pub async fn prepare(&self) -> Result<Option<PreparedReview>, Error> {
        let transactions = self.provider.get_unreviewed_transactions().await?;
        for transaction in transactions {
            let transaction_id = transaction.id;
            let claimed = self
                .repository
                .claim(
                    WORKFLOW_KIND,
                    transaction_id.to_string(),
                    Utc::now() + chrono::Duration::hours(24),
                )
                .await?;
            if !claimed {
                continue;
            }
            match self.hydrate(transaction).await {
                Ok(review) => return Ok(Some(review)),
                Err(error) => {
                    if let Err(release_error) = self.release(transaction_id).await {
                        tracing::error!(%release_error, transaction_id, "Failed to release unenriched workflow claim");
                    }
                    return Err(error);
                }
            }
        }
        Ok(None)
    }

    /// Associates a prepared transaction with the Matrix conversation that delivered it.
    ///
    /// # Errors
    ///
    /// Returns an error when the durable workflow claim cannot be updated.
    pub async fn track(
        &self,
        room_id: String,
        thread_id: String,
        review: PreparedReview,
    ) -> Result<(), Error> {
        let transaction_id = review.transaction.id;
        self.repository
            .bind_conversation(
                WORKFLOW_KIND,
                transaction_id.to_string(),
                room_id.clone(),
                thread_id.clone(),
            )
            .await?;
        self.pending
            .lock()
            .await
            .insert((room_id, thread_id), review);
        Ok(())
    }

    /// Releases a transaction so a later schedule may claim it again.
    ///
    /// # Errors
    ///
    /// Returns an error when the durable claim cannot be removed.
    pub async fn release(&self, transaction_id: i64) -> Result<(), Error> {
        self.repository
            .release(WORKFLOW_KIND, transaction_id.to_string())
            .await
            .map_err(Error::from)
    }

    /// Reconciles mutations that may have completed before Pan stopped.
    ///
    /// # Errors
    ///
    /// Returns an error when durable workflow state cannot be read or updated.
    pub async fn recover(&self) -> Result<usize, Error> {
        let claims = self.repository.list_recoverable(WORKFLOW_KIND).await?;
        let mut completed = 0;
        for claim in claims
            .into_iter()
            .filter(|claim| claim.state == WorkflowState::Applying)
        {
            let Some(payload) = claim.action_payload.as_deref() else {
                self.repository
                    .record_failure(
                        WORKFLOW_KIND,
                        claim.resource_id,
                        "confirmed action payload is missing; manual confirmation required"
                            .to_owned(),
                        None,
                    )
                    .await?;
                continue;
            };
            let update: TransactionUpdate = serde_json::from_str(payload)
                .map_err(|error| Error::ActionEncoding(error.to_string()))?;
            let transaction_id = claim.resource_id.parse::<i64>().map_err(|error| {
                crate::domain::workflow::Error::Storage(format!(
                    "invalid Lunch Money transaction ID {}: {error}",
                    claim.resource_id
                ))
            })?;
            match self.provider.get_transaction(transaction_id).await {
                Ok(transaction) if transaction_matches_update(&transaction, &update) => {
                    self.repository
                        .complete(WORKFLOW_KIND, transaction_id.to_string())
                        .await?;
                    completed += 1;
                }
                Ok(_) => {
                    self.repository
                        .record_failure(
                            WORKFLOW_KIND,
                            transaction_id.to_string(),
                            "Lunch Money does not match the confirmed action; manual confirmation required"
                                .to_owned(),
                            None,
                        )
                        .await?;
                }
                Err(error) => {
                    self.repository
                        .record_failure(
                            WORKFLOW_KIND,
                            transaction_id.to_string(),
                            format!("reconciliation lookup failed: {error}"),
                            Some(Utc::now() + chrono::Duration::minutes(1)),
                        )
                        .await?;
                }
            }
        }
        Ok(completed)
    }

    /// Validates a thread reply and applies the confirmed transaction update.
    ///
    /// # Errors
    ///
    /// Returns an error for invalid fields or a rejected Lunch Money update.
    pub async fn handle_reply(
        &self,
        room_id: &str,
        thread_id: &str,
        reply: &str,
    ) -> Result<ReplyOutcome, Error> {
        let Some(review) = self.load_review(room_id, thread_id).await? else {
            return Ok(ReplyOutcome::NotPending);
        };
        if reply
            .lines()
            .any(|line| line.trim().eq_ignore_ascii_case("cancel"))
        {
            self.pending
                .lock()
                .await
                .remove(&(room_id.to_owned(), thread_id.to_owned()));
            self.release(review.transaction.id).await?;
            return Ok(ReplyOutcome::Updated(format!(
                "Transaction {} review was cancelled without changes.",
                review.transaction.id
            )));
        }
        if !reply
            .lines()
            .any(|line| line.trim().eq_ignore_ascii_case("confirm"))
        {
            return Ok(ReplyOutcome::NeedsConfirmation(
                "No changes were made. Include a line containing `confirm` to approve this transaction."
                    .to_owned(),
            ));
        }

        let update = parse_update(&review, reply)?;
        self.repository
            .begin_operation(
                WORKFLOW_KIND,
                review.transaction.id.to_string(),
                uuid::Uuid::new_v4().to_string(),
                serde_json::to_string(&update)
                    .map_err(|error| Error::ActionEncoding(error.to_string()))?,
            )
            .await?;
        let key = (room_id.to_owned(), thread_id.to_owned());
        let removed = self.pending.lock().await.remove(&key);
        let Some(removed) = removed else {
            return Ok(ReplyOutcome::NotPending);
        };
        if let Err(error) =
            update_transaction_with_retry(self.provider.as_ref(), review.transaction.id, &update)
                .await
        {
            self.pending.lock().await.insert(key, removed);
            let next_attempt_at = is_transient_finance_error(&error)
                .then(|| Utc::now() + chrono::Duration::minutes(1));
            self.repository
                .record_failure(
                    WORKFLOW_KIND,
                    review.transaction.id.to_string(),
                    error.to_string(),
                    next_attempt_at,
                )
                .await?;
            return Err(error.into());
        }
        self.repository
            .complete(WORKFLOW_KIND, review.transaction.id.to_string())
            .await?;

        Ok(ReplyOutcome::Updated(format!(
            "Transaction {} was updated and marked reviewed.",
            review.transaction.id
        )))
    }

    async fn load_review(
        &self,
        room_id: &str,
        thread_id: &str,
    ) -> Result<Option<PreparedReview>, Error> {
        let key = (room_id.to_owned(), thread_id.to_owned());
        if let Some(review) = self.pending.lock().await.get(&key).cloned() {
            return Ok(Some(review));
        }
        let Some(claim) = self
            .repository
            .find_by_conversation(WORKFLOW_KIND, room_id.to_owned(), thread_id.to_owned())
            .await?
        else {
            return Ok(None);
        };
        let transaction_id = claim.resource_id.parse::<i64>().map_err(|error| {
            crate::domain::workflow::Error::Storage(format!(
                "invalid Lunch Money transaction ID {}: {error}",
                claim.resource_id
            ))
        })?;
        let transaction = self.provider.get_transaction(transaction_id).await?;
        let review = self.hydrate(transaction).await?;
        self.pending.lock().await.insert(key, review.clone());
        Ok(Some(review))
    }

    async fn hydrate(&self, transaction: Transaction) -> Result<PreparedReview, Error> {
        let (categories, tags, accounts) = tokio::try_join!(
            self.provider.get_categories(),
            self.provider.get_tags(),
            self.provider.get_accounts(),
        )?;
        let account = resolve_account(&transaction, &accounts);
        let recurring = match transaction.recurring_id {
            Some(id) => Some(self.provider.get_recurring_item(id).await?),
            None => None,
        };
        Ok(PreparedReview {
            transaction,
            categories,
            tags,
            account,
            recurring,
        })
    }
}

fn transaction_matches_update(transaction: &Transaction, update: &TransactionUpdate) -> bool {
    transaction.category_id == update.category_id
        && transaction.tag_ids == update.tag_ids
        && transaction.notes == update.notes
}

async fn update_transaction_with_retry<P: FinanceProvider>(
    provider: &P,
    transaction_id: i64,
    update: &TransactionUpdate,
) -> Result<(), crate::domain::finance::Error> {
    let mut delay = std::time::Duration::from_millis(100);
    for attempt in 0..3 {
        match provider
            .update_transaction(transaction_id, update, false)
            .await
        {
            Ok(()) => return Ok(()),
            Err(error) if attempt < 2 && is_transient_finance_error(&error) => {
                tokio::time::sleep(delay).await;
                delay *= 2;
            }
            Err(error) => return Err(error),
        }
    }
    unreachable!("bounded retry loop always returns")
}

fn is_transient_finance_error(error: &crate::domain::finance::Error) -> bool {
    matches!(
        error,
        crate::domain::finance::Error::TooManyRequests
            | crate::domain::finance::Error::Internal
            | crate::domain::finance::Error::NetworkError(_)
    )
}

fn resolve_account(transaction: &Transaction, accounts: &[Account]) -> Option<String> {
    let (id, source) = match (transaction.plaid_account_id, transaction.manual_account_id) {
        (Some(id), _) => (id, AccountSource::Plaid),
        (_, Some(id)) => (id, AccountSource::Manual),
        _ => return None,
    };
    accounts
        .iter()
        .find(|account| account.id == id && account.source == source)
        .map_or_else(
            || Some(format!("Unknown {source:?} account (ID {id})")),
            |account| Some(format!("{} (ID {id})", account.name)),
        )
}

fn parse_update(review: &PreparedReview, reply: &str) -> Result<TransactionUpdate, Error> {
    let mut update = TransactionUpdate {
        category_id: review.transaction.category_id,
        tag_ids: review.transaction.tag_ids.clone(),
        notes: review.transaction.notes.clone(),
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
        let value = value.trim();
        match field.trim().to_ascii_lowercase().as_str() {
            "category" => update.category_id = resolve_category(value, &review.categories)?,
            "tags" => update.tag_ids = resolve_tags(value, &review.tags)?,
            "notes" => {
                update.notes = if value.eq_ignore_ascii_case("none") {
                    None
                } else if value.eq_ignore_ascii_case("keep") {
                    review.transaction.notes.clone()
                } else {
                    Some(value.to_owned())
                };
            }
            field => return Err(Error::InvalidReply(format!("unknown field `{field}`"))),
        }
    }

    Ok(update)
}

fn resolve_category(value: &str, categories: &[Category]) -> Result<Option<i32>, Error> {
    if value.eq_ignore_ascii_case("none") {
        return Ok(None);
    }
    if value.eq_ignore_ascii_case("keep") {
        return Err(Error::InvalidReply(
            "omit `category` to keep the current value".to_owned(),
        ));
    }
    let matches = categories
        .iter()
        .filter(|category| !category.archived && category.name.eq_ignore_ascii_case(value))
        .collect::<Vec<_>>();
    match matches.as_slice() {
        [category] => Ok(Some(category.id)),
        [] => Err(Error::InvalidReply(format!(
            "category `{value}` was not found"
        ))),
        _ => Err(Error::InvalidReply(format!(
            "category `{value}` is ambiguous"
        ))),
    }
}

fn resolve_tags(value: &str, tags: &[Tag]) -> Result<Vec<i32>, Error> {
    if value.eq_ignore_ascii_case("none") {
        return Ok(Vec::new());
    }
    if value.eq_ignore_ascii_case("keep") {
        return Err(Error::InvalidReply(
            "omit `tags` to keep the current value".to_owned(),
        ));
    }

    value
        .split(',')
        .map(str::trim)
        .filter(|name| !name.is_empty())
        .map(|name| {
            let matches = tags
                .iter()
                .filter(|tag| !tag.archived && tag.name.eq_ignore_ascii_case(name))
                .collect::<Vec<_>>();
            match matches.as_slice() {
                [tag] => Ok(tag.id),
                [] => Err(Error::InvalidReply(format!("tag `{name}` was not found"))),
                _ => Err(Error::InvalidReply(format!("tag `{name}` is ambiguous"))),
            }
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use std::sync::{
        Mutex as StdMutex,
        atomic::{AtomicUsize, Ordering},
    };

    use super::*;
    use crate::domain::finance::{AccountStatus, User};
    use crate::infrastructure::workflow_sqlite::SqliteWorkflowRepository;

    async fn repository() -> (tempfile::TempDir, Arc<dyn WorkflowRepository>) {
        let directory = tempfile::tempdir().unwrap();
        let repository = SqliteWorkflowRepository::open(directory.path().join("workflows.sqlite3"))
            .await
            .unwrap();
        (directory, Arc::new(repository))
    }

    struct FakeProvider {
        updates: StdMutex<Vec<(i64, TransactionUpdate, bool)>>,
        transient_failures: AtomicUsize,
    }

    impl FakeProvider {
        fn new() -> Self {
            Self {
                updates: StdMutex::new(Vec::new()),
                transient_failures: AtomicUsize::new(0),
            }
        }

        fn with_transient_failures(failures: usize) -> Self {
            Self {
                updates: StdMutex::new(Vec::new()),
                transient_failures: AtomicUsize::new(failures),
            }
        }
    }

    impl FinanceProvider for FakeProvider {
        async fn get_accounts(&self) -> Result<Vec<Account>, crate::domain::finance::Error> {
            Ok(vec![Account {
                id: 7,
                source: AccountSource::Plaid,
                name: "Checking".to_owned(),
                institution_name: "Bank".to_owned(),
                account_type: "depository".to_owned(),
                subtype: "checking".to_owned(),
                balance: 100.0,
                currency: "EUR".to_owned(),
                balance_as_of: chrono::Utc::now(),
                status: AccountStatus::Active,
            }])
        }

        async fn get_user(&self) -> Result<User, crate::domain::finance::Error> {
            unreachable!()
        }

        async fn get_categories(&self) -> Result<Vec<Category>, crate::domain::finance::Error> {
            Ok(vec![Category {
                id: 2,
                name: "Groceries".to_owned(),
                description: None,
                is_income: false,
                archived: false,
            }])
        }

        async fn get_tags(&self) -> Result<Vec<Tag>, crate::domain::finance::Error> {
            Ok(vec![Tag {
                id: 3,
                name: "Social".to_owned(),
                description: None,
                archived: false,
            }])
        }

        async fn get_unreviewed_transactions(
            &self,
        ) -> Result<Vec<Transaction>, crate::domain::finance::Error> {
            Ok(vec![
                Transaction {
                    id: 42,
                    date: "2026-09-12".to_owned(),
                    payee: "Market".to_owned(),
                    amount: 12.5,
                    currency: "eur".to_owned(),
                    to_base: 12.5,
                    category_id: Some(2),
                    tag_ids: vec![3],
                    recurring_id: Some(9),
                    plaid_account_id: Some(7),
                    manual_account_id: None,
                    original_name: Some("MARKET 123".to_owned()),
                    notes: None,
                    source: Some("plaid".to_owned()),
                },
                Transaction {
                    id: 43,
                    date: "2026-09-11".to_owned(),
                    payee: "Bakery".to_owned(),
                    amount: 4.5,
                    currency: "eur".to_owned(),
                    to_base: 4.5,
                    category_id: None,
                    tag_ids: Vec::new(),
                    recurring_id: None,
                    plaid_account_id: Some(7),
                    manual_account_id: None,
                    original_name: None,
                    notes: None,
                    source: Some("plaid".to_owned()),
                },
            ])
        }

        async fn get_transaction(
            &self,
            id: i64,
        ) -> Result<Transaction, crate::domain::finance::Error> {
            Ok(self
                .get_unreviewed_transactions()
                .await?
                .into_iter()
                .find(|transaction| transaction.id == id)
                .expect("fake transaction should exist"))
        }

        async fn get_recurring_item(
            &self,
            id: i32,
        ) -> Result<RecurringItem, crate::domain::finance::Error> {
            Ok(RecurringItem {
                id,
                description: "Weekly groceries".to_owned(),
            })
        }

        async fn update_transaction(
            &self,
            id: i64,
            update: &TransactionUpdate,
            dry_run: bool,
        ) -> Result<(), crate::domain::finance::Error> {
            if self
                .transient_failures
                .fetch_update(Ordering::SeqCst, Ordering::SeqCst, |remaining| {
                    remaining.checked_sub(1)
                })
                .is_ok()
            {
                return Err(crate::domain::finance::Error::NetworkError(
                    "temporary".to_owned(),
                ));
            }
            self.updates
                .lock()
                .expect("updates mutex should not be poisoned")
                .push((id, update.clone(), dry_run));
            Ok(())
        }
    }

    #[tokio::test]
    async fn prepare_resolves_transaction_ids_to_names() {
        let (_directory, repository) = repository().await;
        let service = TransactionReviewService::new(Arc::new(FakeProvider::new()), repository);

        let review = service
            .prepare()
            .await
            .expect("review should prepare")
            .expect("transaction should exist");

        assert!(review.message().contains("Checking (ID 7)"));
        assert!(review.message().contains("Groceries (ID 2)"));
        assert!(review.message().contains("Social (ID 3)"));
        assert!(review.message().contains("Weekly groceries"));
    }

    #[tokio::test]
    async fn reply_without_confirmation_does_not_update_transaction() {
        let provider = Arc::new(FakeProvider::new());
        let (_directory, repository) = repository().await;
        let service = TransactionReviewService::new(Arc::clone(&provider), repository);
        let review = service.prepare().await.unwrap().unwrap();
        service
            .track("room".to_owned(), "thread".to_owned(), review)
            .await
            .unwrap();

        let outcome = service
            .handle_reply("room", "thread", "notes: maybe later")
            .await
            .unwrap();

        assert!(matches!(outcome, ReplyOutcome::NeedsConfirmation(_)));
        assert!(provider.updates.lock().unwrap().is_empty());
    }

    #[tokio::test]
    async fn confirmed_reply_resolves_names_and_marks_transaction_reviewed() {
        let provider = Arc::new(FakeProvider::new());
        let (_directory, repository) = repository().await;
        let service = TransactionReviewService::new(Arc::clone(&provider), repository);
        let review = service.prepare().await.unwrap().unwrap();
        service
            .track("room".to_owned(), "thread".to_owned(), review)
            .await
            .unwrap();

        let outcome = service
            .handle_reply(
                "room",
                "thread",
                "confirm\ncategory: Groceries\ntags: Social\nnotes: Dinner",
            )
            .await
            .unwrap();

        assert!(matches!(outcome, ReplyOutcome::Updated(_)));
        assert_eq!(
            *provider.updates.lock().unwrap(),
            vec![(
                42,
                TransactionUpdate {
                    category_id: Some(2),
                    tag_ids: vec![3],
                    notes: Some("Dinner".to_owned()),
                },
                false,
            )]
        );
    }

    #[tokio::test]
    async fn unknown_category_does_not_update_and_keeps_review_pending() {
        let provider = Arc::new(FakeProvider::new());
        let (_directory, repository) = repository().await;
        let service = TransactionReviewService::new(Arc::clone(&provider), repository);
        let review = service.prepare().await.unwrap().unwrap();
        service
            .track("room".to_owned(), "thread".to_owned(), review)
            .await
            .unwrap();

        let result = service
            .handle_reply("room", "thread", "confirm\ncategory: Does not exist")
            .await;

        assert!(matches!(result, Err(Error::InvalidReply(_))));
        assert!(provider.updates.lock().unwrap().is_empty());
        assert!(matches!(
            service
                .handle_reply("room", "thread", "later")
                .await
                .unwrap(),
            ReplyOutcome::NeedsConfirmation(_)
        ));
    }

    #[tokio::test]
    async fn pending_threads_reserve_distinct_transactions() {
        let provider = Arc::new(FakeProvider::new());
        let (_directory, repository) = repository().await;
        let service = TransactionReviewService::new(Arc::clone(&provider), repository);

        let first = service.prepare().await.unwrap().unwrap();
        let second = service.prepare().await.unwrap().unwrap();
        assert_eq!(first.transaction_id(), 42);
        assert_eq!(second.transaction_id(), 43);
        assert!(service.prepare().await.unwrap().is_none());

        service
            .track("room".to_owned(), "first-thread".to_owned(), first)
            .await
            .unwrap();
        service
            .track("room".to_owned(), "second-thread".to_owned(), second)
            .await
            .unwrap();
        service
            .handle_reply("room", "second-thread", "confirm")
            .await
            .unwrap();
        service
            .handle_reply("room", "first-thread", "confirm")
            .await
            .unwrap();

        let updated_ids = provider
            .updates
            .lock()
            .unwrap()
            .iter()
            .map(|(id, _, _)| *id)
            .collect::<Vec<_>>();
        assert_eq!(updated_ids, vec![43, 42]);
    }

    #[tokio::test]
    async fn concurrent_preparations_claim_distinct_transactions() {
        let (_directory, repository) = repository().await;
        let service = TransactionReviewService::new(Arc::new(FakeProvider::new()), repository);

        let (first, second) = tokio::join!(service.prepare(), service.prepare());
        let mut ids = vec![
            first.unwrap().unwrap().transaction_id(),
            second.unwrap().unwrap().transaction_id(),
        ];
        ids.sort_unstable();

        assert_eq!(ids, vec![42, 43]);
    }

    #[tokio::test]
    async fn released_transaction_can_be_prepared_again() {
        let (_directory, repository) = repository().await;
        let service = TransactionReviewService::new(Arc::new(FakeProvider::new()), repository);
        let review = service.prepare().await.unwrap().unwrap();
        service.release(review.transaction_id()).await.unwrap();

        let retried = service.prepare().await.unwrap().unwrap();

        assert_eq!(retried.transaction_id(), 42);
    }

    #[tokio::test]
    async fn pending_review_can_be_confirmed_after_service_restart() {
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().join("workflows.sqlite3");
        let provider = Arc::new(FakeProvider::new());
        let repository: Arc<dyn WorkflowRepository> =
            Arc::new(SqliteWorkflowRepository::open(&path).await.unwrap());
        let service = TransactionReviewService::new(Arc::clone(&provider), repository);
        let review = service.prepare().await.unwrap().unwrap();
        service
            .track("room".to_owned(), "thread".to_owned(), review)
            .await
            .unwrap();
        drop(service);

        let reopened: Arc<dyn WorkflowRepository> =
            Arc::new(SqliteWorkflowRepository::open(path).await.unwrap());
        let restarted = TransactionReviewService::new(Arc::clone(&provider), reopened);
        let outcome = restarted
            .handle_reply("room", "thread", "confirm")
            .await
            .unwrap();

        assert!(matches!(outcome, ReplyOutcome::Updated(_)));
        assert_eq!(provider.updates.lock().unwrap()[0].0, 42);
    }

    #[tokio::test(start_paused = true)]
    async fn confirmed_update_retries_transient_failures_with_a_bound() {
        let provider = Arc::new(FakeProvider::with_transient_failures(2));
        let (_directory, repository) = repository().await;
        let service = TransactionReviewService::new(Arc::clone(&provider), repository);
        let review = service.prepare().await.unwrap().unwrap();
        service
            .track("room".to_owned(), "thread".to_owned(), review)
            .await
            .unwrap();

        let outcome = service
            .handle_reply("room", "thread", "confirm")
            .await
            .unwrap();

        assert!(matches!(outcome, ReplyOutcome::Updated(_)));
        assert_eq!(provider.transient_failures.load(Ordering::SeqCst), 0);
        assert_eq!(provider.updates.lock().unwrap().len(), 1);
    }

    #[tokio::test]
    async fn recovery_completes_an_operation_already_visible_in_lunch_money() {
        let provider = Arc::new(FakeProvider::new());
        let (_directory, repository) = repository().await;
        repository
            .claim(
                WORKFLOW_KIND,
                "42".to_owned(),
                Utc::now() + chrono::Duration::hours(1),
            )
            .await
            .unwrap();
        repository
            .bind_conversation(
                WORKFLOW_KIND,
                "42".to_owned(),
                "room".to_owned(),
                "thread".to_owned(),
            )
            .await
            .unwrap();
        repository
            .begin_operation(
                WORKFLOW_KIND,
                "42".to_owned(),
                "operation".to_owned(),
                serde_json::to_string(&TransactionUpdate {
                    category_id: Some(2),
                    tag_ids: vec![3],
                    notes: None,
                })
                .unwrap(),
            )
            .await
            .unwrap();
        let service = TransactionReviewService::new(provider, Arc::clone(&repository));

        let completed = service.recover().await.unwrap();

        assert_eq!(completed, 1);
        assert!(
            repository
                .find_by_conversation(WORKFLOW_KIND, "room".to_owned(), "thread".to_owned())
                .await
                .unwrap()
                .is_none()
        );
    }
}
