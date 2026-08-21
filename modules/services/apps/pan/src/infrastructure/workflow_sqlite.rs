use std::{
    path::Path,
    sync::{Arc, Mutex},
    time::Duration,
};

use chrono::{DateTime, TimeZone, Utc};
use rusqlite::{Connection, OptionalExtension, TransactionBehavior, params};

use crate::domain::workflow::{
    self, WorkflowClaim, WorkflowKind, WorkflowRepository, WorkflowState,
};

const SCHEMA_VERSION: i32 = 2;

pub struct SqliteWorkflowRepository {
    connection: Arc<Mutex<Connection>>,
}

impl SqliteWorkflowRepository {
    /// Opens the workflow database and applies pending schema migrations.
    ///
    /// # Errors
    ///
    /// Returns an error when the directory, database, or migration cannot be initialized.
    pub async fn open(path: impl AsRef<Path>) -> Result<Self, workflow::Error> {
        let path = path.as_ref().to_owned();
        if let Some(parent) = path.parent() {
            tokio::fs::create_dir_all(parent)
                .await
                .map_err(storage_error)?;
        }
        let connection = tokio::task::spawn_blocking(move || {
            let mut connection = Connection::open(path).map_err(storage_error)?;
            connection
                .busy_timeout(Duration::from_secs(5))
                .map_err(storage_error)?;
            migrate(&mut connection)?;
            Ok::<_, workflow::Error>(connection)
        })
        .await
        .map_err(storage_error)??;

        Ok(Self {
            connection: Arc::new(Mutex::new(connection)),
        })
    }

    async fn run<T, F>(&self, operation: F) -> Result<T, workflow::Error>
    where
        T: Send + 'static,
        F: FnOnce(&mut Connection) -> Result<T, workflow::Error> + Send + 'static,
    {
        let connection = Arc::clone(&self.connection);
        tokio::task::spawn_blocking(move || {
            let mut connection = connection
                .lock()
                .map_err(|error| workflow::Error::Storage(error.to_string()))?;
            operation(&mut connection)
        })
        .await
        .map_err(storage_error)?
    }
}

impl WorkflowRepository for SqliteWorkflowRepository {
    fn claim(
        &self,
        kind: WorkflowKind,
        resource_id: String,
        expires_at: DateTime<Utc>,
    ) -> workflow::RepositoryFuture<'_, bool> {
        Box::pin(async move {
            self.run(move |connection| {
                let transaction = connection
                    .transaction_with_behavior(TransactionBehavior::Immediate)
                    .map_err(storage_error)?;
                let now = Utc::now().timestamp_millis();
                delete_expired(&transaction, now)?;
                let inserted = transaction
                    .execute(
                        "INSERT OR IGNORE INTO workflow_claims (
                            kind, resource_id, state, expires_at, created_at, updated_at
                         ) VALUES (?1, ?2, ?3, ?4, ?5, ?5)",
                        params![
                            kind.as_str(),
                            resource_id,
                            WorkflowState::Reserved.as_str(),
                            expires_at.timestamp_millis(),
                            now,
                        ],
                    )
                    .map_err(storage_error)?;
                transaction.commit().map_err(storage_error)?;
                Ok(inserted == 1)
            })
            .await
        })
    }

    fn bind_conversation(
        &self,
        kind: WorkflowKind,
        resource_id: String,
        room_id: String,
        thread_id: String,
    ) -> workflow::RepositoryFuture<'_, ()> {
        Box::pin(async move {
            self.run(move |connection| {
                let now = Utc::now().timestamp_millis();
                let changed = connection
                    .execute(
                        "UPDATE workflow_claims
                         SET state = ?1, room_id = ?2, thread_id = ?3, updated_at = ?4
                         WHERE kind = ?5 AND resource_id = ?6 AND expires_at > ?4",
                        params![
                            WorkflowState::Delivered.as_str(),
                            room_id,
                            thread_id,
                            now,
                            kind.as_str(),
                            resource_id,
                        ],
                    )
                    .map_err(storage_error)?;
                if changed == 0 {
                    return Err(workflow::Error::ClaimNotFound);
                }
                Ok(())
            })
            .await
        })
    }

    fn find_by_conversation(
        &self,
        kind: WorkflowKind,
        room_id: String,
        thread_id: String,
    ) -> workflow::RepositoryFuture<'_, Option<WorkflowClaim>> {
        Box::pin(async move {
            self.run(move |connection| {
                let transaction = connection
                    .transaction_with_behavior(TransactionBehavior::Immediate)
                    .map_err(storage_error)?;
                delete_expired(&transaction, Utc::now().timestamp_millis())?;
                let claim = transaction
                    .query_row(
                        "SELECT resource_id, state, expires_at, created_at, updated_at,
                                operation_id, action_payload, attempts, last_error, next_attempt_at
                         FROM workflow_claims
                         WHERE kind = ?1 AND room_id = ?2 AND thread_id = ?3
                           AND state != 'completed'",
                        params![kind.as_str(), room_id, thread_id],
                        |row| {
                            Ok((
                                row.get::<_, String>(0)?,
                                row.get::<_, String>(1)?,
                                row.get::<_, i64>(2)?,
                                row.get::<_, i64>(3)?,
                                row.get::<_, i64>(4)?,
                                row.get::<_, Option<String>>(5)?,
                                row.get::<_, Option<String>>(6)?,
                                row.get::<_, u32>(7)?,
                                row.get::<_, Option<String>>(8)?,
                                row.get::<_, Option<i64>>(9)?,
                            ))
                        },
                    )
                    .optional()
                    .map_err(storage_error)?;
                transaction.commit().map_err(storage_error)?;
                claim.map_or(
                    Ok(None),
                    |(
                        resource_id,
                        state,
                        expires,
                        created,
                        updated,
                        operation_id,
                        action_payload,
                        attempts,
                        last_error,
                        next_attempt_at,
                    )| {
                        Ok(Some(WorkflowClaim {
                            kind,
                            resource_id,
                            state: parse_state(&state)?,
                            room_id: Some(room_id),
                            thread_id: Some(thread_id),
                            expires_at: timestamp(expires)?,
                            created_at: timestamp(created)?,
                            updated_at: timestamp(updated)?,
                            operation_id,
                            action_payload,
                            attempts,
                            last_error,
                            next_attempt_at: next_attempt_at.map(timestamp).transpose()?,
                        }))
                    },
                )
            })
            .await
        })
    }

    fn release(
        &self,
        kind: WorkflowKind,
        resource_id: String,
    ) -> workflow::RepositoryFuture<'_, ()> {
        Box::pin(async move {
            self.run(move |connection| {
                connection
                    .execute(
                        "DELETE FROM workflow_claims WHERE kind = ?1 AND resource_id = ?2",
                        params![kind.as_str(), resource_id],
                    )
                    .map_err(storage_error)?;
                Ok(())
            })
            .await
        })
    }

    fn begin_operation(
        &self,
        kind: WorkflowKind,
        resource_id: String,
        operation_id: String,
        action_payload: String,
    ) -> workflow::RepositoryFuture<'_, WorkflowClaim> {
        Box::pin(async move {
            self.run(move |connection| {
                let transaction = connection
                    .transaction_with_behavior(TransactionBehavior::Immediate)
                    .map_err(storage_error)?;
                let now = Utc::now().timestamp_millis();
                let changed = transaction
                    .execute(
                        "UPDATE workflow_claims
                         SET state = 'applying', operation_id = ?1, action_payload = ?2,
                             attempts = attempts + 1, last_error = NULL,
                             next_attempt_at = NULL, updated_at = ?3
                         WHERE kind = ?4 AND resource_id = ?5
                           AND state IN ('delivered', 'failed') AND expires_at > ?3",
                        params![operation_id, action_payload, now, kind.as_str(), resource_id],
                    )
                    .map_err(storage_error)?;
                if changed == 0 {
                    let state = transaction
                        .query_row(
                            "SELECT state FROM workflow_claims WHERE kind = ?1 AND resource_id = ?2",
                            params![kind.as_str(), resource_id],
                            |row| row.get::<_, String>(0),
                        )
                        .optional()
                        .map_err(storage_error)?;
                    return Err(state.map_or(workflow::Error::ClaimNotFound, |state| {
                        workflow::Error::InvalidTransition(state)
                    }));
                }
                transaction.commit().map_err(storage_error)?;
                query_claim_by_resource(connection, kind, &resource_id)
            })
            .await
        })
    }

    fn record_failure(
        &self,
        kind: WorkflowKind,
        resource_id: String,
        error: String,
        next_attempt_at: Option<DateTime<Utc>>,
    ) -> workflow::RepositoryFuture<'_, ()> {
        Box::pin(async move {
            self.run(move |connection| {
                let changed = connection
                    .execute(
                        "UPDATE workflow_claims
                         SET state = 'failed', last_error = ?1, next_attempt_at = ?2, updated_at = ?3
                         WHERE kind = ?4 AND resource_id = ?5 AND state = 'applying'",
                        params![
                            error.chars().take(1000).collect::<String>(),
                            next_attempt_at.map(|value| value.timestamp_millis()),
                            Utc::now().timestamp_millis(),
                            kind.as_str(),
                            resource_id,
                        ],
                    )
                    .map_err(storage_error)?;
                if changed == 0 {
                    return Err(workflow::Error::ClaimNotFound);
                }
                Ok(())
            })
            .await
        })
    }

    fn complete(
        &self,
        kind: WorkflowKind,
        resource_id: String,
    ) -> workflow::RepositoryFuture<'_, ()> {
        Box::pin(async move {
            self.run(move |connection| {
                let changed = connection
                    .execute(
                        "UPDATE workflow_claims
                         SET state = 'completed', action_payload = NULL, last_error = NULL,
                             next_attempt_at = NULL, updated_at = ?1
                         WHERE kind = ?2 AND resource_id = ?3 AND state = 'applying'",
                        params![Utc::now().timestamp_millis(), kind.as_str(), resource_id],
                    )
                    .map_err(storage_error)?;
                if changed == 0 {
                    return Err(workflow::Error::ClaimNotFound);
                }
                Ok(())
            })
            .await
        })
    }

    fn list_recoverable(
        &self,
        kind: WorkflowKind,
    ) -> workflow::RepositoryFuture<'_, Vec<WorkflowClaim>> {
        Box::pin(async move {
            self.run(move |connection| {
                let mut statement = connection
                    .prepare(
                        "SELECT resource_id FROM workflow_claims
                         WHERE kind = ?1 AND state IN ('applying', 'failed') AND expires_at > ?2",
                    )
                    .map_err(storage_error)?;
                let ids = statement
                    .query_map(
                        params![kind.as_str(), Utc::now().timestamp_millis()],
                        |row| row.get::<_, String>(0),
                    )
                    .map_err(storage_error)?
                    .collect::<Result<Vec<_>, _>>()
                    .map_err(storage_error)?;
                drop(statement);
                ids.iter()
                    .map(|resource_id| query_claim_by_resource(connection, kind, resource_id))
                    .collect()
            })
            .await
        })
    }
}

fn query_claim_by_resource(
    connection: &Connection,
    kind: WorkflowKind,
    resource_id: &str,
) -> Result<WorkflowClaim, workflow::Error> {
    let row = connection
        .query_row(
            "SELECT state, room_id, thread_id, expires_at, created_at, updated_at,
                    operation_id, action_payload, attempts, last_error, next_attempt_at
             FROM workflow_claims WHERE kind = ?1 AND resource_id = ?2",
            params![kind.as_str(), resource_id],
            |row| {
                Ok((
                    row.get::<_, String>(0)?,
                    row.get::<_, Option<String>>(1)?,
                    row.get::<_, Option<String>>(2)?,
                    row.get::<_, i64>(3)?,
                    row.get::<_, i64>(4)?,
                    row.get::<_, i64>(5)?,
                    row.get::<_, Option<String>>(6)?,
                    row.get::<_, Option<String>>(7)?,
                    row.get::<_, u32>(8)?,
                    row.get::<_, Option<String>>(9)?,
                    row.get::<_, Option<i64>>(10)?,
                ))
            },
        )
        .map_err(storage_error)?;
    Ok(WorkflowClaim {
        kind,
        resource_id: resource_id.to_owned(),
        state: parse_state(&row.0)?,
        room_id: row.1,
        thread_id: row.2,
        expires_at: timestamp(row.3)?,
        created_at: timestamp(row.4)?,
        updated_at: timestamp(row.5)?,
        operation_id: row.6,
        action_payload: row.7,
        attempts: row.8,
        last_error: row.9,
        next_attempt_at: row.10.map(timestamp).transpose()?,
    })
}

fn migrate(connection: &mut Connection) -> Result<(), workflow::Error> {
    let version = connection
        .query_row("PRAGMA user_version", [], |row| row.get::<_, i32>(0))
        .map_err(storage_error)?;
    if version > SCHEMA_VERSION {
        return Err(workflow::Error::Storage(format!(
            "workflow database schema {version} is newer than supported version {SCHEMA_VERSION}"
        )));
    }
    if version == 0 {
        let transaction = connection.transaction().map_err(storage_error)?;
        transaction
            .execute_batch(
                "CREATE TABLE workflow_claims (
                    kind TEXT NOT NULL,
                    resource_id TEXT NOT NULL,
                    state TEXT NOT NULL CHECK (state IN ('reserved', 'delivered', 'applying', 'completed', 'failed')),
                    room_id TEXT,
                    thread_id TEXT,
                    expires_at INTEGER NOT NULL,
                    created_at INTEGER NOT NULL,
                    updated_at INTEGER NOT NULL,
                    operation_id TEXT,
                    action_payload TEXT,
                    attempts INTEGER NOT NULL DEFAULT 0,
                    last_error TEXT,
                    next_attempt_at INTEGER,
                    PRIMARY KEY (kind, resource_id)
                 );
                 CREATE UNIQUE INDEX workflow_claims_conversation
                    ON workflow_claims(kind, room_id, thread_id)
                    WHERE room_id IS NOT NULL AND thread_id IS NOT NULL;
                 PRAGMA user_version = 2;",
            )
            .map_err(storage_error)?;
        transaction.commit().map_err(storage_error)?;
    }
    if version == 1 {
        let transaction = connection.transaction().map_err(storage_error)?;
        transaction
            .execute_batch(
                "DROP INDEX workflow_claims_conversation;
                 ALTER TABLE workflow_claims RENAME TO workflow_claims_v1;
                 CREATE TABLE workflow_claims (
                    kind TEXT NOT NULL,
                    resource_id TEXT NOT NULL,
                    state TEXT NOT NULL CHECK (state IN ('reserved', 'delivered', 'applying', 'completed', 'failed')),
                    room_id TEXT,
                    thread_id TEXT,
                    expires_at INTEGER NOT NULL,
                    created_at INTEGER NOT NULL,
                    updated_at INTEGER NOT NULL,
                    operation_id TEXT,
                    action_payload TEXT,
                    attempts INTEGER NOT NULL DEFAULT 0,
                    last_error TEXT,
                    next_attempt_at INTEGER,
                    PRIMARY KEY (kind, resource_id)
                 );
                 INSERT INTO workflow_claims (
                    kind, resource_id, state, room_id, thread_id, expires_at, created_at, updated_at
                 ) SELECT kind, resource_id, state, room_id, thread_id, expires_at, created_at, updated_at
                   FROM workflow_claims_v1;
                 DROP TABLE workflow_claims_v1;
                 CREATE UNIQUE INDEX workflow_claims_conversation
                    ON workflow_claims(kind, room_id, thread_id)
                    WHERE room_id IS NOT NULL AND thread_id IS NOT NULL;
                 PRAGMA user_version = 2;",
            )
            .map_err(storage_error)?;
        transaction.commit().map_err(storage_error)?;
    }
    Ok(())
}

fn delete_expired(
    transaction: &rusqlite::Transaction<'_>,
    now: i64,
) -> Result<(), workflow::Error> {
    transaction
        .execute(
            "DELETE FROM workflow_claims WHERE expires_at <= ?1",
            params![now],
        )
        .map_err(storage_error)?;
    Ok(())
}

fn timestamp(value: i64) -> Result<DateTime<Utc>, workflow::Error> {
    Utc.timestamp_millis_opt(value)
        .single()
        .ok_or_else(|| workflow::Error::Storage(format!("invalid workflow timestamp {value}")))
}

fn parse_state(value: &str) -> Result<WorkflowState, workflow::Error> {
    match value {
        "reserved" => Ok(WorkflowState::Reserved),
        "delivered" => Ok(WorkflowState::Delivered),
        "applying" => Ok(WorkflowState::Applying),
        "completed" => Ok(WorkflowState::Completed),
        "failed" => Ok(WorkflowState::Failed),
        _ => Err(workflow::Error::Storage(format!(
            "invalid workflow state {value}"
        ))),
    }
}

fn storage_error(error: impl std::fmt::Display) -> workflow::Error {
    workflow::Error::Storage(error.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    async fn repository() -> (tempfile::TempDir, SqliteWorkflowRepository) {
        let directory = tempfile::tempdir().expect("temporary directory should be created");
        let repository = SqliteWorkflowRepository::open(directory.path().join("workflows.sqlite3"))
            .await
            .expect("repository should open");
        (directory, repository)
    }

    #[tokio::test]
    async fn concurrent_claims_allow_only_one_owner() {
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().join("workflows.sqlite3");
        let first_repository = SqliteWorkflowRepository::open(&path).await.unwrap();
        let second_repository = SqliteWorkflowRepository::open(&path).await.unwrap();
        let expires_at = Utc::now() + chrono::Duration::hours(1);

        let (first, second) = tokio::join!(
            first_repository.claim(
                WorkflowKind::LunchMoneyTransaction,
                "42".to_owned(),
                expires_at
            ),
            second_repository.claim(
                WorkflowKind::LunchMoneyTransaction,
                "42".to_owned(),
                expires_at
            )
        );

        assert_ne!(
            first.expect("first claim should execute"),
            second.expect("second claim should execute")
        );
    }

    #[tokio::test]
    async fn claim_and_conversation_survive_repository_reopen() {
        let directory = tempfile::tempdir().expect("temporary directory should be created");
        let path = directory.path().join("workflows.sqlite3");
        let repository = SqliteWorkflowRepository::open(&path).await.unwrap();
        repository
            .claim(
                WorkflowKind::LunchMoneyTransaction,
                "42".to_owned(),
                Utc::now() + chrono::Duration::hours(1),
            )
            .await
            .unwrap();
        repository
            .bind_conversation(
                WorkflowKind::LunchMoneyTransaction,
                "42".to_owned(),
                "!room:example.org".to_owned(),
                "$thread".to_owned(),
            )
            .await
            .unwrap();
        drop(repository);

        let reopened = SqliteWorkflowRepository::open(path).await.unwrap();
        let duplicate_claim = reopened
            .claim(
                WorkflowKind::LunchMoneyTransaction,
                "42".to_owned(),
                Utc::now() + chrono::Duration::hours(1),
            )
            .await
            .unwrap();
        let claim = reopened
            .find_by_conversation(
                WorkflowKind::LunchMoneyTransaction,
                "!room:example.org".to_owned(),
                "$thread".to_owned(),
            )
            .await
            .unwrap()
            .unwrap();

        assert!(!duplicate_claim);
        assert_eq!(claim.resource_id, "42");
        assert_eq!(claim.state, WorkflowState::Delivered);
    }

    #[tokio::test]
    async fn expired_claim_becomes_eligible_again() {
        let (_directory, repository) = repository().await;
        repository
            .claim(
                WorkflowKind::FastmailEmail,
                "email-1".to_owned(),
                Utc::now() - chrono::Duration::seconds(1),
            )
            .await
            .unwrap();

        let reclaimed = repository
            .claim(
                WorkflowKind::FastmailEmail,
                "email-1".to_owned(),
                Utc::now() + chrono::Duration::hours(1),
            )
            .await
            .unwrap();

        assert!(reclaimed);
    }

    #[tokio::test]
    async fn migration_sets_schema_version() {
        let (_directory, repository) = repository().await;

        let version = repository
            .run(|connection| {
                connection
                    .query_row("PRAGMA user_version", [], |row| row.get::<_, i32>(0))
                    .map_err(storage_error)
            })
            .await
            .unwrap();

        assert_eq!(version, SCHEMA_VERSION);
    }

    #[tokio::test]
    async fn schema_stores_only_operational_workflow_fields() {
        let (_directory, repository) = repository().await;

        let columns = repository
            .run(|connection| {
                let mut statement = connection
                    .prepare("PRAGMA table_info(workflow_claims)")
                    .map_err(storage_error)?;
                statement
                    .query_map([], |row| row.get::<_, String>(1))
                    .map_err(storage_error)?
                    .collect::<Result<Vec<_>, _>>()
                    .map_err(storage_error)
            })
            .await
            .unwrap();

        assert_eq!(
            columns,
            [
                "kind",
                "resource_id",
                "state",
                "room_id",
                "thread_id",
                "expires_at",
                "created_at",
                "updated_at",
                "operation_id",
                "action_payload",
                "attempts",
                "last_error",
                "next_attempt_at"
            ]
        );
    }

    #[tokio::test]
    async fn operation_lifecycle_is_durable_and_explicit() {
        let (_directory, repository) = repository().await;
        repository
            .claim(
                WorkflowKind::LunchMoneyTransaction,
                "42".to_owned(),
                Utc::now() + chrono::Duration::hours(1),
            )
            .await
            .unwrap();
        repository
            .bind_conversation(
                WorkflowKind::LunchMoneyTransaction,
                "42".to_owned(),
                "room".to_owned(),
                "thread".to_owned(),
            )
            .await
            .unwrap();

        let applying = repository
            .begin_operation(
                WorkflowKind::LunchMoneyTransaction,
                "42".to_owned(),
                "operation-1".to_owned(),
                r#"{"category_id":2}"#.to_owned(),
            )
            .await
            .unwrap();
        assert_eq!(applying.state, WorkflowState::Applying);
        assert_eq!(applying.attempts, 1);

        repository
            .record_failure(
                WorkflowKind::LunchMoneyTransaction,
                "42".to_owned(),
                "timeout".to_owned(),
                None,
            )
            .await
            .unwrap();
        let failed = repository
            .find_by_conversation(
                WorkflowKind::LunchMoneyTransaction,
                "room".to_owned(),
                "thread".to_owned(),
            )
            .await
            .unwrap()
            .unwrap();
        assert_eq!(failed.state, WorkflowState::Failed);
        assert_eq!(failed.last_error.as_deref(), Some("timeout"));

        repository
            .begin_operation(
                WorkflowKind::LunchMoneyTransaction,
                "42".to_owned(),
                "operation-2".to_owned(),
                r#"{"category_id":2}"#.to_owned(),
            )
            .await
            .unwrap();
        repository
            .complete(WorkflowKind::LunchMoneyTransaction, "42".to_owned())
            .await
            .unwrap();

        assert!(
            repository
                .find_by_conversation(
                    WorkflowKind::LunchMoneyTransaction,
                    "room".to_owned(),
                    "thread".to_owned(),
                )
                .await
                .unwrap()
                .is_none()
        );
        assert!(
            !repository
                .claim(
                    WorkflowKind::LunchMoneyTransaction,
                    "42".to_owned(),
                    Utc::now() + chrono::Duration::hours(1),
                )
                .await
                .unwrap()
        );
    }
}
