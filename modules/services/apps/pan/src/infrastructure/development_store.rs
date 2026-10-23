use std::{path::Path, sync::Mutex};

use chrono::Utc;
use rusqlite::{Connection, OptionalExtension, params};

use crate::domain::development::{
    DevelopmentJob, DevelopmentRequest, DevelopmentStore, Error, JobState, Result,
};

pub struct SqliteDevelopmentStore(Mutex<Connection>);

impl SqliteDevelopmentStore {
    /// Opens worker state. Running jobs are reconciled separately by the job runner.
    ///
    /// # Errors
    /// Returns an error if `SQLite` cannot be opened or migrated.
    pub fn open(path: &Path) -> Result<Self> {
        let connection = Connection::open(path).map_err(storage)?;
        connection
            .execute_batch(
                "PRAGMA journal_mode=WAL;
             CREATE TABLE IF NOT EXISTS development_jobs (
               id TEXT PRIMARY KEY, request_id TEXT NOT NULL UNIQUE,
               state TEXT NOT NULL, document TEXT NOT NULL,
               notified INTEGER NOT NULL DEFAULT 0
             );",
            )
            .map_err(storage)?;
        let columns = connection
            .prepare("PRAGMA table_info(development_jobs)")
            .map_err(storage)?
            .query_map([], |row| row.get::<_, String>(1))
            .map_err(storage)?
            .collect::<std::result::Result<Vec<_>, _>>()
            .map_err(storage)?;
        if !columns.iter().any(|column| column == "notified") {
            connection
                .execute(
                    "ALTER TABLE development_jobs ADD COLUMN notified INTEGER NOT NULL DEFAULT 0",
                    [],
                )
                .map_err(storage)?;
        }
        Ok(Self(Mutex::new(connection)))
    }

    fn with<T>(&self, f: impl FnOnce(&mut Connection) -> Result<T>) -> Result<T> {
        let mut connection = self.0.lock().map_err(storage)?;
        f(&mut connection)
    }
}

fn storage(error: impl std::fmt::Display) -> Error {
    Error::Storage(error.to_string())
}

fn load(connection: &Connection, id: &str) -> Result<DevelopmentJob> {
    let document: Option<String> = connection
        .query_row(
            "SELECT document FROM development_jobs WHERE id = ?1",
            [id],
            |row| row.get(0),
        )
        .optional()
        .map_err(storage)?;
    serde_json::from_str(&document.ok_or_else(|| Error::Invalid("unknown job ID".into()))?)
        .map_err(storage)
}

fn save(connection: &Connection, job: &DevelopmentJob) -> Result<()> {
    connection
        .execute(
            "UPDATE development_jobs SET state = ?2, document = ?3 WHERE id = ?1",
            params![
                job.id,
                serde_json::to_string(&job.state).map_err(storage)?,
                serde_json::to_string(job).map_err(storage)?
            ],
        )
        .map_err(storage)?;
    Ok(())
}

impl DevelopmentStore for SqliteDevelopmentStore {
    fn submit(&self, request: &DevelopmentRequest) -> Result<DevelopmentJob> {
        self.with(|connection| {
            let transaction = connection.transaction().map_err(storage)?;
            let existing: Option<String> = transaction.query_row(
                "SELECT id FROM development_jobs WHERE request_id = ?1", [&request.request_id], |row| row.get(0)
            ).optional().map_err(storage)?;
            if let Some(id) = existing {
                let job = load(&transaction, &id)?;
                if job.request != *request {
                    return Err(Error::Invalid("delivery ID already belongs to a different request".into()));
                }
                return Ok(job);
            }
            let count: i64 = transaction.query_row(
                "SELECT count(*) FROM development_jobs WHERE state IN ('\"queued\"', '\"running\"')", [], |row| row.get(0)
            ).map_err(storage)?;
            if count >= 20 { return Err(Error::Invalid("development queue is full".into())); }
            let now = Utc::now().to_rfc3339();
            let job = DevelopmentJob {
                id: uuid::Uuid::new_v4().to_string(), request: request.clone(), state: JobState::Queued,
                created_at: now.clone(), updated_at: now, result: String::new(),
            };
            transaction.execute(
                "INSERT INTO development_jobs (id, request_id, state, document) VALUES (?1, ?2, ?3, ?4)",
                params![job.id, request.request_id, serde_json::to_string(&job.state).map_err(storage)?, serde_json::to_string(&job).map_err(storage)?],
            ).map_err(storage)?;
            transaction.commit().map_err(storage)?;
            Ok(job)
        })
    }

    fn get(&self, id: &str) -> Result<DevelopmentJob> {
        self.with(|connection| load(connection, id))
    }

    fn next(&self) -> Result<Option<DevelopmentJob>> {
        self.with(|connection| {
            let transaction = connection.transaction().map_err(storage)?;
            let id: Option<String> = transaction.query_row(
                "SELECT id FROM development_jobs WHERE state = '\"queued\"' ORDER BY rowid LIMIT 1", [], |row| row.get(0)
            ).optional().map_err(storage)?;
            let Some(id) = id else { return Ok(None); };
            let mut job = load(&transaction, &id)?;
            job.state = JobState::Running;
            job.updated_at = Utc::now().to_rfc3339();
            save(&transaction, &job)?;
            transaction.commit().map_err(storage)?;
            Ok(Some(job))
        })
    }

    fn finish(&self, id: &str, state: JobState, result: &str) -> Result<()> {
        self.with(|connection| {
            let mut job = load(connection, id)?;
            if !job.state.is_terminal() {
                job.state = state;
                job.updated_at = Utc::now().to_rfc3339();
                job.result = result.chars().take(12_000).collect();
                save(connection, &job)?;
            }
            Ok(())
        })
    }

    fn recover(&self) -> Result<()> {
        self.with(|connection| {
            let mut query = connection.prepare("SELECT id FROM development_jobs WHERE state = '\"running\"'").map_err(storage)?;
            let ids = query.query_map([], |row| row.get::<_, String>(0)).map_err(storage)?.collect::<std::result::Result<Vec<_>, _>>().map_err(storage)?;
            drop(query);
            for id in ids {
                let mut job = load(connection, &id)?;
                job.state = JobState::Interrupted;
                job.updated_at = Utc::now().to_rfc3339();
                job.result = "Worker restarted during execution; inspect retained artifacts and submit an explicit new request.".into();
                save(connection, &job)?;
            }
            Ok(())
        })
    }

    fn pending_notifications(&self) -> Result<Vec<DevelopmentJob>> {
        self.with(|connection| {
            let mut query = connection
                .prepare(
                    "SELECT document FROM development_jobs WHERE notified = 0
                 AND state NOT IN ('\"queued\"', '\"running\"')
                 AND json_extract(document, '$.request.conversation') IS NOT NULL
                 ORDER BY rowid LIMIT 3",
                )
                .map_err(storage)?;
            query
                .query_map([], |row| row.get::<_, String>(0))
                .map_err(storage)?
                .map(|document| serde_json::from_str(&document.map_err(storage)?).map_err(storage))
                .collect()
        })
    }

    fn acknowledge_notification(&self, id: &str) -> Result<()> {
        self.with(|connection| {
            let job = load(connection, id)?;
            if !job.state.is_terminal() {
                return Err(Error::Invalid("job has not finished".into()));
            }
            connection
                .execute(
                    "UPDATE development_jobs SET notified = 1 WHERE id = ?1",
                    [id],
                )
                .map_err(storage)?;
            Ok(())
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn request() -> DevelopmentRequest {
        DevelopmentRequest {
            request_id: "matrix-event".into(),
            repository: "pan".into(),
            instruction: "Add a feature".into(),
            origin: "room/thread/user".into(),
            conversation: None,
        }
    }

    #[test]
    fn duplicate_delivery_creates_one_job_and_rejects_changed_payload() {
        let store = SqliteDevelopmentStore::open(Path::new(":memory:")).unwrap();
        let job = store.submit(&request()).unwrap();
        assert_eq!(store.submit(&request()).unwrap().id, job.id);
        let mut changed = request();
        changed.instruction = "Different".into();
        assert!(store.submit(&changed).is_err());
        assert_eq!(store.next().unwrap().unwrap().id, job.id);
        assert!(store.next().unwrap().is_none());
    }

    #[test]
    fn restart_interrupts_running_jobs_but_preserves_queued_work() {
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().join("jobs.sqlite3");
        let store = SqliteDevelopmentStore::open(&path).unwrap();
        let running = store.submit(&request()).unwrap();
        store.next().unwrap();
        let mut second = request();
        second.request_id = "second".into();
        let queued = store.submit(&second).unwrap();
        drop(store);
        let store = SqliteDevelopmentStore::open(&path).unwrap();
        store.recover().unwrap();
        assert_eq!(store.get(&running.id).unwrap().state, JobState::Interrupted);
        assert_eq!(store.next().unwrap().unwrap().id, queued.id);
    }

    #[test]
    fn cancellation_cannot_be_overwritten_by_late_completion() {
        let store = SqliteDevelopmentStore::open(Path::new(":memory:")).unwrap();
        let job = store.submit(&request()).unwrap();
        store.next().unwrap();
        store
            .finish(&job.id, JobState::Cancelled, "cancel")
            .unwrap();
        store.finish(&job.id, JobState::Completed, "done").unwrap();
        assert_eq!(store.get(&job.id).unwrap().state, JobState::Cancelled);
    }

    #[test]
    fn full_queue_accepts_retransmissions_and_reopens_capacity_after_cancellation() {
        let store = SqliteDevelopmentStore::open(Path::new(":memory:")).unwrap();
        let first = store.submit(&request()).unwrap();
        for index in 1..20 {
            let mut request = request();
            request.request_id = index.to_string();
            store.submit(&request).unwrap();
        }
        assert_eq!(store.submit(&request()).unwrap().id, first.id);
        let mut extra = request();
        extra.request_id = "overflow".into();
        assert!(store.submit(&extra).is_err());
        store
            .finish(&first.id, JobState::Cancelled, "cancel")
            .unwrap();
        assert!(store.submit(&extra).is_ok());
    }

    #[test]
    fn terminal_notifications_survive_restart_until_acknowledged() {
        use crate::domain::development::DevelopmentConversation;
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().join("jobs.sqlite3");
        let store = SqliteDevelopmentStore::open(&path).unwrap();
        let mut request = request();
        request.conversation = Some(DevelopmentConversation {
            room_id: "!room:example.org".into(),
            thread_id: "$root:example.org".into(),
            user_id: "@owner:example.org".into(),
        });
        let job = store.submit(&request).unwrap();
        assert!(store.pending_notifications().unwrap().is_empty());
        assert!(store.acknowledge_notification(&job.id).is_err());
        store.next().unwrap();
        store.finish(&job.id, JobState::Completed, "done").unwrap();
        drop(store);
        let store = SqliteDevelopmentStore::open(&path).unwrap();
        assert_eq!(store.pending_notifications().unwrap()[0].id, job.id);
        store.acknowledge_notification(&job.id).unwrap();
        store.acknowledge_notification(&job.id).unwrap();
        assert!(store.pending_notifications().unwrap().is_empty());
    }

    #[test]
    fn original_schema_migrates_without_losing_jobs_or_notifying_cli_requests() {
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().join("jobs.sqlite3");
        let connection = Connection::open(&path).unwrap();
        connection.execute_batch("CREATE TABLE development_jobs (id TEXT PRIMARY KEY, request_id TEXT NOT NULL UNIQUE, state TEXT NOT NULL, document TEXT NOT NULL);").unwrap();
        drop(connection);
        let store = SqliteDevelopmentStore::open(&path).unwrap();
        let job = store.submit(&request()).unwrap();
        store.finish(&job.id, JobState::Failed, "failure").unwrap();
        assert_eq!(store.get(&job.id).unwrap().state, JobState::Failed);
        assert!(store.pending_notifications().unwrap().is_empty());
    }
}
