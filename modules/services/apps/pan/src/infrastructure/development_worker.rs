use std::{
    collections::BTreeMap,
    num::NonZeroU64,
    os::unix::fs::PermissionsExt,
    path::{Path, PathBuf},
    sync::Arc,
    time::Duration,
};

use anyhow::{Context, Result, bail};
use serde::{Deserialize, Serialize};
use tokio::{
    io::{AsyncBufRead, AsyncBufReadExt, AsyncWriteExt, BufReader},
    net::{UnixListener, UnixStream},
    sync::Semaphore,
};

use crate::{
    application::development::DevelopmentService,
    domain::development::{JobState, PatchChunk, WorkerCommand, WorkerResponse},
    infrastructure::{development_store::SqliteDevelopmentStore, pi::PiExecutor},
};

pub const MAX_RECORD: usize = 1_048_576;

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct WorkerConfig {
    pub socket: PathBuf,
    pub state_dir: PathBuf,
    pub sandbox_program: PathBuf,
    pub fetch_program: PathBuf,
    pub pi_program: PathBuf,
    pub git_program: PathBuf,
    pub jj_program: PathBuf,
    pub path: String,
    pub timeout_seconds: NonZeroU64,
    pub repositories: BTreeMap<String, RepositoryConfig>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct RepositoryConfig {
    pub url: String,
    pub branch: String,
}

impl WorkerConfig {
    /// Reads the worker's independent configuration; assistant secrets are never loaded.
    ///
    /// # Errors
    /// Returns an error for missing configuration or unsafe repository definitions.
    pub fn load(path: &Path) -> Result<Self> {
        let config: Self = serde_yaml::from_str(&std::fs::read_to_string(path)?)?;
        anyhow::ensure!(
            config.socket.is_absolute() && config.state_dir.is_absolute(),
            "worker paths must be absolute"
        );
        for program in [
            &config.sandbox_program,
            &config.fetch_program,
            &config.pi_program,
            &config.git_program,
            &config.jj_program,
        ] {
            anyhow::ensure!(
                program.is_absolute(),
                "worker executable paths must be absolute"
            );
        }
        for repository in config.repositories.values() {
            let url = reqwest::Url::parse(&repository.url)?;
            anyhow::ensure!(
                url.scheme() == "https"
                    && url.host_str().is_some()
                    && url.username().is_empty()
                    && url.password().is_none(),
                "repositories must use HTTPS without embedded credentials"
            );
            anyhow::ensure!(
                !repository.branch.is_empty() && !repository.branch.starts_with('-'),
                "invalid repository branch"
            );
        }
        Ok(config)
    }
}

/// Reads a strict, bounded JSONL record, including split UTF-8 and CRLF records.
///
/// # Errors
/// Rejects truncated, oversized, malformed UTF-8, and empty records.
pub async fn read_record(reader: &mut (impl AsyncBufRead + Unpin)) -> Result<String> {
    let mut record = Vec::new();
    loop {
        let available = reader.fill_buf().await?;
        anyhow::ensure!(!available.is_empty(), "unexpected EOF in JSONL stream");
        let count = available
            .iter()
            .position(|byte| *byte == b'\n')
            .map_or(available.len(), |index| index + 1);
        anyhow::ensure!(
            record.len() + count <= MAX_RECORD,
            "JSONL record exceeds size limit"
        );
        record.extend_from_slice(&available[..count]);
        reader.consume(count);
        if record.last() == Some(&b'\n') {
            break;
        }
    }
    record.pop();
    if record.last() == Some(&b'\r') {
        record.pop();
    }
    anyhow::ensure!(!record.is_empty(), "empty JSONL record");
    Ok(String::from_utf8(record)?)
}

/// Sends one request over the worker socket. Socket permissions authorize callers.
///
/// # Errors
/// Returns an error when the worker cannot be reached or rejects the request.
pub async fn request(
    socket: &Path,
    command: &WorkerCommand,
) -> Result<crate::domain::development::DevelopmentJob> {
    match exchange(socket, command).await? {
        WorkerResponse::Job { job } => Ok(*job),
        _ => bail!("unexpected worker response"),
    }
}

/// Exchanges a bounded command/response record with the worker.
///
/// # Errors
/// Returns connection, protocol, timeout, or worker command errors.
pub async fn exchange(socket: &Path, command: &WorkerCommand) -> Result<WorkerResponse> {
    tokio::time::timeout(Duration::from_secs(10), async {
        let mut connection = UnixStream::connect(socket)
            .await
            .context("development worker is unavailable")?;
        let mut bytes = serde_json::to_vec(command)?;
        bytes.push(b'\n');
        connection.write_all(&bytes).await?;
        let response = read_record(&mut BufReader::new(connection)).await?;
        match serde_json::from_str(&response)? {
            WorkerResponse::Error { message } => bail!("{message}"),
            response => Ok(response),
        }
    })
    .await
    .context("development worker request timed out")?
}

/// Fetches the completed patch through the socket without sharing worker storage.
///
/// # Errors
/// Rejects missing, oversized, inconsistent, or incomplete patch data.
pub async fn fetch_patch(socket: &Path, id: &str) -> Result<Vec<u8>> {
    let mut bytes = Vec::new();
    loop {
        let WorkerResponse::Patch { chunk } = exchange(
            socket,
            &WorkerCommand::Patch {
                id: id.into(),
                offset: bytes.len() as u64,
            },
        )
        .await?
        else {
            bail!("unexpected patch response");
        };
        anyhow::ensure!(
            chunk.total <= 4_194_304 && chunk.offset == bytes.len() as u64,
            "invalid patch chunk bounds"
        );
        anyhow::ensure!(
            chunk.bytes.len() <= 65_536 && chunk.offset + chunk.bytes.len() as u64 <= chunk.total,
            "invalid patch chunk size"
        );
        anyhow::ensure!(
            !chunk.bytes.is_empty() || chunk.offset == chunk.total,
            "empty patch chunk before EOF"
        );
        bytes.extend(chunk.bytes);
        if bytes.len() as u64 == chunk.total {
            return Ok(bytes);
        }
    }
}

async fn patch_response<S: crate::domain::development::DevelopmentStore>(
    service: &DevelopmentService<S>,
    root: &Path,
    id: String,
    offset: u64,
) -> Result<WorkerResponse> {
    use tokio::io::{AsyncReadExt, AsyncSeekExt};
    let WorkerResponse::Job { job } = service.handle(WorkerCommand::Status { id }) else {
        bail!("unknown job ID");
    };
    anyhow::ensure!(
        job.state == JobState::Completed,
        "patch is available only for completed jobs"
    );
    anyhow::ensure!(
        uuid::Uuid::parse_str(&job.id).is_ok(),
        "invalid stored job ID"
    );
    let path = root.join(job.id).join("changes.patch");
    let metadata = tokio::fs::symlink_metadata(&path).await?;
    anyhow::ensure!(
        metadata.is_file() && metadata.len() <= 4_194_304 && offset <= metadata.len(),
        "invalid patch artifact or offset"
    );
    let mut file = tokio::fs::File::open(path).await?;
    file.seek(std::io::SeekFrom::Start(offset)).await?;
    let mut bytes = Vec::new();
    file.take(65_536).read_to_end(&mut bytes).await?;
    Ok(WorkerResponse::Patch {
        chunk: PatchChunk {
            offset,
            total: metadata.len(),
            bytes,
        },
    })
}

/// Runs the worker until shutdown. Systemd owns its runtime directory and process tree.
///
/// # Errors
/// Returns an error if startup, persistence, or the job runner fails.
pub async fn serve(config: WorkerConfig) -> Result<()> {
    tokio::fs::create_dir_all(config.state_dir.join("jobs")).await?;
    let store = Arc::new(SqliteDevelopmentStore::open(
        &config.state_dir.join("jobs.sqlite3"),
    )?);
    let service = Arc::new(DevelopmentService::new(
        store,
        config.repositories.keys().cloned().collect(),
    ));
    // A stale socket is expected after a crash; never unlink another kind of file.
    if let Ok(metadata) = tokio::fs::symlink_metadata(&config.socket).await {
        use std::os::unix::fs::FileTypeExt;
        anyhow::ensure!(
            metadata.file_type().is_socket(),
            "worker socket path is not a socket"
        );
        anyhow::ensure!(
            UnixStream::connect(&config.socket).await.is_err(),
            "another worker is already listening"
        );
        tokio::fs::remove_file(&config.socket).await?;
    }
    let listener = UnixListener::bind(&config.socket)?;
    tokio::fs::set_permissions(&config.socket, std::fs::Permissions::from_mode(0o660)).await?;
    let executor = PiExecutor::new(config.clone());
    let jobs_directory = config.state_dir.join("jobs");
    let runner = service.run(
        &executor,
        &jobs_directory,
        Duration::from_secs(config.timeout_seconds.get()),
    );
    tokio::pin!(runner);
    let connections = Arc::new(Semaphore::new(16));
    let mut handlers = tokio::task::JoinSet::new();
    loop {
        tokio::select! {
            result = &mut runner => { result?; return Ok(()); }
            result = tokio::signal::ctrl_c() => { result?; return Ok(()); }
            accepted = listener.accept() => {
                let (connection, _) = accepted?;
                let Ok(permit) = Arc::clone(&connections).try_acquire_owned() else { continue; };
                let service = Arc::clone(&service);
                let root = jobs_directory.clone();
                handlers.spawn(async move {
                    let _permit = permit;
                    let _ = tokio::time::timeout(Duration::from_secs(5), async {
                        let (read, mut write) = connection.into_split();
                        let response = match read_record(&mut BufReader::new(read)).await {
                            Ok(record) => match serde_json::from_str(&record) {
                                Ok(WorkerCommand::Patch { id, offset }) => match patch_response(&service, &root, id, offset).await {
                                    Ok(response) => response,
                                    Err(_) => WorkerResponse::Error { message: "patch unavailable or invalid offset".into() },
                                },
                                Ok(command) => service.handle(command),
                                Err(_) => WorkerResponse::Error { message: "invalid worker command".into() },
                            },
                            Err(_) => WorkerResponse::Error { message: "invalid worker record".into() },
                        };
                        let mut bytes = serde_json::to_vec(&response)?; bytes.push(b'\n');
                        write.write_all(&bytes).await?;
                        Ok::<_, anyhow::Error>(())
                    }).await;
                });
            }
            _ = handlers.join_next(), if !handlers.is_empty() => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn jsonl_accepts_split_records_and_unicode_separators() {
        let (mut writer, reader) = tokio::io::duplex(8);
        let sending = tokio::spawn(async move {
            writer
                .write_all("{\"text\":\"a\u{2028}b\"}\r\n".as_bytes())
                .await
                .unwrap();
        });
        let record = read_record(&mut BufReader::new(reader)).await.unwrap();
        assert_eq!(
            serde_json::from_str::<serde_json::Value>(&record).unwrap()["text"],
            "a\u{2028}b"
        );
        sending.await.unwrap();
    }

    #[tokio::test]
    async fn jsonl_rejects_oversized_and_truncated_records() {
        assert!(read_record(&mut BufReader::new(&b"\n"[..])).await.is_err());
        assert!(
            read_record(&mut BufReader::new(&vec![b'x'; MAX_RECORD + 1][..]))
                .await
                .is_err()
        );
        assert!(read_record(&mut BufReader::new(&b"{}"[..])).await.is_err());
    }

    #[tokio::test]
    async fn patch_reads_are_chunked_and_reject_unfinished_jobs_or_outside_paths() {
        use crate::domain::development::{DevelopmentRequest, DevelopmentStore};
        let root = tempfile::tempdir().unwrap();
        let store =
            Arc::new(SqliteDevelopmentStore::open(&root.path().join("jobs.sqlite3")).unwrap());
        let service = DevelopmentService::new(Arc::clone(&store), ["pan".into()].into());
        let job = store
            .submit(&DevelopmentRequest {
                request_id: "test".into(),
                repository: "pan".into(),
                instruction: "feature".into(),
                origin: "cli".into(),
                conversation: None,
            })
            .unwrap();
        assert!(
            patch_response(&service, root.path(), job.id.clone(), 0)
                .await
                .is_err()
        );
        let artifact_dir = root.path().join(&job.id);
        tokio::fs::create_dir(&artifact_dir).await.unwrap();
        let patch = "diff with UTF-8: é\n".repeat(10_000).into_bytes();
        tokio::fs::write(artifact_dir.join("changes.patch"), &patch)
            .await
            .unwrap();
        store.finish(&job.id, JobState::Completed, "done").unwrap();
        let mut received = Vec::new();
        while received.len() < patch.len() {
            let WorkerResponse::Patch { chunk } =
                patch_response(&service, root.path(), job.id.clone(), received.len() as u64)
                    .await
                    .unwrap()
            else {
                panic!("expected patch chunk");
            };
            assert!(chunk.bytes.len() <= 65_536);
            assert_eq!(chunk.total, patch.len() as u64);
            received.extend(chunk.bytes);
        }
        assert_eq!(received, patch);
        assert!(
            patch_response(
                &service,
                root.path(),
                job.id.clone(),
                patch.len() as u64 + 1
            )
            .await
            .is_err()
        );
        assert!(
            patch_response(&service, root.path(), "../../outside".into(), 0)
                .await
                .is_err()
        );
        tokio::fs::remove_file(artifact_dir.join("changes.patch"))
            .await
            .unwrap();
        std::os::unix::fs::symlink("/etc/passwd", artifact_dir.join("changes.patch")).unwrap();
        assert!(
            patch_response(&service, root.path(), job.id, 0)
                .await
                .is_err()
        );
    }
}
