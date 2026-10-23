use std::{
    path::{Path, PathBuf},
    time::Duration,
};

use anyhow::{Context, Result};
use matrix_sdk::{
    Client, Room, RoomState,
    attachment::AttachmentConfig,
    room::reply::{EnforceThread, Reply},
    ruma::{
        EventId, RoomId,
        events::room::message::{
            AddMentions, Relation, ReplyWithinThread, RoomMessageEventContent,
        },
    },
};

use crate::{
    domain::development::{DevelopmentJob, JobState, WorkerCommand, WorkerResponse},
    infrastructure::development_worker::{exchange, fetch_patch, request},
};

pub async fn run(
    client: Client,
    socket: Option<PathBuf>,
    allowed_room: String,
    allowed_user: String,
) {
    let Some(socket) = socket else {
        std::future::pending::<()>().await;
        return;
    };
    loop {
        if let Err(error) = deliver_pending(&client, &socket, &allowed_room, &allowed_user).await {
            tracing::warn!(%error, "development result delivery will be retried");
        }
        tokio::time::sleep(Duration::from_secs(5)).await;
    }
}

async fn deliver_pending(
    client: &Client,
    socket: &Path,
    allowed_room: &str,
    allowed_user: &str,
) -> Result<()> {
    let WorkerResponse::Notifications { jobs } =
        exchange(socket, &WorkerCommand::PendingNotifications).await?
    else {
        anyhow::bail!("unexpected notification response");
    };
    for job in jobs {
        if !notification_is_allowed(&job, allowed_room, allowed_user) {
            continue;
        }
        let Some(binding) = &job.request.conversation else {
            continue;
        };
        let room = client
            .get_room(&RoomId::parse(&binding.room_id)?)
            .context("development room is unavailable")?;
        anyhow::ensure!(
            room.state() == RoomState::Joined,
            "development room is not joined"
        );
        deliver_job(&room, socket, &job).await?;
        request(
            socket,
            &WorkerCommand::AcknowledgeNotification { id: job.id },
        )
        .await?;
    }
    Ok(())
}

fn notification_is_allowed(job: &DevelopmentJob, room: &str, user: &str) -> bool {
    !room.is_empty()
        && !user.is_empty()
        && job.state.is_terminal()
        && job
            .request
            .conversation
            .as_ref()
            .is_some_and(|binding| binding.room_id == room && binding.user_id == user)
}

fn transaction_id(job: &DevelopmentJob, artifact: &str) -> String {
    format!("pan-development-{}-{artifact}", job.id)
}

fn result_content(job: &DevelopmentJob) -> Result<RoomMessageEventContent> {
    let binding = job
        .request
        .conversation
        .as_ref()
        .context("missing development conversation")?;
    let summary: String = job.result.chars().take(4000).collect();
    let mut content = RoomMessageEventContent::text_plain(format!(
        "Development job {}: {:?}\n\n{summary}",
        job.id, job.state
    ));
    content.relates_to = Some(Relation::Thread(
        matrix_sdk::ruma::events::relation::Thread::reply(
            EventId::parse(&binding.thread_id)?,
            EventId::parse(&job.request.request_id)?,
        ),
    ));
    Ok(content)
}

async fn deliver_job(room: &Room, socket: &Path, job: &DevelopmentJob) -> Result<()> {
    room.send(result_content(job)?)
        .with_transaction_id(transaction_id(job, "result").into())
        .await?;
    if job.state == JobState::Completed {
        let patch = fetch_patch(socket, &job.id).await?;
        if !patch.is_empty() {
            let binding = job
                .request
                .conversation
                .as_ref()
                .context("missing development conversation")?;
            let config = AttachmentConfig::new()
                .txn_id(transaction_id(job, "patch").into())
                .reply(Some(Reply {
                    event_id: EventId::parse(&binding.thread_id)?,
                    enforce_thread: EnforceThread::Threaded(ReplyWithinThread::Yes),
                    add_mentions: AddMentions::No,
                }));
            room.send_attachment(
                format!("pan-{}.patch", job.id),
                &"text/x-diff".parse()?,
                patch,
                config,
            )
            .await?;
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::domain::development::{DevelopmentConversation, DevelopmentRequest};

    fn job() -> DevelopmentJob {
        DevelopmentJob {
            id: "job".into(),
            state: JobState::Completed,
            created_at: String::new(),
            updated_at: String::new(),
            result: "Checks passed".into(),
            request: DevelopmentRequest {
                request_id: "$request:example.org".into(),
                repository: "pan".into(),
                instruction: "feature".into(),
                origin: "matrix".into(),
                conversation: Some(DevelopmentConversation {
                    room_id: "!room:example.org".into(),
                    thread_id: "$root:example.org".into(),
                    user_id: "@owner:example.org".into(),
                }),
            },
        }
    }

    #[test]
    fn notifications_require_explicit_matching_owner_and_room() {
        let mut job = job();
        assert!(notification_is_allowed(
            &job,
            "!room:example.org",
            "@owner:example.org"
        ));
        for (room, user) in [
            ("", "@owner:example.org"),
            ("!room:example.org", ""),
            ("!other:example.org", "@owner:example.org"),
            ("!room:example.org", "@stranger:example.org"),
        ] {
            assert!(!notification_is_allowed(&job, room, user));
        }
        job.state = JobState::Running;
        assert!(!notification_is_allowed(
            &job,
            "!room:example.org",
            "@owner:example.org"
        ));
    }

    #[test]
    fn result_stays_in_original_thread_and_retries_use_stable_distinct_ids() {
        let job = job();
        let content = serde_json::to_value(result_content(&job).unwrap()).unwrap();
        assert_eq!(content["m.relates_to"]["event_id"], "$root:example.org");
        assert_eq!(
            content["m.relates_to"]["m.in_reply_to"]["event_id"],
            "$request:example.org"
        );
        assert_eq!(
            transaction_id(&job, "result"),
            transaction_id(&job, "result")
        );
        assert_ne!(
            transaction_id(&job, "result"),
            transaction_id(&job, "patch")
        );
    }
}
