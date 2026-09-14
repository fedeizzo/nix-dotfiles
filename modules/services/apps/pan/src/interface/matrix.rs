use std::{
    path::Path,
    str::FromStr,
    sync::Arc,
    time::{Duration, SystemTime, UNIX_EPOCH},
};

use anyhow::{Context, Result, bail};
use chrono::Local;
use cron::Schedule;
use matrix_sdk::{
    Client, Room, RoomState,
    authentication::matrix::MatrixSession,
    config::{RequestConfig, SyncSettings},
    room::MessagesOptions,
    ruma::{
        RoomId,
        events::room::{
            member::{MembershipState, StrippedRoomMemberEvent},
            message::{
                MessageType, OriginalSyncRoomMessageEvent, Relation, RoomMessageEventContent,
            },
        },
        uint,
    },
};
use tokio::fs;

use crate::{
    application::{
        email_triage::{
            EmailTriageService, ReplyOutcome as EmailReplyOutcome,
            StartOutcome as EmailStartOutcome,
        },
        transaction_review::{ReplyOutcome, TransactionReviewService},
    },
    domain::{chat::ChatProvider, email::EmailProvider, finance::FinanceProvider},
    infrastructure::config::{JobConfig, MatrixConfig},
};

const MATRIX_SEND_RETRY_LIMIT: usize = 3;
const MATRIX_SEND_RETRY_MAX: Duration = Duration::from_secs(30);

fn matrix_request_config() -> RequestConfig {
    // The SDK reads Matrix's `retry_after_ms` from M_LIMIT_EXCEEDED responses.
    // These bounds make that server-directed delay finite for a bot delivery.
    RequestConfig::new()
        .retry_limit(MATRIX_SEND_RETRY_LIMIT)
        .max_retry_time(MATRIX_SEND_RETRY_MAX)
}

pub struct MatrixBot<C, F, E> {
    chat: Arc<C>,
    reviews: Arc<TransactionReviewService<F>>,
    email_triage: Arc<EmailTriageService<E>>,
    config: MatrixConfig,
    jobs: Vec<JobConfig>,
}

impl<C, F, E> MatrixBot<C, F, E>
where
    C: ChatProvider + 'static,
    F: FinanceProvider + 'static,
    E: EmailProvider + 'static,
{
    #[must_use]
    pub fn new(
        chat: Arc<C>,
        reviews: Arc<TransactionReviewService<F>>,
        email_triage: Arc<EmailTriageService<E>>,
        config: MatrixConfig,
        jobs: Vec<JobConfig>,
    ) -> Self {
        Self {
            chat,
            reviews,
            email_triage,
            config,
            jobs,
        }
    }

    /// Runs the encrypted Matrix sync loop until shutdown.
    ///
    /// # Errors
    ///
    /// Returns an error when storage, authentication, initial sync, or shutdown handling fails.
    pub async fn run(self) -> Result<()> {
        let data_dir = Path::new(&self.config.data_dir);
        fs::create_dir_all(data_dir)
            .await
            .context("failed to create Matrix data directory")?;
        let client = Client::builder()
            .homeserver_url(&self.config.homeserver)
            .request_config(matrix_request_config())
            .sqlite_store(data_dir.join("store"), None)
            .build()
            .await
            .context("failed to build Matrix client")?;

        restore_or_login(&client, &self.config, data_dir).await?;
        register_invite_handler(&client, &self.config);

        let recovered_transactions = self
            .reviews
            .recover()
            .await
            .context("failed to reconcile Lunch Money workflows")?;
        let ambiguous_emails = self
            .email_triage
            .recover()
            .await
            .context("failed to reconcile Fastmail workflows")?;
        tracing::info!(
            recovered_transactions,
            ambiguous_emails,
            "Workflow recovery completed"
        );

        // Do not feed historical messages to the agent when the service starts.
        client
            .sync_once(SyncSettings::default())
            .await
            .context("initial Matrix sync failed")?;

        register_message_handler(
            &client,
            self.chat,
            Arc::clone(&self.reviews),
            Arc::clone(&self.email_triage),
            &self.config,
        );
        tracing::info!(user = %self.config.user, "Matrix bot is ready");

        let retention = self
            .config
            .message_retention
            .as_deref()
            .map(humantime::parse_duration)
            .transpose()
            .context("invalid Matrix message_retention duration")?;
        let retention_job =
            run_message_retention(client.clone(), self.config.allowed_room.clone(), retention);
        let scheduler = run_scheduler(
            client.clone(),
            self.reviews,
            self.email_triage,
            self.jobs,
            self.config.notification_room.clone(),
        );
        tokio::select! {
            result = client.sync(SyncSettings::default()) => {
                result.context("Matrix sync loop failed")?;
            }
            result = tokio::signal::ctrl_c() => {
                result.context("failed to listen for shutdown signal")?;
                tracing::info!("Matrix bot received shutdown signal");
            }
            result = scheduler => {
                result?;
            }
            () = retention_job => {}
        }

        Ok(())
    }
}

async fn run_message_retention(client: Client, allowed_room: String, retention: Option<Duration>) {
    let Some(retention) = retention else {
        std::future::pending::<()>().await;
        return;
    };
    loop {
        if let Err(error) = cleanup_old_messages(&client, &allowed_room, retention).await {
            tracing::error!(%error, "Matrix message retention cleanup failed");
        }
        tokio::time::sleep(Duration::from_hours(24)).await;
    }
}

async fn cleanup_old_messages(
    client: &Client,
    allowed_room: &str,
    retention: Duration,
) -> Result<()> {
    let rooms = if allowed_room.is_empty() {
        client.joined_rooms()
    } else {
        let room_id = RoomId::parse(allowed_room).context("invalid allowed Matrix room")?;
        client.get_room(&room_id).into_iter().collect()
    };
    let now = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .context("system clock is before Unix epoch")?;
    let cutoff_millis = now.saturating_sub(retention).as_millis();
    for room in rooms {
        cleanup_room_messages(&room, cutoff_millis).await;
    }
    Ok(())
}

async fn cleanup_room_messages(room: &Room, cutoff_millis: u128) {
    let mut from = None;
    loop {
        let mut options = MessagesOptions::backward();
        options.from = from;
        options.limit = uint!(100);
        let messages = match room.messages(options).await {
            Ok(messages) => messages,
            Err(error) => {
                tracing::warn!(room = %room.room_id(), %error, "Failed to paginate Matrix messages for retention");
                return;
            }
        };
        if messages.chunk.is_empty() {
            return;
        }
        for event in messages.chunk {
            let Some(timestamp) = event.timestamp() else {
                continue;
            };
            let timestamp_millis = u128::from(u64::from(timestamp.get()));
            if timestamp_millis >= cutoff_millis {
                continue;
            }
            let event_type = event.raw().get_field::<String>("type").ok().flatten();
            if !matches!(
                event_type.as_deref(),
                Some("m.room.message" | "m.room.encrypted")
            ) {
                continue;
            }
            let Some(event_id) = event.event_id() else {
                continue;
            };
            if let Err(error) = room
                .redact(&event_id, Some("Configured message retention policy"), None)
                .await
            {
                tracing::warn!(room = %room.room_id(), %event_id, %error, "Failed to redact old Matrix message");
            }
        }
        let Some(end) = messages.end else {
            return;
        };
        from = Some(end);
    }
}

async fn restore_or_login(client: &Client, config: &MatrixConfig, data_dir: &Path) -> Result<()> {
    let session_path = data_dir.join("session.json");
    if session_path.exists() {
        let serialized = fs::read_to_string(&session_path)
            .await
            .context("failed to read Matrix session")?;
        let session: MatrixSession =
            serde_json::from_str(&serialized).context("failed to parse Matrix session")?;
        if session.meta.user_id.as_str() != config.user {
            bail!(
                "stored Matrix session belongs to {}, not {}",
                session.meta.user_id,
                config.user
            );
        }
        client
            .restore_session(session)
            .await
            .context("failed to restore Matrix session")?;
        return Ok(());
    }

    let password = config
        .get_password()
        .context("failed to load Matrix password")?;
    client
        .matrix_auth()
        .login_username(&config.user, &password)
        .initial_device_display_name("pan")
        .await
        .context("Matrix login failed")?;
    let session = client
        .matrix_auth()
        .session()
        .context("Matrix login succeeded without a session")?;
    write_private_file(&session_path, &serde_json::to_vec(&session)?).await
}

async fn write_private_file(path: &Path, contents: &[u8]) -> Result<()> {
    fs::write(path, contents).await?;
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;

        fs::set_permissions(path, std::fs::Permissions::from_mode(0o600)).await?;
    }
    Ok(())
}

fn register_invite_handler(client: &Client, config: &MatrixConfig) {
    let own_user = config.user.clone();
    let allowed_user = config.allowed_user.clone();
    client.add_event_handler(move |event: StrippedRoomMemberEvent, room: Room| {
        let own_user = own_user.clone();
        let allowed_user = allowed_user.clone();
        async move {
            if event.state_key.as_str() != own_user
                || event.content.membership != MembershipState::Invite
                || (!allowed_user.is_empty() && event.sender.as_str() != allowed_user)
            {
                return;
            }
            if let Err(error) = room.join().await {
                tracing::error!(room = %room.room_id(), %error, "failed to join Matrix room");
            }
        }
    });
}

fn register_message_handler<C, F, E>(
    client: &Client,
    chat: Arc<C>,
    reviews: Arc<TransactionReviewService<F>>,
    email_triage: Arc<EmailTriageService<E>>,
    config: &MatrixConfig,
) where
    C: ChatProvider + 'static,
    F: FinanceProvider + 'static,
    E: EmailProvider + 'static,
{
    let allowed_user = config.allowed_user.clone();
    let allowed_room = config.allowed_room.clone();
    let own_user = config.user.clone();

    client.add_event_handler(move |event: OriginalSyncRoomMessageEvent, room: Room| {
        let chat = Arc::clone(&chat);
        let reviews = Arc::clone(&reviews);
        let email_triage = Arc::clone(&email_triage);
        let allowed_user = allowed_user.clone();
        let allowed_room = allowed_room.clone();
        let own_user = own_user.clone();
        async move {
            if room.state() != RoomState::Joined
                || !message_is_allowed(
                    event.sender.as_str(),
                    room.room_id().as_str(),
                    &own_user,
                    &allowed_user,
                    &allowed_room,
                )
            {
                return;
            }
            let MessageType::Text(text) = &event.content.msgtype else {
                return;
            };
            if text.body.trim().is_empty() {
                return;
            }

            let thread_root = workflow_thread_root(&event);
            if handle_workflow_message(
                &room,
                &event.event_id,
                &thread_root,
                text.body.trim(),
                &reviews,
                &email_triage,
            )
            .await
            {
                return;
            }
            let conversation_id = thread_root.as_str();
            match chat.prompt(conversation_id, text.body.trim()).await {
                Ok(response) => {
                    send_thread_reply(&room, &thread_root, &event.event_id, response).await;
                }
                Err(error) => {
                    tracing::error!(room = %room.room_id(), %error, "agent failed to answer Matrix message");
                }
            }
        }
    });
}

async fn handle_workflow_message<F: FinanceProvider, E: EmailProvider>(
    room: &Room,
    event_id: &matrix_sdk::ruma::EventId,
    thread_root: &matrix_sdk::ruma::EventId,
    body: &str,
    reviews: &TransactionReviewService<F>,
    email_triage: &EmailTriageService<E>,
) -> bool {
    let transaction_message = match reviews
        .handle_reply(room.room_id().as_str(), thread_root.as_str(), body)
        .await
    {
        Ok(ReplyOutcome::NeedsConfirmation(message) | ReplyOutcome::Updated(message)) => {
            Some(message)
        }
        Ok(ReplyOutcome::NotPending) => None,
        Err(error) => Some(format!("The transaction was not changed: {error}")),
    };
    if let Some(message) = transaction_message {
        send_thread_reply(room, thread_root, event_id, message).await;
        return true;
    }

    let email_message = match email_triage
        .handle_reply(room.room_id().as_str(), thread_root.as_str(), body)
        .await
    {
        Ok(EmailReplyOutcome::NeedsConfirmation(message) | EmailReplyOutcome::Updated(message)) => {
            Some(message)
        }
        Ok(EmailReplyOutcome::NotPending) => None,
        Err(error) => Some(format!("The email was not changed: {error}")),
    };
    if let Some(message) = email_message {
        send_thread_reply(room, thread_root, event_id, message).await;
        return true;
    }

    let Some(mailbox) = body.strip_prefix("email:") else {
        return false;
    };
    let message = match email_triage
        .start(
            room.room_id().to_string(),
            thread_root.to_string(),
            mailbox.trim(),
        )
        .await
    {
        Ok(
            EmailStartOutcome::NoUnread(message)
            | EmailStartOutcome::AlreadyActive(message)
            | EmailStartOutcome::Started(message),
        ) => message,
        Err(error) => format!("Could not open the Fastmail message: {error}"),
    };
    send_thread_reply(room, thread_root, event_id, message).await;
    true
}

async fn send_thread_reply(
    room: &Room,
    thread_root: &matrix_sdk::ruma::EventId,
    reply_to: &matrix_sdk::ruma::EventId,
    message: String,
) {
    let mut content = RoomMessageEventContent::text_markdown(message);
    content.relates_to = Some(Relation::Thread(
        matrix_sdk::ruma::events::relation::Thread::reply(
            thread_root.to_owned(),
            reply_to.to_owned(),
        ),
    ));
    if let Err(error) = room.send(content).await {
        tracing::error!(room = %room.room_id(), %error, "failed to send Matrix reply");
    }
}

async fn run_scheduler<F: FinanceProvider + 'static, E: EmailProvider + 'static>(
    client: Client,
    reviews: Arc<TransactionReviewService<F>>,
    email_triage: Arc<EmailTriageService<E>>,
    jobs: Vec<JobConfig>,
    notification_room: String,
) -> Result<()> {
    if jobs.is_empty() {
        std::future::pending::<()>().await;
        return Ok(());
    }

    let mut tasks = tokio::task::JoinSet::new();
    for job in jobs {
        let schedule = parse_cron_spec(&job.spec)?;
        match job.runner.as_str() {
            "lunchmoney" => tasks.spawn(run_transaction_job(
                client.clone(),
                Arc::clone(&reviews),
                job,
                schedule,
                notification_room.clone(),
            )),
            "fastmail" => tasks.spawn(run_email_job(
                client.clone(),
                Arc::clone(&email_triage),
                job,
                schedule,
                notification_room.clone(),
            )),
            runner => anyhow::bail!("unsupported scheduler runner `{runner}`"),
        };
    }

    while let Some(result) = tasks.join_next().await {
        result.context("Lunch Money scheduler task panicked")??;
    }
    Ok(())
}

async fn run_email_job<E: EmailProvider + 'static>(
    client: Client,
    email_triage: Arc<EmailTriageService<E>>,
    job: JobConfig,
    schedule: Schedule,
    notification_room: String,
) -> Result<()> {
    let room_id = RoomId::parse(&notification_room).context("invalid Matrix notification room")?;
    tracing::info!(job = %job.name, schedule = %job.spec, "Fastmail triage job scheduled");
    loop {
        let next = schedule
            .upcoming(Local)
            .next()
            .context("Fastmail schedule has no future occurrence")?;
        tokio::time::sleep(delay_until(next, Local::now())).await;
        let Some(room) = client.get_room(&room_id) else {
            tracing::error!(job = %job.name, room = %room_id, "Matrix notification room is not known to the client");
            continue;
        };
        let Some(email) = email_triage
            .prepare(&job.mailbox)
            .await
            .map_err(anyhow::Error::from)?
        else {
            tracing::debug!(job = %job.name, "No unread Fastmail message found");
            continue;
        };
        let email_id = email.email_id().to_owned();
        match room
            .send(RoomMessageEventContent::text_markdown(email.message()))
            .await
        {
            Ok(response) => {
                if let Err(error) = email_triage
                    .track(
                        room_id.to_string(),
                        response.response.event_id.to_string(),
                        email,
                    )
                    .await
                {
                    tracing::error!(job = %job.name, room = %room_id, email_id, %error, "Failed to persist delivered Fastmail triage");
                }
            }
            Err(error) => {
                if let Err(release_error) = email_triage.release(&email_id).await {
                    tracing::error!(job = %job.name, email_id, %release_error, "Failed to release undelivered Fastmail triage");
                }
                tracing::error!(job = %job.name, room = %room_id, %error, "Failed to send Fastmail triage");
            }
        }
    }
}

async fn run_transaction_job<F: FinanceProvider + 'static>(
    client: Client,
    reviews: Arc<TransactionReviewService<F>>,
    job: JobConfig,
    schedule: Schedule,
    notification_room: String,
) -> Result<()> {
    let room_id = RoomId::parse(&notification_room).context("invalid Matrix notification room")?;
    tracing::info!(job = %job.name, schedule = %job.spec, "Lunch Money review job scheduled");

    loop {
        let next = schedule
            .upcoming(Local)
            .next()
            .context("Lunch Money review schedule has no future occurrence")?;
        let delay = delay_until(next, Local::now());
        tokio::time::sleep(delay).await;
        let Some(room) = client.get_room(&room_id) else {
            tracing::error!(job = %job.name, room = %room_id, "Matrix notification room is not known to the client");
            continue;
        };
        if room.state() != RoomState::Joined {
            tracing::error!(job = %job.name, room = %room_id, "Matrix notification room is not joined");
            continue;
        }
        let review = match reviews.prepare().await {
            Ok(Some(review)) => review,
            Ok(None) => {
                tracing::debug!(job = %job.name, "No unreviewed Lunch Money transaction found");
                continue;
            }
            Err(error) => {
                tracing::error!(job = %job.name, %error, "Failed to prepare Lunch Money review");
                continue;
            }
        };
        let transaction_id = review.transaction_id();
        let message = review.message();
        match room
            .send(RoomMessageEventContent::text_markdown(message))
            // Matrix deduplicates retransmissions with a client transaction ID. The ID is
            // stable for the durable Lunch Money claim, so a crash after the homeserver has
            // accepted the message cannot create another review when the claim is retried.
            .with_transaction_id(matrix_transaction_id(transaction_id).into())
            .await
        {
            Ok(response) => {
                if let Err(error) = reviews
                    .track(
                        room_id.to_string(),
                        response.response.event_id.to_string(),
                        review,
                    )
                    .await
                {
                    tracing::error!(job = %job.name, room = %room_id, transaction_id, %error, "Failed to persist delivered Lunch Money review");
                }
            }
            Err(error) => {
                if let Err(release_error) = reviews.release(transaction_id).await {
                    tracing::error!(job = %job.name, transaction_id, %release_error, "Failed to release undelivered Lunch Money review");
                }
                tracing::error!(room = %room_id, %error, "Failed to send Lunch Money review");
            }
        }
    }
}

fn matrix_transaction_id(transaction_id: i64) -> String {
    format!("pan-lunchmoney-{transaction_id}")
}

fn workflow_thread_root(event: &OriginalSyncRoomMessageEvent) -> matrix_sdk::ruma::OwnedEventId {
    match event.content.relates_to.as_ref() {
        Some(Relation::Thread(thread)) => thread.event_id.clone(),
        Some(Relation::Reply(reply)) => reply.in_reply_to.event_id.clone(),
        _ => event.event_id.clone(),
    }
}

fn delay_until(next: chrono::DateTime<Local>, now: chrono::DateTime<Local>) -> Duration {
    (next - now).to_std().unwrap_or(Duration::ZERO)
}

fn parse_cron_spec(spec: &str) -> Result<Schedule> {
    let fields = spec.split_whitespace().count();
    let normalized = match fields {
        5 => format!("0 {spec} *"),
        6 | 7 => spec.to_owned(),
        _ => bail!("cron spec must contain 5, 6, or 7 fields"),
    };
    Schedule::from_str(&normalized).context("invalid Lunch Money review cron spec")
}

fn message_is_allowed(
    sender: &str,
    room: &str,
    own_user: &str,
    allowed_user: &str,
    allowed_room: &str,
) -> bool {
    sender != own_user
        && (allowed_user.is_empty() || sender == allowed_user)
        && (allowed_room.is_empty() || room == allowed_room)
}

#[cfg(test)]
mod tests {
    use std::{
        sync::{
            Arc,
            atomic::{AtomicUsize, Ordering},
        },
        time::Instant,
    };

    use super::{
        MATRIX_SEND_RETRY_LIMIT, MATRIX_SEND_RETRY_MAX, delay_until, matrix_request_config,
        matrix_transaction_id, message_is_allowed, parse_cron_spec, workflow_thread_root,
    };
    use matrix_sdk::ruma::api::MatrixVersion;
    use rstest::rstest;
    use std::time::Duration;
    use wiremock::{
        Mock, MockServer, ResponseTemplate,
        matchers::{method, path},
    };

    use chrono::{Duration as ChronoDuration, Local};

    #[rstest]
    #[case::allowed_user_and_room("@owner:example.org", "!room:example.org", true)]
    #[case::own_message("@pan:example.org", "!room:example.org", false)]
    #[case::wrong_user("@stranger:example.org", "!room:example.org", false)]
    #[case::wrong_room("@owner:example.org", "!other:example.org", false)]
    fn message_filter_enforces_sender_and_room(
        #[case] sender: &str,
        #[case] room: &str,
        #[case] expected: bool,
    ) {
        assert_eq!(
            message_is_allowed(
                sender,
                room,
                "@pan:example.org",
                "@owner:example.org",
                "!room:example.org"
            ),
            expected
        );
    }

    #[test]
    fn cron_parser_accepts_go_style_five_field_spec() {
        assert!(parse_cron_spec("*/5 10-20 * * *").is_ok());
    }

    #[test]
    fn cron_parser_rejects_incomplete_spec() {
        assert!(parse_cron_spec("every morning").is_err());
    }

    #[test]
    fn matrix_delivery_uses_a_stable_transaction_id_per_review() {
        assert_eq!(matrix_transaction_id(42), "pan-lunchmoney-42");
    }

    #[test]
    fn matrix_rate_limit_retries_are_bounded() {
        let config = format!("{:?}", matrix_request_config());
        assert!(config.contains(&format!("retry_limit: {MATRIX_SEND_RETRY_LIMIT}")));
        assert!(config.contains(&format!("max_retry_time: {MATRIX_SEND_RETRY_MAX:?}")));
    }

    #[tokio::test]
    async fn matrix_http_429_retries_after_the_server_delay() {
        let server = MockServer::start().await;
        let requests = Arc::new(AtomicUsize::new(0));
        let request_times = Arc::new(std::sync::Mutex::new(Vec::<Instant>::new()));
        let requests_for_response = Arc::clone(&requests);
        let request_times_for_response = Arc::clone(&request_times);
        Mock::given(method("POST"))
            .and(path("/_matrix/client/r0/login"))
            .respond_with(move |_: &wiremock::Request| {
                request_times_for_response
                    .lock()
                    .expect("request timestamp lock")
                    .push(Instant::now());
                if requests_for_response.fetch_add(1, Ordering::SeqCst) == 0 {
                    ResponseTemplate::new(429).set_body_json(serde_json::json!({
                        "errcode": "M_LIMIT_EXCEEDED",
                        "error": "slow down",
                        "retry_after_ms": 20
                    }))
                } else {
                    ResponseTemplate::new(200).set_body_json(serde_json::json!({
                        "access_token": "token",
                        "device_id": "PAN",
                        "home_server": "example.org",
                        "user_id": "@pan:example.org"
                    }))
                }
            })
            .mount(&server)
            .await;

        let client = matrix_sdk::Client::builder()
            .homeserver_url(server.uri())
            .server_versions([MatrixVersion::V1_0])
            .request_config(matrix_request_config())
            .build()
            .await
            .expect("build mock Matrix client");
        client
            .matrix_auth()
            .login_username("pan", "secret")
            .send()
            .await
            .expect("429 should be retried");

        assert_eq!(requests.load(Ordering::SeqCst), 2);
        let request_times = request_times.lock().expect("request timestamp lock");
        assert!(
            request_times[1].duration_since(request_times[0]) >= Duration::from_millis(20),
            "the retry must honor Matrix's retry_after_ms"
        );
    }

    #[test]
    fn matrix_thread_events_bind_confirmations_to_the_root_event() {
        let event: matrix_sdk::ruma::events::room::message::OriginalSyncRoomMessageEvent =
            serde_json::from_value(serde_json::json!({
                "type": "m.room.message",
                "event_id": "$reply:example.org",
                "sender": "@owner:example.org",
                "origin_server_ts": 0,
                "content": {
                    "msgtype": "m.text",
                    "body": "confirm",
                    "m.relates_to": {
                        "rel_type": "m.thread",
                        "event_id": "$root:example.org",
                        "is_falling_back": false,
                        "m.in_reply_to": { "event_id": "$previous:example.org" }
                    }
                }
            }))
            .expect("valid Matrix thread event fixture");

        assert_eq!(workflow_thread_root(&event).as_str(), "$root:example.org");
    }

    #[test]
    fn matrix_reply_events_bind_confirmations_to_the_replied_root_event() {
        let event: matrix_sdk::ruma::events::room::message::OriginalSyncRoomMessageEvent =
            serde_json::from_value(serde_json::json!({
                "type": "m.room.message",
                "event_id": "$reply:example.org",
                "sender": "@owner:example.org",
                "origin_server_ts": 0,
                "content": {
                    "msgtype": "m.text",
                    "body": "confirm",
                    "m.relates_to": {
                        "m.in_reply_to": { "event_id": "$root:example.org" }
                    }
                }
            }))
            .expect("valid Matrix reply event fixture");

        assert_eq!(workflow_thread_root(&event).as_str(), "$root:example.org");
    }

    #[tokio::test(start_paused = true)]
    async fn scheduler_delay_is_testable_without_wall_clock_waiting() {
        let now = Local::now();
        let sleeping = tokio::time::sleep(delay_until(now + ChronoDuration::seconds(5), now));
        tokio::pin!(sleeping);

        tokio::time::advance(Duration::from_secs(5)).await;
        sleeping.await;
    }
}
