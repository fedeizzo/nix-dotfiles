use std::{path::PathBuf, process::ExitCode, sync::Arc};

use anyhow::{Context, Result};
use clap::{Parser, Subcommand};

use pan::{
    application::{email_triage::EmailTriageService, transaction_review::TransactionReviewService},
    domain::workflow::WorkflowRepository,
    infrastructure::{
        config::{AppConfig, InterfaceType},
        fastmail, lunchmoney,
    },
    interface::{
        cli,
        email_tools::{GetMailboxesTool, GetUnreadEmailTool},
        tools::{
            CalculateNetWorthTool, GetAccountsTool, GetCategoriesTool, GetTagsTool,
            GetUnreviewedTransactionsTool, GetUserTool,
        },
    },
};

/// Pan: An AI workflow automation service
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Path to the configuration YAML file
    #[arg(short, long, default_value = "config.yaml")]
    config: String,

    #[command(subcommand)]
    command: Option<PanCommand>,
}

#[derive(Debug, Subcommand)]
enum PanCommand {
    /// Resolve and print the next available Lunch Money review without changing it.
    ReviewTransaction {
        /// Emit machine-readable JSON.
        #[arg(long)]
        json: bool,
    },
    /// Apply the currently visible values for an exact transaction after explicit confirmation.
    ApplyTransaction {
        /// Lunch Money transaction ID.
        id: i64,
        /// Required acknowledgement for this exact transaction.
        #[arg(long)]
        confirm: bool,
    },
}

#[tokio::main]
async fn main() -> ExitCode {
    match run().await {
        Ok(code) => code,
        Err(error) => {
            eprintln!("{error:#}");
            exit_code_for_error(&error)
        }
    }
}

#[expect(
    clippy::too_many_lines,
    reason = "top-level command dispatch keeps adapter construction visible"
)]
async fn run() -> Result<ExitCode> {
    let args = Args::parse();
    let config = pan::infrastructure::config::load_config(&args.config)
        .context("Failed to load configuration file")?;
    if matches!(
        args.command,
        Some(PanCommand::ReviewTransaction { .. } | PanCommand::ApplyTransaction { .. })
    ) {
        config
            .validate_lunchmoney_cli()
            .context("Failed to validate configuration file")?;
    } else {
        config
            .validate()
            .context("Failed to validate configuration file")?;
    }

    let _log_guard = pan::infrastructure::logging::init_logging(&config.log);
    tracing::info!("Initializing Pan Agent...");

    let lunchmoney_key = config
        .lunchmoney
        .get_api_key()
        .context("Failed to get lunchmoney API key")?;

    let lunchmoney_client = Arc::new(lunchmoney::LunchmoneyClient::new(
        "https://api.lunchmoney.dev".into(),
        lunchmoney_key,
    ));
    let workflow_repository = workflow_repository(&config).await?;
    let finance_service =
        pan::application::finance::FinanceService::new(Arc::clone(&lunchmoney_client));
    let transaction_reviews = Arc::new(TransactionReviewService::new(
        Arc::clone(&lunchmoney_client),
        Arc::clone(&workflow_repository),
    ));

    if let Some(PanCommand::ReviewTransaction { json }) = args.command {
        if let Some(review) = transaction_reviews
            .prepare()
            .await
            .context("Failed to prepare Lunch Money transaction review")?
        {
            if json {
                println!("{{\"transaction_id\":{}}}", review.transaction_id());
            } else {
                println!("{}", review.message());
            }
            transaction_reviews
                .release(review.transaction_id())
                .await
                .context("Failed to release previewed transaction")?;
        } else {
            println!("No unreviewed Lunch Money transaction is available.");
            return Ok(ExitCode::from(EXIT_NO_WORK));
        }
        return Ok(ExitCode::SUCCESS);
    }

    if let Some(PanCommand::ApplyTransaction { id, confirm }) = args.command {
        anyhow::ensure!(
            confirm,
            "refusing transaction {id}: pass --confirm to apply it"
        );
        let Some(review) = transaction_reviews.prepare_by_id(id).await? else {
            anyhow::bail!("transaction {id} is already in an active workflow");
        };
        transaction_reviews
            .track("cli".to_owned(), id.to_string(), review)
            .await?;
        match transaction_reviews
            .handle_reply("cli", &id.to_string(), "confirm")
            .await?
        {
            pan::application::transaction_review::ReplyOutcome::Updated(message) => {
                println!("{message}");
            }
            _ => anyhow::bail!("transaction {id} was not applied"),
        }
        return Ok(ExitCode::SUCCESS);
    }

    let fastmail_key = config
        .fastmail
        .get_api_key()
        .context("Failed to get Fastmail API key")?;
    let fastmail_client = Arc::new(
        fastmail::FastmailClient::connect(&config.fastmail.session_url, fastmail_key)
            .await
            .context("Failed to initialize Fastmail")?,
    );
    let email_triage = Arc::new(EmailTriageService::new(
        Arc::clone(&fastmail_client),
        workflow_repository,
    ));

    let get_user_tool = GetUserTool::new(finance_service.clone());
    let get_accounts_tool = GetAccountsTool::new(finance_service.clone());
    let calculate_net_worth_tool = CalculateNetWorthTool::new(finance_service.clone());
    let get_categories_tool = GetCategoriesTool::new(finance_service.clone());
    let get_tags_tool = GetTagsTool::new(finance_service.clone());
    let get_unreviewed_transactions_tool =
        GetUnreviewedTransactionsTool::new(finance_service.clone());

    let tools: Vec<Box<dyn rig::tool::ToolDyn>> = vec![
        Box::new(get_user_tool),
        Box::new(get_accounts_tool),
        Box::new(calculate_net_worth_tool),
        Box::new(get_categories_tool),
        Box::new(get_tags_tool),
        Box::new(get_unreviewed_transactions_tool),
        Box::new(GetMailboxesTool::new(Arc::clone(&fastmail_client))),
        Box::new(GetUnreadEmailTool::new(Arc::clone(&fastmail_client))),
    ];

    let rig = Arc::new(
        pan::infrastructure::ai::Rig::new(
            &config.models.openai_base_url,
            &config.models.openai_api_key,
            &config.models.name,
            tools,
        )
        .context("Failed to initialize the rig")?,
    );

    match config.interface.interface_type {
        InterfaceType::Cli => cli::run_chat_loop(rig.as_ref()).await?,
        InterfaceType::Matrix => {
            let matrix = config
                .matrix
                .context("Matrix interface selected without Matrix configuration")?;
            pan::interface::matrix::MatrixBot::new(
                rig,
                transaction_reviews,
                email_triage,
                matrix,
                config.jobs,
            )
            .run()
            .await?;
        }
    }

    Ok(ExitCode::SUCCESS)
}

const EXIT_CONFIG: u8 = 2;
const EXIT_NO_WORK: u8 = 3;
const EXIT_PROVIDER: u8 = 4;

fn exit_code_for_error(error: &anyhow::Error) -> ExitCode {
    let message = format!("{error:#}").to_ascii_lowercase();
    if message.contains("configuration")
        || message.contains("config.yaml")
        || message.contains("api key")
    {
        ExitCode::from(EXIT_CONFIG)
    } else if message.contains("lunch money")
        || message.contains("fastmail")
        || message.contains("matrix")
        || message.contains("provider")
    {
        ExitCode::from(EXIT_PROVIDER)
    } else {
        ExitCode::FAILURE
    }
}

async fn workflow_repository(config: &AppConfig) -> Result<Arc<dyn WorkflowRepository>> {
    let path = config.matrix.as_ref().map_or_else(
        || PathBuf::from("pan-workflows.sqlite3"),
        |matrix| PathBuf::from(&matrix.data_dir).join("workflows.sqlite3"),
    );
    Ok(Arc::new(
        pan::infrastructure::workflow_sqlite::SqliteWorkflowRepository::open(path)
            .await
            .context("Failed to initialize workflow state")?,
    ))
}

#[cfg(test)]
mod tests {
    use std::process::ExitCode;

    use anyhow::anyhow;

    use super::{EXIT_CONFIG, EXIT_PROVIDER, exit_code_for_error};

    #[test]
    fn configuration_errors_have_a_dedicated_exit_code() {
        assert_eq!(
            exit_code_for_error(&anyhow!("Failed to validate configuration file")),
            ExitCode::from(EXIT_CONFIG)
        );
    }

    #[test]
    fn provider_errors_have_a_dedicated_exit_code() {
        assert_eq!(
            exit_code_for_error(&anyhow!("Lunch Money request failed: 503")),
            ExitCode::from(EXIT_PROVIDER)
        );
    }
}
