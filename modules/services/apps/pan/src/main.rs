use std::{path::PathBuf, sync::Arc};

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
    ReviewTransaction,
}

#[tokio::main]
async fn main() -> Result<()> {
    let args = Args::parse();
    let config = pan::infrastructure::config::load_config(&args.config)
        .context("Failed to load configuration file")?;
    config
        .validate()
        .context("Failed to validate configuration file")?;

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

    if matches!(args.command, Some(PanCommand::ReviewTransaction)) {
        match transaction_reviews
            .prepare()
            .await
            .context("Failed to prepare Lunch Money transaction review")?
        {
            Some(review) => {
                println!("{}", review.message());
                transaction_reviews
                    .release(review.transaction_id())
                    .await
                    .context("Failed to release previewed transaction")?;
            }
            None => println!("No unreviewed Lunch Money transaction is available."),
        }
        return Ok(());
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

    Ok(())
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
