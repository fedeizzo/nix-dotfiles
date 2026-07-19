mod application;
mod domain;
mod infrastructure;
mod interface;

use std::sync::Arc;

use anyhow::{Context, Ok, Result};
use clap::Parser;

use crate::{
    infrastructure::lunchmoney,
    interface::{
        cli,
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
}

#[tokio::main]
async fn main() -> Result<()> {
    let args = Args::parse();
    let config = infrastructure::config::load_config(&args.config)
        .context("Failed to load configuration file")?;
    config
        .validate()
        .context("Failed to validate configuration file")?;

    let _log_guard = infrastructure::logging::init_logging(&config.log);
    tracing::info!("Initializing Pan Agent...");

    let lunchmoney_key = config
        .lunchmoney
        .get_api_key()
        .context("Failed to get lunchmoney API key")?;

    let lunchmoney_client = Arc::new(lunchmoney::LunchmoneyClient::new(
        "https://api.lunchmoney.dev".into(),
        lunchmoney_key,
    ));

    let finance_service =
        crate::application::finance::FinanceService::new(Arc::clone(&lunchmoney_client));

    let get_user_tool = GetUserTool::new(finance_service.clone());
    let get_accounts_tool = GetAccountsTool::new(finance_service.clone());
    let calculate_net_worth_tool = CalculateNetWorthTool::new(finance_service.clone());
    let get_categories_tool = GetCategoriesTool::new(finance_service.clone());
    let get_tags_tool = GetTagsTool::new(finance_service.clone());
    let get_unreviewed_transactions_tool = GetUnreviewedTransactionsTool::new(finance_service.clone());

    let tools: Vec<Box<dyn rig::tool::ToolDyn>> = vec![
        Box::new(get_user_tool),
        Box::new(get_accounts_tool),
        Box::new(calculate_net_worth_tool),
        Box::new(get_categories_tool),
        Box::new(get_tags_tool),
        Box::new(get_unreviewed_transactions_tool),
    ];

    let rig = infrastructure::ai::Rig::new(
        &config.models.openai_base_url,
        &config.models.openai_api_key,
        &config.models.name,
        tools,
    )
    .context("Failed to initialize the rig")?;

    let app_state = interface::api::AppState {
        rig: Arc::new(rig),
    };

    let app = interface::api::create_router(app_state);

    let addr = tokio::net::TcpListener::bind("127.0.0.1:3000").await?;
    tracing::info!("Server running on http://127.0.0.1:3000");
    axum::serve(addr, app).await?;

    Ok(())
}
