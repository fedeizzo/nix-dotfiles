use rig::tool::Tool;
use serde::{Deserialize, Serialize};

use crate::domain::finance::FinanceProvider;

#[derive(Deserialize, Serialize)]
pub struct GetUserArgs {}

#[derive(Deserialize, Serialize)]
pub struct GetAccountsArgs {}

#[derive(Deserialize, Serialize)]
pub struct CalculateNetWorthArgs {}

#[derive(Debug, thiserror::Error)]
#[error("Tool Error: {0}")]
pub struct ToolError(pub String);

pub struct GetUserTool<P: FinanceProvider> {
    service: crate::application::finance::FinanceService<P>,
}

impl<P: FinanceProvider> GetUserTool<P> {
    pub fn new(service: crate::application::finance::FinanceService<P>) -> Self {
        Self { service }
    }
}

impl<P: FinanceProvider + Send + Sync> Tool for GetUserTool<P> {
    const NAME: &'static str = "get_finance_user";
    type Error = ToolError;
    type Args = GetUserArgs;
    type Output = String;

    fn description(&self) -> String {
        "Fetches the current user profile from the financial provider".to_string()
    }

    fn parameters(&self) -> serde_json::Value {
        serde_json::json!({
            "type": "object",
            "properties": {},
            "required": []
        })
    }

    async fn call(&self, _args: Self::Args) -> Result<Self::Output, Self::Error> {
        match self.service.get_user().await {
            Ok(user) => Ok(format!(
                "Found user: {} (Budget: {}, API Label: {})",
                user.name, user.budget_name, user.api_key_label
            )),
            Err(e) => Err(ToolError(e.to_string())),
        }
    }
}

pub struct GetAccountsTool<P: FinanceProvider> {
    service: crate::application::finance::FinanceService<P>,
}

impl<P: FinanceProvider> GetAccountsTool<P> {
    pub fn new(service: crate::application::finance::FinanceService<P>) -> Self {
        Self { service }
    }
}

impl<P: FinanceProvider + Send + Sync> Tool for GetAccountsTool<P> {
    const NAME: &'static str = "get_finance_account";
    type Error = ToolError;
    type Args = GetAccountsArgs;
    type Output = String;

    fn description(&self) -> String {
        "Fetches all accounts from the financial provider".to_string()
    }

    fn parameters(&self) -> serde_json::Value {
        serde_json::json!({
            "type": "object",
            "properties": {},
            "required": []
        })
    }

    async fn call(&self, _args: Self::Args) -> Result<Self::Output, Self::Error> {
        match self.service.get_accounts().await {
            Ok(accounts) => serde_json::to_string(&accounts).map_err(|e| ToolError(e.to_string())),
            Err(e) => Err(ToolError(e.to_string())),
        }
    }
}

pub struct CalculateNetWorthTool<P: FinanceProvider> {
    service: crate::application::finance::FinanceService<P>,
}

impl<P: FinanceProvider> CalculateNetWorthTool<P> {
    pub fn new(service: crate::application::finance::FinanceService<P>) -> Self {
        Self { service }
    }
}

impl<P: FinanceProvider + Send + Sync> Tool for CalculateNetWorthTool<P> {
    const NAME: &'static str = "calculate_net_worth";
    type Error = ToolError;
    type Args = CalculateNetWorthArgs;
    type Output = String;

    fn description(&self) -> String {
        "Calculates the total net worth by summing the balances of all active financial accounts"
            .to_string()
    }

    fn parameters(&self) -> serde_json::Value {
        serde_json::json!({
            "type": "object",
            "properties": {},
            "required": []
        })
    }

    async fn call(&self, _args: Self::Args) -> Result<Self::Output, Self::Error> {
        let currency = self
            .service
            .get_user()
            .await
            .map_err(|e| ToolError(e.to_string()))?
            .primary_currency;
        match self.service.calculate_net_worth().await {
            Ok(net_worth) => Ok(format!("Total net worth is: {currency} {net_worth:.2}")),
            Err(e) => Err(ToolError(e.to_string())),
        }
    }
}

#[derive(Deserialize, Serialize)]
pub struct GetCategoriesArgs {}

pub struct GetCategoriesTool<P: FinanceProvider> {
    service: crate::application::finance::FinanceService<P>,
}

impl<P: FinanceProvider> GetCategoriesTool<P> {
    pub fn new(service: crate::application::finance::FinanceService<P>) -> Self {
        Self { service }
    }
}

impl<P: FinanceProvider + Send + Sync> Tool for GetCategoriesTool<P> {
    const NAME: &'static str = "get_finance_categories";
    type Error = ToolError;
    type Args = GetCategoriesArgs;
    type Output = String;

    fn description(&self) -> String {
        "Fetches all budget categories from the financial provider".to_string()
    }

    fn parameters(&self) -> serde_json::Value {
        serde_json::json!({
            "type": "object",
            "properties": {},
            "required": []
        })
    }

    async fn call(&self, _args: Self::Args) -> Result<Self::Output, Self::Error> {
        match self.service.get_categories().await {
            Ok(categories) => {
                serde_json::to_string(&categories).map_err(|e| ToolError(e.to_string()))
            }
            Err(e) => Err(ToolError(e.to_string())),
        }
    }
}

#[derive(Deserialize, Serialize)]
pub struct GetTagsArgs {}

pub struct GetTagsTool<P: FinanceProvider> {
    service: crate::application::finance::FinanceService<P>,
}

impl<P: FinanceProvider> GetTagsTool<P> {
    pub fn new(service: crate::application::finance::FinanceService<P>) -> Self {
        Self { service }
    }
}

impl<P: FinanceProvider + Send + Sync> Tool for GetTagsTool<P> {
    const NAME: &'static str = "get_finance_tags";
    type Error = ToolError;
    type Args = GetTagsArgs;
    type Output = String;

    fn description(&self) -> String {
        "Fetches all budget tags from the financial provider".to_string()
    }

    fn parameters(&self) -> serde_json::Value {
        serde_json::json!({
            "type": "object",
            "properties": {},
            "required": []
        })
    }

    async fn call(&self, _args: Self::Args) -> Result<Self::Output, Self::Error> {
        match self.service.get_tags().await {
            Ok(tags) => serde_json::to_string(&tags).map_err(|e| ToolError(e.to_string())),
            Err(e) => Err(ToolError(e.to_string())),
        }
    }
}

#[derive(Deserialize, Serialize)]
pub struct GetUnreviewedTransactionsArgs {}

pub struct GetUnreviewedTransactionsTool<P: FinanceProvider> {
    service: crate::application::finance::FinanceService<P>,
}

impl<P: FinanceProvider> GetUnreviewedTransactionsTool<P> {
    pub fn new(service: crate::application::finance::FinanceService<P>) -> Self {
        Self { service }
    }
}

impl<P: FinanceProvider + Send + Sync> Tool for GetUnreviewedTransactionsTool<P> {
    const NAME: &'static str = "get_finance_unreviewed_transactions";
    type Error = ToolError;
    type Args = GetUnreviewedTransactionsArgs;
    type Output = String;

    fn description(&self) -> String {
        "Fetches unreviewed transactions from the financial provider".to_string()
    }

    fn parameters(&self) -> serde_json::Value {
        serde_json::json!({
            "type": "object",
            "properties": {},
            "required": []
        })
    }

    async fn call(&self, _args: Self::Args) -> Result<Self::Output, Self::Error> {
        match self.service.get_unreviewed_transactions().await {
            Ok(transactions) => serde_json::to_string(&transactions).map_err(|e| ToolError(e.to_string())),
            Err(e) => Err(ToolError(e.to_string())),
        }
    }
}
