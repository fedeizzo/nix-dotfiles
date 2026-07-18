use anyhow::Result;
use chrono::{DateTime, Utc};
use serde::Serialize;

/// `Account` represents the financial account for lunchmoney
#[allow(clippy::struct_field_names)]
#[derive(Debug, PartialEq, Serialize)]
pub struct Account {
    pub name: String,
    pub institution_name: String,
    pub account_type: String,
    pub subtype: String,
    pub balance: f64,
    pub currency: String,
    pub balance_as_of: DateTime<Utc>,
    pub status: AccountStatus,
}

#[derive(Debug, PartialEq, Serialize)]
pub enum AccountStatus {
    Active,
    Closed,
}

impl From<&str> for AccountStatus {
    fn from(status: &str) -> Self {
        match status.to_lowercase().as_str() {
            "closed" => Self::Closed,
            _ => Self::Active, // Fallback to active for anything unrecognized
        }
    }
}

impl From<String> for AccountStatus {
    fn from(status: String) -> Self {
        Self::from(status.as_str())
    }
}

/// `User` is the authenticated user for the financial service
#[derive(Debug, PartialEq)]
pub struct User {
    pub name: String,
    pub budget_name: String,
    pub api_key_label: String,
    pub primary_currency: String,
}

/// `Category` represents a budget category
#[derive(Debug, PartialEq, Serialize)]
pub struct Category {
    pub id: i32,
    pub name: String,
    pub description: Option<String>,
    pub is_income: bool,
    pub archived: bool,
}

/// `Tag` represents a budget tag
#[derive(Debug, PartialEq, Serialize)]
pub struct Tag {
    pub id: i32,
    pub name: String,
    pub description: Option<String>,
    pub archived: bool,
}

/// `Transaction` represents a financial transaction
#[derive(Debug, PartialEq, Serialize)]
pub struct Transaction {
    pub id: i64,
    pub date: String,
    pub payee: String,
    pub amount: f64,
    pub currency: String,
    pub to_base: f64,
    pub category_id: Option<i32>,
    pub tag_ids: Vec<i32>,
    pub original_name: Option<String>,
    pub notes: Option<String>,
    pub source: Option<String>,
}

use std::future::Future;

/// The Domain Port for retrieving financial data
pub trait FinanceProvider: Send + Sync {
    /// Retrieves all active accounts and their current balances
    fn get_accounts(&self) -> impl Future<Output = Result<Vec<Account>, Error>> + Send;
    fn get_user(&self) -> impl Future<Output = Result<User, Error>> + Send;
    fn get_categories(&self) -> impl Future<Output = Result<Vec<Category>, Error>> + Send;
    fn get_tags(&self) -> impl Future<Output = Result<Vec<Tag>, Error>> + Send;
    fn get_unreviewed_transactions(&self) -> impl Future<Output = Result<Vec<Transaction>, Error>> + Send;
}

#[allow(clippy::enum_variant_names)]
#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("Failed to authenticate")]
    Unauthorized,
    #[error("Too many requests")]
    TooManyRequests,
    #[error("Couldn't parse the response: {0}")]
    ParsingError(String),
    #[error("Internal failure")]
    Internal,
    #[error("Network failure: {0}")]
    NetworkError(String),
}
