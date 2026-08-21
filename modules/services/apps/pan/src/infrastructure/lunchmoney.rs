use anyhow::Result;
use chrono::Utc;
use reqwest::{Client, StatusCode};
use serde::{Deserialize, Serialize};

use crate::domain::finance::{
    self, Account, AccountSource, FinanceProvider, RecurringItem, TransactionUpdate, User,
};

#[derive(Deserialize)]
struct LunchmoneyUser {
    name: String,
    budget_name: String,
    api_key_label: String,
    primary_currency: String,
}

impl TryFrom<LunchmoneyUser> for User {
    type Error = finance::Error;

    fn try_from(value: LunchmoneyUser) -> Result<Self, Self::Error> {
        Ok(User {
            name: value.name,
            budget_name: value.budget_name,
            api_key_label: value.api_key_label,
            primary_currency: value.primary_currency,
        })
    }
}

#[derive(Deserialize)]
struct LunchmoneyAccount {
    id: i64,
    name: String,
    institution_name: Option<String>,
    #[serde(rename = "type")]
    account_type: String,
    subtype: Option<String>,
    balance: String,
    currency: Option<String>,
    balance_as_of: Option<String>,
    balance_last_updated: Option<String>,
    status: String,
    to_base: Option<f64>,
}

impl TryFrom<LunchmoneyAccount> for Account {
    type Error = finance::Error;

    fn try_from(value: LunchmoneyAccount) -> Result<Self, Self::Error> {
        let balance = if let Some(converted) = value.to_base {
            converted
        } else {
            value
                .balance
                .parse()
                .map_err(|_| finance::Error::ParsingError("Invalid balance".into()))?
        };

        let date_str = value
            .balance_as_of
            .or(value.balance_last_updated)
            .ok_or_else(|| finance::Error::ParsingError("Missing balance date".into()))?;
        let parsed_balance_as_of = parse_balance_date(&date_str)?;

        Ok(Account {
            id: value.id,
            source: AccountSource::Manual,
            name: value.name,
            institution_name: value
                .institution_name
                .unwrap_or_else(|| "Lunchmoney".into()),
            account_type: value.account_type,
            subtype: value.subtype.unwrap_or_else(|| "Unknown".into()),
            balance,
            currency: value.currency.unwrap_or_else(|| "USD".into()),
            balance_as_of: parsed_balance_as_of,
            status: value.status.into(),
        })
    }
}

#[derive(Deserialize)]
struct LunchmoneyCategory {
    id: i32,
    name: String,
    description: Option<String>,
    is_income: bool,
    archived: bool,
}

impl TryFrom<LunchmoneyCategory> for finance::Category {
    type Error = finance::Error;

    fn try_from(value: LunchmoneyCategory) -> Result<Self, Self::Error> {
        Ok(finance::Category {
            id: value.id,
            name: value.name,
            description: value.description,
            is_income: value.is_income,
            archived: value.archived,
        })
    }
}

#[derive(Deserialize)]
struct CategoriesResponse {
    categories: Vec<LunchmoneyCategory>,
}

#[derive(Deserialize)]
struct LunchmoneyTag {
    id: i32,
    name: String,
    description: Option<String>,
    archived: bool,
}

impl TryFrom<LunchmoneyTag> for finance::Tag {
    type Error = finance::Error;

    fn try_from(value: LunchmoneyTag) -> Result<Self, Self::Error> {
        Ok(finance::Tag {
            id: value.id,
            name: value.name,
            description: value.description,
            archived: value.archived,
        })
    }
}

#[derive(Deserialize)]
struct TagsResponse {
    tags: Vec<LunchmoneyTag>,
}

#[derive(Deserialize)]
struct LunchmoneyTransaction {
    id: i64,
    date: String,
    payee: String,
    amount: String,
    currency: String,
    to_base: f64,
    category_id: Option<i32>,
    tag_ids: Vec<i32>,
    recurring_id: Option<i32>,
    plaid_account_id: Option<i64>,
    manual_account_id: Option<i64>,
    #[serde(default)]
    original_name: Option<String>,
    #[serde(default)]
    notes: Option<String>,
    #[serde(default)]
    source: Option<String>,
}

impl TryFrom<LunchmoneyTransaction> for finance::Transaction {
    type Error = finance::Error;

    fn try_from(value: LunchmoneyTransaction) -> Result<Self, Self::Error> {
        let amount = value.amount.parse::<f64>().map_err(|_| {
            finance::Error::ParsingError(format!("Invalid amount {}", value.amount))
        })?;

        Ok(finance::Transaction {
            id: value.id,
            date: value.date,
            payee: value.payee,
            amount,
            currency: value.currency,
            to_base: value.to_base,
            category_id: value.category_id,
            tag_ids: value.tag_ids,
            recurring_id: value.recurring_id,
            plaid_account_id: value.plaid_account_id,
            manual_account_id: value.manual_account_id,
            original_name: value.original_name,
            notes: value.notes,
            source: value.source,
        })
    }
}

#[derive(Deserialize)]
struct TransactionsResponse {
    transactions: Vec<LunchmoneyTransaction>,
}

#[derive(Deserialize)]
struct LunchmoneyRecurringItem {
    id: i32,
    description: String,
}

#[derive(Serialize)]
struct UpdateTransactionRequest<'a> {
    category_id: Option<i32>,
    tag_ids: &'a [i32],
    notes: Option<&'a str>,
    status: &'static str,
}

pub struct LunchmoneyClient {
    base_url: String,
    api_token: String,
    client: Client,
}

impl LunchmoneyClient {
    #[must_use]
    pub fn new(base_url: String, api_token: String) -> Self {
        Self {
            base_url,
            api_token,
            client: Client::new(),
        }
    }

    async fn get_accounts_by_type(
        &self,
        account_type: &str,
        source: AccountSource,
    ) -> Result<Vec<Account>, finance::Error> {
        let url = format!("{}/v2/{}", self.base_url, account_type);
        let response = self
            .client
            .get(url)
            .bearer_auth(&self.api_token)
            .send()
            .await
            .map_err(|e| finance::Error::NetworkError(e.to_string()))?;
        let response = check_status(response)?;

        let payload: serde_json::Value = response
            .json()
            .await
            .map_err(|e| finance::Error::ParsingError(e.to_string()))?;

        let accounts_array = payload
            .get(account_type)
            .ok_or_else(|| finance::Error::ParsingError(format!("Missing key {account_type}")))?;

        let parsed_accounts: Vec<LunchmoneyAccount> =
            serde_json::from_value(accounts_array.clone())
                .map_err(|e| finance::Error::ParsingError(e.to_string()))?;

        let accounts: Result<Vec<Account>, finance::Error> = parsed_accounts
            .into_iter()
            .map(|account| {
                let mut account: Account = account.try_into()?;
                account.source = source;
                Ok(account)
            })
            .collect();
        accounts
    }
}

impl FinanceProvider for LunchmoneyClient {
    #[doc = " Retrieves all active accounts and their current balances"]
    #[tracing::instrument(skip(self), err)]
    async fn get_accounts(&self) -> Result<Vec<Account>, finance::Error> {
        let (mut manual_accounts, mut plaid_accounts) = tokio::try_join!(
            self.get_accounts_by_type("manual_accounts", AccountSource::Manual),
            self.get_accounts_by_type("plaid_accounts", AccountSource::Plaid)
        )?;

        manual_accounts.append(&mut plaid_accounts);

        Ok(manual_accounts)
    }

    #[tracing::instrument(skip(self), err)]
    async fn get_user(&self) -> Result<User, finance::Error> {
        let url = format!("{}/v2/me", self.base_url);
        let response = self
            .client
            .get(url)
            .bearer_auth(&self.api_token)
            .send()
            .await
            .map_err(|e| finance::Error::NetworkError(e.to_string()))?;
        let response = check_status(response)?;

        let user = response
            .json::<LunchmoneyUser>()
            .await
            .map_err(|e| finance::Error::ParsingError(e.to_string()))?;

        user.try_into()
    }

    #[tracing::instrument(skip(self), err)]
    async fn get_categories(&self) -> Result<Vec<finance::Category>, finance::Error> {
        let url = format!("{}/v2/categories?format=flattened", self.base_url);
        let response = self
            .client
            .get(url)
            .bearer_auth(&self.api_token)
            .send()
            .await
            .map_err(|e| finance::Error::NetworkError(e.to_string()))?;
        let response = check_status(response)?;

        let payload = response
            .json::<CategoriesResponse>()
            .await
            .map_err(|e| finance::Error::ParsingError(e.to_string()))?;

        let categories: Result<Vec<finance::Category>, finance::Error> = payload
            .categories
            .into_iter()
            .map(std::convert::TryInto::try_into)
            .collect();

        categories
    }

    #[tracing::instrument(skip(self), err)]
    async fn get_tags(&self) -> Result<Vec<finance::Tag>, finance::Error> {
        let url = format!("{}/v2/tags", self.base_url);
        let response = self
            .client
            .get(url)
            .bearer_auth(&self.api_token)
            .send()
            .await
            .map_err(|e| finance::Error::NetworkError(e.to_string()))?;
        let response = check_status(response)?;

        let payload = response
            .json::<TagsResponse>()
            .await
            .map_err(|e| finance::Error::ParsingError(e.to_string()))?;

        let tags: Result<Vec<finance::Tag>, finance::Error> = payload
            .tags
            .into_iter()
            .map(std::convert::TryInto::try_into)
            .collect();

        tags
    }

    #[tracing::instrument(skip(self), err)]
    async fn get_unreviewed_transactions(
        &self,
    ) -> Result<Vec<finance::Transaction>, finance::Error> {
        let url = format!(
            "{}/v2/transactions?status=unreviewed&limit=100",
            self.base_url
        );
        let response = self
            .client
            .get(url)
            .bearer_auth(&self.api_token)
            .send()
            .await
            .map_err(|e| finance::Error::NetworkError(e.to_string()))?;

        let response = check_status(response)?;

        let payload = response
            .json::<TransactionsResponse>()
            .await
            .map_err(|e| finance::Error::ParsingError(e.to_string()))?;

        let transactions: Result<Vec<finance::Transaction>, finance::Error> = payload
            .transactions
            .into_iter()
            .map(std::convert::TryInto::try_into)
            .collect();

        transactions
    }

    #[tracing::instrument(skip(self), err)]
    async fn get_transaction(&self, id: i64) -> Result<finance::Transaction, finance::Error> {
        let url = format!("{}/v2/transactions/{id}", self.base_url);
        let response = self
            .client
            .get(url)
            .bearer_auth(&self.api_token)
            .send()
            .await
            .map_err(|error| finance::Error::NetworkError(error.to_string()))?;
        let response = check_status(response)?;
        response
            .json::<LunchmoneyTransaction>()
            .await
            .map_err(|error| finance::Error::ParsingError(error.to_string()))?
            .try_into()
    }

    #[tracing::instrument(skip(self), err)]
    async fn get_recurring_item(&self, id: i32) -> Result<RecurringItem, finance::Error> {
        let url = format!("{}/v2/recurring_items/{id}", self.base_url);
        let response = self
            .client
            .get(url)
            .bearer_auth(&self.api_token)
            .send()
            .await
            .map_err(|error| finance::Error::NetworkError(error.to_string()))?;
        let response = check_status(response)?;
        let item = response
            .json::<LunchmoneyRecurringItem>()
            .await
            .map_err(|error| finance::Error::ParsingError(error.to_string()))?;

        Ok(RecurringItem {
            id: item.id,
            description: item.description,
        })
    }

    #[tracing::instrument(skip(self, update), err)]
    async fn update_transaction(
        &self,
        id: i64,
        update: &TransactionUpdate,
        dry_run: bool,
    ) -> Result<(), finance::Error> {
        if dry_run {
            tracing::info!(
                transaction_id = id,
                "Skipping Lunch Money update in dry-run mode"
            );
            return Ok(());
        }
        let url = format!("{}/v2/transactions/{id}", self.base_url);
        let response = self
            .client
            .put(url)
            .bearer_auth(&self.api_token)
            .json(&UpdateTransactionRequest {
                category_id: update.category_id,
                tag_ids: &update.tag_ids,
                notes: update.notes.as_deref(),
                status: "reviewed",
            })
            .send()
            .await
            .map_err(|error| finance::Error::NetworkError(error.to_string()))?;
        check_status(response)?;

        Ok(())
    }
}

fn check_status(response: reqwest::Response) -> Result<reqwest::Response, finance::Error> {
    match response.status() {
        StatusCode::UNAUTHORIZED | StatusCode::FORBIDDEN => Err(finance::Error::Unauthorized),
        StatusCode::TOO_MANY_REQUESTS => Err(finance::Error::TooManyRequests),
        status if status.is_server_error() => Err(finance::Error::Internal),
        status if !status.is_success() => Err(finance::Error::HttpStatus(status.as_u16())),
        _ => Ok(response),
    }
}

fn parse_balance_date(value: &str) -> Result<chrono::DateTime<Utc>, finance::Error> {
    if value.len() == 10 {
        let date = chrono::NaiveDate::parse_from_str(value, "%Y-%m-%d")
            .map_err(|error| finance::Error::ParsingError(error.to_string()))?;
        return date
            .and_hms_opt(0, 0, 0)
            .map(|date_time| date_time.and_utc())
            .ok_or_else(|| finance::Error::ParsingError(format!("Invalid date {value}")));
    }

    chrono::DateTime::parse_from_rfc3339(value)
        .map(|date| date.with_timezone(&Utc))
        .map_err(|error| finance::Error::ParsingError(error.to_string()))
}

#[cfg(test)]
mod tests {
    use rstest::rstest;
    use serde_json::Value;
    use wiremock::{
        Mock, MockServer, ResponseTemplate,
        matchers::{body_json, header, method, path, query_param},
    };

    use super::*;

    #[rstest]
    #[case::successful(
        200,
        serde_json::json!({ "name": "mario", "budget_name": "name", "api_key_label": "api_key_name", "primary_currency": "eur" }),
        true
    )]
    #[case::unsuccessful(
        400,
        serde_json::json!({ "message": "Bad Request", "errors": [{ "errMsg": "Invalid token" }] }),
        false
    )]
    #[tokio::test]
    async fn test_get_user(
        #[case] status_code: u16,
        #[case] mocked_answer: Value,
        #[case] should_succeed: bool,
    ) {
        let mock_sever = MockServer::start().await;

        Mock::given(method("GET"))
            .and(path("/v2/me"))
            .and(header("Authorization", "Bearer 12345"))
            .respond_with(ResponseTemplate::new(status_code).set_body_json(mocked_answer))
            .mount(&mock_sever)
            .await;

        let client = LunchmoneyClient::new(mock_sever.uri(), "12345".into());

        let result = client.get_user().await;

        assert_eq!(result.is_ok(), should_succeed);

        if should_succeed {
            let expected_user = User {
                name: "mario".into(),
                budget_name: "name".into(),
                api_key_label: "api_key_name".into(),
                primary_currency: "eur".into(),
            };
            assert_eq!(result.unwrap(), expected_user);
        }
    }

    #[rstest]
    #[case::successful(
        200,
        serde_json::json!({
            "categories": [
                {
                    "id": 1,
                    "name": "Food",
                    "description": "Yummy stuff",
                    "is_income": false,
                    "archived": false
                }
            ]
        }),
        true
    )]
    #[tokio::test]
    async fn test_get_categories(
        #[case] status_code: u16,
        #[case] mocked_answer: Value,
        #[case] should_succeed: bool,
    ) {
        let mock_server = MockServer::start().await;

        Mock::given(method("GET"))
            .and(path("/v2/categories"))
            .and(header("Authorization", "Bearer 12345"))
            .respond_with(ResponseTemplate::new(status_code).set_body_json(mocked_answer))
            .mount(&mock_server)
            .await;

        let client = LunchmoneyClient::new(mock_server.uri(), "12345".into());

        let result = client.get_categories().await;

        assert_eq!(result.is_ok(), should_succeed);
        if should_succeed {
            let cats = result.unwrap();
            assert_eq!(cats.len(), 1);
            assert_eq!(cats[0].name, "Food");
        }
    }

    #[rstest]
    #[case::successful(
        200,
        serde_json::json!({
            "tags": [
                {
                    "id": 10,
                    "name": "Vacation",
                    "description": "Trip to Hawaii",
                    "archived": false
                }
            ]
        }),
        true
    )]
    #[tokio::test]
    async fn test_get_tags(
        #[case] status_code: u16,
        #[case] mocked_answer: Value,
        #[case] should_succeed: bool,
    ) {
        let mock_server = MockServer::start().await;

        Mock::given(method("GET"))
            .and(path("/v2/tags"))
            .and(header("Authorization", "Bearer 12345"))
            .respond_with(ResponseTemplate::new(status_code).set_body_json(mocked_answer))
            .mount(&mock_server)
            .await;

        let client = LunchmoneyClient::new(mock_server.uri(), "12345".into());

        let result = client.get_tags().await;

        assert_eq!(result.is_ok(), should_succeed);
        if should_succeed {
            let tags = result.unwrap();
            assert_eq!(tags.len(), 1);
            assert_eq!(tags[0].name, "Vacation");
        }
    }

    #[rstest]
    #[case::successful(
        200,
        serde_json::json!({
            "transactions": [
                {
                    "id": 123,
                    "date": "2026-07-19",
                    "amount": "100.50",
                    "currency": "USD",
                    "to_base": 100.50,
                    "payee": "Test Payee",
                    "original_name": "Test Payee INC",
                    "category_id": 5,
                    "tag_ids": [10],
                    "notes": "some notes",
                    "source": "plaid",
                    "status": "unreviewed"
                }
            ]
        }),
        true
    )]
    #[tokio::test]
    async fn test_get_unreviewed_transactions(
        #[case] status_code: u16,
        #[case] mocked_answer: Value,
        #[case] should_succeed: bool,
    ) {
        let mock_server = MockServer::start().await;

        Mock::given(method("GET"))
            .and(path("/v2/transactions"))
            .and(query_param("status", "unreviewed"))
            .and(query_param("limit", "100"))
            .and(header("Authorization", "Bearer 12345"))
            .respond_with(ResponseTemplate::new(status_code).set_body_json(mocked_answer))
            .mount(&mock_server)
            .await;

        let client = LunchmoneyClient::new(mock_server.uri(), "12345".into());

        let result = client.get_unreviewed_transactions().await;

        assert_eq!(result.is_ok(), should_succeed);
        if should_succeed {
            let transactions = result.unwrap();
            assert_eq!(transactions.len(), 1);
            assert_eq!(transactions[0].payee, "Test Payee");
            assert!((transactions[0].amount - 100.50).abs() < f64::EPSILON);
        }
    }

    #[tokio::test]
    async fn get_transaction_fetches_exact_id() {
        let mock_server = MockServer::start().await;
        Mock::given(method("GET"))
            .and(path("/v2/transactions/123"))
            .and(header("Authorization", "Bearer 12345"))
            .respond_with(ResponseTemplate::new(200).set_body_json(serde_json::json!({
                "id": 123,
                "date": "2026-07-19",
                "amount": "100.50",
                "currency": "USD",
                "to_base": 100.50,
                "payee": "Test Payee",
                "category_id": 5,
                "tag_ids": [10]
            })))
            .mount(&mock_server)
            .await;
        let client = LunchmoneyClient::new(mock_server.uri(), "12345".into());

        let transaction = client.get_transaction(123).await.unwrap();

        assert_eq!(transaction.id, 123);
        assert_eq!(transaction.payee, "Test Payee");
    }

    #[tokio::test]
    async fn test_get_accounts_success() {
        let mock_server = MockServer::start().await;

        // Mock manual accounts
        Mock::given(method("GET"))
            .and(path("/v2/manual_accounts"))
            .and(header("Authorization", "Bearer 12345"))
            .respond_with(ResponseTemplate::new(200).set_body_json(serde_json::json!({
                "manual_accounts": [
                    {
                        "id": 1,
                        "name": "Cash",
                        "type": "cash",
                        "balance": "100.50",
                        "balance_as_of": "2026-07-19",
                        "status": "active"
                    }
                ]
            })))
            .mount(&mock_server)
            .await;

        // Mock plaid accounts
        Mock::given(method("GET"))
            .and(path("/v2/plaid_accounts"))
            .and(header("Authorization", "Bearer 12345"))
            .respond_with(ResponseTemplate::new(200).set_body_json(serde_json::json!({
                "plaid_accounts": [
                    {
                        "id": 2,
                        "name": "Checking",
                        "type": "depository",
                        "balance": "1000.00",
                        "balance_last_updated": "2026-07-19T12:00:00Z",
                        "status": "active"
                    }
                ]
            })))
            .mount(&mock_server)
            .await;

        let client = LunchmoneyClient::new(mock_server.uri(), "12345".into());

        let result = client.get_accounts().await;

        assert!(result.is_ok());
        let accounts = result.unwrap();
        assert_eq!(accounts.len(), 2);
        assert_eq!(accounts[0].name, "Cash");
        assert_eq!(accounts[1].name, "Checking");
    }

    #[tokio::test]
    async fn update_transaction_sends_confirmed_review_fields() {
        let mock_server = MockServer::start().await;
        Mock::given(method("PUT"))
            .and(path("/v2/transactions/123"))
            .and(header("Authorization", "Bearer 12345"))
            .and(body_json(serde_json::json!({
                "category_id": 5,
                "tag_ids": [10, 11],
                "notes": "Dinner",
                "status": "reviewed"
            })))
            .respond_with(ResponseTemplate::new(201).set_body_json(serde_json::json!({})))
            .expect(1)
            .mount(&mock_server)
            .await;
        let client = LunchmoneyClient::new(mock_server.uri(), "12345".into());

        client
            .update_transaction(
                123,
                &TransactionUpdate {
                    category_id: Some(5),
                    tag_ids: vec![10, 11],
                    notes: Some("Dinner".to_owned()),
                },
                false,
            )
            .await
            .expect("update should succeed");
    }

    #[tokio::test]
    async fn update_transaction_dry_run_does_not_call_api() {
        let mock_server = MockServer::start().await;
        let client = LunchmoneyClient::new(mock_server.uri(), "12345".into());

        client
            .update_transaction(
                123,
                &TransactionUpdate {
                    category_id: None,
                    tag_ids: Vec::new(),
                    notes: None,
                },
                true,
            )
            .await
            .expect("dry-run should succeed");

        assert!(mock_server.received_requests().await.unwrap().is_empty());
    }
}
