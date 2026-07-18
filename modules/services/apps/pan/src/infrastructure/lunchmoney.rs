use anyhow::Result;
use chrono::Utc;
use reqwest::{Client, StatusCode};
use serde::Deserialize;

use crate::domain::finance::{self, Account, FinanceProvider, User};

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

        // Plaid accounts use 'balance_last_updated', Manual accounts use 'balance_as_of'
        let date_str = value
            .balance_as_of
            .or(value.balance_last_updated)
            .unwrap_or_default();

        // The API returns either "YYYY-MM-DD" or a full RFC3339 datetime
        let parsed_balance_as_of = if date_str.len() == 10 {
            chrono::NaiveDate::parse_from_str(&date_str, "%Y-%m-%d").map_or_else(|_| Utc::now(), |d| d.and_hms_opt(0, 0, 0).unwrap().and_utc())
        } else if !date_str.is_empty() {
            chrono::DateTime::parse_from_rfc3339(&date_str).map_or_else(|_| Utc::now(), |d| d.with_timezone(&Utc))
        } else {
            Utc::now()
        };

        Ok(Account {
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
        let amount = value
            .amount
            .parse::<f64>()
            .map_err(|_| finance::Error::ParsingError(format!("Invalid amount {}", value.amount)))?;

        Ok(finance::Transaction {
            id: value.id,
            date: value.date,
            payee: value.payee,
            amount,
            currency: value.currency,
            to_base: value.to_base,
            category_id: value.category_id,
            tag_ids: value.tag_ids,
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

pub struct LunchmoneyClient {
    base_url: String,
    api_token: String,
    client: Client,
}

impl LunchmoneyClient {
    pub fn new(base_url: String, api_token: String) -> Self {
        Self {
            base_url,
            api_token,
            client: Client::new(),
        }
    }

    async fn get_accounts_by_type(&self, account_type: String) -> Result<Vec<Account>, finance::Error> {
        let url = format!("{}/v2/{}", self.base_url, account_type);
        let response = self
            .client
            .get(url)
            .bearer_auth(&self.api_token)
            .send()
            .await
            .map_err(|e| finance::Error::NetworkError(e.to_string()))?;
        match response.status() {
            StatusCode::UNAUTHORIZED => return Err(finance::Error::Unauthorized),
            StatusCode::TOO_MANY_REQUESTS => return Err(finance::Error::TooManyRequests),
            StatusCode::INTERNAL_SERVER_ERROR => return Err(finance::Error::Internal),
            _ => {}
        }

        let payload: serde_json::Value = response
            .json()
            .await
            .map_err(|e| finance::Error::ParsingError(e.to_string()))?;

        let accounts_array = payload
            .get(&account_type)
            .ok_or_else(|| finance::Error::ParsingError(format!("Missing key {account_type}")))?;

        let parsed_accounts: Vec<LunchmoneyAccount> =
            serde_json::from_value(accounts_array.clone())
                .map_err(|e| finance::Error::ParsingError(e.to_string()))?;

        let accounts: Result<Vec<Account>, finance::Error> = parsed_accounts
            .into_iter()
            .map(std::convert::TryInto::try_into)
            .collect();
        accounts
    }
}

impl FinanceProvider for LunchmoneyClient {
    #[doc = " Retrieves all active accounts and their current balances"]
    #[tracing::instrument(skip(self), err)]
    async fn get_accounts(&self) -> Result<Vec<Account>, finance::Error> {
        let mut manual_accounts = self.get_accounts_by_type("manual_accounts".into()).await?;
        let mut plaid_accounts = self.get_accounts_by_type("plaid_accounts".into()).await?;

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
        match response.status() {
            StatusCode::UNAUTHORIZED => return Err(finance::Error::Unauthorized),
            StatusCode::TOO_MANY_REQUESTS => return Err(finance::Error::TooManyRequests),
            StatusCode::INTERNAL_SERVER_ERROR => return Err(finance::Error::Internal),
            _ => {}
        }

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
        match response.status() {
            StatusCode::UNAUTHORIZED => return Err(finance::Error::Unauthorized),
            StatusCode::TOO_MANY_REQUESTS => return Err(finance::Error::TooManyRequests),
            StatusCode::INTERNAL_SERVER_ERROR => return Err(finance::Error::Internal),
            _ => {}
        }

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
        match response.status() {
            StatusCode::UNAUTHORIZED => return Err(finance::Error::Unauthorized),
            StatusCode::TOO_MANY_REQUESTS => return Err(finance::Error::TooManyRequests),
            StatusCode::INTERNAL_SERVER_ERROR => return Err(finance::Error::Internal),
            _ => {}
        }

        let payload = response
            .json::<TagsResponse>()
            .await
            .map_err(|e| finance::Error::ParsingError(e.to_string()))?;

        let tags: Result<Vec<finance::Tag>, finance::Error> =
            payload.tags.into_iter().map(std::convert::TryInto::try_into).collect();

        tags
    }

    #[tracing::instrument(skip(self), err)]
    async fn get_unreviewed_transactions(&self) -> Result<Vec<finance::Transaction>, finance::Error> {
        let url = format!("{}/v2/transactions?status=unreviewed", self.base_url);
        let response = self
            .client
            .get(url)
            .bearer_auth(&self.api_token)
            .send()
            .await
            .map_err(|e| finance::Error::NetworkError(e.to_string()))?;

        match response.status() {
            StatusCode::UNAUTHORIZED => return Err(finance::Error::Unauthorized),
            StatusCode::TOO_MANY_REQUESTS => return Err(finance::Error::TooManyRequests),
            StatusCode::INTERNAL_SERVER_ERROR => return Err(finance::Error::Internal),
            _ => {}
        }

        let payload = response
            .json::<TransactionsResponse>()
            .await
            .map_err(|e| finance::Error::ParsingError(e.to_string()))?;

        let transactions: Result<Vec<finance::Transaction>, finance::Error> =
            payload.transactions.into_iter().map(std::convert::TryInto::try_into).collect();

        transactions
    }
}

#[cfg(test)]
mod tests {
    use rstest::rstest;
    use serde_json::Value;
    use wiremock::{
        Mock, MockServer, ResponseTemplate,
        matchers::{header, method, path},
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
            // wiremock handles query parameters explicitly or we can ignore them if path match is enough
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
            assert_eq!(transactions[0].amount, 100.50);
        }
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
                        "name": "Cash",
                        "type": "cash",
                        "balance": "100.50",
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
                        "name": "Checking",
                        "type": "depository",
                        "balance": "1000.00",
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
}
