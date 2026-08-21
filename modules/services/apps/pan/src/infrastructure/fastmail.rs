use std::collections::HashMap;

use reqwest::{Client, StatusCode};
use serde::Deserialize;
use serde_json::{Value, json};

use crate::domain::email::{self, Email, EmailProvider, Mailbox};

const CORE_CAPABILITY: &str = "urn:ietf:params:jmap:core";
const MAIL_CAPABILITY: &str = "urn:ietf:params:jmap:mail";

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct Session {
    api_url: String,
    primary_accounts: HashMap<String, String>,
}

#[derive(Debug, Deserialize)]
struct JmapResponse {
    #[serde(rename = "methodResponses")]
    method_responses: Vec<(String, Value, String)>,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct JmapMailbox {
    id: String,
    name: String,
    total_emails: u64,
    unread_emails: u64,
}

#[derive(Debug, Deserialize)]
struct GetMailboxesResponse {
    list: Vec<JmapMailbox>,
}

#[derive(Debug, Deserialize)]
struct QueryEmailResponse {
    ids: Vec<String>,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct JmapEmail {
    id: String,
    subject: String,
    #[serde(default)]
    from: Vec<EmailAddress>,
    #[serde(default)]
    to: Vec<EmailAddress>,
    preview: String,
}

#[derive(Debug, Deserialize)]
struct EmailAddress {
    name: Option<String>,
    email: String,
}

#[derive(Debug, Deserialize)]
struct GetEmailResponse {
    list: Vec<JmapEmail>,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct SetEmailResponse {
    #[serde(default)]
    not_updated: HashMap<String, SetError>,
}

#[derive(Debug, Deserialize)]
struct SetError {
    #[serde(rename = "type")]
    error_type: String,
    description: Option<String>,
}

pub struct FastmailClient {
    client: Client,
    api_token: String,
    api_url: String,
    account_id: String,
}

impl FastmailClient {
    /// Discovers the Fastmail JMAP API and primary mail account.
    ///
    /// # Errors
    ///
    /// Returns an error on authentication, network, HTTP, or malformed session responses.
    pub async fn connect(session_url: &str, api_token: String) -> Result<Self, email::Error> {
        let client = Client::new();
        let response = client
            .get(session_url)
            .bearer_auth(&api_token)
            .send()
            .await
            .map_err(|error| network_error(&error))?;
        let response = check_status(response)?;
        let session = response.json::<Session>().await.map_err(invalid_response)?;
        let account_id = session
            .primary_accounts
            .get(MAIL_CAPABILITY)
            .cloned()
            .ok_or_else(|| {
                email::Error::InvalidResponse("missing primary mail account".to_owned())
            })?;

        Ok(Self {
            client,
            api_token,
            api_url: session.api_url,
            account_id,
        })
    }

    async fn call(&self, method: &str, arguments: Value) -> Result<Value, email::Error> {
        let request = json!({
            "using": [CORE_CAPABILITY, MAIL_CAPABILITY],
            "methodCalls": [[method, arguments, "pan-0"]],
        });
        let response = self
            .client
            .post(&self.api_url)
            .bearer_auth(&self.api_token)
            .json(&request)
            .send()
            .await
            .map_err(|error| network_error(&error))?;
        let response = check_status(response)?;
        let mut response = response
            .json::<JmapResponse>()
            .await
            .map_err(invalid_response)?;
        let (response_method, arguments, _) = response.method_responses.pop().ok_or_else(|| {
            email::Error::InvalidResponse("missing JMAP method response".to_owned())
        })?;

        if response_method == "error" {
            return Err(email::Error::InvalidResponse(arguments.to_string()));
        }
        if response_method != method {
            return Err(email::Error::InvalidResponse(format!(
                "expected {method}, received {response_method}"
            )));
        }

        Ok(arguments)
    }

    async fn fetch_email(&self, email_id: &str) -> Result<Email, email::Error> {
        let response = self
            .call(
                "Email/get",
                json!({
                    "accountId": self.account_id,
                    "ids": [email_id],
                    "properties": ["id", "subject", "from", "to", "preview"],
                }),
            )
            .await?;
        let mut response: GetEmailResponse =
            serde_json::from_value(response).map_err(invalid_response)?;
        let email = response.list.pop().ok_or_else(|| {
            email::Error::InvalidResponse(format!("email {email_id} was not returned"))
        })?;

        Ok(Email {
            id: email.id,
            subject: email.subject,
            from: format_addresses(&email.from),
            to: format_addresses(&email.to),
            preview: email.preview,
        })
    }

    async fn update_email(
        &self,
        email_id: &str,
        patch: Value,
        dry_run: bool,
    ) -> Result<(), email::Error> {
        if dry_run {
            tracing::info!(email_id, "Skipping Fastmail mutation in dry-run mode");
            return Ok(());
        }

        let response = self
            .call(
                "Email/set",
                json!({
                    "accountId": self.account_id,
                    "update": { (email_id): patch },
                }),
            )
            .await?;
        let response: SetEmailResponse =
            serde_json::from_value(response).map_err(invalid_response)?;

        if let Some(error) = response.not_updated.get(email_id) {
            let description = error.description.as_deref().unwrap_or("no description");
            return Err(email::Error::MutationRejected(format!(
                "{}: {description}",
                error.error_type
            )));
        }

        Ok(())
    }
}

impl EmailProvider for FastmailClient {
    #[tracing::instrument(skip(self), err)]
    async fn get_mailboxes(&self) -> Result<Vec<Mailbox>, email::Error> {
        let response = self
            .call("Mailbox/get", json!({ "accountId": self.account_id }))
            .await?;
        let response: GetMailboxesResponse =
            serde_json::from_value(response).map_err(invalid_response)?;

        Ok(response
            .list
            .into_iter()
            .map(|mailbox| Mailbox {
                id: mailbox.id,
                name: mailbox.name,
                total_emails: mailbox.total_emails,
                unread_emails: mailbox.unread_emails,
            })
            .collect())
    }

    #[tracing::instrument(skip(self), err)]
    async fn get_unread_email(&self, mailbox_id: &str) -> Result<Option<Email>, email::Error> {
        let response = self
            .call(
                "Email/query",
                json!({
                    "accountId": self.account_id,
                    "filter": { "inMailbox": mailbox_id, "notKeyword": "$seen" },
                    "limit": 1,
                }),
            )
            .await?;
        let response: QueryEmailResponse =
            serde_json::from_value(response).map_err(invalid_response)?;
        let Some(email_id) = response.ids.first() else {
            return Ok(None);
        };

        self.fetch_email(email_id).await.map(Some)
    }

    #[tracing::instrument(skip(self), err)]
    async fn get_email(&self, email_id: &str) -> Result<Email, email::Error> {
        self.fetch_email(email_id).await
    }

    #[tracing::instrument(skip(self), err)]
    async fn add_mailbox_to_email(
        &self,
        email_id: &str,
        mailbox_id: &str,
        dry_run: bool,
    ) -> Result<(), email::Error> {
        self.update_email(
            email_id,
            json!({ (format!("mailboxIds/{mailbox_id}")): true }),
            dry_run,
        )
        .await
    }

    #[tracing::instrument(skip(self), err)]
    async fn mark_email_as_seen(&self, email_id: &str, dry_run: bool) -> Result<(), email::Error> {
        self.update_email(email_id, json!({ "keywords/$seen": true }), dry_run)
            .await
    }
}

fn format_addresses(addresses: &[EmailAddress]) -> String {
    addresses
        .iter()
        .map(
            |address| match address.name.as_deref().filter(|name| !name.is_empty()) {
                Some(name) => format!("{name} <{}>", address.email),
                None => address.email.clone(),
            },
        )
        .collect::<Vec<_>>()
        .join(", ")
}

fn check_status(response: reqwest::Response) -> Result<reqwest::Response, email::Error> {
    match response.status() {
        StatusCode::UNAUTHORIZED | StatusCode::FORBIDDEN => Err(email::Error::Unauthorized),
        StatusCode::TOO_MANY_REQUESTS => Err(email::Error::TooManyRequests),
        status if !status.is_success() => Err(email::Error::HttpStatus(status.as_u16())),
        _ => Ok(response),
    }
}

fn network_error(error: &reqwest::Error) -> email::Error {
    email::Error::Network(error.to_string())
}

fn invalid_response(error: impl std::fmt::Display) -> email::Error {
    email::Error::InvalidResponse(error.to_string())
}

#[cfg(test)]
mod tests {
    use serde_json::json;
    use wiremock::{
        Mock, MockServer, ResponseTemplate,
        matchers::{body_partial_json, header, method, path},
    };

    use super::*;

    async fn client(server: &MockServer) -> FastmailClient {
        Mock::given(method("GET"))
            .and(path("/jmap/session"))
            .and(header("authorization", "Bearer secret"))
            .respond_with(ResponseTemplate::new(200).set_body_json(json!({
                "apiUrl": format!("{}/jmap/api", server.uri()),
                "primaryAccounts": { MAIL_CAPABILITY: "account-1" }
            })))
            .mount(server)
            .await;

        FastmailClient::connect(
            &format!("{}/jmap/session", server.uri()),
            "secret".to_owned(),
        )
        .await
        .expect("test session should connect")
    }

    #[tokio::test]
    async fn get_mailboxes_maps_jmap_response() {
        let server = MockServer::start().await;
        let client = client(&server).await;
        Mock::given(method("POST"))
            .and(path("/jmap/api"))
            .and(body_partial_json(json!({
                "methodCalls": [["Mailbox/get", { "accountId": "account-1" }, "pan-0"]]
            })))
            .respond_with(ResponseTemplate::new(200).set_body_json(json!({
                "methodResponses": [["Mailbox/get", { "list": [{
                    "id": "inbox", "name": "Inbox", "totalEmails": 5, "unreadEmails": 2
                }] }, "pan-0"]]
            })))
            .mount(&server)
            .await;

        let mailboxes = client
            .get_mailboxes()
            .await
            .expect("mailboxes should parse");

        assert_eq!(
            mailboxes,
            vec![Mailbox {
                id: "inbox".to_owned(),
                name: "Inbox".to_owned(),
                total_emails: 5,
                unread_emails: 2,
            }]
        );
    }

    #[tokio::test]
    async fn get_unread_email_queries_then_fetches_message() {
        let server = MockServer::start().await;
        let client = client(&server).await;
        Mock::given(method("POST"))
            .and(path("/jmap/api"))
            .and(body_partial_json(json!({
                "methodCalls": [["Email/query", {
                    "filter": { "inMailbox": "inbox", "notKeyword": "$seen" },
                    "limit": 1
                }, "pan-0"]]
            })))
            .respond_with(ResponseTemplate::new(200).set_body_json(json!({
                "methodResponses": [["Email/query", { "ids": ["email-1"] }, "pan-0"]]
            })))
            .mount(&server)
            .await;
        Mock::given(method("POST"))
            .and(path("/jmap/api"))
            .and(body_partial_json(json!({
                "methodCalls": [["Email/get", { "ids": ["email-1"] }, "pan-0"]]
            })))
            .respond_with(ResponseTemplate::new(200).set_body_json(json!({
                "methodResponses": [["Email/get", { "list": [{
                    "id": "email-1",
                    "subject": "Hello",
                    "from": [{ "name": "Alice", "email": "alice@example.com" }],
                    "to": [{ "email": "pan@example.com" }],
                    "preview": "A short preview"
                }] }, "pan-0"]]
            })))
            .mount(&server)
            .await;

        let email = client
            .get_unread_email("inbox")
            .await
            .expect("email should parse");

        assert_eq!(
            email,
            Some(Email {
                id: "email-1".to_owned(),
                subject: "Hello".to_owned(),
                from: "Alice <alice@example.com>".to_owned(),
                to: "pan@example.com".to_owned(),
                preview: "A short preview".to_owned(),
            })
        );
    }

    #[tokio::test]
    async fn get_email_fetches_exact_id() {
        let server = MockServer::start().await;
        let client = client(&server).await;
        Mock::given(method("POST"))
            .and(path("/jmap/api"))
            .and(body_partial_json(json!({
                "methodCalls": [["Email/get", { "ids": ["email-1"] }, "pan-0"]]
            })))
            .respond_with(ResponseTemplate::new(200).set_body_json(json!({
                "methodResponses": [["Email/get", { "list": [{
                    "id": "email-1",
                    "subject": "Hello",
                    "from": [{ "email": "alice@example.com" }],
                    "to": [{ "email": "pan@example.com" }],
                    "preview": "A short preview"
                }] }, "pan-0"]]
            })))
            .mount(&server)
            .await;

        let email = client.get_email("email-1").await.unwrap();

        assert_eq!(email.id, "email-1");
        assert_eq!(email.subject, "Hello");
    }

    #[tokio::test]
    async fn mark_seen_dry_run_does_not_call_jmap_api() {
        let server = MockServer::start().await;
        let client = client(&server).await;

        client
            .mark_email_as_seen("email-1", true)
            .await
            .expect("dry run should succeed");

        let requests = server
            .received_requests()
            .await
            .expect("request history should be available");
        assert_eq!(requests.len(), 1);
    }

    #[tokio::test]
    async fn mark_seen_reports_jmap_set_error() {
        let server = MockServer::start().await;
        let client = client(&server).await;
        Mock::given(method("POST"))
            .and(path("/jmap/api"))
            .respond_with(ResponseTemplate::new(200).set_body_json(json!({
                "methodResponses": [["Email/set", {
                    "notUpdated": { "email-1": {
                        "type": "forbidden", "description": "read only"
                    }}
                }, "pan-0"]]
            })))
            .mount(&server)
            .await;

        let error = client
            .mark_email_as_seen("email-1", false)
            .await
            .expect_err("rejected mutation should fail");

        assert!(matches!(error, email::Error::MutationRejected(_)));
    }
}
