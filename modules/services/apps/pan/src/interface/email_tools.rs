use std::sync::Arc;

use rig::tool::Tool;
use serde::{Deserialize, Serialize};

use crate::domain::email::EmailProvider;

#[derive(Debug, thiserror::Error)]
#[error("Fastmail tool failed: {0}")]
pub struct EmailToolError(String);

#[derive(Deserialize, Serialize)]
pub struct GetMailboxesArgs {}

pub struct GetMailboxesTool<P> {
    provider: Arc<P>,
}

impl<P> GetMailboxesTool<P> {
    #[must_use]
    pub fn new(provider: Arc<P>) -> Self {
        Self { provider }
    }
}

impl<P: EmailProvider + 'static> Tool for GetMailboxesTool<P> {
    const NAME: &'static str = "get_email_mailboxes";
    type Error = EmailToolError;
    type Args = GetMailboxesArgs;
    type Output = String;

    fn description(&self) -> String {
        "Lists Fastmail mailboxes and their total and unread message counts".to_owned()
    }

    fn parameters(&self) -> serde_json::Value {
        serde_json::json!({
            "type": "object",
            "properties": {},
            "required": []
        })
    }

    async fn call(&self, _args: Self::Args) -> Result<Self::Output, Self::Error> {
        let mailboxes = self
            .provider
            .get_mailboxes()
            .await
            .map_err(|error| EmailToolError(error.to_string()))?;
        serde_json::to_string(&mailboxes).map_err(|error| EmailToolError(error.to_string()))
    }
}

#[derive(Deserialize, Serialize)]
pub struct GetUnreadEmailArgs {
    pub mailbox: String,
}

pub struct GetUnreadEmailTool<P> {
    provider: Arc<P>,
}

impl<P> GetUnreadEmailTool<P> {
    #[must_use]
    pub fn new(provider: Arc<P>) -> Self {
        Self { provider }
    }
}

impl<P: EmailProvider + 'static> Tool for GetUnreadEmailTool<P> {
    const NAME: &'static str = "get_unread_email";
    type Error = EmailToolError;
    type Args = GetUnreadEmailArgs;
    type Output = String;

    fn description(&self) -> String {
        "Fetches an unread email from a named Fastmail mailbox".to_owned()
    }

    fn parameters(&self) -> serde_json::Value {
        serde_json::json!({
            "type": "object",
            "properties": {
                "mailbox": {
                    "type": "string",
                    "description": "Mailbox name, matched case-insensitively"
                }
            },
            "required": ["mailbox"]
        })
    }

    async fn call(&self, args: Self::Args) -> Result<Self::Output, Self::Error> {
        let mailboxes = self
            .provider
            .get_mailboxes()
            .await
            .map_err(|error| EmailToolError(error.to_string()))?;
        let mailbox = mailboxes
            .iter()
            .find(|mailbox| mailbox.name.eq_ignore_ascii_case(args.mailbox.trim()))
            .ok_or_else(|| EmailToolError(format!("mailbox {:?} was not found", args.mailbox)))?;
        let email = self
            .provider
            .get_unread_email(&mailbox.id)
            .await
            .map_err(|error| EmailToolError(error.to_string()))?;

        serde_json::to_string(&email).map_err(|error| EmailToolError(error.to_string()))
    }
}
