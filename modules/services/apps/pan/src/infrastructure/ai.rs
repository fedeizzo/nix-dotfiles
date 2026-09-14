use std::time::Duration;

use anyhow::{Context, Result};
use rig::agent::Agent;
use rig::client::CompletionClient;
use rig::memory::InMemoryConversationMemory;
use rig::prelude::Prompt;
use rig::providers::openai;
use rig::providers::openai::responses_api::GenericResponsesCompletionModel;

use crate::domain::chat::ChatProvider;

pub const MAX_TOOL_TURNS: usize = 5;
pub const ASSISTANT_TIMEOUT: Duration = Duration::from_secs(30);

/// The general assistant may only use read-only provider tools.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ToolAccess {
    Read,
    Mutate,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AssistantRoute {
    Assistant,
    WorkflowConfirmationRequired,
}

#[must_use]
pub const fn assistant_allows(access: ToolAccess) -> bool {
    matches!(access, ToolAccess::Read)
}

#[must_use]
pub const fn route_tool_access(access: ToolAccess) -> AssistantRoute {
    if assistant_allows(access) {
        AssistantRoute::Assistant
    } else {
        AssistantRoute::WorkflowConfirmationRequired
    }
}

/// The Infrastructure Adapter that implements our `ChatProvider` port using `rig-core`
pub struct Rig {
    agent: Agent<GenericResponsesCompletionModel>,
}

impl Rig {
    /// Initializes the API client and builds the agent with injected tools
    ///
    /// # Errors
    ///
    /// Returns an error when the model client configuration is invalid.
    pub fn new(
        base_url: &str,
        api_key: &str,
        model: &str,
        tools: Vec<Box<dyn rig::tool::ToolDyn>>,
    ) -> Result<Self> {
        let client = openai::Client::builder()
            .base_url(base_url)
            .api_key(api_key)
            .build()?;

        let agent = client
            .agent(model)
            .tools(tools)
            .memory(InMemoryConversationMemory::new())
            .default_max_turns(MAX_TOOL_TURNS)
            .build();

        Ok(Self { agent })
    }
}

impl ChatProvider for Rig {
    #[doc = " Sends a prompt to the model and returns the response string."]
    async fn prompt(&self, conversation_id: &str, input: &str) -> Result<String> {
        within_timeout(async {
            Ok(self
                .agent
                .prompt(input)
                .conversation(conversation_id)
                .await?)
        })
        .await
    }
}

async fn within_timeout<T>(operation: impl Future<Output = Result<T>>) -> Result<T> {
    tokio::time::timeout(ASSISTANT_TIMEOUT, operation)
        .await
        .context("assistant request timed out")?
}

#[cfg(test)]
mod tests {
    use std::{future, time::Duration};

    use super::{
        ASSISTANT_TIMEOUT, AssistantRoute, MAX_TOOL_TURNS, ToolAccess, assistant_allows,
        route_tool_access, within_timeout,
    };

    #[test]
    fn assistant_contract_exposes_only_read_tools() {
        assert!(assistant_allows(ToolAccess::Read));
        assert!(!assistant_allows(ToolAccess::Mutate));
        assert_eq!(MAX_TOOL_TURNS, 5);
    }

    #[test]
    fn evaluation_routes_mutation_requests_to_confirmed_workflows() {
        assert_eq!(
            route_tool_access(ToolAccess::Read),
            AssistantRoute::Assistant
        );
        assert_eq!(
            route_tool_access(ToolAccess::Mutate),
            AssistantRoute::WorkflowConfirmationRequired
        );
    }

    #[tokio::test(start_paused = true)]
    async fn assistant_timeout_bounds_a_stalled_request() {
        let result = within_timeout(future::pending::<anyhow::Result<()>>()).await;

        assert!(result.is_err());
        assert_eq!(ASSISTANT_TIMEOUT, Duration::from_secs(30));
    }
}
