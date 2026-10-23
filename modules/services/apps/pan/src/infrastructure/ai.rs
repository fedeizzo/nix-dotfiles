use std::time::Duration;

use anyhow::{Context, Result};
use rig::agent::Agent;
use rig::client::CompletionClient;
use rig::memory::InMemoryConversationMemory;
use rig::prelude::Prompt;
use rig::providers::openai;
use rig::providers::openai::completion::CompletionModel;

use crate::domain::chat::ChatProvider;

pub const MAX_TOOL_TURNS: usize = 5;

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
    agent: Agent<CompletionModel>,
    timeout: Duration,
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
        timeout: Duration,
        tools: Vec<Box<dyn rig::tool::ToolDyn>>,
    ) -> Result<Self> {
        let client = openai::Client::builder()
            .base_url(base_url)
            .api_key(api_key)
            .build()?
            .completions_api();

        let agent = client
            .agent(model)
            .tools(tools)
            .memory(InMemoryConversationMemory::new())
            .default_max_turns(MAX_TOOL_TURNS)
            .build();

        Ok(Self { agent, timeout })
    }
}

impl ChatProvider for Rig {
    #[doc = " Sends a prompt to the model and returns the response string."]
    async fn prompt(&self, conversation_id: &str, input: &str) -> Result<String> {
        within_timeout(self.timeout, async {
            Ok(self
                .agent
                .prompt(input)
                .conversation(conversation_id)
                .await?)
        })
        .await
    }
}

async fn within_timeout<T>(
    timeout: Duration,
    operation: impl Future<Output = Result<T>>,
) -> Result<T> {
    tokio::time::timeout(timeout, operation)
        .await
        .context("assistant request timed out")?
}

#[cfg(test)]
mod tests {
    #[tokio::test]
    async fn local_model_uses_chat_completions_with_the_configured_model() {
        use crate::domain::chat::ChatProvider;
        use wiremock::{
            Mock, MockServer, ResponseTemplate,
            matchers::{body_partial_json, method, path},
        };
        let server = MockServer::start().await;
        Mock::given(method("POST"))
            .and(path("/v1/chat/completions"))
            .and(body_partial_json(serde_json::json!({"model": "qwen-flash"})))
            .respond_with(ResponseTemplate::new(200).set_body_json(serde_json::json!({
                "id": "local-test", "object": "chat.completion", "created": 1, "model": "qwen-flash",
                "choices": [{"index": 0, "message": {"role": "assistant", "content": "Flash ready"}, "finish_reason": "stop"}],
                "usage": {"prompt_tokens": 1, "completion_tokens": 2, "total_tokens": 3}
            }))).expect(1).mount(&server).await;
        let rig = super::Rig::new(
            &format!("{}/v1", server.uri()),
            "placeholder",
            "qwen-flash",
            Duration::from_secs(5),
            vec![],
        )
        .unwrap();
        assert_eq!(rig.prompt("test", "hello").await.unwrap(), "Flash ready");
    }
    use std::{future, time::Duration};

    use super::{
        AssistantRoute, MAX_TOOL_TURNS, ToolAccess, assistant_allows, route_tool_access,
        within_timeout,
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
    async fn custom_timeout_allows_requests_longer_than_the_old_default() {
        let result = within_timeout(Duration::from_mins(3), async {
            tokio::time::sleep(Duration::from_mins(1)).await;
            Ok(42)
        })
        .await
        .unwrap();
        assert_eq!(result, 42);
    }

    #[tokio::test(start_paused = true)]
    async fn assistant_timeout_bounds_a_stalled_request() {
        let timeout = Duration::from_mins(3);
        let start = tokio::time::Instant::now();
        let result = within_timeout(timeout, future::pending::<anyhow::Result<()>>()).await;

        assert!(result.is_err());
        assert_eq!(start.elapsed(), timeout);
    }
}
