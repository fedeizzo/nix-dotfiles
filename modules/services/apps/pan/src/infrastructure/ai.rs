use anyhow::Result;
use rig::agent::Agent;
use rig::client::CompletionClient;
use rig::memory::InMemoryConversationMemory;
use rig::prelude::Prompt;
use rig::providers::openai;
use rig::providers::openai::responses_api::GenericResponsesCompletionModel;

use crate::domain::chat::ChatProvider;

/// The Infrastructure Adapter that implements our `ChatProvider` port using `rig-core`
pub struct Rig {
    agent: Agent<GenericResponsesCompletionModel>,
}

impl Rig {
    /// Initializes the API client and builds the agent with injected tools
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
            .default_max_turns(5) // Allow the LLM to take up to 5 actions (e.g. call a tool, read the result, then answer!)
            .build();

        Ok(Self { agent })
    }
}

impl ChatProvider for Rig {
    #[doc = " Sends a prompt to the model and returns the response string."]
    async fn prompt(&self, conversation_id: &str, input: &str) -> Result<String> {
        Ok(self
            .agent
            .prompt(input)
            .conversation(conversation_id)
            .await?)
    }
}
