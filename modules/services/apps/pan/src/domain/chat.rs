use anyhow::Result;

/// The core Domain Port for chatting with an AI model.
/// In Hexagonal Architecture, our CLI and App logic only know about this Trait,
/// never about `rig` or `OpenAI` directly!
pub trait ChatProvider {
    /// Sends a prompt to the model and returns the response string.
    async fn prompt(&self, conversation_id: &str, input: &str) -> Result<String>;
}
