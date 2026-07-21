use anyhow::Result;
use crate::domain::narrative::AnnotatedLine;
/// Core trait for all Text-to-Speech backends.
pub trait TtsEngine {
    /// Takes an annotated line (speaker, text, emotion) and returns the raw audio bytes (e.g., WAV format).
    async fn generate_audio(&self, line: &AnnotatedLine) -> Result<Vec<u8>>;
}


