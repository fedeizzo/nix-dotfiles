use anyhow::{Context, Result};
use crate::domain::narrative::AnnotatedLine;
use super::engine::TtsEngine;
use reqwest::Client;
use serde_json::json;

pub struct OpenAiTtsEngine {
    endpoint: String,
    client: reqwest::Client,
    model: String,
    voices: std::collections::HashMap<String, String>,
    language: Option<String>,
}

impl OpenAiTtsEngine {
    pub fn new(base_url: &str, model: &str, voices: std::collections::HashMap<String, String>, language: Option<String>) -> Self {
        Self {
            endpoint: format!("{}/audio/speech", base_url.trim_end_matches('/')),
            client: reqwest::Client::new(),
            model: model.to_string(),
            voices,
            language,
        }
    }
}

impl TtsEngine for OpenAiTtsEngine {
    async fn generate_audio(&self, line: &AnnotatedLine) -> Result<Vec<u8>> {
        let default_voice = self.voices.get("narrator").cloned().unwrap_or_else(|| "narrator_ita".to_string());
        let final_voice = self.voices.get(&line.speaker.0).cloned().unwrap_or(default_voice);

        let mut payload = json!({
            "model": self.model,
            "input": line.text,
            "voice": final_voice
        });
        
        if let Some(lang) = &self.language {
            payload["language"] = json!(lang);
        }

        if line.emotion != crate::domain::narrative::Emotion::Inline && line.emotion != crate::domain::narrative::Emotion::Neutral {
            let emotion_str = format!("{:?}", line.emotion).to_lowercase();
            payload["instruct"] = json!(format!("Read with a {} tone.", emotion_str));
        }

        let mut retries = 0;
        let max_retries = 3;
        loop {
            match self.client.post(&self.endpoint).json(&payload).send().await {
                Ok(response) => {
                    if response.status().is_success() {
                        let audio_bytes = response.bytes().await.context("Failed to read audio payload")?;
                        return Ok(audio_bytes.to_vec());
                    } else {
                        let status = response.status();
                        let error_text = response.text().await.unwrap_or_default();
                        if retries >= max_retries {
                            return Err(anyhow::anyhow!("TTS API error {}: {}", status, error_text));
                        }
                        crate::log_info!("  [Retry {}/{}] TTS API error {}: {}", retries + 1, max_retries, status, error_text);
                    }
                },
                Err(e) => {
                    if retries >= max_retries {
                        return Err(anyhow::anyhow!("TTS request failed: {}", e));
                    }
                    crate::log_info!("  [Retry {}/{}] TTS request failed: {}", retries + 1, max_retries, e);
                }
            }
            retries += 1;
            tokio::time::sleep(tokio::time::Duration::from_secs(2u64.pow(retries as u32))).await;
        }
    }
}
