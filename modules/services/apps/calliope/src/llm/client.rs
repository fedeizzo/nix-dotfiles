use anyhow::{Context, Result};
use reqwest::Client;
use reqwest::header::{HeaderMap, HeaderValue};
use serde_json::json;
use serde::{Deserialize, Serialize};
use crate::domain::narrative::AnnotatedLine;

/// The entire response payload expected from the LLM after analyzing a context window
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LlmChunkResponse {
    pub lines: Vec<AnnotatedLine>,
}

pub struct LlmClient {
    client: Client,
    endpoint: String,
    model: String,
    is_inline: bool,
    language: String,
}

impl LlmClient {
    pub fn new(base_url: &str, api_key: &str, model: &str, is_inline: bool, language: &str) -> Self {
        let mut headers = HeaderMap::new();
        let auth_value = HeaderValue::from_str(&format!("Bearer {}", api_key))
            .expect("Invalid API key format");
        headers.insert(reqwest::header::AUTHORIZATION, auth_value);
        
        let client = reqwest::Client::builder()
            .default_headers(headers)
            .build()
            .expect("Failed to build HTTP client");
            
        Self {
            endpoint: format!("{}/chat/completions", base_url.trim_end_matches('/')),
            client,
            model: model.to_string(),
            is_inline,
            language: language.to_string(),
        }
    }

    /// Sends a chunk of text (with preceding context) to the LLM to get structured
    /// JSON containing the sentence-by-sentence speaker and emotion annotations.
    pub async fn annotate_chunk(&self, context: &str, new_text: &str) -> Result<LlmChunkResponse> {
        let system_prompt = if self.is_inline {
            include_str!("annotate_inline.md")
        } else {
            include_str!("annotate.md")
        };
        
        let user_prompt = format!(
            "[LANGUAGE: {}]\n\n[CONTEXT - DO NOT ANNOTATE]\n{}\n[END CONTEXT]\n\n[NEW TEXT - ANNOTATE THIS]\n{}\n[END NEW TEXT]",
            self.language,
            context,
            new_text
        );

        let body = json!({
            "model": self.model,
            "messages": [
                { "role": "system", "content": system_prompt },
                { "role": "user", "content": user_prompt }
            ],
            // Force the local LLM to return valid JSON
            "response_format": { "type": "json_object" }
        });

        let mut retries = 0;
        let max_retries = 3;
        
        let content_str = loop {
            match self.client.post(&self.endpoint).json(&body).send().await {
                Ok(response) => {
                    if response.status().is_success() {
                        if let Ok(res) = response.json::<serde_json::Value>().await {
                            break res["choices"][0]["message"]["content"].as_str().unwrap_or("{}").to_string();
                        } else {
                            if retries >= max_retries {
                                return Err(anyhow::anyhow!("Failed to parse LLM JSON response body"));
                            }
                            println!("  [Retry {}/{}] Failed to parse LLM JSON response body", retries + 1, max_retries);
                        }
                    } else {
                        let status = response.status();
                        let text = response.text().await.unwrap_or_default();
                        if retries >= max_retries {
                            return Err(anyhow::anyhow!("LLM API error {}: {}", status, text));
                        }
                        println!("  [Retry {}/{}] LLM API error {}: {}", retries + 1, max_retries, status, text);
                    }
                },
                Err(e) => {
                    if retries >= max_retries {
                        return Err(anyhow::anyhow!("LLM request failed: {}", e));
                    }
                    println!("  [Retry {}/{}] LLM request failed: {}", retries + 1, max_retries, e);
                }
            }
            retries += 1;
            tokio::time::sleep(tokio::time::Duration::from_secs(2u64.pow(retries as u32))).await;
        };
        
        let parsed: LlmChunkResponse = serde_json::from_str(&content_str)
            .context("Failed to parse LLM JSON response into AnnotatedLine array")?;
            
        Ok(parsed)
    }
}
