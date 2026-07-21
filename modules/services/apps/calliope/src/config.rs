use anyhow::{Context, Result};
use serde::Deserialize;
use std::path::Path;

/// Root configuration object mapped from a TOML file
#[derive(Debug, Deserialize, Default)]
pub struct CalliopeConfig {
    #[serde(default)]
    pub llm: LlmConfig,
    
    #[serde(default)]
    pub tts: TtsConfig,
}

#[derive(Debug, Deserialize, Default)]
pub struct LlmConfig {
    pub base_url: Option<String>,
    pub model: Option<String>,
    pub api_key: Option<String>,
    pub emotion_strategy: Option<String>,
    pub language: Option<String>,
}

#[derive(Debug, Deserialize, Default)]
pub struct TtsConfig {
    pub engine: Option<String>,
    pub base_url: Option<String>,
    pub model: Option<String>,
    pub language: Option<String>,
    pub voices: Option<std::collections::HashMap<String, String>>,
}

impl CalliopeConfig {
    /// Loads a TOML configuration from the specified path.
    /// If the file does not exist, returns a default (empty) configuration.
    pub fn load(path: &Path) -> Result<Self> {
        if path.exists() {
            let content = std::fs::read_to_string(path)
                .context(format!("Failed to read config file at {:?}", path))?;
            let config: CalliopeConfig = toml::from_str(&content)
                .context("Failed to parse TOML configuration")?;
            Ok(config)
        } else {
            // Return an empty config if no file exists
            Ok(Self::default())
        }
    }
}
