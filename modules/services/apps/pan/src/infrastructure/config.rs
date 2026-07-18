use std::{
    fs::{self},
    process::Command,
};

use anyhow::{Result, bail};
use serde::{Deserialize, Serialize};
use thiserror::Error;

#[derive(Debug, Deserialize, Serialize)]
pub struct AppConfig {
    pub models: ModelsConfig,
    pub fastmail: FastMailConfig,
    pub lunchmoney: LunchmoneyConfig,
    pub fusion: Fusion,
    pub interface: InterfaceConfig,
    pub cli: CliConfig,
    pub matrix: Option<MatrixConfig>,
    pub log: LogConfig,
    pub telemetry: TelemetryConfig,
    pub hindsight: HindsightConfig,
    #[serde(default)]
    pub jobs: Vec<JobConfig>,
}

impl AppConfig {
    /// Validates cross-field dependencies and business rules
    pub fn validate(&self) -> Result<()> {
        self.validate_interface()?;
        self.validate_fastmail()?;
        self.validate_lunchmoney()?;
        self.validate_fusion()?;
        self.validate_matrix()?;

        Ok(())
    }

    fn validate_interface(&self) -> Result<()> {
        if self.interface.interface_type == InterfaceType::Matrix && self.matrix.is_none() {
            bail!("Interface type is Matrix, but the matrix configuration block is missing");
        }
        Ok(())
    }

    fn validate_fastmail(&self) -> Result<()> {
        match (&self.fastmail.api_file, &self.fastmail.api_cmd) {
            (Some(_), Some(_)) => {
                bail!("api_file and api_cmd are mutually exclusive in fastmail");
            }
            (None, None) => {
                bail!("you must provide either api_file or api_cmd in fastmail");
            }
            _ => {}
        }

        if let Some(path) = &self.fastmail.api_file {
            match fs::exists(path) {
                Ok(true) => {}
                Ok(false) => bail!("The specified fastmail api_file does not exist"),
                Err(_) => {
                    bail!("Could not check if fastmail api_file exists (permission error)");
                }
            }
        }

        Ok(())
    }

    fn validate_lunchmoney(&self) -> Result<()> {
        match (&self.lunchmoney.api_file, &self.lunchmoney.api_cmd) {
            (Some(_), Some(_)) => {
                bail!("api_file and api_cmd are mutually exclusive in lunchmoney")
            }
            (None, None) => bail!("you must provide either api_file or api_cmd in lunchmoney"),
            _ => Ok(()),
        }
    }

    fn validate_fusion(&self) -> Result<()> {
        match (&self.fusion.password_file, &self.fusion.password_cmd) {
            (Some(_), Some(_)) => {
                bail!("password_file and password_cmd are mutually exclusive in fusion")
            }
            (None, None) => {
                bail!("you must provide either password_file or password_cmd in fusion")
            }
            _ => Ok(()),
        }
    }

    fn validate_matrix(&self) -> Result<()> {
        // Matrix is optional, so we only validate it if it exists
        if let Some(matrix) = &self.matrix {
            match (&matrix.password_file, &matrix.password_cmd) {
                (Some(_), Some(_)) => {
                    bail!("password_file and password_cmd are mutually exclusive in matrix");
                }
                (None, None) => {
                    bail!("you must provide either password_file or password_cmd in matrix");
                }
                _ => {}
            }
        }
        Ok(())
    }
}

#[derive(Debug, Deserialize, Serialize)]
pub struct ModelsConfig {
    pub name: String,
    pub openai_api_key: String,
    pub openai_base_url: String,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct LunchmoneyConfig {
    pub api_file: Option<String>,
    pub api_cmd: Option<String>,
}

impl LunchmoneyConfig {
    pub fn get_api_key(&self) -> Result<String> {
        match (&self.api_file, &self.api_cmd) {
            (None, Some(cmd)) => {
                // We use 'sh -c' so that users can pass complex commands with arguments
                let output = Command::new("sh").arg("-c").arg(cmd).output()?;
                let api_key = String::from_utf8(output.stdout)?;
                Ok(api_key.trim().to_string())
            }
            (Some(path), None) => {
                let api_key = fs::read_to_string(path)?;
                Ok(api_key.trim().to_string())
            }
            _ => bail!("Neither api_file nor api_cmd specified"),
        }
    }
}

#[derive(Debug, Deserialize, Serialize)]
pub struct FastMailConfig {
    pub api_file: Option<String>,
    pub api_cmd: Option<String>,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct Fusion {
    pub endpoint: String,
    pub password_file: Option<String>,
    pub password_cmd: Option<String>,
}

#[derive(Debug, Deserialize, Serialize, PartialEq)]
#[serde(rename_all = "lowercase")]
pub enum InterfaceType {
    Matrix,
    Cli,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct InterfaceConfig {
    #[serde(rename = "type")]
    pub interface_type: InterfaceType,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct CliConfig {
    pub conversation_path: String,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct MatrixConfig {
    pub homeserver: String,
    pub user: String,
    pub password_file: Option<String>,
    pub password_cmd: Option<String>,
    pub allowed_user: String,
    pub allowed_room: String,
    pub data_dir: String,
    pub notification_room: String,
    pub message_retention: Option<String>,
}

#[derive(Debug, Deserialize, Serialize, PartialEq)]
#[serde(rename_all = "lowercase")]
pub enum LogLevel {
    Trace,
    Debug,
    Info,
    Warn,
    Error,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct LogConfig {
    pub path: String,
    pub level: LogLevel,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct TelemetryConfig {
    // Port could be u16 in Rust, but yaml has it as a string "8080". We can deserialize it as a String or directly as a u16 if the YAML parser allows string-to-int conversion or if we change the YAML to an int. Let's stick to String for safety.
    pub port: String,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct HindsightConfig {
    pub url: String,
    pub api_key: String,
    pub bank_id: String,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct JobConfig {
    pub name: String,
    pub spec: String,
    pub condition: String,
    pub runner: String,
    pub prompt: String,
}

/// Represents any errors that can occur during configuration loading.
#[derive(Debug, Error)]
pub enum Error {
    #[error("I/O error: {0}")]
    IoError(#[from] std::io::Error),
    #[error("YAML parsing error: {0}")]
    YamlError(#[from] serde_yaml::Error),
}

pub fn load_config(path: &str) -> Result<AppConfig, Error> {
    let config = fs::read_to_string(path)?;
    Ok(serde_yaml::from_str::<AppConfig>(config.as_str())?)
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    // A helper function that returns a valid configuration struct filled with dummy data
    fn base_config() -> AppConfig {
        AppConfig {
            models: ModelsConfig {
                name: "test".into(),
                openai_api_key: "test".into(),
                openai_base_url: "http://test".into(),
            },
            fastmail: FastMailConfig {
                api_file: Some("Cargo.toml".into()),
                api_cmd: None,
            },
            lunchmoney: LunchmoneyConfig {
                api_file: Some("Cargo.toml".into()),
                api_cmd: None,
            },
            fusion: Fusion {
                endpoint: "http://test".into(),
                password_file: Some("Cargo.toml".into()),
                password_cmd: None,
            },
            interface: InterfaceConfig {
                interface_type: InterfaceType::Cli,
            },
            cli: CliConfig {
                conversation_path: "test.json".into(),
            },
            matrix: Some(MatrixConfig {
                homeserver: "https://matrix.org".to_string(),
                user: "@pan_agent:matrix.org".to_string(),
                // We use Cargo.toml here again to pass any potential fs::exists checks!
                password_file: Some("Cargo.toml".to_string()),
                password_cmd: None,
                allowed_user: "@my_user:matrix.org".to_string(),
                allowed_room: "!my_room:matrix.org".to_string(),
                data_dir: "./data".to_string(),
                notification_room: "!notif_room:matrix.org".to_string(),
                message_retention: None,
            }),
            log: LogConfig {
                path: "test.log".into(),
                level: LogLevel::Info,
            },
            telemetry: TelemetryConfig {
                port: "8080".into(),
            },
            hindsight: HindsightConfig {
                url: "http://test".into(),
                api_key: "test".into(),
                bank_id: "test".into(),
            },
            jobs: vec![],
        }
    }

    #[rstest]
    #[case::cli_without_matrix(InterfaceType::Cli, None, true)]
    #[case::matrix_with_config(InterfaceType::Matrix, Some(()), true)]
    #[case::matrix_missing_config_fails(InterfaceType::Matrix, None, false)]
    fn test_interface_validation(
        #[case] interface_type: InterfaceType,
        #[case] matrix: Option<()>,
        #[case] expected_to_pass: bool,
    ) {
        let mut config = base_config();
        config.interface.interface_type = interface_type;
        if matrix.is_none() {
            config.matrix = None;
        }

        assert_eq!(config.validate_interface().is_ok(), expected_to_pass);
    }

    #[rstest]
    #[case::only_api_file(Some("Cargo.toml".to_string()), None, true)]
    #[case::only_api_cmd(None, Some("echo key".to_string()), true)]
    #[case::both_provided_fails(Some("Cargo.toml".to_string()), Some("echo key".to_string()), false)]
    #[case::missing_both_fails(None, None, false)]
    fn test_fastmail_validation(
        #[case] api_file: Option<String>,
        #[case] api_cmd: Option<String>,
        #[case] expected_to_pass: bool,
    ) {
        let mut config = base_config();
        config.fastmail.api_file = api_file;
        config.fastmail.api_cmd = api_cmd;

        assert_eq!(config.validate_fastmail().is_ok(), expected_to_pass);
    }

    #[rstest]
    #[case::only_api_file(Some("Cargo.toml".to_string()), None, true)]
    #[case::only_api_cmd(None, Some("echo key".to_string()), true)]
    #[case::both_provided_fails(Some("Cargo.toml".to_string()), Some("echo key".to_string()), false)]
    #[case::missing_both_fails(None, None, false)]
    fn test_lunchmoney_validation(
        #[case] api_file: Option<String>,
        #[case] api_cmd: Option<String>,
        #[case] expected_to_pass: bool,
    ) {
        let mut config = base_config();
        config.lunchmoney.api_file = api_file;
        config.lunchmoney.api_cmd = api_cmd;

        assert_eq!(config.validate_lunchmoney().is_ok(), expected_to_pass);
    }

    #[rstest]
    #[case::only_password_file(Some("Cargo.toml".to_string()), None, true)]
    #[case::only_password_cmd(None, Some("echo key".to_string()), true)]
    #[case::both_provided_fails(Some("Cargo.toml".to_string()), Some("echo key".to_string()), false)]
    #[case::missing_both_fails(None, None, false)]
    fn test_fusion_validation(
        #[case] password_file: Option<String>,
        #[case] password_cmd: Option<String>,
        #[case] expected_to_pass: bool,
    ) {
        let mut config = base_config();
        config.fusion.password_file = password_file;
        config.fusion.password_cmd = password_cmd;

        assert_eq!(config.validate_fusion().is_ok(), expected_to_pass);
    }

    #[rstest]
    #[case::only_password_file(Some("Cargo.toml".to_string()), None, true)]
    #[case::only_password_cmd(None, Some("echo key".to_string()), true)]
    #[case::both_provided_fails(Some("Cargo.toml".to_string()), Some("echo key".to_string()), false)]
    #[case::missing_both_fails(None, None, false)]
    fn test_matrix_validation(
        #[case] password_file: Option<String>,
        #[case] password_cmd: Option<String>,
        #[case] expected_to_pass: bool,
    ) {
        let mut config = base_config();
        let mut matrix = config.matrix.unwrap();
        matrix.password_file = password_file;
        matrix.password_cmd = password_cmd;
        config.matrix = Some(matrix);

        assert_eq!(config.validate_matrix().is_ok(), expected_to_pass);
    }
}
