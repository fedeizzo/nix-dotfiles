use std::{path::Path, process::Stdio};

use serde_json::{Value, json};
use tokio::{
    io::{AsyncReadExt, AsyncWriteExt, BufReader},
    process::Command,
};

use crate::{
    domain::development::{DevelopmentExecutor, DevelopmentJob, Error, Result},
    infrastructure::development_worker::{WorkerConfig, read_record},
};

pub struct PiExecutor {
    config: WorkerConfig,
}

impl PiExecutor {
    #[must_use]
    pub const fn new(config: WorkerConfig) -> Self {
        Self { config }
    }

    fn command(&self, workspace: &Path, fetch: bool) -> Command {
        let mut command = Command::new(if fetch {
            &self.config.fetch_program
        } else {
            &self.config.sandbox_program
        });
        command
            .arg(workspace)
            .env_clear()
            .env("PATH", &self.config.path)
            .env("LANG", "C.UTF-8")
            .kill_on_drop(true)
            .stdin(Stdio::null())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped());
        command
    }

    async fn tool(
        &self,
        workspace: &Path,
        fetch: bool,
        program: &Path,
        args: &[&str],
    ) -> Result<String> {
        let mut command = self.command(workspace, fetch);
        command.arg(program).args(args);
        let mut child = command.spawn().map_err(execution)?;
        let stdout = child
            .stdout
            .take()
            .ok_or_else(|| execution("missing tool stdout"))?;
        let stderr = child
            .stderr
            .take()
            .ok_or_else(|| execution("missing tool stderr"))?;
        let output = async {
            let mut bytes = Vec::new();
            stdout
                .take(4_194_305)
                .read_to_end(&mut bytes)
                .await
                .map_err(execution)?;
            if bytes.len() > 4_194_304 {
                return Err(execution("review artifact exceeds 4 MiB"));
            }
            Ok(bytes)
        };
        let drain = async {
            tokio::io::copy(&mut BufReader::new(stderr), &mut tokio::io::sink())
                .await
                .map_err(execution)
        };
        let (bytes, _, status) = tokio::try_join!(output, drain, async {
            child.wait().await.map_err(execution)
        })?;
        if !status.success() {
            return Err(execution(format!("development tool exited with {status}")));
        }
        String::from_utf8(bytes).map_err(execution)
    }

    async fn prompt(&self, workspace: &Path, instruction: &str) -> Result<String> {
        let mut command = self.command(workspace, false);
        command
            .arg(&self.config.pi_program)
            .args([
                "--mode",
                "rpc",
                "--no-session",
                "--offline",
                "--no-approve",
                "--no-extensions",
                "--no-mcp",
                "--no-skills",
                "--no-prompt-templates",
                "--no-themes",
                "--no-context-files",
                "--provider",
                "pan-local",
                "--model",
                "qwen-flash",
                "--thinking",
                "medium",
                "--tools",
                "read,bash,edit,write,grep,find,ls",
                "--append-system-prompt",
                "/config/instructions.md",
            ])
            .stdin(Stdio::piped());
        let mut child = command.spawn().map_err(execution)?;
        let mut stdin = child
            .stdin
            .take()
            .ok_or_else(|| execution("missing Pi stdin"))?;
        let stdout = child
            .stdout
            .take()
            .ok_or_else(|| execution("missing Pi stdout"))?;
        let stderr = child
            .stderr
            .take()
            .ok_or_else(|| execution("missing Pi stderr"))?;
        let protocol = async {
            let mut bytes = serde_json::to_vec(
                &json!({"id": "job-prompt", "type": "prompt", "message": instruction}),
            )
            .map_err(execution)?;
            bytes.push(b'\n');
            stdin.write_all(&bytes).await.map_err(execution)?;
            let result = consume_protocol(&mut BufReader::new(stdout)).await?;
            drop(stdin);
            Ok::<_, Error>(result)
        };
        // Always drain diagnostics, but do not retain source content or possible credentials.
        let drain = async {
            tokio::io::copy(&mut BufReader::new(stderr), &mut tokio::io::sink())
                .await
                .map_err(execution)
        };
        let (result, _, status) = tokio::try_join!(protocol, drain, async {
            child.wait().await.map_err(execution)
        })?;
        if !status.success() {
            return Err(execution(format!("Pi exited with {status}")));
        }
        Ok(result)
    }
}

impl DevelopmentExecutor for PiExecutor {
    async fn execute(&self, job: &DevelopmentJob, directory: &Path) -> Result<String> {
        let repository = self
            .config
            .repositories
            .get(&job.request.repository)
            .ok_or_else(|| execution("repository no longer allowlisted"))?;
        tokio::fs::create_dir(directory).await.map_err(execution)?;
        let workspace = directory.join("workspace");
        tokio::fs::create_dir(&workspace).await.map_err(execution)?;
        self.tool(
            &workspace,
            true,
            &self.config.git_program,
            &[
                "-c",
                "core.hooksPath=/dev/null",
                "-c",
                "init.templateDir=",
                "clone",
                "--depth",
                "1",
                "--single-branch",
                "--branch",
                &repository.branch,
                "--",
                &repository.url,
                "/workspace",
            ],
        )
        .await?;
        self.tool(
            &workspace,
            false,
            &self.config.jj_program,
            &["git", "init", "--colocate", "/workspace"],
        )
        .await?;
        let summary = self.prompt(&workspace, &format!(
            "Implement this request in /workspace. Read the repository AGENTS.md instructions before editing. Do not publish, push, merge, or deploy. Run relevant available checks and report their commands, results, and limitations.\n\n{}", job.request.instruction
        )).await?;
        // Capture only the working-copy delta; publication and signing are unavailable.
        let patch = self
            .tool(
                &workspace,
                false,
                &self.config.jj_program,
                &["diff", "--git", "--color", "never"],
            )
            .await?;
        tokio::fs::write(directory.join("changes.patch"), patch)
            .await
            .map_err(execution)?;
        tokio::fs::write(directory.join("summary.md"), &summary)
            .await
            .map_err(execution)?;
        Ok(summary)
    }
}

fn execution(error: impl std::fmt::Display) -> Error {
    Error::Execution(error.to_string())
}

async fn consume_protocol(reader: &mut (impl tokio::io::AsyncBufRead + Unpin)) -> Result<String> {
    let mut accepted = false;
    let mut settled = false;
    let mut summary = String::new();
    let mut failed = false;
    loop {
        let line = read_record(reader).await.map_err(execution)?;
        let record: Value =
            serde_json::from_str(&line).map_err(|_| execution("malformed Pi JSON record"))?;
        match record["type"].as_str() {
            Some("response") if record["id"] == "job-prompt" => {
                if record["command"] != "prompt" || record["success"] != true {
                    return Err(execution("Pi rejected the prompt"));
                }
                if record["data"]["disposition"] == "handled" {
                    return Err(execution(
                        "Pi handled the prompt without starting a coding run",
                    ));
                }
                accepted = true;
            }
            Some("message_end") if record["message"]["role"] == "assistant" => {
                let message = &record["message"];
                failed = matches!(message["stopReason"].as_str(), Some("error" | "aborted"));
                summary.clear();
                if let Some(content) = message["content"].as_array() {
                    for block in content {
                        if block["type"] == "text"
                            && let Some(text) = block["text"].as_str()
                        {
                            summary.extend(text.chars().take(12_000 - summary.chars().count()));
                        }
                    }
                }
            }
            Some("agent_settled") => settled = true,
            Some("extension_ui_request") => {
                return Err(execution(
                    "Pi requested an unsupported extension interaction",
                ));
            }
            Some(_) => {}
            None => return Err(execution("Pi record has no type")),
        }
        if accepted && settled {
            if failed {
                return Err(execution("Pi model run failed or was aborted"));
            }
            if summary.is_empty() {
                return Err(execution("Pi finished without a review summary"));
            }
            return Ok(summary);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fmt::Write;

    #[tokio::test]
    async fn acceptance_and_agent_end_are_not_completion() {
        let stream = concat!(
            "{\"type\":\"response\",\"id\":\"job-prompt\",\"command\":\"prompt\",\"success\":true}\n",
            "{\"type\":\"agent_end\"}\n"
        );
        assert!(
            consume_protocol(&mut BufReader::new(stream.as_bytes()))
                .await
                .is_err()
        );
    }

    #[tokio::test]
    async fn completion_requires_settled_and_reports_model_errors() {
        for (stop, succeeds) in [("stop", true), ("error", false), ("aborted", false)] {
            let records = [
                json!({"type":"response","id":"unrelated","success":false}),
                json!({"type":"response","id":"job-prompt","command":"prompt","success":true,"data":{"disposition":"started"}}),
                json!({"type":"message_end","message":{"role":"assistant","stopReason":stop,"content":[{"type":"text","text":"Checks passed."}]}}),
                json!({"type":"agent_end"}),
                json!({"type":"agent_settled"}),
            ];
            let mut stream = String::new();
            for record in records {
                writeln!(stream, "{record}").unwrap();
            }
            assert_eq!(
                consume_protocol(&mut BufReader::new(stream.as_bytes()))
                    .await
                    .is_ok(),
                succeeds
            );
        }
    }
}
