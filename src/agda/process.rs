use std::{process::Stdio, string::FromUtf8Error, time::Duration};

use serde_json::Value;
use thiserror::Error;
use tokio::{
    io::{AsyncReadExt, AsyncWriteExt, BufReader},
    process::{Child, ChildStdin, ChildStdout, Command as TokioCommand},
    task::JoinHandle,
    time::timeout,
};

use crate::agda::command;

const PROMPT: &[u8] = b"JSON> ";
const PROMPT_STR: &str = "JSON> ";

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Error)]
pub enum Error {
    #[error("failed to spawn Agda process `{program}`: {source}")]
    Spawn {
        program: String,
        #[source]
        source: std::io::Error,
    },

    #[error("Agda process did not provide a piped {stream}")]
    MissingPipe { stream: &'static str },

    #[error("I/O error while communicating with Agda: {0}")]
    Io(#[from] std::io::Error),

    #[error("Agda process closed stdout before the next prompt")]
    EofBeforePrompt,

    #[error("timed out waiting for Agda prompt after {0:?}")]
    PromptTimeout(Duration),

    #[error("Agda output was not UTF-8: {0}")]
    Utf8(#[from] FromUtf8Error),

    #[error("failed to parse Agda JSON response line `{line}`: {source}")]
    JsonLine {
        line: String,
        #[source]
        source: serde_json::Error,
    },
}

#[derive(Debug, Clone)]
pub struct AgdaConfig {
    pub program: String,
    pub args: Vec<String>,
    pub prompt_timeout: Duration,
}

impl AgdaConfig {
    pub fn from_env() -> Self {
        Self {
            program: std::env::var("AGDA_BIN").unwrap_or_else(|_| "agda".to_owned()),
            ..Self::default()
        }
    }
}

impl Default for AgdaConfig {
    fn default() -> Self {
        Self {
            program: "agda".to_owned(),
            args: vec![
                "-v0".to_owned(),
                "--interaction-json".to_owned(),
                "--color=never".to_owned(),
            ],
            prompt_timeout: Duration::from_secs(30),
        }
    }
}

#[derive(Debug)]
pub struct AgdaProcess {
    child: Child,
    stdin: ChildStdin,
    stdout: BufReader<ChildStdout>,
    stderr_task: JoinHandle<()>,
    prompt_timeout: Duration,
}

impl AgdaProcess {
    pub async fn spawn(config: AgdaConfig) -> Result<Self> {
        let mut child = TokioCommand::new(&config.program)
            .args(&config.args)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .map_err(|source| Error::Spawn {
                program: config.program.clone(),
                source,
            })?;

        let stdin = child
            .stdin
            .take()
            .ok_or(Error::MissingPipe { stream: "stdin" })?;
        let stdout = child
            .stdout
            .take()
            .ok_or(Error::MissingPipe { stream: "stdout" })?;
        let stderr = child
            .stderr
            .take()
            .ok_or(Error::MissingPipe { stream: "stderr" })?;

        let stderr_task = tokio::spawn(async move {
            let mut stderr = BufReader::new(stderr);
            let mut buffer = Vec::new();
            let _ = stderr.read_to_end(&mut buffer).await;
        });

        let mut process = Self {
            child,
            stdin,
            stdout: BufReader::new(stdout),
            stderr_task,
            prompt_timeout: config.prompt_timeout,
        };
        process.read_prompt_output().await?;
        Ok(process)
    }

    pub async fn send_command(&mut self, command: &command::Command<'_>) -> Result<Vec<Value>> {
        self.send_raw_command_line(&command.to_string()).await
    }

    pub async fn send_raw_command_line(&mut self, line: &str) -> Result<Vec<Value>> {
        self.stdin.write_all(line.as_bytes()).await?;
        self.stdin.write_all(b"\n").await?;
        self.stdin.flush().await?;

        let output = self.read_prompt_output().await?;
        parse_json_responses(&output)
    }

    async fn read_prompt_output(&mut self) -> Result<String> {
        timeout(
            self.prompt_timeout,
            self.read_prompt_output_without_timeout(),
        )
        .await
        .map_err(|_| Error::PromptTimeout(self.prompt_timeout))?
    }

    async fn read_prompt_output_without_timeout(&mut self) -> Result<String> {
        let mut output = Vec::new();
        let mut byte = [0];

        loop {
            let bytes_read = self.stdout.read(&mut byte).await?;
            if bytes_read == 0 {
                return Err(Error::EofBeforePrompt);
            }

            output.push(byte[0]);
            if output.ends_with(PROMPT) {
                output.truncate(output.len() - PROMPT.len());
                return String::from_utf8(output).map_err(Error::Utf8);
            }
        }
    }
}

impl Drop for AgdaProcess {
    fn drop(&mut self) {
        let _ = self.child.start_kill();
        self.stderr_task.abort();
    }
}

fn parse_json_responses(output: &str) -> Result<Vec<Value>> {
    output
        .lines()
        .filter_map(|line| {
            let line = line.strip_prefix(PROMPT_STR).unwrap_or(line).trim();
            (!line.is_empty()).then_some(line)
        })
        .map(|line| {
            serde_json::from_str(line).map_err(|source| Error::JsonLine {
                line: line.to_owned(),
                source,
            })
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_prompt_prefixed_json_lines() {
        let responses =
            parse_json_responses("JSON> {\"kind\":\"Status\"}\n{\"kind\":\"ClearRunningInfo\"}\n")
                .unwrap();

        assert_eq!(responses.len(), 2);
        assert_eq!(responses[0]["kind"], "Status");
        assert_eq!(responses[1]["kind"], "ClearRunningInfo");
    }
}
