use std::{process::Stdio, string::FromUtf8Error, time::Duration};

use serde_json::Value;
use thiserror::Error;
use tokio::{
    io::{AsyncReadExt, AsyncWriteExt, BufReader},
    process::{self, Child, ChildStdin, ChildStdout},
    task::JoinHandle,
    time::timeout,
};

use crate::agda::command;

const PROMPT: &str = "JSON> ";

const PROMPT_TIMEOUT: Duration = Duration::from_secs(30);
const AGDA_ARGUMENTS: &[&str] = &["-v0", "--interaction-json", "--color=never"];

// TODO: Investigate whether any Agda CLI flags should be runtime configurable
// for the MCP server. Agda declares command-line options in
// `Agda.Interaction.Options.Base`:
// https://github.com/agda/agda/blob/3b57742a311b3a90b755737968d437f1ef902318/src/full/Agda/Interaction/Options/Base.hs#L1348-L1370

#[derive(Debug)]
pub struct AgdaProcess {
    child: Child,
    stdin: ChildStdin,
    stdout: BufReader<ChildStdout>,
    stderr_task: JoinHandle<()>,
}

impl AgdaProcess {
    pub async fn spawn() -> Result<Self> {
        let program = std::env::var("AGDA_BIN").unwrap_or_else(|_| "agda".to_owned());
        let mut child = process::Command::new(&program)
            .args(AGDA_ARGUMENTS)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .map_err(|source| Error::Spawn {
                program: program.clone(),
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
        timeout(PROMPT_TIMEOUT, self.read_prompt_output_without_timeout())
            .await
            .map_err(|_| Error::PromptTimeout(PROMPT_TIMEOUT))?
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
            if output.ends_with(PROMPT.as_bytes()) {
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

fn parse_json_responses(output: &str) -> Result<Vec<Value>> {
    output
        .lines()
        .filter_map(|line| {
            let line = line.strip_prefix(PROMPT).unwrap_or(line).trim();
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
