use std::{process::Stdio, string::FromUtf8Error, time::Duration};

use serde_json::Value;
use thiserror::Error;
use tokio::{
    io::{AsyncBufReadExt, AsyncReadExt, AsyncWriteExt, BufReader},
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

        // Correctness: We configured stdin/stdout/stderr with `Stdio::piped()`
        // above. Tokio populates these fields from the corresponding
        // `std::process::Child` fields when building `tokio::process::Child`:
        //
        // https://github.com/tokio-rs/tokio/blob/tokio-1.52.3/tokio/src/process/mod.rs#L948-L958
        //
        // The platform implementations take the captured stdio handles here:
        //
        // https://github.com/tokio-rs/tokio/blob/tokio-1.52.3/tokio/src/process/unix/mod.rs#L118-L122
        let stdin = child
            .stdin
            .take()
            .expect("bug: Agda process stdin should be piped");
        let stdout = child
            .stdout
            .take()
            .expect("bug: Agda process stdout should be piped");
        let stderr = child
            .stderr
            .take()
            .expect("bug: Agda process stderr should be piped");

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
        let command_line = format!("{command}\n");
        self.stdin.write_all(command_line.as_bytes()).await?;
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

        loop {
            let chunk = self.stdout.fill_buf().await?;
            if chunk.is_empty() {
                return Err(Error::EofBeforePrompt);
            }

            let length = chunk.len();
            output.extend_from_slice(chunk);
            self.stdout.consume(length);

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
