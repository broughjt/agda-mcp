use std::{process::Stdio, string::FromUtf8Error, time::Duration};

use thiserror::Error;
use tokio::{
    io::{AsyncBufReadExt, AsyncReadExt, AsyncWriteExt, BufReader},
    process::{self, Child, ChildStdin, ChildStdout},
    task::JoinHandle,
    time::timeout,
};

use crate::agda::{
    command,
    response::{self, ParseError, Response},
};

pub(crate) const PROMPT: &str = "JSON> ";

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

    pub async fn send(&mut self, command: &command::Command<'_>) -> Result<Vec<Response>> {
        let command_line = format!("{command}\n");
        self.stdin.write_all(command_line.as_bytes()).await?;
        self.stdin.flush().await?;

        let payload = self.read_prompt_output().await?;
        match response::parse_all(&payload) {
            Ok(responses) => Ok(responses),
            Err(source) => {
                // Strict parsing failed. Re-parse the same payload as raw JSON
                // values for diagnostics. If even that fails, the payload
                // string is still attached. Either way, the protocol is out of
                // sync with our types. The caller is expected to treat this as
                // fatal for the session.
                let payload_values = response::parse_raw_values(&payload).ok();
                Err(Error::Parse(Box::new(ParseFailure {
                    source,
                    payload,
                    payload_values,
                })))
            }
        }
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

    #[error("failed to parse Agda response: {0}")]
    Parse(Box<ParseFailure>),
}

impl Error {
    /// Whether this error indicates the wire-level protocol is out of sync
    /// with our parser. Callers should treat protocol-fatal errors as
    /// terminal for the session.
    pub fn is_protocol_fatal(&self) -> bool {
        matches!(self, Self::Parse(_) | Self::Utf8(_) | Self::EofBeforePrompt)
    }
}

/// Rich diagnostic captured when strict response parsing fails.
///
/// `source` is the strict-parse error (line index, raw line, underlying
/// `serde_json::Error`). `payload` is the full prompt-delimited output we
/// read from Agda. `payload_values` is a best-effort fallback parse of the
/// same payload into untyped JSON values; `None` means even the loose parse
/// failed (a line wasn't valid JSON at all), in which case `payload`
/// remains the authoritative diagnostic.
#[derive(Debug)]
pub struct ParseFailure {
    pub source: ParseError,
    pub payload: String,
    pub payload_values: Option<Vec<serde_json::Value>>,
}

impl std::fmt::Display for ParseFailure {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "{}", self.source)
    }
}

impl std::error::Error for ParseFailure {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        Some(&self.source)
    }
}
