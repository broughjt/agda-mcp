//! State shared between MCP tool calls.
//!
//! The MCP transport wraps this state in a `Mutex` and routes tool calls
//! through it. Keeping the load/give logic here (rather than in `main.rs`)
//! also lets integration tests drive the same code paths without standing up
//! the JSON-RPC layer.

use std::path::PathBuf;

use rmcp::ErrorData;
use thiserror::Error;
use tokio_util::sync::CancellationToken;

use crate::agda::{
    command as agda_command,
    process::{self, AgdaProcess},
    response::Response,
};
use crate::tools::{Goal, LoadRequest, LoadResponse, LoadResponseError};

/// Cached state about the most recent successful `load`.
#[derive(Debug, Clone)]
pub struct LoadedFile {
    pub current_file: String,
    pub goals: Vec<Goal>,
}

/// Backing state for the MCP server.
#[derive(Debug)]
pub struct ServerState {
    agda: AgdaProcess,
    loaded: Option<LoadedFile>,
    /// Signalled when the underlying Agda process produces a protocol-fatal
    /// error (parse failure, non-UTF-8 output, premature EOF). The caller is
    /// expected to race this against `RunningService::waiting()` and trigger
    /// a graceful shutdown of the MCP service.
    shutdown: CancellationToken,
}

impl ServerState {
    /// Spawn the Agda interaction process and return a ready server state.
    ///
    /// `shutdown` is cancelled by the state whenever a protocol-fatal error
    /// is observed, signalling that the MCP service should drain in-flight
    /// requests and exit. The same token can be cancelled externally to
    /// trigger shutdown for other reasons.
    pub async fn spawn(shutdown: CancellationToken) -> Result<Self, Error> {
        Ok(Self {
            agda: AgdaProcess::spawn().await?,
            loaded: None,
            shutdown,
        })
    }

    /// Run an Agda `Cmd_load` for `params.path` and summarise the response.
    ///
    /// The returned `LoadOutput` is also cached so future `give` calls can
    /// resolve goal ranges without an extra round trip.
    pub async fn load(&mut self, params: &LoadRequest) -> Result<LoadResponse, Error> {
        let current_file = absolute_path(&params.path);

        let responses = self
            .send(&agda_command::Command::load(&current_file, &[]))
            .await?;

        let output = match LoadResponse::try_from(responses) {
            Ok(output) => output,
            Err(error) => {
                self.shutdown.cancel();
                return Err(Error::LoadResponse(error));
            }
        };

        if output.errors.is_empty() {
            self.loaded = Some(LoadedFile {
                current_file,
                goals: output.goals.clone(),
            });
        }

        Ok(output)
    }

    pub fn loaded(&self) -> Option<&LoadedFile> {
        self.loaded.as_ref()
    }

    /// Shared `send` wrapper. Forwards to `AgdaProcess::send` and, on a
    /// protocol-fatal error, signals shutdown so the MCP service exits after
    /// the current tool call returns.
    async fn send(&mut self, command: &agda_command::Command<'_>) -> Result<Vec<Response>, Error> {
        match self.agda.send(command).await {
            Ok(responses) => Ok(responses),
            Err(error) => {
                if error.is_protocol_fatal() {
                    self.shutdown.cancel();
                }
                Err(Error::Process(error))
            }
        }
    }
}

/// Resolve `path` to an absolute path, falling back to the input on failure.
///
/// `canonicalize` requires the file to exist on disk. When it doesn't (the
/// caller passed a typo or a missing file), we let Agda surface the error
/// from `Cmd_load` rather than failing the tool call here.
fn absolute_path(path: &str) -> String {
    if let Ok(canonical) = std::fs::canonicalize(path) {
        return canonical.to_string_lossy().into_owned();
    }

    let candidate = PathBuf::from(path);
    if candidate.is_absolute() {
        return path.to_owned();
    }

    if let Ok(cwd) = std::env::current_dir() {
        return cwd.join(&candidate).to_string_lossy().into_owned();
    }

    path.to_owned()
}

#[derive(Debug, Error)]
pub enum Error {
    #[error(transparent)]
    Process(#[from] process::Error),

    #[error("unexpected Agda response sequence for Cmd_load: {0}")]
    LoadResponse(#[from] LoadResponseError),
}

impl From<Error> for ErrorData {
    /// Convert a [`server::Error`] into a JSON-RPC error response.
    ///
    /// Protocol-fatal parse failures carry the offending payload along with the
    /// parse error so the MCP client can see what we choked on before the server
    /// exits.
    fn from(error: Error) -> Self {
        match &error {
            Error::Process(process::Error::Parse(failure)) => {
                let data = serde_json::json!({
                    "kind": "agda_response_parse_failure",
                    "line_index": failure.source.index,
                    "raw_line": failure.source.raw,
                    "payload": failure.payload,
                    "payload_values": failure.payload_values,
                });
                ErrorData::internal_error(
                    format!("Agda response parse failure: {}", failure),
                    Some(data),
                )
            }
            Error::LoadResponse(load_error) => {
                let data = serde_json::json!({
                    "kind": "agda_load_response_parse_failure",
                    "error": load_error.to_string(),
                });
                ErrorData::internal_error(error.to_string(), Some(data))
            }
            _ => ErrorData::internal_error(error.to_string(), None),
        }
    }
}
