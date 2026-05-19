//! State shared between MCP tool calls.
//!
//! The MCP transport wraps this state in a `Mutex` and routes tool calls
//! through it. Keeping the load/give logic here (rather than in `main.rs`)
//! also lets integration tests drive the same code paths without standing up
//! the JSON-RPC layer.

use std::path::{Path, PathBuf};

use rmcp::ErrorData;
use thiserror::Error;
use tokio_util::sync::CancellationToken;

use crate::agda::{
    command as agda_command,
    process::{self, AgdaProcess},
    response::Response,
};
use crate::edit;
use crate::tools::{
    GiveRequest, GiveResponse, GiveResponseError, GiveToolOutput, LoadRequest, LoadResponse,
    LoadResponseError,
};

/// Backing state for the MCP server.
#[derive(Debug)]
pub struct ServerState {
    agda: AgdaProcess,
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
            shutdown,
        })
    }

    /// Run an Agda `Cmd_load` for `params.path` and summarise the response.
    pub async fn load(&mut self, params: &LoadRequest) -> Result<LoadResponse, Error> {
        let current_file = absolute_path(&params.path);
        self.load_canonical(&current_file).await
    }

    /// Run an Agda `Cmd_give` for `params`, apply the resulting edit to
    /// the source file, and then reload.
    ///
    /// The outer `Result` reports protocol-fatal failures (parse errors,
    /// dead Agda process); the inner `Result` reports edit failures where
    /// Agda accepted the give but the file rewrite couldn't be applied.
    /// In the edit-failure branch the function still issues a reload to
    /// resync Agda's in-memory state with the (unchanged) file before
    /// returning, since `Cmd_give` has already solved the meta in memory.
    pub async fn give(
        &mut self,
        params: &GiveRequest,
    ) -> Result<Result<GiveToolOutput, edit::Error>, Error> {
        let current_file = absolute_path(&params.path);

        let responses = self
            .send(&agda_command::Command::give(
                &current_file,
                agda_command::UseForce::WithoutForce,
                params.goal_id,
                &agda_command::NO_RANGE,
                &params.expression,
            ))
            .await?;

        let give = match GiveResponse::try_from(responses) {
            Ok(give) => give,
            Err(error) => {
                self.shutdown.cancel();
                return Err(Error::GiveResponse(error));
            }
        };

        if let GiveResponse::Accepted {
            interaction_point,
            give_result,
        } = &give
        {
            let Some(interval) = interaction_point.range.first().copied() else {
                self.shutdown.cancel();
                return Err(Error::GiveMissingRange);
            };
            let replacement = give_result.clone().replacement(&params.expression);

            if let Err(error) = edit::apply(Path::new(&current_file), interval, &replacement) {
                // Resync Agda with the unchanged file; discard the reload
                // since the caller only sees the edit error in this branch.
                let _ = self.load_canonical(&current_file).await?;
                return Ok(Err(error));
            }
        }

        let reload = self.load_canonical(&current_file).await?;
        Ok(Ok(GiveToolOutput { give, reload }))
    }

    /// Issue `Cmd_load` for an already-canonicalised file path. Used by
    /// [`Self::load`] directly and by [`Self::give`] for the post-edit
    /// reload.
    async fn load_canonical(&mut self, current_file: &str) -> Result<LoadResponse, Error> {
        let responses = self
            .send(&agda_command::Command::load(current_file, &[]))
            .await?;

        match LoadResponse::try_from(responses) {
            Ok(output) => Ok(output),
            Err(error) => {
                self.shutdown.cancel();
                Err(Error::LoadResponse(error))
            }
        }
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

    #[error("unexpected Agda response sequence for Cmd_give: {0}")]
    GiveResponse(#[from] GiveResponseError),

    #[error("Cmd_give response carried an empty interaction-point range")]
    GiveMissingRange,
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
                    "responses": load_error.0,
                });
                ErrorData::internal_error(error.to_string(), Some(data))
            }
            Error::GiveResponse(give_error) => {
                let data = serde_json::json!({
                    "kind": "agda_give_response_parse_failure",
                    "responses": give_error.0,
                });
                ErrorData::internal_error(error.to_string(), Some(data))
            }
            _ => ErrorData::internal_error(error.to_string(), None),
        }
    }
}
