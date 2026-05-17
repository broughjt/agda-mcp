//! Stub module for the MCP `give` tool.
//!
//! The tool currently only needs request parameters and Agda command
//! conversion. Server-side execution and result summarisation can live here
//! once `give` grows more behavior.

use schemars::JsonSchema;
use serde::Deserialize;

use crate::agda::command as agda_command;

/// Parameters for the MCP `give` tool.
#[derive(Debug, Deserialize, JsonSchema)]
pub struct Give {
    /// Path to the Agda file containing the interaction point.
    pub path: String,

    /// The Agda interaction point id, for example `0` for `?0`.
    pub goal_id: u32,

    /// Expression to give to the interaction point.
    pub expression: String,
    // TODO: Whether to use the `force` option here?
}

impl Give {
    pub fn to_agda_command(&self) -> agda_command::Command<'_> {
        agda_command::Command::give(
            &self.path,
            agda_command::UseForce::WithoutForce,
            self.goal_id,
            &agda_command::NO_RANGE,
            &self.expression,
        )
    }
}
