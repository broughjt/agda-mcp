use schemars::JsonSchema;
use serde::Deserialize;

use crate::agda::command as agda_command;

/// Parameters for the MCP `load` tool.
#[derive(Debug, Deserialize, JsonSchema)]
pub struct Load {
    /// Path to the Agda file to load.
    pub path: String,
    // TODO: Whether to use the command line flags here?
}

impl Load {
    pub fn to_agda_command(&self) -> agda_command::Command<'_> {
        agda_command::Command::load(&self.path, &[])
    }
}

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
