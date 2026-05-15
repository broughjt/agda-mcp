use schemars::JsonSchema;
use serde::Deserialize;

/// Parameters for the MCP `load` tool.
#[derive(Debug, Deserialize, JsonSchema)]
pub struct Load {
    /// Path to the Agda file to load.
    pub path: String,
    // /// Agda command-line flags to use for this load, such as `-i`,
    // /// `--library`, or `--no-default-libraries`.
    // #[serde(default)]
    // pub flags: Vec<String>,
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
    // /// Whether to use Agda's `WithForce` mode for give.
    // #[serde(default)]
    // pub force: bool,
}
