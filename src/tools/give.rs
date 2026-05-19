use std::mem;

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::agda::response::{GiveResult, Info, InteractionPoint, Response, Status};
use crate::tools::LoadResponse;

/// Parameters for the MCP `give` tool.
#[derive(Debug, Deserialize, JsonSchema)]
pub struct GiveRequest {
    /// Path to the Agda file containing the interaction point.
    pub path: String,

    /// The Agda interaction point id, for example `0` for `?0`.
    pub goal_id: u32,

    /// Expression to give to the interaction point.
    pub expression: String,
    // TODO: Whether to use the `force` option here?
    // TODO: Whether to use the command line flags here?
}

/// Agda's response to `Cmd_give`. Models the success-or-rejection split
/// at the type level: a `Cmd_give` either yields a [`GiveAction`] +
/// updated goal state, or a [`DisplayInfo`] error.
///
/// [`GiveAction`]: crate::agda::response::Response::GiveAction
/// [`DisplayInfo`]: crate::agda::response::Response::DisplayInfo
#[derive(Debug, Clone, Serialize, JsonSchema)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum GiveResponse {
    /// Agda accepted the expression and solved the meta.
    Accepted {
        /// The interaction point Agda confirms it acted on, including
        /// its source range.
        interaction_point: InteractionPoint,
        /// What Agda wants written back into the source file.
        give_result: GiveResult,
    },
    /// Agda rejected the expression (parse error, type error,
    /// out-of-scope name, unknown interaction point). `Cmd_give` only
    /// ever emits one `Info::Error`, so this is a single message rather
    /// than a list.
    Rejected { error: String },
}

impl TryFrom<Vec<Response>> for GiveResponse {
    type Error = GiveResponseError;

    fn try_from(mut responses: Vec<Response>) -> Result<Self, Self::Error> {
        match &mut responses[..] {
            [
                Response::GiveAction {
                    interaction_point,
                    give_result,
                },
                Response::Status { status: _ },
                Response::DisplayInfo {
                    info: Info::AllGoalsWarnings { .. },
                },
                Response::InteractionPoints { points: _ },
            ] => {
                let interaction_point = mem::replace(
                    interaction_point,
                    InteractionPoint {
                        id: 0,
                        range: Vec::new(),
                    },
                );
                let give_result = mem::replace(
                    give_result,
                    GiveResult::String {
                        expression: String::new(),
                    },
                );

                Ok(GiveResponse::Accepted {
                    interaction_point,
                    give_result,
                })
            }
            // Cmd_give errors carry no file-anchored range (the range refers
            // to the parsed expression string, not the source file), so
            // `tellEmacsToJumpToError` returns `[]` and no `JumpToError` is
            // emitted. We still accept the `JumpToError`-prefixed variant
            // that load uses, mirroring the load parser for robustness.
            [
                Response::DisplayInfo {
                    info: Info::Error { warnings: _, error },
                },
                Response::JumpToError {
                    filepath: _,
                    position: _,
                },
                Response::HighlightingInfo {},
                Response::Status {
                    status:
                        Status {
                            checked: false,
                            show_implicit_arguments: _,
                            show_irrelevant_arguments: _,
                        },
                },
            ]
            | [
                Response::DisplayInfo {
                    info: Info::Error { warnings: _, error },
                },
                Response::HighlightingInfo {},
                Response::Status {
                    status:
                        Status {
                            checked: false,
                            show_implicit_arguments: _,
                            show_irrelevant_arguments: _,
                        },
                },
            ] => {
                let error = mem::take(&mut error.message);
                Ok(GiveResponse::Rejected { error })
            }
            _ => Err(GiveResponseError(responses)),
        }
    }
}

#[derive(Debug, Error)]
#[error("unexpected Agda response sequence ({} responses)", .0.len())]
pub struct GiveResponseError(pub Vec<Response>);

/// What the MCP `give` tool returns: Agda's response to the `Cmd_give`
/// followed by the authoritative post-edit state from the auto-reload.
///
/// `give` and `reload` are independent: `reload` reflects the file on
/// disk after any edit was applied, while `give` describes only what
/// happened during the `Cmd_give` itself. A rejected give still carries
/// a meaningful `reload` because the LLM caller benefits from seeing
/// current goals/errors regardless.
#[derive(Debug, Clone, Serialize, JsonSchema)]
pub struct GiveToolOutput {
    pub give: GiveResponse,
    pub reload: LoadResponse,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::agda::response;

    fn parse_responses(payload: &str) -> Vec<Response> {
        response::parse_all(payload).expect("response payload should parse")
    }

    /// Verbatim Agda 2.8.0 `Cmd_give` reply for `Cmd_give 0 noRange "zero"`
    /// against a single-hole `x : Nat` file. Captured by sending the command
    /// to a live `agda --interaction-json` session.
    const GIVE_SUCCESS_CLEARS_GOAL: &str = "\
{\"giveResult\":{\"str\":\"zero\"},\"interactionPoint\":{\"id\":0,\"range\":[{\"end\":{\"col\":10,\"line\":5,\"pos\":68},\"start\":{\"col\":5,\"line\":5,\"pos\":63}}]},\"kind\":\"GiveAction\"}
{\"kind\":\"Status\",\"status\":{\"checked\":false,\"showImplicitArguments\":false,\"showIrrelevantArguments\":false}}
{\"info\":{\"errors\":[],\"invisibleGoals\":[],\"kind\":\"AllGoalsWarnings\",\"visibleGoals\":[],\"warnings\":[]},\"kind\":\"DisplayInfo\"}
{\"interactionPoints\":[],\"kind\":\"InteractionPoints\"}
";

    /// Verbatim Agda 2.8.0 `Cmd_give` reply for giving `zero` to the first of
    /// two `Nat` holes; goal `?1` remains afterwards. Confirms that
    /// `GiveResponse` does not extract the transient `AllGoalsWarnings`
    /// payload even when remaining goals are present.
    const GIVE_SUCCESS_LEAVES_GOAL: &str = "\
{\"giveResult\":{\"str\":\"zero\"},\"interactionPoint\":{\"id\":0,\"range\":[{\"end\":{\"col\":10,\"line\":5,\"pos\":67},\"start\":{\"col\":5,\"line\":5,\"pos\":62}}]},\"kind\":\"GiveAction\"}
{\"kind\":\"Status\",\"status\":{\"checked\":false,\"showImplicitArguments\":false,\"showIrrelevantArguments\":false}}
{\"info\":{\"errors\":[],\"invisibleGoals\":[],\"kind\":\"AllGoalsWarnings\",\"visibleGoals\":[{\"constraintObj\":{\"id\":1,\"range\":[{\"end\":{\"col\":10,\"line\":8,\"pos\":86},\"start\":{\"col\":5,\"line\":8,\"pos\":81}}]},\"kind\":\"OfType\",\"type\":\"Nat\"}],\"warnings\":[]},\"kind\":\"DisplayInfo\"}
{\"interactionPoints\":[{\"id\":1,\"range\":[{\"end\":{\"col\":10,\"line\":8,\"pos\":86},\"start\":{\"col\":5,\"line\":8,\"pos\":81}}]}],\"kind\":\"InteractionPoints\"}
";

    /// Verbatim Agda 2.8.0 `Cmd_give` reply for giving an out-of-scope name.
    /// Same shape applies to `[ParseError]` and
    /// `[Interaction.NoSuchInteractionPoint]`.
    const GIVE_ERROR_NOT_IN_SCOPE: &str = "\
{\"info\":{\"error\":{\"message\":\"1.1-5: error: [NotInScope]\\nNot in scope:\\n  true at 1.1-5\\nwhen scope checking true\"},\"kind\":\"Error\",\"warnings\":[]},\"kind\":\"DisplayInfo\"}
{\"direct\":false,\"filepath\":\"/tmp/agda2-mode/scratch\",\"kind\":\"HighlightingInfo\"}
{\"kind\":\"Status\",\"status\":{\"checked\":false,\"showImplicitArguments\":false,\"showIrrelevantArguments\":false}}
";

    #[test]
    fn parses_success_with_no_remaining_goals() {
        let responses = parse_responses(GIVE_SUCCESS_CLEARS_GOAL);
        let output = GiveResponse::try_from(responses).expect("should parse success sequence");

        let GiveResponse::Accepted {
            interaction_point,
            give_result,
        } = output
        else {
            panic!("expected Accepted, got {output:?}");
        };
        assert_eq!(interaction_point.id, 0);
        assert_eq!(interaction_point.range.len(), 1);
        match give_result {
            GiveResult::String { expression } => assert_eq!(expression, "zero"),
            GiveResult::Paren { paren } => panic!("expected String result, got Paren({paren})"),
        }
    }

    #[test]
    fn parses_success_with_remaining_goals() {
        // Even when `AllGoalsWarnings` reports a leftover `?1`, GiveResponse
        // does not surface it. The caller is expected to consume the
        // follow-up reload for post-edit goals.
        let responses = parse_responses(GIVE_SUCCESS_LEAVES_GOAL);
        let output = GiveResponse::try_from(responses).expect("should parse success sequence");

        let GiveResponse::Accepted {
            interaction_point, ..
        } = output
        else {
            panic!("expected Accepted, got {output:?}");
        };
        assert_eq!(interaction_point.id, 0, "give acted on ?0");
    }

    #[test]
    fn parses_error_without_jump_to_error() {
        let responses = parse_responses(GIVE_ERROR_NOT_IN_SCOPE);
        let output = GiveResponse::try_from(responses).expect("should parse error sequence");

        let GiveResponse::Rejected { error } = output else {
            panic!("expected Rejected, got {output:?}");
        };
        assert!(
            error.contains("[NotInScope]"),
            "error should carry Agda's diagnostic, got {error:?}"
        );
    }

    #[test]
    fn parses_error_with_jump_to_error() {
        // Synthetic: `Cmd_give` errors never carry a file-anchored range in
        // practice, but the parser accepts the `JumpToError`-prefixed variant
        // for parity with `load` and to keep the parser robust if Agda
        // changes its mind.
        let payload = "\
{\"info\":{\"error\":{\"message\":\"some error with a range\"},\"kind\":\"Error\",\"warnings\":[]},\"kind\":\"DisplayInfo\"}
{\"filepath\":\"/tmp/whatever.agda\",\"position\":42,\"kind\":\"JumpToError\"}
{\"direct\":false,\"filepath\":\"/tmp/agda2-mode/scratch\",\"kind\":\"HighlightingInfo\"}
{\"kind\":\"Status\",\"status\":{\"checked\":false,\"showImplicitArguments\":false,\"showIrrelevantArguments\":false}}
";

        let responses = parse_responses(payload);
        let output = GiveResponse::try_from(responses).expect("should parse JumpToError variant");

        let GiveResponse::Rejected { error } = output else {
            panic!("expected Rejected, got {output:?}");
        };
        assert_eq!(error, "some error with a range");
    }

    #[test]
    fn rejects_unexpected_response_sequence() {
        // An `AllGoalsWarnings` reply without a leading `GiveAction` is not a
        // valid `Cmd_give` response shape.
        let payload = "\
{\"info\":{\"errors\":[],\"invisibleGoals\":[],\"kind\":\"AllGoalsWarnings\",\"visibleGoals\":[],\"warnings\":[]},\"kind\":\"DisplayInfo\"}
";
        let responses = parse_responses(payload);
        let error = GiveResponse::try_from(responses).expect_err("should reject unknown shape");
        assert_eq!(error.0.len(), 1);
    }
}
