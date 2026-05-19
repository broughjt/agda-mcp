use std::mem;

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::agda::{
    command::{Command, NO_RANGE, UseForce},
    response::{GiveResult, Info, InteractionPoint, Response, Status},
};

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

impl GiveRequest {
    pub fn to_command(&self) -> Command<'_> {
        Command::give(
            &self.path,
            UseForce::WithoutForce,
            self.goal_id,
            &NO_RANGE,
            &self.expression,
        )
    }
}

/// Summary returned to the MCP client for a `give` call.
///
/// Goals/warnings emitted alongside `Cmd_give` describe Agda's in-memory
/// state immediately after the give but before the edit is applied to
/// disk. That snapshot is transient: a follow-up `load` re-derives goals
/// from scratch. To avoid surfacing misleading state, only the
/// give-specific outputs (and any errors) are kept; the caller is
/// expected to reload to see the authoritative file state.
#[derive(Debug, Clone, Serialize, JsonSchema)]
pub struct GiveResponse {
    /// The interaction point Agda confirms it acted on, including its
    /// source range. `None` when the give failed.
    pub interaction_point: Option<InteractionPoint>,
    /// What Agda wants written back into the source file. `None` when the
    /// give failed.
    pub give_result: Option<GiveResult>,
    /// Errors reported by Agda. Populated when Agda rejects the give
    /// (parse error, type error, unknown interaction point).
    pub errors: Vec<String>,
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

                Ok(GiveResponse {
                    interaction_point: Some(interaction_point),
                    give_result: Some(give_result),
                    errors: Vec::new(),
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

                Ok(GiveResponse {
                    interaction_point: None,
                    give_result: None,
                    errors: vec![error],
                })
            }
            _ => Err(GiveResponseError(responses)),
        }
    }
}

#[derive(Debug, Error)]
#[error("unexpected Agda response sequence ({} responses)", .0.len())]
pub struct GiveResponseError(pub Vec<Response>);

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

        let point = output
            .interaction_point
            .as_ref()
            .expect("success carries an interaction point");
        assert_eq!(point.id, 0);
        assert_eq!(point.range.len(), 1);

        let give_result = output.give_result.as_ref().expect("success carries a result");
        match give_result {
            GiveResult::String { expression } => assert_eq!(expression, "zero"),
            GiveResult::Paren { paren } => panic!("expected String result, got Paren({paren})"),
        }

        assert!(output.errors.is_empty());
    }

    #[test]
    fn parses_success_with_remaining_goals() {
        // Even when `AllGoalsWarnings` reports a leftover `?1`, GiveResponse
        // does not surface it. The caller is expected to reload to see
        // post-edit goals.
        let responses = parse_responses(GIVE_SUCCESS_LEAVES_GOAL);
        let output = GiveResponse::try_from(responses).expect("should parse success sequence");

        let given = output.interaction_point.as_ref().expect("interaction point");
        assert_eq!(given.id, 0, "give acted on ?0");
        assert!(
            output.errors.is_empty(),
            "remaining goals are not errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn parses_error_without_jump_to_error() {
        let responses = parse_responses(GIVE_ERROR_NOT_IN_SCOPE);
        let output = GiveResponse::try_from(responses).expect("should parse error sequence");

        assert!(output.interaction_point.is_none());
        assert!(output.give_result.is_none());
        assert_eq!(output.errors.len(), 1);
        assert!(
            output.errors[0].contains("[NotInScope]"),
            "errors should carry Agda's diagnostic, got {:?}",
            output.errors
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

        assert_eq!(output.errors, vec!["some error with a range".to_owned()]);
        assert!(output.interaction_point.is_none());
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
