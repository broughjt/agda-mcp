//! Typed views over Agda's `--interaction-json` responses.
//!
//! Every Agda response kind we know exists is enumerated explicitly. If Agda
//! ever emits something we have not modelled, parsing hard-fails with a
//! `serde_json` error that names the unknown variant or missing field — that
//! is the signal to update this module. Tolerating unknown kinds silently
//! would just push the broken JSON to the MCP caller, who has no way to act
//! on it.
//!
//! Mirrors Agda's `Resp_*` constructors and their JSON encoder:
//! https://github.com/agda/agda/blob/3b57742a311b3a90b755737968d437f1ef902318/src/full/Agda/Interaction/JSONTop.hs#L438-L483
//!
//! For Response kinds whose body we have no reason to interpret in the spike
//! (`HighlightingInfo`, `MakeCase`, `SolveAll`, `Mimer`), we still list the
//! variant so unexpected kinds remain a hard parse error. Internally-tagged
//! struct variants with no fields accept arbitrary extra keys, which is what
//! we want for these "known noise" kinds.

use serde::Deserialize;
use serde_json::Value;
use thiserror::Error;

/// A single response object Agda emits between `JSON> ` prompts.
#[derive(Debug, Clone, Deserialize)]
#[serde(tag = "kind")]
pub enum Response {
    Status {
        status: Status,
    },
    ClearRunningInfo,
    ClearHighlighting {
        #[serde(rename = "tokenBased")]
        token_based: String,
    },
    RunningInfo {
        #[serde(rename = "debugLevel")]
        debug_level: u32,
        message: String,
    },
    DisplayInfo {
        info: Info,
    },
    InteractionPoints {
        #[serde(rename = "interactionPoints")]
        points: Vec<InteractionPoint>,
    },
    GiveAction {
        #[serde(rename = "interactionPoint")]
        interaction_point: u32,
        #[serde(rename = "giveResult")]
        give_result: GiveResult,
    },
    JumpToError {
        filepath: String,
        position: i32,
    },
    DoneAborting,
    DoneExiting,
    /// Highlighting payload — body intentionally unmodelled.
    HighlightingInfo {},
    /// Make-case clauses — body intentionally unmodelled for the load/give spike.
    MakeCase {},
    /// Solve-all solutions — body intentionally unmodelled.
    SolveAll {},
    /// Mimer solution — body intentionally unmodelled.
    Mimer {},
}

impl Response {
    /// Parse a batch of raw responses, hard-failing on the first one that
    /// does not match an enumerated kind/field.
    pub fn parse_all(values: &[Value]) -> Result<Vec<Self>, ParseError> {
        values
            .iter()
            .enumerate()
            .map(|(index, value)| {
                serde_json::from_value(value.clone()).map_err(|source| ParseError {
                    index,
                    raw: value.clone(),
                    source,
                })
            })
            .collect()
    }
}

#[derive(Debug, Clone, Deserialize)]
pub struct Status {
    pub checked: bool,
    #[serde(rename = "showImplicitArguments")]
    pub show_implicit_arguments: bool,
    #[serde(rename = "showIrrelevantArguments")]
    pub show_irrelevant_arguments: bool,
}

/// The `info` payload inside a `DisplayInfo` response.
///
/// Only the kinds the spike acts on (or wants to surface) are modelled.
/// Anything else hard-fails so we can decide how to handle it on the next
/// pass.
#[derive(Debug, Clone, Deserialize)]
#[serde(tag = "kind")]
pub enum Info {
    Version {
        version: String,
    },
    AllGoalsWarnings {
        #[serde(rename = "visibleGoals")]
        visible_goals: Vec<VisibleGoal>,
        #[serde(rename = "invisibleGoals")]
        invisible_goals: Vec<InvisibleGoal>,
        warnings: Vec<Message>,
        errors: Vec<Message>,
    },
    Error {
        warnings: Vec<Message>,
        error: Message,
    },
}

/// A visible goal in `AllGoalsWarnings.visibleGoals`.
///
/// Agda emits one of many `OutputConstraint` shapes here. For the spike we
/// only model `OfType` (the common case for `Cmd_load`); anything else
/// hard-fails.
#[derive(Debug, Clone, Deserialize)]
#[serde(tag = "kind")]
pub enum VisibleGoal {
    OfType {
        #[serde(rename = "constraintObj")]
        constraint_obj: InteractionPoint,
        #[serde(rename = "type")]
        ty: String,
    },
}

/// `NamedMeta`-shaped invisible goal.
#[derive(Debug, Clone, Deserialize)]
pub struct InvisibleGoal {
    pub name: String,
    pub range: Vec<Interval>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct Message {
    pub message: String,
}

#[derive(Debug, Clone, Deserialize)]
pub struct InteractionPoint {
    pub id: u32,
    pub range: Vec<Interval>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct Interval {
    pub start: Position,
    pub end: Position,
}

#[derive(Debug, Clone, Copy, Deserialize)]
pub struct Position {
    pub pos: u32,
    pub line: u32,
    pub col: u32,
}

/// The `giveResult` payload inside a `GiveAction` response.
///
/// Mirrors Agda's `GiveResult` JSON encoding:
/// https://github.com/agda/agda/blob/3b57742a311b3a90b755737968d437f1ef902318/src/full/Agda/Interaction/JSONTop.hs#L152-L156
#[derive(Debug, Clone, Deserialize)]
#[serde(untagged)]
pub enum GiveResult {
    String {
        #[serde(rename = "str")]
        expression: String,
    },
    Paren {
        paren: bool,
    },
}

#[derive(Debug, Error)]
#[error(
    "failed to parse Agda response #{index}: {source}\nraw response: {}",
    serde_json::to_string(.raw).unwrap_or_else(|_| ".raw".to_owned())
)]
pub struct ParseError {
    pub index: usize,
    pub raw: Value,
    #[source]
    pub source: serde_json::Error,
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::agda::process::parse_json_responses;

    /// Verbatim copy of `~/scratch/agda/test/interaction/ParenJSON.out` from the
    /// upstream Agda repo (commit 3b57742). Kept inline so the unit test is
    /// self-contained and survives reorganising the source tree.
    const PAREN_JSON_OUT: &str = "JSON> {\"kind\":\"Status\",\"status\":{\"checked\":false,\"showImplicitArguments\":false,\"showIrrelevantArguments\":false}}\n{\"kind\":\"ClearRunningInfo\"}\n{\"kind\":\"ClearHighlighting\",\"tokenBased\":\"NotOnlyTokenBased\"}\n{\"kind\":\"Status\",\"status\":{\"checked\":false,\"showImplicitArguments\":false,\"showIrrelevantArguments\":false}}\n{\"info\":{\"errors\":[],\"invisibleGoals\":[],\"kind\":\"AllGoalsWarnings\",\"visibleGoals\":[{\"constraintObj\":{\"id\":0,\"range\":[{\"end\":{\"col\":15,\"line\":10,\"pos\":110},\"start\":{\"col\":10,\"line\":10,\"pos\":105}}]},\"kind\":\"OfType\",\"type\":\"P a\"}],\"warnings\":[]},\"kind\":\"DisplayInfo\"}\n{\"interactionPoints\":[{\"id\":0,\"range\":[{\"end\":{\"col\":15,\"line\":10,\"pos\":110},\"start\":{\"col\":10,\"line\":10,\"pos\":105}}]}],\"kind\":\"InteractionPoints\"}\n";

    fn parse_one(value: Value) -> Response {
        serde_json::from_value(value).expect("response should parse")
    }

    #[test]
    fn parses_paren_json_load_transcript() {
        let raw = parse_json_responses(PAREN_JSON_OUT).expect("ParenJSON output should be JSON");
        let parsed = Response::parse_all(&raw).expect("ParenJSON transcript should parse");

        let kinds: Vec<&'static str> = parsed
            .iter()
            .map(|response| match response {
                Response::Status { .. } => "Status",
                Response::ClearRunningInfo => "ClearRunningInfo",
                Response::ClearHighlighting { .. } => "ClearHighlighting",
                Response::RunningInfo { .. } => "RunningInfo",
                Response::DisplayInfo { .. } => "DisplayInfo",
                Response::InteractionPoints { .. } => "InteractionPoints",
                Response::GiveAction { .. } => "GiveAction",
                Response::JumpToError { .. } => "JumpToError",
                Response::DoneAborting => "DoneAborting",
                Response::DoneExiting => "DoneExiting",
                Response::HighlightingInfo { .. } => "HighlightingInfo",
                Response::MakeCase { .. } => "MakeCase",
                Response::SolveAll { .. } => "SolveAll",
                Response::Mimer { .. } => "Mimer",
            })
            .collect();

        assert_eq!(
            kinds,
            [
                "Status",
                "ClearRunningInfo",
                "ClearHighlighting",
                "Status",
                "DisplayInfo",
                "InteractionPoints",
            ]
        );
    }

    #[test]
    fn extracts_all_goals_warnings_from_paren_json() {
        let raw = parse_json_responses(PAREN_JSON_OUT).expect("ParenJSON output should be JSON");
        let parsed = Response::parse_all(&raw).expect("ParenJSON transcript should parse");

        let info = parsed
            .into_iter()
            .find_map(|response| match response {
                Response::DisplayInfo { info } => Some(info),
                _ => None,
            })
            .expect("transcript should contain a DisplayInfo response");

        let Info::AllGoalsWarnings {
            visible_goals,
            invisible_goals,
            warnings,
            errors,
        } = info
        else {
            panic!("expected AllGoalsWarnings");
        };

        assert!(invisible_goals.is_empty());
        assert!(warnings.is_empty());
        assert!(errors.is_empty());
        assert_eq!(visible_goals.len(), 1);

        let VisibleGoal::OfType { constraint_obj, ty } = &visible_goals[0];
        assert_eq!(constraint_obj.id, 0);
        assert_eq!(ty, "P a");
        assert_eq!(constraint_obj.range.len(), 1);
        assert_eq!(constraint_obj.range[0].start.pos, 105);
        assert_eq!(constraint_obj.range[0].end.pos, 110);
    }

    #[test]
    fn extracts_interaction_points_from_paren_json() {
        let raw = parse_json_responses(PAREN_JSON_OUT).expect("ParenJSON output should be JSON");
        let parsed = Response::parse_all(&raw).expect("ParenJSON transcript should parse");

        let points = parsed
            .into_iter()
            .find_map(|response| match response {
                Response::InteractionPoints { points } => Some(points),
                _ => None,
            })
            .expect("transcript should contain an InteractionPoints response");

        assert_eq!(points.len(), 1);
        assert_eq!(points[0].id, 0);
        assert_eq!(points[0].range[0].start.line, 10);
        assert_eq!(points[0].range[0].start.col, 10);
    }

    #[test]
    fn parses_give_action_with_string_result() {
        let response = parse_one(serde_json::json!({
            "kind": "GiveAction",
            "interactionPoint": 7,
            "giveResult": { "str": "suc zero" },
        }));

        let Response::GiveAction {
            interaction_point,
            give_result,
        } = response
        else {
            panic!("expected GiveAction");
        };
        assert_eq!(interaction_point, 7);
        match give_result {
            GiveResult::String { expression } => assert_eq!(expression, "suc zero"),
            GiveResult::Paren { paren } => {
                panic!("expected String give result, got Paren({paren})")
            }
        }
    }

    #[test]
    fn parses_give_action_with_paren_true() {
        let response = parse_one(serde_json::json!({
            "kind": "GiveAction",
            "interactionPoint": 0,
            "giveResult": { "paren": true },
        }));

        let Response::GiveAction { give_result, .. } = response else {
            panic!("expected GiveAction");
        };
        assert!(matches!(give_result, GiveResult::Paren { paren: true }));
    }

    #[test]
    fn parses_give_action_with_paren_false() {
        let response = parse_one(serde_json::json!({
            "kind": "GiveAction",
            "interactionPoint": 0,
            "giveResult": { "paren": false },
        }));

        let Response::GiveAction { give_result, .. } = response else {
            panic!("expected GiveAction");
        };
        assert!(matches!(give_result, GiveResult::Paren { paren: false }));
    }

    #[test]
    fn parses_version_display_info() {
        let response = parse_one(serde_json::json!({
            "kind": "DisplayInfo",
            "info": { "kind": "Version", "version": "2.7.0-abc123" },
        }));

        let Response::DisplayInfo {
            info: Info::Version { version },
        } = response
        else {
            panic!("expected DisplayInfo/Version, got: {response:?}");
        };
        assert_eq!(version, "2.7.0-abc123");
    }

    #[test]
    fn parses_error_display_info() {
        let response = parse_one(serde_json::json!({
            "kind": "DisplayInfo",
            "info": {
                "kind": "Error",
                "warnings": [],
                "error": { "message": "Not in scope: foo" },
            },
        }));

        let Response::DisplayInfo {
            info: Info::Error { warnings, error },
        } = response
        else {
            panic!("expected DisplayInfo/Error, got: {response:?}");
        };
        assert!(warnings.is_empty());
        assert_eq!(error.message, "Not in scope: foo");
    }

    #[test]
    fn unknown_top_level_kind_is_hard_error() {
        let raw = vec![serde_json::json!({
            "kind": "SomeFutureResponse",
            "payload": 42,
        })];
        let error = Response::parse_all(&raw).expect_err("unknown kinds must hard-fail");

        assert_eq!(error.index, 0);
        let message = format!("{error}");
        assert!(
            message.contains("SomeFutureResponse"),
            "error should name the unknown variant: {message}"
        );
        assert!(
            message.contains("\"payload\":42"),
            "error should include the raw response: {message}"
        );
    }

    #[test]
    fn unknown_display_info_kind_is_hard_error() {
        let raw = vec![serde_json::json!({
            "kind": "DisplayInfo",
            "info": { "kind": "Time", "time": "0.01s" },
        })];
        let error =
            Response::parse_all(&raw).expect_err("unmodelled DisplayInfo kinds must hard-fail");

        assert_eq!(error.index, 0);
        assert!(
            format!("{error}").contains("Time"),
            "error should name the unmodelled info kind: {error}"
        );
    }

    #[test]
    fn unknown_visible_goal_kind_is_hard_error() {
        let raw = vec![serde_json::json!({
            "kind": "DisplayInfo",
            "info": {
                "kind": "AllGoalsWarnings",
                "visibleGoals": [{ "kind": "CmpInType" }],
                "invisibleGoals": [],
                "warnings": [],
                "errors": [],
            },
        })];
        let error =
            Response::parse_all(&raw).expect_err("unmodelled visible-goal kinds must hard-fail");

        assert!(
            format!("{error}").contains("CmpInType"),
            "error should name the unmodelled goal kind: {error}"
        );
    }

    #[test]
    fn response_without_kind_is_hard_error() {
        let raw = vec![serde_json::json!({ "foo": "bar" })];
        let error = Response::parse_all(&raw).expect_err("missing kind must hard-fail");
        let message = format!("{error}");
        assert!(
            message.contains("kind") || message.contains("variant"),
            "error should hint at missing variant tag: {message}"
        );
    }

    #[test]
    fn parse_error_includes_response_index_in_batch() {
        let raw = vec![
            serde_json::json!({ "kind": "ClearRunningInfo" }),
            serde_json::json!({ "kind": "Bogus" }),
        ];
        let error = Response::parse_all(&raw).expect_err("second response should fail");
        assert_eq!(error.index, 1);
    }

    #[test]
    fn known_noise_kinds_accept_arbitrary_bodies() {
        // We deliberately do not model the bodies of these kinds; they should
        // still parse so that an incoming Resp_HighlightingInfo or
        // Resp_MakeCase from Agda doesn't tank the whole batch.
        let raw = vec![
            serde_json::json!({
                "kind": "HighlightingInfo",
                "direct": false,
                "filepath": "/tmp/whatever",
            }),
            serde_json::json!({
                "kind": "MakeCase",
                "interactionPoint": 0,
                "variant": "Function",
                "clauses": ["foo = bar"],
            }),
        ];
        let parsed = Response::parse_all(&raw).expect("known noise kinds should parse");
        assert!(matches!(parsed[0], Response::HighlightingInfo { .. }));
        assert!(matches!(parsed[1], Response::MakeCase { .. }));
    }
}
