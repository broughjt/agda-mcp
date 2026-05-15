//! Typed views over Agda's `--interaction-json` responses.
//!
//! Every Agda response is also retained as a raw `serde_json::Value` upstream
//! so the MCP layer can surface unrecognised kinds untouched. The types below
//! deliberately fall through to `Unknown(Value)` whenever Agda emits a kind we
//! do not model (or model only partially), so future Agda versions cannot break
//! the server.
//!
//! Mirrors Agda's `Resp_*` constructors and their JSON encoder:
//! https://github.com/agda/agda/blob/3b57742a311b3a90b755737968d437f1ef902318/src/full/Agda/Interaction/JSONTop.hs#L438-L483

use serde::Deserialize;
use serde_json::Value;

/// A single response object Agda emits between `JSON> ` prompts.
#[derive(Debug, Clone, Deserialize)]
#[serde(untagged)]
pub enum Response {
    Known(Box<KnownResponse>),
    Unknown(Value),
}

impl Response {
    pub fn from_value(value: &Value) -> Self {
        match serde_json::from_value::<KnownResponse>(value.clone()) {
            Ok(known) => Self::Known(Box::new(known)),
            Err(_) => Self::Unknown(value.clone()),
        }
    }

    pub fn parse_all(values: &[Value]) -> Vec<Self> {
        values.iter().map(Self::from_value).collect()
    }
}

#[derive(Debug, Clone, Deserialize)]
#[serde(tag = "kind")]
pub enum KnownResponse {
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
#[derive(Debug, Clone, Deserialize)]
#[serde(untagged)]
pub enum Info {
    Known(Box<KnownInfo>),
    Unknown(Value),
}

#[derive(Debug, Clone, Deserialize)]
#[serde(tag = "kind")]
pub enum KnownInfo {
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
/// only model `OfType` (the common case for `Cmd_load`); everything else
/// falls through to `Unknown(Value)`.
#[derive(Debug, Clone, Deserialize)]
#[serde(untagged)]
pub enum VisibleGoal {
    Known(KnownVisibleGoal),
    Unknown(Value),
}

#[derive(Debug, Clone, Deserialize)]
#[serde(tag = "kind")]
pub enum KnownVisibleGoal {
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

#[cfg(test)]
mod tests {
    use super::*;

    use crate::agda::process::parse_json_responses;

    /// Verbatim copy of `~/scratch/agda/test/interaction/ParenJSON.out` from the
    /// upstream Agda repo (commit 3b57742). Kept inline so the unit test is
    /// self-contained and survives reorganising the source tree.
    const PAREN_JSON_OUT: &str = "JSON> {\"kind\":\"Status\",\"status\":{\"checked\":false,\"showImplicitArguments\":false,\"showIrrelevantArguments\":false}}\n{\"kind\":\"ClearRunningInfo\"}\n{\"kind\":\"ClearHighlighting\",\"tokenBased\":\"NotOnlyTokenBased\"}\n{\"kind\":\"Status\",\"status\":{\"checked\":false,\"showImplicitArguments\":false,\"showIrrelevantArguments\":false}}\n{\"info\":{\"errors\":[],\"invisibleGoals\":[],\"kind\":\"AllGoalsWarnings\",\"visibleGoals\":[{\"constraintObj\":{\"id\":0,\"range\":[{\"end\":{\"col\":15,\"line\":10,\"pos\":110},\"start\":{\"col\":10,\"line\":10,\"pos\":105}}]},\"kind\":\"OfType\",\"type\":\"P a\"}],\"warnings\":[]},\"kind\":\"DisplayInfo\"}\n{\"interactionPoints\":[{\"id\":0,\"range\":[{\"end\":{\"col\":15,\"line\":10,\"pos\":110},\"start\":{\"col\":10,\"line\":10,\"pos\":105}}]}],\"kind\":\"InteractionPoints\"}\n";

    fn parse_known(value: Value) -> KnownResponse {
        match Response::from_value(&value) {
            Response::Known(known) => *known,
            Response::Unknown(value) => {
                panic!("expected Known response, got Unknown({value})")
            }
        }
    }

    #[test]
    fn parses_paren_json_load_transcript() {
        let raw = parse_json_responses(PAREN_JSON_OUT).expect("ParenJSON output should be JSON");
        let parsed = Response::parse_all(&raw);

        let kinds: Vec<&'static str> = parsed
            .iter()
            .map(|response| match response {
                Response::Known(known) => match known.as_ref() {
                    KnownResponse::Status { .. } => "Status",
                    KnownResponse::ClearRunningInfo => "ClearRunningInfo",
                    KnownResponse::ClearHighlighting { .. } => "ClearHighlighting",
                    KnownResponse::RunningInfo { .. } => "RunningInfo",
                    KnownResponse::DisplayInfo { .. } => "DisplayInfo",
                    KnownResponse::InteractionPoints { .. } => "InteractionPoints",
                    KnownResponse::GiveAction { .. } => "GiveAction",
                    KnownResponse::JumpToError { .. } => "JumpToError",
                    KnownResponse::DoneAborting => "DoneAborting",
                    KnownResponse::DoneExiting => "DoneExiting",
                },
                Response::Unknown(_) => "Unknown",
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
        let parsed = Response::parse_all(&raw);

        let display_info = parsed
            .into_iter()
            .find_map(|response| match response {
                Response::Known(boxed) => match *boxed {
                    KnownResponse::DisplayInfo { info } => Some(info),
                    _ => None,
                },
                Response::Unknown(_) => None,
            })
            .expect("transcript should contain a DisplayInfo response");

        let Info::Known(boxed) = display_info else {
            panic!("expected a known DisplayInfo.info kind");
        };
        let KnownInfo::AllGoalsWarnings {
            visible_goals,
            invisible_goals,
            warnings,
            errors,
        } = *boxed
        else {
            panic!("expected AllGoalsWarnings");
        };

        assert!(invisible_goals.is_empty());
        assert!(warnings.is_empty());
        assert!(errors.is_empty());
        assert_eq!(visible_goals.len(), 1);

        let VisibleGoal::Known(KnownVisibleGoal::OfType { constraint_obj, ty }) = &visible_goals[0]
        else {
            panic!(
                "expected an OfType visible goal, got: {:?}",
                visible_goals[0]
            );
        };
        assert_eq!(constraint_obj.id, 0);
        assert_eq!(ty, "P a");
        assert_eq!(constraint_obj.range.len(), 1);
        assert_eq!(constraint_obj.range[0].start.pos, 105);
        assert_eq!(constraint_obj.range[0].end.pos, 110);
    }

    #[test]
    fn extracts_interaction_points_from_paren_json() {
        let raw = parse_json_responses(PAREN_JSON_OUT).expect("ParenJSON output should be JSON");
        let parsed = Response::parse_all(&raw);

        let points = parsed
            .into_iter()
            .find_map(|response| match response {
                Response::Known(boxed) => match *boxed {
                    KnownResponse::InteractionPoints { points } => Some(points),
                    _ => None,
                },
                Response::Unknown(_) => None,
            })
            .expect("transcript should contain an InteractionPoints response");

        assert_eq!(points.len(), 1);
        assert_eq!(points[0].id, 0);
        assert_eq!(points[0].range[0].start.line, 10);
        assert_eq!(points[0].range[0].start.col, 10);
    }

    #[test]
    fn parses_give_action_with_string_result() {
        let known = parse_known(serde_json::json!({
            "kind": "GiveAction",
            "interactionPoint": 7,
            "giveResult": { "str": "suc zero" },
        }));

        let KnownResponse::GiveAction {
            interaction_point,
            give_result,
        } = known
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
        let known = parse_known(serde_json::json!({
            "kind": "GiveAction",
            "interactionPoint": 0,
            "giveResult": { "paren": true },
        }));

        let KnownResponse::GiveAction { give_result, .. } = known else {
            panic!("expected GiveAction");
        };
        assert!(matches!(give_result, GiveResult::Paren { paren: true }));
    }

    #[test]
    fn parses_give_action_with_paren_false() {
        let known = parse_known(serde_json::json!({
            "kind": "GiveAction",
            "interactionPoint": 0,
            "giveResult": { "paren": false },
        }));

        let KnownResponse::GiveAction { give_result, .. } = known else {
            panic!("expected GiveAction");
        };
        assert!(matches!(give_result, GiveResult::Paren { paren: false }));
    }

    #[test]
    fn parses_version_display_info() {
        let known = parse_known(serde_json::json!({
            "kind": "DisplayInfo",
            "info": { "kind": "Version", "version": "2.7.0-abc123" },
        }));

        let KnownResponse::DisplayInfo {
            info: Info::Known(boxed),
        } = known
        else {
            panic!("expected DisplayInfo with Known info");
        };
        let KnownInfo::Version { version } = *boxed else {
            panic!("expected Version info");
        };
        assert_eq!(version, "2.7.0-abc123");
    }

    #[test]
    fn parses_error_display_info() {
        let known = parse_known(serde_json::json!({
            "kind": "DisplayInfo",
            "info": {
                "kind": "Error",
                "warnings": [],
                "error": { "message": "Not in scope: foo" },
            },
        }));

        let KnownResponse::DisplayInfo {
            info: Info::Known(boxed),
        } = known
        else {
            panic!("expected DisplayInfo with Known info");
        };
        let KnownInfo::Error { warnings, error } = *boxed else {
            panic!("expected Error info");
        };
        assert!(warnings.is_empty());
        assert_eq!(error.message, "Not in scope: foo");
    }

    #[test]
    fn unknown_top_level_kind_falls_through_to_unknown() {
        let response = Response::from_value(&serde_json::json!({
            "kind": "SomeFutureResponse",
            "payload": 42,
        }));
        assert!(matches!(response, Response::Unknown(_)));
    }

    #[test]
    fn unknown_display_info_kind_falls_through_to_unknown_info() {
        let known = parse_known(serde_json::json!({
            "kind": "DisplayInfo",
            "info": { "kind": "Time", "time": "0.01s" },
        }));

        let KnownResponse::DisplayInfo { info } = known else {
            panic!("expected DisplayInfo");
        };
        assert!(matches!(info, Info::Unknown(_)));
    }

    #[test]
    fn response_without_kind_falls_through_to_unknown() {
        let response = Response::from_value(&serde_json::json!({ "foo": "bar" }));
        assert!(matches!(response, Response::Unknown(_)));
    }
}
