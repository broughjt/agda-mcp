//! Types for Agda's `--interaction-json` responses.
//!
//! Every Agda response kind we know exists is enumerated explicitly. If Agda
//! ever emits something we have not modelled, parsing hard-fails with a
//! `serde_json` error that names the unknown variant or missing field. This
//! acts as a signal to update this module. Tolerating unknown kinds silently
//! would just push the broken JSON to the MCP caller, who has no way to act on
//! it.
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
use thiserror::Error;

use crate::agda::process::PROMPT;
use crate::agda::source::Interval;

/// A single response object Agda emits between `JSON> ` prompts.
///
/// Variants are listed in the same order as Agda's `Response_boot` constructors:
/// https://github.com/agda/agda/blob/3b57742a311b3a90b755737968d437f1ef902318/src/full/Agda/Interaction/Response/Base.hs#L51-L78
#[derive(Debug, Clone, Deserialize)]
#[serde(tag = "kind")]
pub enum Response {
    /// Highlighting payload (body intentionally unmodelled)
    HighlightingInfo {},
    Status {
        status: Status,
    },
    JumpToError {
        filepath: String,
        position: i32,
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
    /// Make-case clauses (body intentionally unmodelled)
    MakeCase {},
    /// Solve-all solutions (body intentionally unmodelled)
    SolveAll {},
    /// Mimer solution (body intentionally unmodelled)
    Mimer {},
    DisplayInfo {
        info: Info,
    },
    RunningInfo {
        #[serde(rename = "debugLevel")]
        debug_level: u32,
        message: String,
    },
    ClearRunningInfo,
    ClearHighlighting {
        #[serde(rename = "tokenBased")]
        token_based: String,
    },
    DoneAborting,
    DoneExiting,
}

/// Parse and collect responses. Each response is JSON terminated by a newline.
pub fn parse_all(output: &str) -> Result<Vec<Response>, ParseError> {
    response_lines(output)
        .map(|(index, line)| {
            serde_json::from_str(line).map_err(|source| ParseError {
                index,
                raw: line.to_owned(),
                source,
            })
        })
        .collect()
}

/// Parse Agda's prompt-delimited output into a list of raw JSON values, one
/// per response line. Preserves the original JSON structure exactly so that
/// callers can pass it through to MCP clients for debugging.
pub fn parse_raw_values(output: &str) -> Result<Vec<serde_json::Value>, ParseError> {
    response_lines(output)
        .map(|(index, line)| {
            serde_json::from_str(line).map_err(|source| ParseError {
                index,
                raw: line.to_owned(),
                source,
            })
        })
        .collect()
}

fn response_lines(output: &str) -> impl Iterator<Item = (usize, &str)> {
    output
        .lines()
        .filter_map(|line| {
            let line = line.strip_prefix(PROMPT).unwrap_or(line).trim();
            (!line.is_empty()).then_some(line)
        })
        .enumerate()
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
/// Visible goals are output constraints whose constraint object is an
/// interaction point. These are the goals an editor can act on with commands
/// such as give/refine.
pub type VisibleGoal = OutputConstraint<InteractionPoint>;

/// An invisible goal in `AllGoalsWarnings.invisibleGoals`.
///
/// Invisible goals are output constraints whose constraint object is a named
/// meta-variable. They are diagnostics for unsolved hidden/internal metas, not
/// editor interaction points.
pub type InvisibleGoal = OutputConstraint<NamedMeta>;

/// An Agda `OutputConstraint` as emitted in JSON for visible and invisible
/// goals.
///
/// The type parameter is the JSON shape of the constraint object:
/// [`InteractionPoint`] for visible goals and [`NamedMeta`] for invisible
/// goals. Pretty-printed Agda terms/types are represented as strings because
/// that is how `Agda.Interaction.JSONTop` encodes them.
#[derive(Debug, Clone, Deserialize)]
#[serde(tag = "kind")]
pub enum OutputConstraint<C> {
    OfType {
        #[serde(rename = "constraintObj")]
        constraint_obj: C,
        #[serde(rename = "type")]
        _type: String,
    },
    CmpInType {
        comparison: String,
        #[serde(rename = "type")]
        _type: String,
        #[serde(rename = "constraintObjs")]
        constraint_objs: Vec<C>,
    },
    CmpElim {
        polarities: Vec<String>,
        #[serde(rename = "type")]
        _type: String,
        #[serde(rename = "constraintObjs")]
        constraint_objs: Vec<Vec<C>>,
    },
    JustType {
        #[serde(rename = "constraintObj")]
        constraint_obj: C,
    },
    JustSort {
        #[serde(rename = "constraintObj")]
        constraint_obj: C,
    },
    CmpTypes {
        comparison: String,
        #[serde(rename = "constraintObjs")]
        constraint_objs: Vec<C>,
    },
    CmpLevels {
        comparison: String,
        #[serde(rename = "constraintObjs")]
        constraint_objs: Vec<C>,
    },
    CmpTeles {
        comparison: String,
        #[serde(rename = "constraintObjs")]
        constraint_objs: Vec<C>,
    },
    CmpSorts {
        comparison: String,
        #[serde(rename = "constraintObjs")]
        constraint_objs: Vec<C>,
    },
    Assign {
        #[serde(rename = "constraintObj")]
        constraint_obj: C,
        value: String,
    },
    TypedAssign {
        #[serde(rename = "constraintObj")]
        constraint_obj: C,
        value: String,
        #[serde(rename = "type")]
        _type: String,
    },
    PostponedCheckArgs {
        #[serde(rename = "constraintObj")]
        constraint_obj: C,
        #[serde(rename = "ofType")]
        of_type: String,
        arguments: Vec<String>,
        #[serde(rename = "type")]
        _type: String,
    },
    IsEmptyType {
        #[serde(rename = "type")]
        _type: String,
    },
    SizeLtSat {
        #[serde(rename = "type")]
        _type: String,
    },
    FindInstanceOF {
        #[serde(rename = "constraintObj")]
        constraint_obj: C,
        candidates: Vec<Candidate>,
        #[serde(rename = "type")]
        _type: String,
    },
    ResolveInstanceOF {
        name: String,
    },
    PTSInstance {
        #[serde(rename = "constraintObjs")]
        constraint_objs: Vec<C>,
    },
    PostponedCheckFunDef {
        name: String,
        #[serde(rename = "type")]
        _type: String,
        error: Message,
    },
    DataSort {
        name: String,
        sort: String,
    },
    CheckLock {
        head: String,
        lock: String,
    },
    UsableAtMod {
        #[serde(rename = "mod")]
        module: String,
        term: String,
    },
}

#[derive(Debug, Clone, Deserialize)]
pub struct Candidate {
    pub value: String,
    #[serde(rename = "type")]
    pub _type: String,
}

#[derive(Debug, Clone, Deserialize)]
pub struct NamedMeta {
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
#[error("failed to parse Agda response #{index}: {source}\nraw response: {raw}")]
pub struct ParseError {
    pub index: usize,
    pub raw: String,
    #[source]
    pub source: serde_json::Error,
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Verbatim copy of `~/scratch/agda/test/interaction/ParenJSON.out` from the
    /// upstream Agda repo (commit 3b57742). Kept inline so the unit test is
    /// self-contained and survives reorganising the source tree.
    const PAREN_JSON_OUT: &str = "JSON> {\"kind\":\"Status\",\"status\":{\"checked\":false,\"showImplicitArguments\":false,\"showIrrelevantArguments\":false}}\n{\"kind\":\"ClearRunningInfo\"}\n{\"kind\":\"ClearHighlighting\",\"tokenBased\":\"NotOnlyTokenBased\"}\n{\"kind\":\"Status\",\"status\":{\"checked\":false,\"showImplicitArguments\":false,\"showIrrelevantArguments\":false}}\n{\"info\":{\"errors\":[],\"invisibleGoals\":[],\"kind\":\"AllGoalsWarnings\",\"visibleGoals\":[{\"constraintObj\":{\"id\":0,\"range\":[{\"end\":{\"col\":15,\"line\":10,\"pos\":110},\"start\":{\"col\":10,\"line\":10,\"pos\":105}}]},\"kind\":\"OfType\",\"type\":\"P a\"}],\"warnings\":[]},\"kind\":\"DisplayInfo\"}\n{\"interactionPoints\":[{\"id\":0,\"range\":[{\"end\":{\"col\":15,\"line\":10,\"pos\":110},\"start\":{\"col\":10,\"line\":10,\"pos\":105}}]}],\"kind\":\"InteractionPoints\"}\n";

    fn parse_one(line: &str) -> Response {
        serde_json::from_str(line).expect("response should parse")
    }

    #[test]
    fn parses_paren_json_load_transcript() {
        let parsed = parse_all(PAREN_JSON_OUT).expect("ParenJSON transcript should parse");

        let kinds: Vec<&'static str> = parsed
            .iter()
            .map(|response| match response {
                Response::HighlightingInfo { .. } => "HighlightingInfo",
                Response::Status { .. } => "Status",
                Response::JumpToError { .. } => "JumpToError",
                Response::InteractionPoints { .. } => "InteractionPoints",
                Response::GiveAction { .. } => "GiveAction",
                Response::MakeCase { .. } => "MakeCase",
                Response::SolveAll { .. } => "SolveAll",
                Response::Mimer { .. } => "Mimer",
                Response::DisplayInfo { .. } => "DisplayInfo",
                Response::RunningInfo { .. } => "RunningInfo",
                Response::ClearRunningInfo => "ClearRunningInfo",
                Response::ClearHighlighting { .. } => "ClearHighlighting",
                Response::DoneAborting => "DoneAborting",
                Response::DoneExiting => "DoneExiting",
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
        let parsed = parse_all(PAREN_JSON_OUT).expect("ParenJSON transcript should parse");

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

        let OutputConstraint::OfType {
            constraint_obj,
            _type,
        } = &visible_goals[0]
        else {
            panic!("expected OfType visible goal")
        };
        assert_eq!(constraint_obj.id, 0);
        assert_eq!(_type, "P a");
        assert_eq!(constraint_obj.range.len(), 1);
        assert_eq!(constraint_obj.range[0].start.pos, 105);
        assert_eq!(constraint_obj.range[0].end.pos, 110);
    }

    #[test]
    fn extracts_interaction_points_from_paren_json() {
        let parsed = parse_all(PAREN_JSON_OUT).expect("ParenJSON transcript should parse");

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
        let response = parse_one(
            r#"{"kind":"GiveAction","interactionPoint":7,"giveResult":{"str":"suc zero"}}"#,
        );

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
        let response =
            parse_one(r#"{"kind":"GiveAction","interactionPoint":0,"giveResult":{"paren":true}}"#);

        let Response::GiveAction { give_result, .. } = response else {
            panic!("expected GiveAction");
        };
        assert!(matches!(give_result, GiveResult::Paren { paren: true }));
    }

    #[test]
    fn parses_give_action_with_paren_false() {
        let response =
            parse_one(r#"{"kind":"GiveAction","interactionPoint":0,"giveResult":{"paren":false}}"#);

        let Response::GiveAction { give_result, .. } = response else {
            panic!("expected GiveAction");
        };
        assert!(matches!(give_result, GiveResult::Paren { paren: false }));
    }

    #[test]
    fn parses_version_display_info() {
        let response = parse_one(
            r#"{"kind":"DisplayInfo","info":{"kind":"Version","version":"2.7.0-abc123"}}"#,
        );

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
        let response = parse_one(
            r#"{"kind":"DisplayInfo","info":{"kind":"Error","warnings":[],"error":{"message":"Not in scope: foo"}}}"#,
        );

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
        let error = parse_all(r#"{"kind":"SomeFutureResponse","payload":42}"#)
            .expect_err("unknown kinds must hard-fail");

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
        let error = parse_all(r#"{"kind":"DisplayInfo","info":{"kind":"Time","time":"0.01s"}}"#)
            .expect_err("unmodelled DisplayInfo kinds must hard-fail");

        assert_eq!(error.index, 0);
        assert!(
            format!("{error}").contains("Time"),
            "error should name the unmodelled info kind: {error}"
        );
    }

    #[test]
    fn parses_invisible_of_type_goal() {
        let response = parse_one(
            r#"{"kind":"DisplayInfo","info":{"kind":"AllGoalsWarnings","visibleGoals":[],"invisibleGoals":[{"kind":"OfType","constraintObj":{"name":"_0","range":[{"start":{"pos":95,"line":6,"col":7},"end":{"pos":96,"line":6,"col":8}}]},"type":"Type"}],"warnings":[],"errors":[]}}"#,
        );

        let Response::DisplayInfo {
            info: Info::AllGoalsWarnings {
                invisible_goals, ..
            },
        } = response
        else {
            panic!("expected DisplayInfo/AllGoalsWarnings");
        };

        assert_eq!(invisible_goals.len(), 1);
        let OutputConstraint::OfType {
            constraint_obj,
            _type,
        } = &invisible_goals[0]
        else {
            panic!("expected OfType invisible goal")
        };
        assert_eq!(constraint_obj.name, "_0");
        assert_eq!(constraint_obj.range[0].start.line, 6);
        assert_eq!(constraint_obj.range[0].start.col, 7);
        assert_eq!(_type, "Type");
    }

    #[test]
    fn parses_visible_non_of_type_goal() {
        let response = parse_one(
            r#"{"kind":"DisplayInfo","info":{"kind":"AllGoalsWarnings","visibleGoals":[{"kind":"CmpInType","comparison":"CmpEq","type":"Nat","constraintObjs":[{"id":0,"range":[]},{"id":1,"range":[]}]}],"invisibleGoals":[],"warnings":[],"errors":[]}}"#,
        );

        let Response::DisplayInfo {
            info: Info::AllGoalsWarnings { visible_goals, .. },
        } = response
        else {
            panic!("expected DisplayInfo/AllGoalsWarnings");
        };

        assert_eq!(visible_goals.len(), 1);
        let OutputConstraint::CmpInType {
            comparison,
            _type,
            constraint_objs,
        } = &visible_goals[0]
        else {
            panic!("expected CmpInType visible goal")
        };
        assert_eq!(comparison, "CmpEq");
        assert_eq!(_type, "Nat");
        assert_eq!(constraint_objs.len(), 2);
        assert_eq!(constraint_objs[0].id, 0);
        assert_eq!(constraint_objs[1].id, 1);
    }

    #[test]
    fn unknown_output_constraint_kind_is_hard_error() {
        let error = parse_all(
            r#"{"kind":"DisplayInfo","info":{"kind":"AllGoalsWarnings","visibleGoals":[{"kind":"FutureConstraint"}],"invisibleGoals":[],"warnings":[],"errors":[]}}"#,
        )
        .expect_err("unknown output-constraint kinds must hard-fail");

        assert!(
            format!("{error}").contains("FutureConstraint"),
            "error should name the unknown output-constraint kind: {error}"
        );
    }

    #[test]
    fn response_without_kind_is_hard_error() {
        let error = parse_all(r#"{"foo":"bar"}"#).expect_err("missing kind must hard-fail");
        let message = format!("{error}");
        assert!(
            message.contains("kind") || message.contains("variant"),
            "error should hint at missing variant tag: {message}"
        );
    }

    #[test]
    fn parse_error_includes_response_index_in_batch() {
        let error = parse_all("{\"kind\":\"ClearRunningInfo\"}\n{\"kind\":\"Bogus\"}\n")
            .expect_err("second response should fail");
        assert_eq!(error.index, 1);
    }

    #[test]
    fn known_noise_kinds_accept_arbitrary_bodies() {
        // We deliberately do not model the bodies of these kinds; they should
        // still parse so that an incoming Resp_HighlightingInfo or
        // Resp_MakeCase from Agda doesn't tank the whole batch.
        let parsed = parse_all(
            "{\"kind\":\"HighlightingInfo\",\"direct\":false,\"filepath\":\"/tmp/whatever\"}\n{\"kind\":\"MakeCase\",\"interactionPoint\":0,\"variant\":\"Function\",\"clauses\":[\"foo = bar\"]}\n",
        )
        .expect("known noise kinds should parse");
        assert!(matches!(parsed[0], Response::HighlightingInfo { .. }));
        assert!(matches!(parsed[1], Response::MakeCase { .. }));
    }
}
