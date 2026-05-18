use std::{fmt, mem};

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::agda::{
    command::Command,
    response::{Info, InteractionPoint, Message, NamedMeta, OutputConstraint, Response, Status},
    source::Interval,
};

/// Parameters for the MCP `load` tool.
#[derive(Debug, Deserialize, JsonSchema)]
pub struct LoadRequest {
    /// Path to the Agda file to load.
    pub path: String,
    // TODO: Whether to use the command line flags here?
}

impl LoadRequest {
    pub fn to_command(&self) -> Command<'_> {
        Command::load(&self.path, &[])
    }
}

/// Summary returned to the MCP client for a `load` call.
#[derive(Debug, Clone, Serialize, JsonSchema)]
pub struct LoadResponse {
    /// Agda's final `Status.checked` value after the load.
    ///
    /// This is `true` only when Agda considers the file checked. A file with
    /// open interaction goals can load without hard errors while still having
    /// `checked = false`.
    pub checked: bool,
    /// Visible `OfType` interaction goals after the load, in the order Agda reported.
    ///
    /// These are the actionable goals that can be passed to editor commands
    /// such as `give`/`refine`.
    pub goals: Vec<Goal>,
    /// Other visible output constraints Agda reported.
    ///
    /// These are associated with interaction points, but are not the simple `id
    /// : type` goal shape represented by [`Goal`]. The full
    /// [`OutputConstraint`] payload is preserved so we can format it in
    /// `Display` the way Agda does.
    pub visible_constraints: Vec<OutputConstraint<InteractionPoint>>,
    /// Invisible/hidden unsolved metas Agda reported.
    ///
    /// These are diagnostics only: they are not interaction points and cannot
    /// be passed to `give`. The full [`OutputConstraint`] payload is preserved
    /// so we can format them in `Display` the way Agda does.
    pub invisible_goals: Vec<OutputConstraint<NamedMeta>>,
    /// Non-fatal warnings reported by Agda.
    pub warnings: Vec<String>,
    /// Errors reported by Agda.
    pub errors: Vec<String>,
}

impl fmt::Display for LoadResponse {
    /// Render the load result textually.
    ///
    /// This text is intentionally close to Agda's Emacs info buffer: goals
    /// first, then grouped errors, then grouped warnings, but without the long
    /// horizontal rule delimiters.
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        let has_goal_lines = !self.goals.is_empty()
            || !self.invisible_goals.is_empty()
            || !self.visible_constraints.is_empty();

        if !has_goal_lines && self.errors.is_empty() && self.warnings.is_empty() {
            return if self.checked {
                formatter.write_str("Checked. No goals, warnings, or errors.")
            } else {
                formatter.write_str(
                    "Loaded, but Agda reports `checked=false`. No goals, invisible goals, constraints, warnings, or errors.",
                )
            };
        }

        let mut wrote_any = false;
        let mut write_line = |formatter: &mut fmt::Formatter<'_>, line: &dyn fmt::Display| {
            if wrote_any {
                formatter.write_str("\n")?;
            }
            wrote_any = true;
            write!(formatter, "{line}")
        };

        for goal in &self.goals {
            write_line(formatter, goal)?;
        }
        for constraint in &self.invisible_goals {
            write_line(formatter, &InvisibleEntry(constraint))?;
        }
        for constraint in &self.visible_constraints {
            write_line(formatter, constraint)?;
        }

        if has_goal_lines && (!self.errors.is_empty() || !self.warnings.is_empty()) {
            formatter.write_str("\n\n")?;
        }

        match self.errors.as_slice() {
            [] => {}
            [error] => write!(formatter, "Error:\n{error}")?,
            [first_error, remaining_errors @ ..] => {
                write!(formatter, "Errors:\n{first_error}")?;
                for error in remaining_errors {
                    write!(formatter, "\n\n{error}")?;
                }
            }
        }

        if !self.errors.is_empty() && !self.warnings.is_empty() {
            formatter.write_str("\n\n")?;
        }

        match self.warnings.as_slice() {
            [] => {}
            [warning] => write!(formatter, "Warning:\n{warning}")?,
            [first_warning, remaining_warnings @ ..] => {
                write!(formatter, "Warnings:\n{first_warning}")?;
                for warning in remaining_warnings {
                    write!(formatter, "\n\n{warning}")?;
                }
            }
        }

        Ok(())
    }
}

impl TryFrom<Vec<Response>> for LoadResponse {
    type Error = LoadResponseError;

    fn try_from(mut responses: Vec<Response>) -> Result<Self, Self::Error> {
        match &mut responses[..] {
            [
                Response::Status { status: _ },
                Response::ClearRunningInfo,
                Response::ClearHighlighting { token_based: _ },
                Response::Status {
                    status:
                        Status {
                            checked,
                            show_implicit_arguments: _,
                            show_irrelevant_arguments: _,
                        },
                },
                Response::DisplayInfo {
                    info:
                        Info::AllGoalsWarnings {
                            visible_goals,
                            invisible_goals,
                            warnings,
                            errors,
                        },
                },
                Response::InteractionPoints { points: _ },
            ] => {
                let visible_goals = mem::take(visible_goals);
                let invisible_goals = mem::take(invisible_goals);
                let warnings = mem::take(warnings);
                let errors = mem::take(errors);

                let mut goals = Vec::new();
                let mut visible_constraints = Vec::new();
                for constraint in visible_goals {
                    match constraint {
                        OutputConstraint::OfType {
                            constraint_obj: InteractionPoint { id, range },
                            _type,
                        } => goals.push(Goal { id, range, _type }),
                        other => visible_constraints.push(other),
                    }
                }

                Ok(LoadResponse {
                    checked: *checked,
                    goals,
                    visible_constraints,
                    invisible_goals,
                    warnings: warnings
                        .into_iter()
                        .map(|Message { message }| message)
                        .collect(),
                    errors: errors
                        .into_iter()
                        .map(|Message { message }| message)
                        .collect(),
                })
            }
            [
                Response::Status { status: _ },
                Response::ClearRunningInfo,
                Response::ClearHighlighting { token_based: _ },
                Response::DisplayInfo {
                    info: Info::Error { warnings, error },
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
                Response::Status { status: _ },
                Response::ClearRunningInfo,
                Response::ClearHighlighting { token_based: _ },
                Response::DisplayInfo {
                    info: Info::Error { warnings, error },
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
                let warnings = mem::take(warnings);
                let error = mem::take(&mut error.message);

                Ok(LoadResponse {
                    checked: false,
                    goals: Vec::new(),
                    visible_constraints: Vec::new(),
                    invisible_goals: Vec::new(),
                    warnings: warnings
                        .into_iter()
                        .map(|Message { message }| message)
                        .collect(),
                    errors: vec![error],
                })
            }
            _ => Err(LoadResponseError(responses)),
        }
    }
}

#[derive(Debug, Error)]
#[error("unexpected Agda response sequence ({} responses)", .0.len())]
pub struct LoadResponseError(pub Vec<Response>);

/// A visible interaction goal in the current file.
#[derive(Debug, Clone, Serialize, JsonSchema)]
pub struct Goal {
    pub id: u32,
    pub range: Vec<Interval>,
    #[serde(rename = "type")]
    pub _type: String,
}

impl fmt::Display for Goal {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "?{} : {}", self.id, self._type)
    }
}

/// Display adapter that renders an invisible goal as Agda's Emacs goal buffer
/// does: the constraint prose followed by `[ at <range> ]` when a range is
/// available.
///
/// Mirrors `pr` in Agda's `prettyGoals`:
/// https://github.com/agda/agda/blob/3b57742a311b3a90b755737968d437f1ef902318/src/full/Agda/Interaction/BasicOps.hs#L862-L867
struct InvisibleEntry<'a>(&'a OutputConstraint<NamedMeta>);

impl fmt::Display for InvisibleEntry<'_> {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn invisible_meta_range(
            constraint: &OutputConstraint<NamedMeta>,
        ) -> Option<RangeDisplay<'_>> {
            let intervals: &[Interval] = match constraint {
                OutputConstraint::OfType { constraint_obj, .. }
                | OutputConstraint::JustType { constraint_obj }
                | OutputConstraint::JustSort { constraint_obj }
                | OutputConstraint::Assign { constraint_obj, .. }
                | OutputConstraint::TypedAssign { constraint_obj, .. }
                | OutputConstraint::PostponedCheckArgs { constraint_obj, .. }
                | OutputConstraint::FindInstanceOF { constraint_obj, .. } => &constraint_obj.range,
                OutputConstraint::CmpInType {
                    constraint_objs, ..
                }
                | OutputConstraint::CmpTypes {
                    constraint_objs, ..
                }
                | OutputConstraint::CmpLevels {
                    constraint_objs, ..
                }
                | OutputConstraint::CmpTeles {
                    constraint_objs, ..
                }
                | OutputConstraint::CmpSorts {
                    constraint_objs, ..
                }
                | OutputConstraint::PTSInstance { constraint_objs } => {
                    constraint_objs.first().map(|m| m.range.as_slice())?
                }
                OutputConstraint::CmpElim {
                    constraint_objs, ..
                } => constraint_objs
                    .iter()
                    .flatten()
                    .next()
                    .map(|m| m.range.as_slice())?,
                OutputConstraint::IsEmptyType { .. }
                | OutputConstraint::SizeLtSat { .. }
                | OutputConstraint::ResolveInstanceOF { .. }
                | OutputConstraint::PostponedCheckFunDef { .. }
                | OutputConstraint::DataSort { .. }
                | OutputConstraint::CheckLock { .. }
                | OutputConstraint::UsableAtMod { .. } => return None,
            };
            (!intervals.is_empty()).then_some(RangeDisplay(intervals))
        }

        write!(formatter, "{}", self.0)?;
        if let Some(range) = invisible_meta_range(self.0) {
            write!(formatter, " [ at {range} ]")?;
        }
        Ok(())
    }
}

/// Render a list of intervals in Agda's `<line>.<startCol>-<endCol>` form.
///
/// Mirrors Agda's `prettyInterval`:
/// https://github.com/agda/agda/blob/3b57742a311b3a90b755737968d437f1ef902318/src/full/Agda/Syntax/Common/Pretty.hs#L163-L182
struct RangeDisplay<'a>(&'a [Interval]);

impl fmt::Display for RangeDisplay<'_> {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        let first = self.0.first().expect("RangeDisplay is non-empty");
        let last = self.0.last().expect("RangeDisplay is non-empty");
        let (start, end) = (first.start, last.end);
        if start == end {
            write!(formatter, "{}.{}", start.line, start.col)
        } else if start.line == end.line {
            write!(formatter, "{}.{}-{}", start.line, start.col, end.col)
        } else {
            write!(
                formatter,
                "{}.{}-{}.{}",
                start.line, start.col, end.line, end.col
            )
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::agda::source::{Interval, Position};
    use serde_json::json;

    fn point(id: u32, start_pos: u32, end_pos: u32) -> serde_json::Value {
        json!({
            "id": id,
            "range": [{
                "start": { "pos": start_pos, "line": 5, "col": 5 },
                "end":   { "pos": end_pos,   "line": 5, "col": 5 + (end_pos - start_pos) },
            }],
        })
    }

    fn status(checked: bool) -> serde_json::Value {
        json!({
            "kind": "Status",
            "status": {
                "checked": checked,
                "showImplicitArguments": false,
                "showIrrelevantArguments": false,
            },
        })
    }

    fn all_goals_warnings(goals: serde_json::Value) -> serde_json::Value {
        json!({
            "kind": "DisplayInfo",
            "info": {
                "kind": "AllGoalsWarnings",
                "visibleGoals": goals,
                "invisibleGoals": [],
                "warnings": [],
                "errors": [],
            }
        })
    }

    fn interaction_points(points: serde_json::Value) -> serde_json::Value {
        json!({
            "kind": "InteractionPoints",
            "interactionPoints": points,
        })
    }

    fn parse_responses(raw: Vec<serde_json::Value>) -> Vec<Response> {
        raw.into_iter()
            .map(|value| serde_json::from_value::<Response>(value).unwrap())
            .collect()
    }

    #[test]
    fn load_output_extracts_visible_goals_and_checked_status() {
        let point = point(0, 100, 105);
        let responses = parse_responses(vec![
            status(false),
            json!({ "kind": "ClearRunningInfo" }),
            json!({ "kind": "ClearHighlighting", "tokenBased": "NotOnlyTokenBased" }),
            status(false),
            all_goals_warnings(json!([{
                "kind": "OfType",
                "constraintObj": point.clone(),
                "type": "Nat",
            }])),
            interaction_points(json!([point])),
        ]);

        let output = LoadResponse::try_from(responses).unwrap();

        assert!(!output.checked, "holes should keep checked=false");
        assert_eq!(output.goals.len(), 1);
        assert_eq!(output.goals[0].id, 0);
        assert_eq!(output.goals[0]._type, "Nat");
        assert_eq!(
            output.goals[0].range,
            vec![Interval::new(
                Position::new(100, 5, 5),
                Position::new(105, 5, 10)
            )]
        );
        assert!(output.errors.is_empty());
    }

    fn goal(id: u32, ty: &str, line: u32, col: u32) -> Goal {
        Goal {
            id,
            range: vec![Interval::new(
                Position::new(100 + id, line, col),
                Position::new(101 + id, line, col + 1),
            )],
            _type: ty.to_owned(),
        }
    }

    fn invisible_goal(name: &str, ty: &str, line: u32, col: u32) -> OutputConstraint<NamedMeta> {
        OutputConstraint::OfType {
            constraint_obj: NamedMeta {
                name: name.to_owned(),
                range: vec![Interval::new(
                    Position::new(100, line, col),
                    Position::new(101, line, col + 1),
                )],
            },
            _type: ty.to_owned(),
        }
    }

    fn just_sort(id: u32, line: u32, col: u32) -> OutputConstraint<InteractionPoint> {
        OutputConstraint::JustSort {
            constraint_obj: InteractionPoint {
                id,
                range: vec![Interval::new(
                    Position::new(100 + id, line, col),
                    Position::new(101 + id, line, col + 1),
                )],
            },
        }
    }

    fn cmp_in_type(comparison: &str, ty: &str, ids: &[u32]) -> OutputConstraint<InteractionPoint> {
        OutputConstraint::CmpInType {
            comparison: comparison.to_owned(),
            _type: ty.to_owned(),
            constraint_objs: ids
                .iter()
                .map(|&id| InteractionPoint {
                    id,
                    range: Vec::new(),
                })
                .collect(),
        }
    }

    fn load_response(
        checked: bool,
        goals: Vec<Goal>,
        invisible_goals: Vec<OutputConstraint<NamedMeta>>,
        visible_constraints: Vec<OutputConstraint<InteractionPoint>>,
        warnings: Vec<&str>,
        errors: Vec<&str>,
    ) -> LoadResponse {
        LoadResponse {
            checked,
            goals,
            visible_constraints,
            invisible_goals,
            warnings: warnings.into_iter().map(str::to_owned).collect(),
            errors: errors.into_iter().map(str::to_owned).collect(),
        }
    }

    #[test]
    fn display_scenario_successful_typecheck_no_holes() {
        let output = load_response(true, vec![], vec![], vec![], vec![], vec![]);

        assert_eq!(
            output.to_string(),
            "Checked. No goals, warnings, or errors."
        );
    }

    #[test]
    fn display_scenario_successful_load_with_holes() {
        let output = load_response(
            false,
            vec![goal(0, "Unit", 9, 5)],
            vec![],
            vec![],
            vec![],
            vec![],
        );

        assert_eq!(output.to_string(), "?0 : Unit");
    }

    #[test]
    fn display_scenario_type_error_and_holes() {
        let message = "/home/jackson/repositories/hott-reals/source/MCPExamples/TypeErrorAndHole.agda:15.7-11: error: [UnequalTerms]\nBool !=< Unit\nwhen checking that the expression true has type Unit";
        let output = load_response(false, vec![], vec![], vec![], vec![], vec![message]);

        assert_eq!(output.to_string(), format!("Error:\n{message}"));
    }

    #[test]
    fn display_scenario_type_error_no_holes() {
        let message = "/home/jackson/repositories/hott-reals/source/MCPExamples/TypeErrorNoHoles.agda:12.7-11: error: [UnequalTerms]\nBool !=< Unit\nwhen checking that the expression true has type Unit";
        let output = load_response(false, vec![], vec![], vec![], vec![], vec![message]);

        assert_eq!(output.to_string(), format!("Error:\n{message}"));
    }

    #[test]
    fn display_scenario_type_warning() {
        let message = "/home/jackson/repositories/hott-reals/source/MCPExamples/Warning.agda:3.1-8: warning: -W[no]EmptyPrivate\nEmpty private block.";
        let output = load_response(true, vec![], vec![], vec![], vec![message], vec![]);

        assert_eq!(output.to_string(), format!("Warning:\n{message}"));
    }

    #[test]
    fn display_scenario_parse_error() {
        let message = "/home/jackson/repositories/hott-reals/source/MCPExamples/ParseError.agda:6.5: error: [ParseError]\n<EOF><ERROR> ...";
        let output = load_response(false, vec![], vec![], vec![], vec![], vec![message]);

        assert_eq!(output.to_string(), format!("Error:\n{message}"));
    }

    #[test]
    fn display_groups_multiple_errors() {
        let output = load_response(
            false,
            vec![],
            vec![],
            vec![],
            vec![],
            vec!["first error", "second error"],
        );

        assert_eq!(output.to_string(), "Errors:\nfirst error\n\nsecond error");
    }

    #[test]
    fn display_groups_multiple_warnings() {
        let output = load_response(
            true,
            vec![],
            vec![],
            vec![],
            vec!["first warning", "second warning"],
            vec![],
        );

        assert_eq!(
            output.to_string(),
            "Warnings:\nfirst warning\n\nsecond warning"
        );
    }

    #[test]
    fn display_groups_goals_errors_and_warnings() {
        let output = load_response(
            false,
            vec![goal(0, "Nat", 5, 10)],
            vec![],
            vec![],
            vec!["Unsolved meta"],
            vec!["Not in scope: foo"],
        );

        assert_eq!(
            output.to_string(),
            "?0 : Nat\n\nError:\nNot in scope: foo\n\nWarning:\nUnsolved meta"
        );
    }

    #[test]
    fn display_formats_multiple_goals() {
        let output = load_response(
            false,
            vec![goal(0, "A", 1, 2), goal(1, "B", 3, 4)],
            vec![],
            vec![],
            vec![],
            vec![],
        );

        assert_eq!(output.to_string(), "?0 : A\n?1 : B");
    }

    #[test]
    fn display_formats_invisible_goals() {
        let output = load_response(
            false,
            vec![],
            vec![invisible_goal("_0", "Type", 6, 7)],
            vec![],
            vec![],
            vec![],
        );

        assert_eq!(output.to_string(), "_0 : Type [ at 6.7-8 ]");
    }

    #[test]
    fn display_formats_visible_constraints() {
        let output = load_response(
            false,
            vec![],
            vec![],
            vec![cmp_in_type("CmpEq", "Nat", &[0, 1])],
            vec![],
            vec![],
        );

        assert_eq!(output.to_string(), "?0 CmpEq ?1 : Nat");
    }

    #[test]
    fn display_groups_goals_invisible_goals_and_constraints() {
        let output = load_response(
            false,
            vec![goal(0, "Unit", 9, 5)],
            vec![invisible_goal("_1", "Type", 12, 3)],
            vec![just_sort(2, 0, 0)],
            vec![],
            vec![],
        );

        assert_eq!(
            output.to_string(),
            "?0 : Unit\n_1 : Type [ at 12.3-4 ]\nSort ?2"
        );
    }

    #[test]
    fn display_handles_checked_false_without_visible_messages() {
        let output = load_response(false, vec![], vec![], vec![], vec![], vec![]);

        assert_eq!(
            output.to_string(),
            "Loaded, but Agda reports `checked=false`. No goals, invisible goals, constraints, warnings, or errors."
        );
    }

    #[test]
    fn load_output_treats_display_info_error_as_failure() {
        let responses = parse_responses(vec![
            status(false),
            json!({ "kind": "ClearRunningInfo" }),
            json!({ "kind": "ClearHighlighting", "tokenBased": "NotOnlyTokenBased" }),
            json!({
                "kind": "DisplayInfo",
                "info": {
                    "kind": "Error",
                    "warnings": [],
                    "error": { "message": "Not in scope: foo" },
                }
            }),
            json!({
                "kind": "JumpToError",
                "filepath": "/tmp/X.agda",
                "position": 1,
            }),
            json!({ "kind": "HighlightingInfo", "direct": false, "filepath": "/tmp/agda" }),
            status(false),
        ]);

        let output = LoadResponse::try_from(responses).unwrap();

        assert!(!output.checked);
        assert_eq!(output.errors, vec!["Not in scope: foo".to_owned()]);
        assert!(output.goals.is_empty());
    }
}
