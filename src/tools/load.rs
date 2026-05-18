use std::fmt;

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::agda::{
    command::Command,
    response::{Info, Response, VisibleGoal},
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
    /// Visible interaction goals after the load, in the order Agda reported.
    pub goals: Vec<Goal>,
    /// Non-fatal warnings reported by Agda.
    pub warnings: Vec<String>,
    /// Errors reported by Agda.
    pub errors: Vec<String>,
}

impl fmt::Display for LoadResponse {
    /// Render the load result in an Agda-like format for humans.
    ///
    /// The structured fields remain the source of truth for clients that need
    /// machine-readable data. This text is intentionally close to Agda's Emacs
    /// info buffer: goals first, then grouped errors, then grouped warnings,
    /// but without the long horizontal rule delimiters.
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.goals.is_empty() && self.errors.is_empty() && self.warnings.is_empty() {
            return if self.checked {
                formatter.write_str("Checked. No goals, warnings, or errors.")
            } else {
                formatter.write_str(
                    "Loaded, but Agda reports `checked=false`. No visible goals, warnings, or errors.",
                )
            };
        }

        if let Some((goal, remaining_goals)) = self.goals.split_first() {
            write!(formatter, "{goal}")?;
            for goal in remaining_goals {
                write!(formatter, "\n{goal}")?;
            }
        }

        if !self.goals.is_empty() && (!self.errors.is_empty() || !self.warnings.is_empty()) {
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

    fn try_from(responses: Vec<Response>) -> Result<Self, Self::Error> {
        todo!()
    }
}

#[derive(Debug, Error)]
pub enum LoadResponseError {
    // TODO:
}

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
        match self.range.first() {
            Some(interval) => write!(
                formatter,
                "?{} : {} at {}:{}",
                self.id, self._type, interval.start.line, interval.start.col
            ),
            None => write!(formatter, "?{} : {}", self.id, self._type),
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

    fn load_response(
        checked: bool,
        goals: Vec<Goal>,
        warnings: Vec<&str>,
        errors: Vec<&str>,
    ) -> LoadResponse {
        LoadResponse {
            checked,
            goals,
            warnings: warnings.into_iter().map(str::to_owned).collect(),
            errors: errors.into_iter().map(str::to_owned).collect(),
        }
    }

    #[test]
    fn display_scenario_successful_typecheck_no_holes() {
        let output = load_response(true, vec![], vec![], vec![]);

        assert_eq!(
            output.to_string(),
            "Checked. No goals, warnings, or errors."
        );
    }

    #[test]
    fn display_scenario_successful_load_with_holes() {
        let output = load_response(false, vec![goal(0, "Unit", 9, 5)], vec![], vec![]);

        assert_eq!(output.to_string(), "?0 : Unit at 9:5");
    }

    #[test]
    fn display_scenario_type_error_and_holes() {
        let message = "/home/jackson/repositories/hott-reals/source/MCPExamples/TypeErrorAndHole.agda:15.7-11: error: [UnequalTerms]\nBool !=< Unit\nwhen checking that the expression true has type Unit";
        let output = load_response(false, vec![], vec![], vec![message]);

        assert_eq!(output.to_string(), format!("Error:\n{message}"));
    }

    #[test]
    fn display_scenario_type_error_no_holes() {
        let message = "/home/jackson/repositories/hott-reals/source/MCPExamples/TypeErrorNoHoles.agda:12.7-11: error: [UnequalTerms]\nBool !=< Unit\nwhen checking that the expression true has type Unit";
        let output = load_response(false, vec![], vec![], vec![message]);

        assert_eq!(output.to_string(), format!("Error:\n{message}"));
    }

    #[test]
    fn display_scenario_type_warning() {
        let message = "/home/jackson/repositories/hott-reals/source/MCPExamples/Warning.agda:3.1-8: warning: -W[no]EmptyPrivate\nEmpty private block.";
        let output = load_response(true, vec![], vec![message], vec![]);

        assert_eq!(output.to_string(), format!("Warning:\n{message}"));
    }

    #[test]
    fn display_scenario_parse_error() {
        let message = "/home/jackson/repositories/hott-reals/source/MCPExamples/ParseError.agda:6.5: error: [ParseError]\n<EOF><ERROR> ...";
        let output = load_response(false, vec![], vec![], vec![message]);

        assert_eq!(output.to_string(), format!("Error:\n{message}"));
    }

    #[test]
    fn display_groups_multiple_errors() {
        let output = load_response(false, vec![], vec![], vec!["first error", "second error"]);

        assert_eq!(output.to_string(), "Errors:\nfirst error\n\nsecond error");
    }

    #[test]
    fn display_groups_multiple_warnings() {
        let output = load_response(
            true,
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
            vec!["Unsolved meta"],
            vec!["Not in scope: foo"],
        );

        assert_eq!(
            output.to_string(),
            "?0 : Nat at 5:10\n\nError:\nNot in scope: foo\n\nWarning:\nUnsolved meta"
        );
    }

    #[test]
    fn display_formats_multiple_goals() {
        let output = load_response(
            false,
            vec![goal(0, "A", 1, 2), goal(1, "B", 3, 4)],
            vec![],
            vec![],
        );

        assert_eq!(output.to_string(), "?0 : A at 1:2\n?1 : B at 3:4");
    }

    #[test]
    fn display_handles_checked_false_without_visible_messages() {
        let output = load_response(false, vec![], vec![], vec![]);

        assert_eq!(
            output.to_string(),
            "Loaded, but Agda reports `checked=false`. No visible goals, warnings, or errors."
        );
    }

    #[test]
    fn load_output_treats_display_info_error_as_failure() {
        let responses = parse_responses(vec![
            status(false),
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

    // #[test]
    // fn rejects_give_responses() {
    //     let responses = parse_responses(vec![
    //         status(false),
    //         json!({ "kind": "GiveAction", "interactionPoint": 0, "giveResult": { "str": "tt" } }),
    //     ]);

    //     let error =
    //         LoadResponse::try_from(responses).expect_err("GiveAction is not Cmd_load output");
    //     assert!(matches!(
    //         error,
    //         LoadResponseError::UnexpectedResponse {
    //             kind: "GiveAction",
    //             ..
    //         }
    //     ));
    // }

    // #[test]
    // fn rejects_metas_responses_without_interaction_points() {
    //     let responses = parse_responses(vec![status(false), all_goals_warnings(json!([]))]);

    //     let error = LoadResponse::try_from(responses)
    //         .expect_err("Cmd_metas-like output should not be accepted as Cmd_load");
    //     assert!(matches!(
    //         error,
    //         LoadResponseError::InteractionPointsCount { count: 0 }
    //     ));
    // }
}
