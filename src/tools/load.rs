use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::agda::{
    command::Command,
    response::{Info, Response, VisibleGoal},
    source::Interval,
};

/// Parameters for the MCP `load` tool.
#[derive(Debug, Deserialize, JsonSchema)]
pub struct Load {
    /// Path to the Agda file to load.
    pub path: String,
    // TODO: Whether to use the command line flags here?
}

impl Load {
    pub fn to_agda_command(&self) -> Command<'_> {
        Command::load(&self.path, &[])
    }
}

/// Summary returned to the MCP client for a `load` call.
#[derive(Debug, Clone, Serialize, JsonSchema)]
pub struct LoadOutput {
    /// `true` when Agda accepted the file with no errors.
    pub ok: bool,
    /// `true` when Agda finished type checking the file without errors.
    ///
    /// This is derived from the same signal as `ok` for now: the spike does
    /// not distinguish "type-checking ran" from "type-checking succeeded".
    pub checked: bool,
    /// Absolute path of the file that was loaded, canonicalised if possible.
    pub current_file: String,
    /// Visible interaction goals after the load, in the order Agda reported.
    pub goals: Vec<Goal>,
    /// Non-fatal warnings reported by Agda.
    pub warnings: Vec<String>,
    /// Errors reported by Agda. Non-empty when `ok` is `false`.
    pub errors: Vec<String>,
}

/// A visible interaction goal in the current file.
#[derive(Debug, Clone, Serialize, JsonSchema)]
pub struct Goal {
    pub id: u32,
    pub range: Vec<Interval>,
    #[serde(rename = "type")]
    pub ty: String,
}

impl LoadOutput {
    /// Compact human-readable summary, one line per goal/warning/error.
    ///
    /// Intended for the `content` field of the MCP tool result, where an LLM
    /// reads it directly. The full structured payload lives in
    /// `structured_content` alongside this text.
    pub fn format_text(&self) -> String {
        let mut lines: Vec<String> = Vec::new();
        for error in &self.errors {
            lines.push(format!("[Error] {error}"));
        }
        for warning in &self.warnings {
            lines.push(format!("[Warning] {warning}"));
        }
        for goal in &self.goals {
            let location = goal
                .range
                .first()
                .map(|interval| format!(" at {}:{}", interval.start.line, interval.start.col))
                .unwrap_or_default();
            lines.push(format!("[Goal {}] {}{location}", goal.id, goal.ty));
        }

        if lines.is_empty() {
            return if self.checked {
                format!(
                    "Checked {}. No goals, warnings, or errors.",
                    self.current_file
                )
            } else {
                format!("Failed to check {}.", self.current_file)
            };
        }

        let header = if self.checked {
            format!("Checked {}.", self.current_file)
        } else {
            format!("Failed to check {}.", self.current_file)
        };
        lines.insert(0, header);
        lines.join("\n")
    }
}

/// Build a `LoadOutput` from the responses Agda emitted for one `Cmd_load`.
///
/// Errors are collected from any `DisplayInfo Error` response and from the
/// `errors` field of `AllGoalsWarnings`. Either source makes `ok` and
/// `checked` `false`.
pub fn summarize_load(current_file: String, responses: &[Response]) -> LoadOutput {
    let mut goals = Vec::new();
    let mut warnings = Vec::new();
    let mut errors = Vec::new();

    for response in responses {
        match response {
            Response::DisplayInfo {
                info:
                    Info::AllGoalsWarnings {
                        visible_goals,
                        warnings: agda_warnings,
                        errors: agda_errors,
                        ..
                    },
            } => {
                for goal in visible_goals {
                    let VisibleGoal::OfType { constraint_obj, ty } = goal;
                    goals.push(Goal {
                        id: constraint_obj.id,
                        range: constraint_obj.range.clone(),
                        ty: ty.clone(),
                    });
                }
                warnings.extend(agda_warnings.iter().map(|m| m.message.clone()));
                errors.extend(agda_errors.iter().map(|m| m.message.clone()));
            }
            Response::DisplayInfo {
                info:
                    Info::Error {
                        warnings: agda_warnings,
                        error,
                    },
            } => {
                warnings.extend(agda_warnings.iter().map(|m| m.message.clone()));
                errors.push(error.message.clone());
            }
            _ => {}
        }
    }

    let ok = errors.is_empty();
    LoadOutput {
        ok,
        checked: ok,
        current_file,
        goals,
        warnings,
        errors,
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

    #[test]
    fn summarize_load_extracts_visible_goals() {
        let raw = [all_goals_warnings(json!([{
            "kind": "OfType",
            "constraintObj": point(0, 100, 105),
            "type": "Nat",
        }]))];
        let responses = raw
            .iter()
            .map(|value| serde_json::from_value::<Response>(value.clone()).unwrap())
            .collect::<Vec<_>>();

        let output = summarize_load("/tmp/X.agda".to_owned(), &responses);

        assert!(output.ok);
        assert!(output.checked);
        assert_eq!(output.goals.len(), 1);
        assert_eq!(output.goals[0].id, 0);
        assert_eq!(output.goals[0].ty, "Nat");
        assert_eq!(
            output.goals[0].range,
            vec![Interval::new(
                Position::new(100, 5, 5),
                Position::new(105, 5, 10)
            )]
        );
        assert!(output.errors.is_empty());
    }

    #[test]
    fn format_text_summarises_goals_warnings_errors() {
        let output = LoadOutput {
            ok: false,
            checked: false,
            current_file: "/tmp/X.agda".to_owned(),
            goals: vec![Goal {
                id: 0,
                range: vec![Interval::new(
                    Position::new(100, 5, 10),
                    Position::new(105, 5, 15),
                )],
                ty: "Nat".to_owned(),
            }],
            warnings: vec!["Unsolved meta".to_owned()],
            errors: vec!["Not in scope: foo".to_owned()],
        };

        assert_eq!(
            output.format_text(),
            "Failed to check /tmp/X.agda.\n\
             [Error] Not in scope: foo\n\
             [Warning] Unsolved meta\n\
             [Goal 0] Nat at 5:10"
        );
    }

    #[test]
    fn format_text_handles_clean_load() {
        let output = LoadOutput {
            ok: true,
            checked: true,
            current_file: "/tmp/X.agda".to_owned(),
            goals: vec![],
            warnings: vec![],
            errors: vec![],
        };

        assert_eq!(
            output.format_text(),
            "Checked /tmp/X.agda. No goals, warnings, or errors."
        );
    }

    #[test]
    fn summarize_load_treats_display_info_error_as_failure() {
        let raw = [json!({
            "kind": "DisplayInfo",
            "info": {
                "kind": "Error",
                "warnings": [],
                "error": { "message": "Not in scope: foo" },
            }
        })];
        let responses = raw
            .iter()
            .map(|value| serde_json::from_value::<Response>(value.clone()).unwrap())
            .collect::<Vec<_>>();

        let output = summarize_load("/tmp/X.agda".to_owned(), &responses);

        assert!(!output.ok);
        assert!(!output.checked);
        assert_eq!(output.errors, vec!["Not in scope: foo".to_owned()]);
        assert!(output.goals.is_empty());
    }
}
