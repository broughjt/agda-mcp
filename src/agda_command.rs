use std::fmt;

/// A complete command sent to Agda's `--interaction-json` REPL.
///
/// Mirrors Agda's `IOTCM` wrapper:
/// https://github.com/agda/agda/blob/3b57742a311b3a90b755737968d437f1ef902318/src/full/Agda/Interaction/Base.hs#L343-L351
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Command<'a> {
    /// The editor buffer/current file this command belongs to.
    pub path: &'a str,
    pub highlighting_level: HighlightingLevel,
    pub highlighting_method: HighlightingMethod,
    pub interaction: Interaction<'a>,
}

impl<'a> Command<'a> {
    pub fn load(path: &'a str, load_flags: &'a [String]) -> Self {
        Self {
            path,
            highlighting_level: HighlightingLevel::default(),
            highlighting_method: HighlightingMethod::default(),
            interaction: Interaction::Load(Load {
                path,
                flags: load_flags,
            }),
        }
    }

    pub fn give(
        path: &'a str,
        force: UseForce,
        interaction_point: u32,
        range: &'a RangeArgument,
        expression: &'a str,
    ) -> Self {
        Self {
            path,
            highlighting_level: HighlightingLevel::default(),
            highlighting_method: HighlightingMethod::default(),
            interaction: Interaction::Give(Give {
                force,
                interaction_point,
                range,
                expression,
            }),
        }
    }
}

impl fmt::Display for Command<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "IOTCM {} {} {} ({})",
            render_haskell_string(self.path),
            self.highlighting_level,
            self.highlighting_method,
            self.interaction
        )
    }
}

/// The interaction payload inside an `IOTCM` command.
///
/// Mirrors Agda's `Interaction'` type:
/// https://github.com/agda/agda/blob/3b57742a311b3a90b755737968d437f1ef902318/src/full/Agda/Interaction/Base.hs#L158-L287
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Interaction<'a> {
    Load(Load<'a>),
    Give(Give<'a>),
}

impl fmt::Display for Interaction<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Load(load) => write!(f, "{load}"),
            Self::Give(give) => write!(f, "{give}"),
        }
    }
}

/// Loads the module in file `path` using the command line flags in `load_flags`.
///
/// Mirrors Agda's `Cmd_load`:
/// https://github.com/agda/agda/blob/3b57742a311b3a90b755737968d437f1ef902318/src/full/Agda/Interaction/Base.hs#L161-L163
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Load<'a> {
    pub path: &'a str,
    pub flags: &'a [String],
}

impl fmt::Display for Load<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Cmd_load {} {}",
            render_haskell_string(self.path),
            render_string_list(self.flags)
        )
    }
}

/// Gives an expression to an interaction point in the current file.
///
/// Mirrors Agda's `Cmd_give`:
/// https://github.com/agda/agda/blob/3b57742a311b3a90b755737968d437f1ef902318/src/full/Agda/Interaction/Base.hs#L284-L287
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Give<'a> {
    pub force: UseForce,
    pub interaction_point: u32,
    pub range: &'a RangeArgument,
    pub expression: &'a str,
}

impl fmt::Display for Give<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Cmd_give {} {} {} {}",
            self.force,
            self.interaction_point,
            self.range,
            render_haskell_string(self.expression)
        )
    }
}

/// How much highlighting should Agda send to the UI.
///
/// Mirrors Agda's `HighlightingLevel`:
/// https://github.com/agda/agda/blob/3b57742a311b3a90b755737968d437f1ef902318/src/full/Agda/TypeChecking/Monad/Base/Types.hs#L140-L148
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum HighlightingLevel {
    #[default]
    None,
    NonInteractive,
    Interactive,
}

impl fmt::Display for HighlightingLevel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::None => f.write_str("None"),
            Self::NonInteractive => f.write_str("NonInteractive"),
            Self::Interactive => f.write_str("Interactive"),
        }
    }
}

/// How Agda should send highlighting to the UI.
///
/// Mirrors Agda's `HighlightingMethod`:
/// https://github.com/agda/agda/blob/3b57742a311b3a90b755737968d437f1ef902318/src/full/Agda/TypeChecking/Monad/Base/Types.hs#L151-L157
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum HighlightingMethod {
    Direct,
    #[default]
    Indirect,
}

impl fmt::Display for HighlightingMethod {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Direct => f.write_str("Direct"),
            Self::Indirect => f.write_str("Indirect"),
        }
    }
}

/// Whether Agda should skip some safety checks when giving an expression.
///
/// Mirrors Agda's `UseForce`:
/// https://github.com/agda/agda/blob/3b57742a311b3a90b755737968d437f1ef902318/src/full/Agda/Interaction/Base.hs#L501-L504
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UseForce {
    WithForce,
    WithoutForce,
}

impl fmt::Display for UseForce {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::WithForce => f.write_str("WithForce"),
            Self::WithoutForce => f.write_str("WithoutForce"),
        }
    }
}

/// A source position in Agda's interaction protocol.
///
/// Agda positions are 1-based. The `position` field is Agda's character
/// position in the file, while `line` and `column` are the user-facing line and
/// column.
///
/// Mirrors Agda's `Position'` / `Pn` pattern:
/// https://github.com/agda/agda/blob/3b57742a311b3a90b755737968d437f1ef902318/src/full/Agda/Syntax/Position.hs#L134-L154
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AgdaPosition {
    pub position: u32,
    pub line: u32,
    pub column: u32,
}

impl AgdaPosition {
    pub const fn new(position: u32, line: u32, column: u32) -> Self {
        Self {
            position,
            line,
            column,
        }
    }
}

impl fmt::Display for AgdaPosition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Pn () {} {} {}", self.position, self.line, self.column)
    }
}

/// A half-open source interval in Agda's interaction protocol.
///
/// Mirrors Agda's `Interval'`; Agda documents that `iEnd` is not included:
/// https://github.com/agda/agda/blob/3b57742a311b3a90b755737968d437f1ef902318/src/full/Agda/Syntax/Position.hs#L238-L249
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AgdaInterval {
    pub start: AgdaPosition,
    pub end: AgdaPosition,
}

impl AgdaInterval {
    pub const fn new(start: AgdaPosition, end: AgdaPosition) -> Self {
        Self { start, end }
    }
}

impl fmt::Display for AgdaInterval {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Interval () ({}) ({})", self.start, self.end)
    }
}

/// A source range in Agda's interaction protocol.
///
/// Mirrors Agda's `Range'` and its `intervalsToRange` constructor function:
/// https://github.com/agda/agda/blob/3b57742a311b3a90b755737968d437f1ef902318/src/full/Agda/Syntax/Position.hs#L309-L363
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AgdaRange {
    /// Absolute path for the range, if known.
    pub file: Option<String>,
    pub intervals: Vec<AgdaInterval>,
}

impl AgdaRange {
    pub fn new(file: Option<String>, intervals: Vec<AgdaInterval>) -> Self {
        Self { file, intervals }
    }

    pub fn single(file: Option<String>, start: AgdaPosition, end: AgdaPosition) -> Self {
        Self::new(file, vec![AgdaInterval::new(start, end)])
    }
}

impl fmt::Display for AgdaRange {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.file {
            Some(file) => write!(
                f,
                "(intervalsToRange (Just (mkAbsolute {})) [",
                render_haskell_string(file)
            )?,
            None => f.write_str("(intervalsToRange Nothing [")?,
        }

        for (index, interval) in self.intervals.iter().enumerate() {
            if index > 0 {
                f.write_str(", ")?;
            }
            write!(f, "{interval}")?;
        }

        f.write_str("])")
    }
}

/// Range argument for a goal-specific Agda command.
///
/// Agda command parsing accepts either `noRange` or `intervalsToRange ...`:
/// https://github.com/agda/agda/blob/3b57742a311b3a90b755737968d437f1ef902318/src/full/Agda/Interaction/Base.hs#L428-L431
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct RangeArgument(pub Option<AgdaRange>);

impl From<AgdaRange> for RangeArgument {
    fn from(range: AgdaRange) -> Self {
        Self(Some(range))
    }
}

impl fmt::Display for RangeArgument {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.0 {
            None => f.write_str("noRange"),
            Some(range) => write!(f, "{range}"),
        }
    }
}

/// Render a Rust string as a Haskell `Read`-compatible string literal.
pub fn render_haskell_string(input: &str) -> String {
    let mut rendered = String::with_capacity(input.len() + 2);
    rendered.push('"');
    for ch in input.chars() {
        match ch {
            '\\' => rendered.push_str("\\\\"),
            '"' => rendered.push_str("\\\""),
            '\n' => rendered.push_str("\\n"),
            '\t' => rendered.push_str("\\t"),
            '\r' => rendered.push_str("\\r"),
            '\0' => rendered.push_str("\\NUL"),
            '\x07' => rendered.push_str("\\a"),
            '\x08' => rendered.push_str("\\b"),
            '\x0c' => rendered.push_str("\\f"),
            '\x0b' => rendered.push_str("\\v"),
            '\x01' => rendered.push_str("\\SOH"),
            '\x02' => rendered.push_str("\\STX"),
            '\x03' => rendered.push_str("\\ETX"),
            '\x04' => rendered.push_str("\\EOT"),
            '\x05' => rendered.push_str("\\ENQ"),
            '\x06' => rendered.push_str("\\ACK"),
            '\x0e' => rendered.push_str("\\SO"),
            '\x0f' => rendered.push_str("\\SI"),
            '\x10' => rendered.push_str("\\DLE"),
            '\x11' => rendered.push_str("\\DC1"),
            '\x12' => rendered.push_str("\\DC2"),
            '\x13' => rendered.push_str("\\DC3"),
            '\x14' => rendered.push_str("\\DC4"),
            '\x15' => rendered.push_str("\\NAK"),
            '\x16' => rendered.push_str("\\SYN"),
            '\x17' => rendered.push_str("\\ETB"),
            '\x18' => rendered.push_str("\\CAN"),
            '\x19' => rendered.push_str("\\EM"),
            '\x1a' => rendered.push_str("\\SUB"),
            '\x1b' => rendered.push_str("\\ESC"),
            '\x1c' => rendered.push_str("\\FS"),
            '\x1d' => rendered.push_str("\\GS"),
            '\x1e' => rendered.push_str("\\RS"),
            '\x1f' => rendered.push_str("\\US"),
            '\x7f' => rendered.push_str("\\DEL"),
            ch if ch.is_control() => {
                rendered.push_str(&format!("\\x{:x}\\&", ch as u32));
            }
            ch => rendered.push(ch),
        }
    }
    rendered.push('"');
    rendered
}

pub fn render_string_list<S: AsRef<str>>(items: &[S]) -> String {
    let items = items
        .iter()
        .map(|item| render_haskell_string(item.as_ref()))
        .collect::<Vec<_>>()
        .join(", ");
    format!("[{items}]")
}

pub fn render_load_command(path: &str, load_flags: &[String]) -> String {
    Command::load(path, load_flags).to_string()
}

pub fn render_give_command(
    path: &str,
    force: UseForce,
    interaction_point: u32,
    range: &RangeArgument,
    expression: &str,
) -> String {
    Command::give(path, force, interaction_point, range, expression).to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn haskell_string_escapes_common_special_characters() {
        assert_eq!(
            render_haskell_string("quote: \" slash: \\ newline:\n tab:\t carriage:\r"),
            "\"quote: \\\" slash: \\\\ newline:\\n tab:\\t carriage:\\r\""
        );
    }

    #[test]
    fn haskell_string_preserves_unicode() {
        assert_eq!(render_haskell_string("λ x → x"), "\"λ x → x\"");
    }

    #[test]
    fn haskell_string_escapes_control_characters() {
        assert_eq!(render_haskell_string("\0\x01\x7f"), "\"\\NUL\\SOH\\DEL\"");
    }

    #[test]
    fn renders_string_lists() {
        assert_eq!(
            render_string_list(&["-i", ".", "--flag=quoted\"value"]),
            "[\"-i\", \".\", \"--flag=quoted\\\"value\"]"
        );
    }

    #[test]
    fn renders_load_command_without_flags_like_agda_fixtures() {
        assert_eq!(
            render_load_command("ParenJSON.agda", &[]),
            "IOTCM \"ParenJSON.agda\" None Indirect (Cmd_load \"ParenJSON.agda\" [])"
        );
    }

    #[test]
    fn renders_load_command_with_load_flags() {
        assert_eq!(
            render_load_command(
                "Spike.agda",
                &[
                    "--no-default-libraries".to_owned(),
                    "-i".to_owned(),
                    ".".to_owned(),
                ],
            ),
            "IOTCM \"Spike.agda\" None Indirect (Cmd_load \"Spike.agda\" [\"--no-default-libraries\", \"-i\", \".\"])"
        );
    }

    #[test]
    fn renders_give_command_with_no_range_like_agda_fixtures() {
        assert_eq!(
            render_give_command(
                "Issue2174a.agda",
                UseForce::WithoutForce,
                0,
                &RangeArgument(None),
                "F ?",
            ),
            "IOTCM \"Issue2174a.agda\" None Indirect (Cmd_give WithoutForce 0 noRange \"F ?\")"
        );
    }

    #[test]
    fn renders_give_command_with_explicit_range() {
        let range = AgdaRange::single(
            Some("/tmp/Issue2174a.agda".to_owned()),
            AgdaPosition::new(1, 1, 1),
            AgdaPosition::new(1, 1, 1),
        );

        assert_eq!(
            render_give_command(
                "Issue2174a.agda",
                UseForce::WithoutForce,
                0,
                &RangeArgument(Some(range)),
                "F ?",
            ),
            "IOTCM \"Issue2174a.agda\" None Indirect (Cmd_give WithoutForce 0 (intervalsToRange (Just (mkAbsolute \"/tmp/Issue2174a.agda\")) [Interval () (Pn () 1 1 1) (Pn () 1 1 1)]) \"F ?\")"
        );
    }
}
