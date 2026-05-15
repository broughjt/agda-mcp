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
    pub fn load(path: &'a str, flags: &'a [String]) -> Self {
        Self {
            path,
            highlighting_level: HighlightingLevel::default(),
            highlighting_method: HighlightingMethod::default(),
            interaction: Interaction::Load(Load { path, flags }),
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
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            formatter,
            "IOTCM {} {} {} ({})",
            HaskellString(self.path),
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
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Load(load) => write!(formatter, "{load}"),
            Self::Give(give) => write!(formatter, "{give}"),
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
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            formatter,
            "Cmd_load {} {}",
            HaskellString(self.path),
            HaskellList(self.flags)
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
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            formatter,
            "Cmd_give {} {} {} {}",
            self.force,
            self.interaction_point,
            self.range,
            HaskellString(self.expression)
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
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::None => formatter.write_str("None"),
            Self::NonInteractive => formatter.write_str("NonInteractive"),
            Self::Interactive => formatter.write_str("Interactive"),
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
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Direct => formatter.write_str("Direct"),
            Self::Indirect => formatter.write_str("Indirect"),
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
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::WithForce => formatter.write_str("WithForce"),
            Self::WithoutForce => formatter.write_str("WithoutForce"),
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
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            formatter,
            "Pn () {} {} {}",
            self.position, self.line, self.column
        )
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
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "Interval () ({}) ({})", self.start, self.end)
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
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.file {
            Some(file) => write!(
                formatter,
                "(intervalsToRange (Just (mkAbsolute {})) [",
                HaskellString(file)
            )?,
            None => formatter.write_str("(intervalsToRange Nothing [")?,
        }

        for (index, interval) in self.intervals.iter().enumerate() {
            if index > 0 {
                formatter.write_str(", ")?;
            }
            write!(formatter, "{interval}")?;
        }

        formatter.write_str("])")
    }
}

/// Range argument for a goal-specific Agda command.
///
/// Agda command parsing accepts either `noRange` or `intervalsToRange ...`:
/// https://github.com/agda/agda/blob/3b57742a311b3a90b755737968d437f1ef902318/src/full/Agda/Interaction/Base.hs#L428-L431
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct RangeArgument(pub Option<AgdaRange>);

pub const NO_RANGE: RangeArgument = RangeArgument(None);

impl From<AgdaRange> for RangeArgument {
    fn from(range: AgdaRange) -> Self {
        Self(Some(range))
    }
}

impl fmt::Display for RangeArgument {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.0 {
            None => formatter.write_str("noRange"),
            Some(range) => write!(formatter, "{range}"),
        }
    }
}

struct HaskellString<'a>(&'a str);

impl fmt::Display for HaskellString<'_> {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write_haskell_string(formatter, self.0)
    }
}

struct HaskellList<'a, T>(&'a [T]);

impl<T: AsRef<str>> fmt::Display for HaskellList<'_, T> {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str("[")?;
        for (index, item) in self.0.iter().enumerate() {
            if index > 0 {
                formatter.write_str(", ")?;
            }
            write_haskell_string(formatter, item.as_ref())?;
        }
        formatter.write_str("]")
    }
}

/// Write a Rust string as a Haskell `Read`-compatible string literal.
pub fn write_haskell_string(formatter: &mut impl fmt::Write, input: &str) -> fmt::Result {
    formatter.write_char('"')?;

    let mut previous_was_numeric_escape = false;
    for character in input.chars() {
        if previous_was_numeric_escape && character.is_ascii_digit() {
            formatter.write_str("\\&")?;
        }

        previous_was_numeric_escape = character == '\0'
            || (character.is_control() && !matches!(character, '\n' | '\t' | '\r'));
        write_haskell_char_escaped(formatter, character, '"')?;
    }

    formatter.write_char('"')
}

fn write_haskell_char_escaped(
    formatter: &mut impl fmt::Write,
    character: char,
    quote: char,
) -> fmt::Result {
    match character {
        '\\' => formatter.write_str("\\\\"),
        '\n' => formatter.write_str("\\n"),
        '\t' => formatter.write_str("\\t"),
        '\r' => formatter.write_str("\\r"),
        '\0' => formatter.write_str("\\0"),
        character if character == quote => {
            formatter.write_char('\\')?;
            formatter.write_char(quote)
        }
        character if character.is_control() => write!(formatter, "\\{}", character as u32),
        character => formatter.write_char(character),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn haskell_string_escapes_common_special_characters() {
        assert_eq!(
            HaskellString("quote: \" slash: \\ newline:\n tab:\t carriage:\r").to_string(),
            "\"quote: \\\" slash: \\\\ newline:\\n tab:\\t carriage:\\r\""
        );
    }

    #[test]
    fn haskell_string_preserves_unicode() {
        assert_eq!(HaskellString("λ x → x").to_string(), "\"λ x → x\"");
    }

    #[test]
    fn haskell_string_escapes_control_characters() {
        assert_eq!(HaskellString("\0\x01\x7f").to_string(), "\"\\0\\1\\127\"");
    }

    #[test]
    fn haskell_string_disambiguates_numeric_escapes_before_digits() {
        assert_eq!(HaskellString("\x01 2").to_string(), "\"\\1 2\"");
        assert_eq!(HaskellString("\x012").to_string(), "\"\\1\\&2\"");
    }

    #[test]
    fn renders_string_lists() {
        assert_eq!(
            HaskellList(&["-i", ".", "--flag=quoted\"value"]).to_string(),
            "[\"-i\", \".\", \"--flag=quoted\\\"value\"]"
        );
    }

    #[test]
    fn renders_load_command_without_flags_like_agda_fixtures() {
        assert_eq!(
            Command::load("ParenJSON.agda", &[]).to_string(),
            "IOTCM \"ParenJSON.agda\" None Indirect (Cmd_load \"ParenJSON.agda\" [])"
        );
    }

    #[test]
    fn renders_load_command_with_load_flags() {
        let load_flags = [
            "--no-default-libraries".to_owned(),
            "-i".to_owned(),
            ".".to_owned(),
        ];

        assert_eq!(
            Command::load("Spike.agda", &load_flags).to_string(),
            "IOTCM \"Spike.agda\" None Indirect (Cmd_load \"Spike.agda\" [\"--no-default-libraries\", \"-i\", \".\"])"
        );
    }

    #[test]
    fn renders_give_command_with_no_range_like_agda_fixtures() {
        assert_eq!(
            Command::give(
                "Issue2174a.agda",
                UseForce::WithoutForce,
                0,
                &RangeArgument(None),
                "F ?",
            )
            .to_string(),
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
            Command::give(
                "Issue2174a.agda",
                UseForce::WithoutForce,
                0,
                &RangeArgument(Some(range)),
                "F ?",
            )
            .to_string(),
            "IOTCM \"Issue2174a.agda\" None Indirect (Cmd_give WithoutForce 0 (intervalsToRange (Just (mkAbsolute \"/tmp/Issue2174a.agda\")) [Interval () (Pn () 1 1 1) (Pn () 1 1 1)]) \"F ?\")"
        );
    }
}
