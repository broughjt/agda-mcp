//! Source positions and intervals shared between Agda commands and responses.

use std::fmt;

use serde::Deserialize;

/// A source position in Agda's interaction protocol.
///
/// Agda positions are 1-based. `pos` is Agda's character position in the
/// file; `line` and `col` are the user-facing line and column. Field names
/// match the JSON keys Agda emits in responses; the `Display` impl renders
/// the Haskell `Pn () pos line col` form used by commands.
///
/// Mirrors Agda's `Position'` / `Pn` pattern:
/// https://github.com/agda/agda/blob/3b57742a311b3a90b755737968d437f1ef902318/src/full/Agda/Syntax/Position.hs#L134-L154
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
pub struct Position {
    pub pos: u32,
    pub line: u32,
    pub col: u32,
}

impl Position {
    pub const fn new(pos: u32, line: u32, col: u32) -> Self {
        Self { pos, line, col }
    }
}

impl fmt::Display for Position {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "Pn () {} {} {}", self.pos, self.line, self.col)
    }
}

/// A half-open source interval in Agda's interaction protocol.
///
/// Mirrors Agda's `Interval'`; Agda documents that `iEnd` is not included:
/// https://github.com/agda/agda/blob/3b57742a311b3a90b755737968d437f1ef902318/src/full/Agda/Syntax/Position.hs#L238-L249
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
pub struct Interval {
    pub start: Position,
    pub end: Position,
}

impl Interval {
    pub const fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }
}

impl fmt::Display for Interval {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "Interval () ({}) ({})", self.start, self.end)
    }
}
