use std::fmt;

pub mod command;
pub mod process;
pub mod response;
pub mod source;

/// Write items as a Haskell list literal: `[item1, item2, item3]`.
///
/// Used by command rendering (the `[flag1, flag2]` list in `Cmd_load`) and by
/// response display (Agda's pretty-printer uses the same `[a, b]` form for the
/// inner element lists of `CmpElim`).
pub(crate) fn write_haskell_list(
    formatter: &mut impl fmt::Write,
    items: impl IntoIterator<Item = impl fmt::Display>,
) -> fmt::Result {
    formatter.write_str("[")?;
    for (index, item) in items.into_iter().enumerate() {
        if index > 0 {
            formatter.write_str(", ")?;
        }
        write!(formatter, "{item}")?;
    }
    formatter.write_str("]")
}
