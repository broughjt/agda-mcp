//! Apply Agda-shaped source edits to UTF-8 files.
//!
//! Agda's `Cmd_give` does not modify the source file — it emits a
//! `GiveAction` describing the edit and expects the frontend to apply
//! it. This module turns an Agda [`Interval`] + replacement text into an
//! atomic in-place rewrite of the file on disk.

use std::fs;
use std::io;
use std::path::Path;

use thiserror::Error;

use crate::agda::source::Interval;

/// Replace `range` in `path` with `replacement` and write the result back
/// atomically (temp file in the same directory, then `rename`).
///
/// `range` carries Agda's 1-based character positions; `pos` is converted
/// to a UTF-8 byte offset by walking [`str::char_indices`]. The `line` and
/// `col` fields are ignored because `pos` is the canonical source of
/// truth per Agda's `Position'` type.
pub fn apply(path: &Path, range: Interval, replacement: &str) -> Result<(), Error> {
    let contents = fs::read_to_string(path).map_err(Error::Read)?;
    let start = char_pos_to_byte_offset(&contents, range.start.pos)?;
    let end = char_pos_to_byte_offset(&contents, range.end.pos)?;

    if start > end {
        return Err(Error::InvertedRange {
            start: range.start.pos,
            end: range.end.pos,
        });
    }

    let mut updated = String::with_capacity(contents.len() - (end - start) + replacement.len());
    updated.push_str(&contents[..start]);
    updated.push_str(replacement);
    updated.push_str(&contents[end..]);

    write_atomic(path, &updated).map_err(Error::Write)
}

/// Convert Agda's 1-based character `pos` to a UTF-8 byte offset into `s`.
///
/// Agda positions count from 1, and `end.pos` may legitimately point one
/// past the last character (half-open intervals), which we map to
/// `s.len()`.
fn char_pos_to_byte_offset(s: &str, pos: u32) -> Result<usize, Error> {
    if pos == 0 {
        return Err(Error::InvalidPosition {
            pos,
            total_chars: s.chars().count(),
        });
    }
    let target = (pos - 1) as usize;

    let mut count = 0usize;
    for (byte_offset, _) in s.char_indices() {
        if count == target {
            return Ok(byte_offset);
        }
        count += 1;
    }
    // Loop exited without matching: `count` is now the total char count.
    // `pos == count + 1` (i.e. one past the last character) is a valid
    // half-open endpoint and maps to the end of the string.
    if count == target {
        return Ok(s.len());
    }
    Err(Error::InvalidPosition {
        pos,
        total_chars: count,
    })
}

/// Write `contents` to `path` via a sibling temp file + `rename` so a
/// crash mid-write cannot leave a half-written source file on disk.
fn write_atomic(path: &Path, contents: &str) -> io::Result<()> {
    let parent = path.parent().unwrap_or_else(|| Path::new("."));
    let file_name = path
        .file_name()
        .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "path has no file name"))?;
    let temp = parent.join(format!(
        ".{}.tmp.{}",
        file_name.to_string_lossy(),
        std::process::id()
    ));
    fs::write(&temp, contents)?;
    fs::rename(&temp, path)
}

#[derive(Debug, Error)]
pub enum Error {
    #[error("failed to read source file: {0}")]
    Read(#[source] io::Error),

    #[error("failed to write source file: {0}")]
    Write(#[source] io::Error),

    #[error("Agda position {pos} is out of range for file with {total_chars} characters")]
    InvalidPosition { pos: u32, total_chars: usize },

    #[error("inverted Agda range: start.pos={start} end.pos={end}")]
    InvertedRange { start: u32, end: u32 },
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::agda::source::Position;
    use tempfile::tempdir;

    /// Build an interval from raw 1-based character positions; `line`/`col`
    /// are filled with dummy values since [`apply`] ignores them.
    fn interval(start_pos: u32, end_pos: u32) -> Interval {
        Interval::new(Position::new(start_pos, 0, 0), Position::new(end_pos, 0, 0))
    }

    #[test]
    fn applies_ascii_replacement() {
        // "x = {! !}\n"
        //  ^   ^   ^
        //  1   5   9   (1-based char positions)
        // The hole `{! !}` covers chars 5..10 (half-open).
        let dir = tempdir().expect("create tempdir");
        let path = dir.path().join("Spike.agda");
        fs::write(&path, "x = {! !}\n").expect("seed file");

        apply(&path, interval(5, 10), "zero").expect("edit should apply");

        let after = fs::read_to_string(&path).expect("read back");
        assert_eq!(after, "x = zero\n");
    }

    #[test]
    fn applies_replacement_after_multibyte_prefix() {
        // 'α' is 2 bytes in UTF-8 but still one character. The hole
        // sits at the same 1-based character positions as the ASCII
        // case above; if we accidentally use byte offsets the splice
        // will mangle the file.
        let dir = tempdir().expect("create tempdir");
        let path = dir.path().join("Greek.agda");
        fs::write(&path, "α = {! !}\n").expect("seed file");

        apply(&path, interval(5, 10), "zero").expect("edit should apply");

        let after = fs::read_to_string(&path).expect("read back");
        assert_eq!(after, "α = zero\n");
    }

    #[test]
    fn applies_replacement_at_end_of_file() {
        // end.pos may be one past the last character (half-open range).
        let dir = tempdir().expect("create tempdir");
        let path = dir.path().join("End.agda");
        fs::write(&path, "abc").expect("seed file"); // 3 chars
        // Replace "bc" (chars 2..4, where 4 == len + 1).
        apply(&path, interval(2, 4), "XY").expect("edit should apply");

        let after = fs::read_to_string(&path).expect("read back");
        assert_eq!(after, "aXY");
    }

    #[test]
    fn rejects_position_past_end_of_file() {
        let dir = tempdir().expect("create tempdir");
        let path = dir.path().join("Short.agda");
        fs::write(&path, "abc").expect("seed file"); // 3 chars

        let error =
            apply(&path, interval(1, 99), "x").expect_err("out-of-range pos should be rejected");
        match error {
            Error::InvalidPosition { pos, total_chars } => {
                assert_eq!(pos, 99);
                assert_eq!(total_chars, 3);
            }
            other => panic!("expected InvalidPosition, got {other:?}"),
        }
    }

    #[test]
    fn rejects_zero_position() {
        let dir = tempdir().expect("create tempdir");
        let path = dir.path().join("Zero.agda");
        fs::write(&path, "abc").expect("seed file");

        let error = apply(&path, interval(0, 2), "x").expect_err("pos=0 should be rejected");
        assert!(matches!(error, Error::InvalidPosition { pos: 0, .. }));
    }

    #[test]
    fn rejects_inverted_range() {
        let dir = tempdir().expect("create tempdir");
        let path = dir.path().join("Inv.agda");
        fs::write(&path, "abcdef").expect("seed file");

        let error =
            apply(&path, interval(5, 2), "x").expect_err("inverted range should be rejected");
        assert!(matches!(error, Error::InvertedRange { start: 5, end: 2 }));
    }

    #[test]
    fn write_is_atomic_no_temp_files_left_behind() {
        let dir = tempdir().expect("create tempdir");
        let path = dir.path().join("Atomic.agda");
        fs::write(&path, "x = {! !}\n").expect("seed file");

        apply(&path, interval(5, 10), "zero").expect("edit should apply");

        let leftover: Vec<_> = fs::read_dir(dir.path())
            .expect("readdir")
            .filter_map(Result::ok)
            .map(|e| e.file_name().to_string_lossy().into_owned())
            .filter(|name| name != "Atomic.agda")
            .collect();
        assert!(
            leftover.is_empty(),
            "temp file(s) left behind after atomic write: {leftover:?}"
        );
    }
}
