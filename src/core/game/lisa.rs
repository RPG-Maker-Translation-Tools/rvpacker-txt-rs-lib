//! Custom processing for the `LisaRPG` series.
//!
//! Dialogue lines carry an engine prefix marking the tile the textbox should
//! appear above (`\nbt`, `\nblt`) or the actor speaking (`\et[n]`). It is not
//! translatable text, so it is stripped before the line reaches the translation
//! file and put back on write.

/// Byte index just past the prefix, or [`None`] if the line has none.
pub(super) fn dialogue_prefix_len(string: &str) -> Option<usize> {
    if string.starts_with(r"\et[") {
        let mut index = r"\et[".len() + 1;

        loop {
            let char = string.as_bytes()[index];

            if char == b']' {
                return Some(index + 1);
            }

            index += 1;

            if index == 10 {
                return None;
            }
        }
    } else if string.starts_with(r"\nbt") {
        Some(r"\nbt".len())
    } else if string.starts_with(r"\nblt") {
        Some(r"\nblt".len())
    } else {
        None
    }
}
