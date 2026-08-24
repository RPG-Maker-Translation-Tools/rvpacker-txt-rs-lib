//! Text handling for RPG Maker plugins, as opposed to particular games.
//!
//! A plugin's markup turns up in every game that uses it, so these are keyed off
//! the markup and the engine that can run the plugin - never off a game
//! identifier.

/// Byte index just past an Advanced Text System dialogue prefix, or [`None`] if
/// the line carries none.
///
/// The VX Ace Advanced Text System plugin prefixes dialogue with the tile the
/// textbox should appear above (`\nbt`, `\nblt`) or the actor speaking
/// (`\et[n]`). None of it is translatable text, so it is stripped before the line
/// reaches the translation file and restored on write.
pub(super) fn ats_dialogue_prefix_len(string: &str) -> Option<usize> {
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
