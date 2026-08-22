//! Per-game custom processing.
//!
//! Some games need filtering beyond what the generic path does - dropping
//! internal identifiers that look like dialogue, stripping engine prefixes,
//! reattaching notes to descriptions. That logic is game-specific and useless to
//! everyone else, so each game sits behind its own feature:
//!
//! - `game-termina` - Fear & Hunger 2: Termina
//!
//! It is on by default. With a feature off, the matching
//! [`GameType`](crate::GameType) variant still exists but is inert: files are
//! processed as though [`GameType::None`](crate::GameType::None) were set.
//!
//! Every entry point below is a shim that compiles to nothing when its feature
//! is disabled, so call sites never need a `cfg`.

#[cfg(feature = "game-termina")]
mod termina;

use crate::types::{
    Code, GameType, Mode, RPGMFileType, TranslationMap, Variable,
};
use std::borrow::Cow;

/// Whether this game wants `file_type` skipped entirely.
pub(super) fn skips_file(game_type: GameType, file_type: RPGMFileType) -> bool {
    #[cfg(feature = "game-termina")]
    if game_type.is_termina() {
        return termina::skips_file(file_type);
    }

    let _ = (game_type, file_type);
    false
}

/// Whether this game wants `parameter` dropped rather than translated.
pub(super) fn drops_parameter(
    game_type: GameType,
    code: Code,
    parameter: &str,
) -> bool {
    #[cfg(feature = "game-termina")]
    if game_type.is_termina() {
        return termina::drops_parameter(code, parameter);
    }

    let _ = (game_type, code, parameter);
    false
}

/// Source entries this game needs prepended to a freshly read translation file.
pub(super) fn additional_data(
    game_type: GameType,
    file_type: RPGMFileType,
) -> &'static [&'static str] {
    #[cfg(feature = "game-termina")]
    if game_type.is_termina() && file_type.is_items() {
        return termina::ITEM_CATEGORIES;
    }

    let _ = (game_type, file_type);
    &[]
}

/// How many leading lines of a translation file this game reserves for its own
/// entries, which are keyed under `u16::MAX` rather than by section id.
pub(super) fn reserved_leading_lines(
    game_type: GameType,
    file_type: RPGMFileType,
) -> usize {
    #[cfg(feature = "game-termina")]
    if game_type.is_termina() && file_type.is_items() {
        return termina::ITEM_CATEGORIES.len();
    }

    let _ = (game_type, file_type);
    0
}

/// Suffix this game appends to a translated variable of `variable_type`.
pub(super) fn variable_suffix(
    game_type: GameType,
    variable_type: Variable,
) -> &'static str {
    #[cfg(feature = "game-termina")]
    if game_type.is_termina() {
        return termina::variable_suffix(variable_type);
    }

    let _ = (game_type, variable_type);
    ""
}

/// Whether a translated variable of `variable_type` must start on a new line.
pub(super) fn variable_needs_leading_newline(
    game_type: GameType,
    variable_type: Variable,
) -> bool {
    #[cfg(feature = "game-termina")]
    if game_type.is_termina() {
        return variable_type.is_note();
    }

    let _ = (game_type, variable_type);
    false
}

/// Whether this game folds an entry's note into its description.
pub(super) fn description_absorbs_note(
    game_type: GameType,
    variable_type: Variable,
) -> bool {
    #[cfg(feature = "game-termina")]
    if game_type.is_termina() {
        return variable_type.is_description();
    }

    let _ = (game_type, variable_type);
    false
}

/// What a game's custom filter decided about one variable.
///
/// Only games with a custom variable filter construct anything but
/// [`VariableOutcome::Continue`]; with every such feature off the other variants
/// are unreachable, but the call site still matches on them.
#[cfg_attr(not(feature = "game-termina"), allow(dead_code))]
pub(super) enum VariableOutcome<'a> {
    /// Drop the variable entirely.
    Drop,
    /// Carry on with this text, looking the translation up as usual.
    Continue(Cow<'a, str>),
    /// The filter produced the final text itself; skip the translation lookup.
    Done(String),
}

/// Runs this game's custom filter over one variable.
pub(super) fn process_variable<'a>(
    game_type: GameType,
    variable_text: Cow<'a, str>,
    variable_type: Variable,
    note_text: Option<&str>,
    mode: Mode,
    file_type: RPGMFileType,
    reserved: Option<&TranslationMap>,
) -> VariableOutcome<'a> {
    #[cfg(feature = "game-termina")]
    if game_type.is_termina() {
        let Some(text) = termina::process_variable(
            variable_text,
            variable_type,
            note_text,
            mode,
            file_type,
            reserved,
        ) else {
            return VariableOutcome::Drop;
        };

        // The item-category substitution builds the final text itself.
        if mode.is_write() && file_type.is_items() && variable_type.is_note() {
            return VariableOutcome::Done(text);
        }

        return VariableOutcome::Continue(Cow::Owned(text));
    }

    let _ = (variable_type, note_text, mode, file_type, reserved);
    let _ = game_type;
    VariableOutcome::Continue(variable_text)
}
