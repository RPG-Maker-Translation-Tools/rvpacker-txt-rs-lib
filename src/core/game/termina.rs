//! Custom processing for Fear & Hunger 2: Termina.
//!
//! The game keeps a lot of internal bookkeeping in fields that otherwise look
//! like translatable text - lowercase identifiers, `choice_text` keys, unused
//! placeholder items - and it stores item descriptions split across the
//! description and note fields. Everything here exists to filter the former out
//! and stitch the latter back together.

use crate::{
    constants::NEW_LINE,
    types::{Code, Mode, RPGMFileType, TranslationMap, Variable},
};
use std::borrow::Cow;

/// Item category markers, which head `items.txt` rather than belonging to a section.
pub(super) const ITEM_CATEGORIES: &[&str] = &[
    "<Menu Category: Items>",
    "<Menu Category: Food>",
    "<Menu Category: Healing>",
    "<Menu Category: Body bag>",
];

/// `States` holds nothing translatable in this game.
pub(super) fn skips_file(file_type: RPGMFileType) -> bool {
    file_type.is_states()
}

/// Drops internal identifiers that reach the generic path looking like dialogue.
pub(super) fn drops_parameter(code: Code, parameter: &str) -> bool {
    // Lowercase-and-punctuation only is always an identifier, never a line.
    if parameter.chars().all(|c| {
        c.is_ascii_lowercase() || (c.is_ascii_punctuation() && c != '"')
    }) {
        return true;
    }

    // Of the system strings, only gab text and unfinished choice text are real.
    code.is_system()
        && !parameter.starts_with("Gab")
        && (!parameter.starts_with("choice_text")
            || parameter.ends_with("????"))
}

/// Descriptions are padded so the following note lands off-screen in-game.
pub(super) fn variable_suffix(variable_type: Variable) -> &'static str {
    if variable_type.is_description() {
        "



"
    } else {
        ""
    }
}

pub(super) fn process_variable(
    mut variable_text: Cow<'_, str>,
    variable_type: Variable,
    note_text: Option<&str>,
    mode: Mode,
    file_type: RPGMFileType,
    item_categories: Option<&TranslationMap>,
) -> Option<String> {
    if variable_text.starts_with("///") || variable_text.contains("---") {
        return None;
    }

    match variable_type {
        Variable::Description => {
            if let Some(note) = note_text {
                let mut note_is_continuation = false;

                if !note.starts_with("flesh puppetry") {
                    let mut note_chars = note.chars();

                    if let Some((note_first_char, note_second_char)) =
                        note_chars.next().zip(note_chars.next())
                    {
                        let is_continuation =
                            note_first_char == '\n' && note_second_char != '\n';

                        let first_char_is_valid = note_first_char
                            .is_ascii_alphabetic()
                            || note_first_char == '"'
                            || note.starts_with("4 sticks");

                        let first_char_is_punctuation =
                            matches!(note_first_char, '.' | '!' | '/' | '?');

                        if (is_continuation || first_char_is_valid)
                            && !first_char_is_punctuation
                        {
                            note_is_continuation = true;
                        }
                    }
                }

                if note_is_continuation {
                    let mut note_string = String::from(note);

                    if let Some((mut left, _)) =
                        note.trim_start().split_once('\n')
                    {
                        left = left.trim();

                        if left.ends_with(['.', '%', '!', '"']) {
                            note_string = String::from(if mode.is_write() {
                                "\n"
                            } else {
                                NEW_LINE
                            }) + left;
                        } else if mode.is_read() {
                            return None;
                        }
                    } else if note.ends_with(['.', '%', '!', '"'])
                        || note.ends_with("takes place?")
                    {
                        note_string = note.into();
                    } else if mode.is_read() {
                        return None;
                    }

                    if note_string.is_empty() {
                        if mode.is_read() {
                            return None;
                        }
                    } else {
                        variable_text =
                            Cow::Owned(format!("{variable_text}{note_string}"));
                    }
                }
            }
        }
        Variable::Message1
        | Variable::Message2
        | Variable::Message3
        | Variable::Message4 => {
            return None;
        }
        Variable::Note => {
            if mode.is_write()
                && file_type.is_items()
                && let Some(categories) = item_categories
            {
                for string in ITEM_CATEGORIES {
                    if variable_text.rfind(string).is_some() {
                        return Some(
                            variable_text.replace(string, &categories[*string]),
                        );
                    }
                }
            }

            if !file_type.is_classes() {
                return None;
            }
        }
        Variable::Name | Variable::Nickname => match file_type {
            RPGMFileType::Actors => {
                if ![
                    "Levi",
                    "Marina",
                    "Daan",
                    "Abella",
                    "O'saa",
                    "Blood golem",
                    "Black Kalev",
                    "Marcoh",
                    "Karin",
                    "Olivia",
                    "Ghoul",
                    "Villager",
                    "August",
                    "Caligura",
                    "Henryk",
                    "Pav",
                    "Tanaka",
                    "Samarie",
                ]
                .contains(&variable_text.as_ref())
                {
                    return None;
                }
            }
            RPGMFileType::Armors => {
                if variable_text.starts_with("test_armor") {
                    return None;
                }
            }
            RPGMFileType::Classes => {
                if [
                    "Girl",
                    "Kid demon",
                    "Captain",
                    "Marriage",
                    "Marriage2",
                    "Baby demon",
                    "Buckman",
                    "Nas'hrah",
                    "Skeleton",
                ]
                .contains(&variable_text.as_ref())
                {
                    return None;
                }
            }
            RPGMFileType::Enemies => {
                if ["Spank Tank", "giant", "test"]
                    .contains(&variable_text.as_ref())
                {
                    return None;
                }
            }
            RPGMFileType::Items => {
                if [
                    "Torch",
                    "Flashlight",
                    "Stick",
                    "Quill",
                    "Empty scroll",
                    "Soul stone_NOT_USE",
                    "Cube of depths",
                    "Worm juice",
                    "Silver shilling",
                    "Coded letter #1 - UNUSED",
                    "Black vial",
                    "Torturer's notes 1",
                    "Purple vial",
                    "Orange vial",
                    "Red vial",
                    "Green vial",
                    "Pinecone pig instructions",
                    "Grilled salmonsnake meat",
                    "Empty scroll",
                    "Water vial",
                    "Blood vial",
                    "Devil's Grass",
                    "Stone",
                    "Codex #1",
                    "The Tale of the Pocketcat I",
                    "The Tale of the Pocketcat II",
                ]
                .contains(&variable_text.as_ref())
                    || variable_text.starts_with("The Fellowship")
                    || variable_text.starts_with("Studies of")
                    || variable_text.starts_with("Blueish")
                    || variable_text.starts_with("Skeletal")
                    || variable_text.ends_with("soul")
                    || variable_text.ends_with("schematics")
                {
                    return None;
                }
            }
            RPGMFileType::Weapons => {
                if variable_text == "makeshift2" {
                    return None;
                }
            }
            _ => {}
        },
    }

    Some(variable_text.into_owned())
}
