use super::*;
use crate::get_line_break;
use rm2k::{field::DbStr, raw::EventCommandList, rpg::enums::EventCommandCode as Code};
use smallvec::SmallVec;
use std::borrow::Cow;

impl Base {
    /// The rm2k analogue of `core::list::process_list`/`join_dialogue_lines`.
    ///
    /// `EventCommandList` is a flat `Vec<EventCommand { code, indent, string,
    /// parameters }>`, structurally close to MV's `list` array but without the
    /// `member`/`at` indirection a [`crate::marshal_compat::Value`] cursor needs -
    /// fields are mutated directly.
    ///
    /// Consecutive `ShowMessage`(10110)/`ShowMessage_2`(20110) commands are one
    /// dialogue box split across lines, so they're grouped and joined exactly
    /// like MV/VX's code-401/405 runs. `ShowChoiceOption`(20140) commands are
    /// each their own list entry (unlike MV, where choices are one array
    /// parameter), so they're translated individually.
    pub(super) fn process_rm2k_event_list(&mut self, commands: &mut EventCommandList<'_>) {
        let mut dialogue_lines: SmallVec<[String; 4]> = SmallVec::with_capacity(4);
        let mut dialogue_line_indices: SmallVec<[usize; 4]> = SmallVec::with_capacity(4);

        for index in 0..commands.0.len() {
            let code = commands.0[index].code;
            let is_dialogue = code == Code::SHOW_MESSAGE.0 || code == Code::SHOW_MESSAGE_2.0;

            if !is_dialogue && !dialogue_lines.is_empty() {
                self.join_rm2k_dialogue_lines(commands, &mut dialogue_lines, &mut dialogue_line_indices);
            }

            if is_dialogue {
                let text = self.decode_with_fallback(commands.0[index].string.as_bytes());

                if !text.is_empty() {
                    dialogue_lines.push(text);

                    if self.mode.is_write() {
                        dialogue_line_indices.push(index);
                    }
                }

                continue;
            }

            if code == Code::SHOW_CHOICE_OPTION.0 {
                let text = self.decode_with_fallback(commands.0[index].string.as_bytes());

                if text.trim().is_empty() || string_is_only_symbols(&text) {
                    continue;
                }

                if self.mode.is_write() {
                    if let Some(translated) = self.get_key(&text) {
                        let bytes = self.encode_with_fallback(&translated.translation);
                        commands.0[index].string = DbStr::from_vec(bytes);
                    }
                } else {
                    self.insert_string(Cow::Owned(text));
                }
            }
        }

        if !dialogue_lines.is_empty() {
            self.join_rm2k_dialogue_lines(commands, &mut dialogue_lines, &mut dialogue_line_indices);
        }
    }

    fn join_rm2k_dialogue_lines(
        &mut self,
        commands: &mut EventCommandList<'_>,
        dialogue_lines: &mut SmallVec<[String; 4]>,
        dialogue_line_indices: &mut SmallVec<[usize; 4]>,
    ) {
        let joined = dialogue_lines.join(if self.mode.is_write() { "\n" } else { get_line_break() });

        if self.mode.is_write() {
            // Cloned to end the borrow of `self.translation` before writing back
            // into `commands`, mirroring `list::join_dialogue_lines`.
            if let Some(translation) = self.get_key(&joined).map(|t| t.translation.clone()) {
                let translation_lines: Vec<&str> = translation.lines().collect();
                let split_line_count = translation_lines.len();
                let dialogue_line_count = dialogue_lines.len();

                for (i, &index) in dialogue_line_indices.iter().enumerate() {
                    let bytes = self.encode_with_fallback(if i < split_line_count {
                        translation_lines[i]
                    } else {
                        " "
                    });
                    commands.0[index].string = DbStr::from_vec(bytes);
                }

                if split_line_count > dialogue_line_count
                    && let Some(&last_index) = dialogue_line_indices.last()
                {
                    let remaining = translation_lines[dialogue_line_count - 1..].join("\n");
                    commands.0[last_index].string = DbStr::from_vec(self.encode_with_fallback(&remaining));
                }
            }
        } else {
            self.insert_string(Cow::Owned(joined));
        }

        dialogue_lines.clear();
        dialogue_line_indices.clear();
    }
}
