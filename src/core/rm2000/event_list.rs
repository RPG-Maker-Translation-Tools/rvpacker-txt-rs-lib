use super::super::list::Walker;
use super::{Base, string_is_only_symbols};
use crate::get_line_break;
use rm2k::{field::DbStr, raw::EventCommandList, rpg::enums::EventCommandCode as Code};
use smallvec::SmallVec;

impl Walker<'_> {
    /// The rm2k analogue of `core::list::process_list`/`join_dialogue_lines`.
    ///
    /// `EventCommandList` is a flat `Vec<EventCommand { code, indent, string,
    /// parameters }>`, structurally close to MV's `list` array but without the
    /// `member`/`at` indirection a [`crate::marshal_compat::Value`] cursor needs -
    /// fields are mutated directly.
    ///
    /// A `ShowMessage`(10110) followed by its `ShowMessage_2`(20110) lines is one
    /// dialogue box split across lines, so they're grouped and joined exactly
    /// like MV/VX's code-401/405 runs; the next `ShowMessage` starts a new box. `ShowChoiceOption`(20140) commands are
    /// each their own list entry (unlike MV, where choices are one array
    /// parameter), so they're translated individually.
    pub(super) fn process_rm2k_event_list(&mut self, commands: &mut EventCommandList<'_>) {
        let mut dialogue_lines: SmallVec<[String; 4]> = SmallVec::with_capacity(4);
        let mut dialogue_line_indices: SmallVec<[usize; 4]> = SmallVec::with_capacity(4);

        for index in 0..commands.0.len() {
            let code = commands.0[index].code;
            let is_dialogue = code == Code::SHOW_MESSAGE.0 || code == Code::SHOW_MESSAGE_2.0;

            // `SHOW_MESSAGE` opens a new box, so it ends the previous one even though both are dialogue.
            if (!is_dialogue || code == Code::SHOW_MESSAGE.0) && !dialogue_lines.is_empty() {
                self.join_rm2k_dialogue_lines(commands, &mut dialogue_lines, &mut dialogue_line_indices);
            }

            if is_dialogue {
                let text = self.decode_text(commands.0[index].string.as_bytes());

                if !text.is_empty() {
                    dialogue_lines.push(text);

                    if self.mode.is_write() {
                        dialogue_line_indices.push(index);
                    }
                }

                continue;
            }

            // A choice option, or the name or title a hero is given: each carries one visible string.
            if code == Code::SHOW_CHOICE_OPTION.0
                || code == Code::CHANGE_HERO_NAME.0
                || code == Code::CHANGE_HERO_TITLE.0
            {
                let text = self.decode_text(commands.0[index].string.as_bytes());

                if text.trim().is_empty() || string_is_only_symbols(&text) {
                    continue;
                }

                if self.mode.is_write() {
                    if let Some(translated) = self.get_key(&text) {
                        let bytes = self.encode_text(&translated.translation);
                        commands.0[index].string = DbStr::from_vec(bytes);
                    }
                } else {
                    self.found.push(text);
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

        // Dropped like MV/VX's symbol-only text.
        if string_is_only_symbols(&joined) {
            dialogue_lines.clear();
            dialogue_line_indices.clear();
            return;
        }

        if self.mode.is_write() {
            // Cloned to end the borrow of `self.translation` before writing back
            // into `commands`, mirroring `list::join_dialogue_lines`.
            if let Some(translation) = self.get_key(&joined).map(|t| t.translation.clone()) {
                let translation_lines: Vec<&str> = translation.lines().collect();
                let split_line_count = translation_lines.len();
                let dialogue_line_count = dialogue_lines.len();

                for (i, &index) in dialogue_line_indices.iter().enumerate() {
                    let bytes = self.encode_text(if i < split_line_count {
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
                    commands.0[last_index].string = DbStr::from_vec(self.encode_text(&remaining));
                }
            }
        } else {
            self.found.push(joined);
        }

        dialogue_lines.clear();
        dialogue_line_indices.clear();
    }
}

impl Base {
    /// [`Walker::process_rm2k_event_list`] on the selected translation map, recording what it found.
    pub(super) fn process_rm2k_event_list(&mut self, commands: &mut EventCommandList<'_>) {
        let mut walker = self.walker(self.selected_translation_map());
        walker.process_rm2k_event_list(commands);

        let found = walker.found;
        self.replay(found);
    }
}
