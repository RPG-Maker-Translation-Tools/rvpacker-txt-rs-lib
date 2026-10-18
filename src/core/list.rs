use super::{Base, string_is_only_symbols};
use crate::{
    get_line_break,
    marshal_compat::Value,
    types::{Code, TranslationEntry, TranslationMap},
};
use smallvec::SmallVec;
use std::{borrow::Cow, cell::Cell, ops::Deref};

/// Walks event command lists with [`Base`] only borrowed, so several maps can be walked at once.
///
/// On read it collects the text it finds instead of inserting it, and [`Base::replay`] inserts it afterwards, in
/// order - the same inserts a walk with `&mut Base` would have made. On write it only looks translations up and
/// patches the document it walks.
pub(super) struct Walker<'b> {
    base: &'b Base,
    /// The translations of the entry being walked, for [`Walker::get_key`] when duplicates are allowed.
    map: Option<&'b TranslationMap>,
    /// On read, the text found so far, in order.
    pub(super) found: Vec<String>,
    /// On write, how many lines of the walk were translated. Counted through `&self`, so a `Cell`.
    pub(super) hits: Cell<u32>,
}

impl Deref for Walker<'_> {
    type Target = Base;

    fn deref(&self) -> &Base {
        self.base
    }
}

impl Base {
    /// A [`Walker`] that looks translations up in `map`.
    pub(super) const fn walker<'b>(&'b self, map: Option<&'b TranslationMap>) -> Walker<'b> {
        Walker {
            base: self,
            map,
            found: Vec::new(),
            hits: Cell::new(0),
        }
    }

    /// Inserts text a [`Walker`] found, as if it had been inserted while walking.
    pub(super) fn replay(&mut self, found: impl IntoIterator<Item = String>) {
        for text in found {
            self.insert_string(Cow::Owned(text));
        }
    }
}

/// Whether `parsed`, read from `source`, is all of it: then `source` can be recorded as it is instead of copied.
/// On read `parsed` only ever borrows from `source`, so the same length means the same text.
const fn is_whole(parsed: &str, source: &str) -> bool {
    parsed.len() == source.len()
}

impl<'b> Walker<'b> {
    pub(super) fn get_key(&self, key: &str) -> Option<&'b TranslationEntry> {
        let entry = self.base.lookup(self.map, key);

        if entry.is_some() && self.mode.is_write() {
            self.hits.set(self.hits.get() + 1);
        }

        entry
    }

    pub(super) fn found(&mut self, text: Cow<'_, str>) {
        if !self.mode.is_write() {
            self.found.push(text.into_owned());
        }
    }

    /// Returns a borrow of `parameter` itself on read, when nothing had to be
    /// stripped off it - the common case - so the caller doesn't have to pay for a
    /// clone it may not need.
    pub(super) fn process_parameter<'a>(&self, code: Code, mut parameter: &'a str) -> Option<Cow<'a, str>> {
        if string_is_only_symbols(parameter) {
            return None;
        }

        // Markup cut off around the translatable text, and put back around its translation on write.
        let mut prefix = "";
        let mut shop_prefix: Option<&str> = None;

        // Advanced Text System is a VX Ace plugin; its prefixes are recognised
        // by their own markup, so no game identifier is involved.
        if code.is_any_dialogue()
            && self.engine_type.is_vx_ace()
            && let Some(i) = ats_dialogue_prefix_len(parameter)
        {
            if string_is_only_symbols(&parameter[i..]) {
                return None;
            }

            if self.mode.is_write() {
                prefix = &parameter[..i];
            }

            if !parameter.starts_with(r"\et") {
                parameter = &parameter[i..];
            }
        }

        if !self.engine_type.is_mvmz() && code.is_shop() {
            if !parameter.contains("shop_talk") {
                return None;
            }

            let (_, actual_string) = parameter.split_once('=')?;
            // Everything up to the opening quote, kept as written, so the line keeps its spacing.
            shop_prefix = Some(&parameter[..parameter.len() - actual_string.trim_start().len()]);

            let mut quoted = actual_string.trim().chars();
            quoted.next()?;
            quoted.next_back()?;
            let without_quotes = quoted.as_str();

            if without_quotes.is_empty() || string_is_only_symbols(without_quotes) {
                return None;
            }

            parameter = without_quotes;
        }

        if self.mode.is_write() {
            self.get_key(parameter).map(|t| {
                let prefix = if t.translation.starts_with(prefix) { "" } else { prefix };
                let mut translation = format!("{prefix}{t}", t = t.translation);

                // Put the `shop_talk_xxx=` prefix back on, so that the result is
                // self-contained and the caller needn't keep the source string
                // borrowed while it writes to the value it came from.
                if let Some(prefix) = shop_prefix {
                    translation = format!("{prefix}\"{translation}\"");
                }

                Cow::Owned(translation)
            })
        } else {
            Some(Cow::Borrowed(parameter))
        }
    }

    /// Applies an already-processed parameter, produced by [`Base::process_parameter`].
    pub(super) fn process_param(&mut self, value: &mut Value<'_>, parsed: Cow<'_, str>) {
        if self.mode.is_write() {
            self.write_translated(value, parsed.into_owned(), self.engine_type.is_mvmz());
        } else {
            self.found(parsed);
        }
    }

    pub(super) fn join_dialogue_lines(
        &mut self,
        list: &mut Value<'_>,
        dialogue_lines: &SmallVec<[String; 4]>,
        dialogue_line_indices: &mut SmallVec<[usize; 4]>,
        write_string_literally: bool,
    ) {
        let joined = dialogue_lines.join(if self.mode.is_write() { "\n" } else { get_line_break() });

        if self.mode.is_write() {
            let Some(translation) = self.process_parameter(Code::Dialogue, &joined) else {
                return;
            };

            let translation_lines: Vec<&str> = translation.lines().collect();
            let split_line_count = translation_lines.len();
            let dialogue_line_count = dialogue_lines.len();

            for (i, &index) in dialogue_line_indices.iter().enumerate() {
                let Some(mut item) = list.at(index) else { continue };
                let Some(mut params) = item.member(self.labels.parameters) else {
                    continue;
                };
                let Some(mut slot) = params.at(0) else { continue };

                if i < split_line_count {
                    self.write_translated(&mut slot, translation_lines[i].to_owned(), write_string_literally);
                } else {
                    // Overwrite leftover source text
                    self.write_translated(&mut slot, " ".to_owned(), write_string_literally);
                }
            }

            if split_line_count > dialogue_line_count {
                let remaining = translation_lines[dialogue_line_count - 1..].join("\n");

                // SAFETY: We checked that `dialogue_lines` are not empty before calling this.
                let last_index = unsafe { *dialogue_line_indices.last().unwrap_unchecked() };

                if let Some(mut item) = list.at(last_index)
                    && let Some(mut params) = item.member(self.labels.parameters)
                    && let Some(mut slot) = params.at(0)
                {
                    self.write_translated(&mut slot, remaining, write_string_literally);
                }
            }
        } else if let Some(parsed) = self.process_parameter(Code::Dialogue, &joined) {
            if is_whole(&parsed, &joined) {
                drop(parsed);
                self.found.push(joined);
            } else {
                self.found(parsed);
            }
        }
    }

    /// Processes the list of objects found in `Map`, `CommonEvents` and `Troops` files.
    ///
    /// # Parameters
    ///
    /// - `list` - the `list` array [`Value`] cursor.
    ///
    // Splitting this would rewrite working code, and the lint is about length only.
    #[allow(clippy::too_many_lines)]
    pub(super) fn process_list(&mut self, list: &mut Value<'_>) {
        let mut in_sequence = false;
        let mut write_string_literally = self.engine_type.is_mvmz();
        let mut dialogue_lines = SmallVec::with_capacity(4);
        let mut dialogue_line_indices = SmallVec::with_capacity(4);

        // Indexed rather than iterated, because `join_dialogue_lines` needs `list`
        // itself while we're partway through it.
        for item_idx in 0..list.len() {
            // A malformed command without a code is treated like an unknown one.
            let code = list
                .at(item_idx)
                .and_then(|mut item| item.member(self.labels.code)?.as_int())
                .map_or(Code::Bad, |code| Code::from(code as u16));

            let code = if code.is_dialogue_start() && !self.engine_type.is_xp() {
                Code::Bad
            } else {
                code
            };

            // Only VX Ace tags its strings, so only there does the source decide how a string is written. XP and VX
            // (Ruby 1.8) keep raw bytes, whatever the text, as the editor writes them.
            if self.mode.is_write()
                && self.engine_type.is_vx_ace()
                && let Some(mut item) = list.at(item_idx)
                && let Some(mut parameters) = item.member(self.labels.parameters)
            {
                let is_bytes = match code {
                    Code::ChoiceArray => parameters.at(0).and_then(|mut choices| Some(choices.at(0)?.is_bytes())),
                    Code::Misc1 | Code::Misc2 | Code::Choice => parameters.at(1).map(|value| value.is_bytes()),
                    _ => parameters.at(0).map(|value| value.is_bytes()),
                };

                if let Some(is_bytes) = is_bytes {
                    write_string_literally = !is_bytes;
                }
            }

            if in_sequence && (!self.engine_type.is_xp() && !code.is_any_dialogue())
                || (code.is_dialogue_start() && !dialogue_lines.is_empty())
            {
                if !dialogue_lines.is_empty() {
                    self.join_dialogue_lines(
                        list,
                        &dialogue_lines,
                        &mut dialogue_line_indices,
                        write_string_literally,
                    );
                    dialogue_lines.clear();
                    dialogue_line_indices.clear();
                }

                in_sequence = false;
            }

            if code.is_bad() {
                continue;
            }

            let Some(mut item) = list.at(item_idx) else { continue };
            let Some(mut parameters) = item.member(self.labels.parameters) else {
                continue;
            };

            if parameters.len() == 0 {
                continue;
            }

            let value_index = usize::from(code.is_any_misc() || code.is_choice());

            if code.is_choice_array() {
                let Some(mut choices) = parameters.at(value_index) else {
                    continue;
                };

                for choice_idx in 0..choices.len() {
                    if self.mode.is_write() {
                        // Scoped so that the borrow of `choices.at(choice_idx)` ends
                        // before we write back into it - `process_parameter` may
                        // borrow from `string`, which doesn't outlive the scope, so
                        // it's forced owned here.
                        let parsed = {
                            let Some(choice) = choices.at(choice_idx) else {
                                continue;
                            };

                            let Some(string) = self.extract_string(&choice, true) else {
                                continue;
                            };

                            self.process_parameter(code, &string).map(Cow::into_owned)
                        };

                        if let Some(parsed) = parsed
                            && let Some(mut choice) = choices.at(choice_idx)
                        {
                            self.process_param(&mut choice, Cow::Owned(parsed));
                        }
                    } else {
                        // Read/purge never write back to `choice`, so there's no
                        // need to re-fetch it after reading - handle it in one pass,
                        // taking the text directly instead of cloning it out.
                        let Some(mut choice) = choices.at(choice_idx) else {
                            continue;
                        };

                        let text = if let Some(taken) = (!self.reads_legacy_text()).then(|| choice.take_str()).flatten()
                        {
                            taken
                        } else {
                            let Some(string) = self.extract_string(&choice, true) else {
                                continue;
                            };

                            string.into_owned()
                        };

                        if let Some(parsed) = self.process_parameter(code, &text) {
                            if is_whole(&parsed, &text) {
                                drop(parsed);
                                self.found.push(text);
                            } else {
                                self.found(parsed);
                            }
                        }
                    }
                }
            } else {
                let Some(mut value) = parameters.at(value_index) else {
                    continue;
                };

                // Fast path on read/purge: move the value's own text out directly instead of
                // extracting a borrow and cloning it - a plain move whenever the
                // source is a JSON string or valid-UTF-8 Marshal text (the common
                // case; declared-non-UTF8/decoded text still has no clone to avoid,
                // so it falls back to the borrow+clone path unchanged). The read
                // document is discarded, so nothing reads the emptied text again.
                // Never on write: a Marshal string that isn't overwritten below (no translation) must keep its text.
                let taken = if self.mode.is_write() || self.reads_legacy_text() {
                    None
                } else {
                    value.take_str()
                };
                let parameter_string = if let Some(taken) = taken {
                    if !code.is_credit() && taken.is_empty() {
                        continue;
                    }

                    taken
                } else {
                    let Some(parameter_string) = self.extract_string(&value, false) else {
                        continue;
                    };

                    if !code.is_credit() && parameter_string.is_empty() {
                        continue;
                    }

                    parameter_string.into_owned()
                };

                if code.is_any_dialogue() {
                    dialogue_lines.push(parameter_string);

                    if self.mode.is_write() {
                        dialogue_line_indices.push(item_idx);
                    }

                    in_sequence = true;
                } else if let Some(parsed) = self.process_parameter(code, &parameter_string) {
                    if !self.mode.is_write() && is_whole(&parsed, &parameter_string) {
                        drop(parsed);
                        self.found.push(parameter_string);
                    } else {
                        self.process_param(&mut value, parsed);
                    }
                }
            }
        }

        // XP ends a box only at the next one, so the last box of a list is still pending here.
        if !dialogue_lines.is_empty() {
            self.join_dialogue_lines(
                list,
                &dialogue_lines,
                &mut dialogue_line_indices,
                write_string_literally,
            );
        }
    }
}

/// Byte index just past an Advanced Text System dialogue prefix, or [`None`] if the line carries none.
///
/// The VX Ace Advanced Text System plugin prefixes dialogue with the tile the textbox should appear above (`\nbt`,
/// `\nblt`) or the actor speaking (`\et[n]`). None of it is translatable text, so it is stripped before the line reaches
/// the translation file and restored on write - except `\et[n]`, which stays in the source line and is restored only if
/// the translation dropped it.
fn ats_dialogue_prefix_len(string: &str) -> Option<usize> {
    if string.starts_with(r"\et[") {
        // The id is at least one byte, and the bracket closes by byte 9.
        let id_end = r"\et[".len() + 1;
        let close = string
            .as_bytes()
            .get(id_end..)?
            .iter()
            .take(10 - id_end)
            .position(|&b| b == b']')?;

        Some(id_end + close + 1)
    } else if string.starts_with(r"\nbt") {
        Some(r"\nbt".len())
    } else if string.starts_with(r"\nblt") {
        Some(r"\nblt".len())
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::ats_dialogue_prefix_len;

    #[test]
    fn a_tile_prefix_ends_after_its_tag() {
        assert_eq!(ats_dialogue_prefix_len(r"\nbtHello"), Some(4));
        assert_eq!(ats_dialogue_prefix_len(r"\nbltHello"), Some(5));
    }

    #[test]
    fn an_actor_prefix_ends_after_its_bracket() {
        assert_eq!(ats_dialogue_prefix_len(r"\et[1]Hi"), Some(6));
        assert_eq!(ats_dialogue_prefix_len(r"\et[12]Hi"), Some(7));
    }

    #[test]
    fn an_actor_prefix_must_close_by_byte_nine() {
        assert_eq!(ats_dialogue_prefix_len(r"\et[12345]Hi"), Some(10));
        assert_eq!(ats_dialogue_prefix_len(r"\et[123456]Hi"), None);
        assert_eq!(ats_dialogue_prefix_len(r"\et[1"), None);
    }

    #[test]
    fn other_text_has_no_prefix() {
        assert_eq!(ats_dialogue_prefix_len("Hello"), None);
        assert_eq!(ats_dialogue_prefix_len(r"\et"), None);
    }
}
