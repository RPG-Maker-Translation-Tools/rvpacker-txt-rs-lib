use super::*;
use crate::{get_line_break, marshal_compat::Value, types::Code};
use smallvec::SmallVec;
use std::borrow::Cow;

impl Base {
    /// Returns a borrow of `parameter` itself on read, when nothing had to be
    /// stripped off it - the common case - so the caller doesn't have to pay for a
    /// clone it may not need.
    pub(super) fn process_parameter<'a>(&self, code: Code, mut parameter: &'a str) -> Option<Cow<'a, str>> {
        if string_is_only_symbols(parameter) {
            return None;
        }

        let mut extra_strings: SmallVec<[(&str, bool); 4]> = SmallVec::with_capacity(4);
        let mut shop_prefix: Option<&str> = None;

        // Advanced Text System is a VX Ace plugin; its prefixes are recognised
        // by their own markup, so no game identifier is involved.
        if code.is_any_dialogue()
            && self.engine_type.is_vx_ace()
            && let Some(i) = plugins::ats_dialogue_prefix_len(parameter)
        {
            if string_is_only_symbols(&parameter[i..]) {
                return None;
            }

            if self.mode.is_write() {
                extra_strings.push((&parameter[..i], false));
            }

            if !parameter.starts_with(r"\et") {
                parameter = &parameter[i..];
            }
        }

        if !self.engine_type.is_mvmz() {
            if let Some(i) = ends_with_if_index(parameter) {
                if self.mode.is_write() {
                    extra_strings.push((&parameter[..i], true));
                }

                parameter = &parameter[..i];
            }

            if code.is_shop() {
                if !parameter.contains("shop_talk") {
                    return None;
                }

                // SAFETY: At this point, shop parameter should always contain '='.
                let (left, mut actual_string) = unsafe { parameter.split_once('=').unwrap_unchecked() };
                shop_prefix = Some(left);
                actual_string = actual_string.trim();

                if actual_string.len() < 2 {
                    return None;
                }

                let without_quotes = &actual_string[1..actual_string.len() - 1];

                if without_quotes.is_empty() || string_is_only_symbols(without_quotes) {
                    return None;
                }

                parameter = without_quotes;
            }
        }

        if self.mode.is_write() {
            self.get_key(parameter).map(|t| {
                let mut translation = if extra_strings.is_empty() {
                    t.translation.to_string()
                } else {
                    String::new()
                };

                for (string, append) in extra_strings {
                    if append {
                        translation = t.to_string() + string;
                    } else {
                        translation = format!("{string}{t}", t = t.translation);
                    }
                }

                // Put the `shop_talk_xxx=` prefix back on, so that the result is
                // self-contained and the caller needn't keep the source string
                // borrowed while it writes to the value it came from.
                if let Some(prefix) = shop_prefix {
                    translation = format!("{prefix}=\"{translation}\"");
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
            self.insert_string(parsed);
        }
    }

    pub(super) fn join_dialogue_lines(
        &mut self,
        list: &mut Value<'_>,
        dialogue_lines: &mut SmallVec<[String; 4]>,
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
                    slot.set_string(" ".to_owned());
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
            self.insert_string(parsed);
        }
    }

    /// Processes the list of objects found in `Map`, `CommonEvents` and `Troops` files.
    ///
    /// # Parameters
    ///
    /// - `list` - the `list` array [`Value`] cursor.
    ///
    pub(super) fn process_list(&mut self, list: &mut Value<'_>) {
        let mut in_sequence = false;
        let mut write_string_literally = self.engine_type.is_mvmz();
        let mut dialogue_lines = SmallVec::with_capacity(4);
        let mut dialogue_line_indices = SmallVec::with_capacity(4);

        // Indexed rather than iterated, because `join_dialogue_lines` needs `list`
        // itself while we're partway through it.
        for item_idx in 0..list.len() {
            // SAFETY: Each item must contain code.
            let code = {
                let mut item = unsafe { list.at(item_idx).unwrap_unchecked() };
                let code = unsafe {
                    item.member(self.labels.code)
                        .unwrap_unchecked()
                        .as_int()
                        .unwrap_unchecked()
                };
                Code::from(code as u16)
            };

            let code = if code.is_dialogue_start() && !self.engine_type.is_xp() {
                Code::Bad
            } else {
                code
            };

            if self.mode.is_write() && !self.engine_type.is_mvmz() {
                let mut item = unsafe { list.at(item_idx).unwrap_unchecked() };
                // SAFETY: Each item must contain parameters.
                let mut parameters = unsafe { item.member(self.labels.parameters).unwrap_unchecked() };

                if parameters.len() > 0 {
                    write_string_literally = !match code {
                        Code::ChoiceArray => unsafe {
                            parameters.at(0).unwrap_unchecked().at(0).unwrap_unchecked().is_bytes()
                        },
                        Code::Misc1 | Code::Misc2 | Code::Choice => unsafe {
                            parameters.at(1).unwrap_unchecked().is_bytes()
                        },
                        _ => unsafe { parameters.at(0).unwrap_unchecked().is_bytes() },
                    }
                }
            }

            if in_sequence && (!self.engine_type.is_xp() && !code.is_any_dialogue())
                || (code.is_dialogue_start() && !dialogue_lines.is_empty())
            {
                if !dialogue_lines.is_empty() {
                    self.join_dialogue_lines(
                        list,
                        &mut dialogue_lines,
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

            let mut item = unsafe { list.at(item_idx).unwrap_unchecked() };
            // SAFETY: Each item must contain parameters.
            let mut parameters = unsafe { item.member(self.labels.parameters).unwrap_unchecked() };

            if parameters.len() == 0 {
                continue;
            }

            let value_index = usize::from(code.is_any_misc() || code.is_choice());

            if code.is_choice_array() {
                // SAFETY: We have just checked - it's an array.
                let mut choices = unsafe { parameters.at(value_index).unwrap_unchecked() };

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

                        let text = if let Some(taken) = choice.take_str() {
                            taken
                        } else {
                            let Some(string) = self.extract_string(&choice, true) else {
                                continue;
                            };

                            string.into_owned()
                        };

                        if let Some(parsed) = self.process_parameter(code, &text) {
                            self.insert_string(parsed);
                        }
                    }
                }
            } else {
                let Some(mut value) = parameters.at(value_index) else {
                    continue;
                };

                // Fast path: move the value's own text out directly instead of
                // extracting a borrow and cloning it - a plain move whenever the
                // source is a JSON string or valid-UTF-8 Marshal text (the common
                // case; declared-non-UTF8/decoded text still has no clone to avoid,
                // so it falls back to the borrow+clone path unchanged). Sound in
                // both modes: nothing reads `value`'s old content again below -
                // write mode only ever overwrites it via `process_param`.
                let parameter_string = if let Some(taken) = value.take_str() {
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
                    self.process_param(&mut value, parsed);
                }
            }
        }
    }
}
