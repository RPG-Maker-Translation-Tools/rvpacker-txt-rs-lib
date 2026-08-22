use super::*;
use crate::{constants::NEW_LINE, types::Code};
use marshal_rs::Value;
use smallvec::SmallVec;
use std::borrow::Cow;

impl Base {
    pub(super) fn process_parameter(
        &self,
        code: Code,
        mut parameter: &str,
    ) -> Option<String> {
        if string_is_only_symbols(parameter) {
            return None;
        }

        let mut extra_strings: SmallVec<[(&str, bool); 4]> =
            SmallVec::with_capacity(4);
        let mut shop_prefix: Option<&str> = None;

        if game::drops_parameter(self.game_type, code, parameter) {
            return None;
        }

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

        if !self.engine_type.is_new() {
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
                let (left, mut actual_string) =
                    unsafe { parameter.split_once('=').unwrap_unchecked() };
                shop_prefix = Some(left);
                actual_string = actual_string.trim();

                if actual_string.len() < 2 {
                    return None;
                }

                let without_quotes = &actual_string[1..actual_string.len() - 1];

                if without_quotes.is_empty()
                    || string_is_only_symbols(without_quotes)
                {
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

                translation
            })
        } else {
            Some(parameter.to_string())
        }
    }

    /// Applies an already-processed parameter, produced by [`Base::process_parameter`].
    pub(super) fn process_param(&mut self, value: &mut Value, parsed: String) {
        if self.mode.is_write() {
            *value =
                Self::make_string_value(&parsed, self.engine_type.is_new());
        } else {
            self.insert_string(Cow::Owned(parsed));
        }
    }

    pub(super) fn join_dialogue_lines(
        &mut self,
        list: &mut [Value],
        dialogue_lines: &mut SmallVec<[String; 4]>,
        dialogue_line_indices: &mut SmallVec<[usize; 4]>,
        write_string_literally: bool,
    ) {
        let joined = dialogue_lines.join(if self.mode.is_write() {
            "\n"
        } else {
            NEW_LINE
        });

        if self.mode.is_write() {
            let Some(translation) =
                self.process_parameter(Code::Dialogue, &joined)
            else {
                return;
            };

            let translation_lines: Vec<&str> = translation.lines().collect();
            let split_line_count = translation_lines.len();
            let dialogue_line_count = dialogue_lines.len();

            for (i, &index) in dialogue_line_indices.iter().enumerate() {
                list[index][self.labels.parameters][0] = if i < split_line_count
                {
                    Self::make_string_value(
                        translation_lines[i],
                        write_string_literally,
                    )
                } else {
                    // Overwrite leftover source text
                    Value::string(" ")
                }
            }

            if split_line_count > dialogue_line_count {
                let remaining =
                    translation_lines[dialogue_line_count - 1..].join("\n");

                // SAFETY: We checked that `dialogue_lines` are not empty before calling this.
                list[unsafe {
                    *dialogue_line_indices.last().unwrap_unchecked()
                }][self.labels.parameters][0] = Value::string(remaining);
            }
        } else if let Some(parsed) =
            self.process_parameter(Code::Dialogue, &joined)
        {
            self.process_param(&mut Value::default(), parsed);
        }
    }

    /// Processes the list of objects found in `Map`, `CommonEvents` and `Troops` files.
    ///
    /// # Parameters
    ///
    /// - `list` - list of [`Value`]s.
    ///
    pub(super) fn process_list(&mut self, list: &mut Vec<Value>) {
        let mut in_sequence = false;
        let mut write_string_literally = self.engine_type.is_new();
        let mut dialogue_lines = SmallVec::with_capacity(4);
        let mut dialogue_line_indices = SmallVec::with_capacity(4);

        // Indexed rather than iterated, because `join_dialogue_lines` needs `list`
        // itself while we're partway through it.
        for item_idx in 0..list.len() {
            let item = &mut list[item_idx];

            // SAFETY: Each item must contain code.
            let code = Code::from(unsafe {
                item[self.labels.code].as_int().unwrap_unchecked()
            } as u16);

            let code = if code.is_dialogue_start() && !self.engine_type.is_xp()
            {
                Code::Bad
            } else {
                code
            };

            if self.mode.is_write() && !self.engine_type.is_new() {
                // SAFETY: Each item must contain parameters.
                let parameters = unsafe {
                    item[self.labels.parameters].as_array().unwrap_unchecked()
                };

                if !parameters.is_empty() {
                    write_string_literally = !match code {
                        Code::ChoiceArray => parameters[0][0].is_bytes(),
                        Code::Misc1 | Code::Misc2 | Code::Choice => {
                            parameters[1].is_bytes()
                        }
                        _ => parameters[0].is_bytes(),
                    }
                }
            }

            if in_sequence
                && (!self.engine_type.is_xp() && !code.is_any_dialogue())
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

            // SAFETY: Each item must contain parameters.
            let parameters = unsafe {
                list[item_idx][self.labels.parameters]
                    .as_array_mut()
                    .unwrap_unchecked()
            };

            if parameters.is_empty() {
                continue;
            }

            let value_index =
                usize::from(code.is_any_misc() || code.is_choice());

            if code.is_choice_array() {
                // SAFETY: We have just checked - it's an array.
                let choices = unsafe {
                    parameters[value_index].as_array_mut().unwrap_unchecked()
                };

                for choice_idx in 0..choices.len() {
                    // Scoped so that the borrow of `choices[choice_idx]` ends
                    // before we write back into it; `process_parameter` returns
                    // owned data, so nothing outlives the scope.
                    let parsed = {
                        let Some(string) =
                            self.extract_string(&choices[choice_idx], true)
                        else {
                            continue;
                        };

                        self.process_parameter(code, string)
                    };

                    if let Some(parsed) = parsed {
                        self.process_param(&mut choices[choice_idx], parsed);
                    }
                }
            } else {
                let value = &mut parameters[value_index];

                let Some(parameter_string) =
                    self.extract_string(&*value, false)
                else {
                    continue;
                };

                if !code.is_credit() && parameter_string.is_empty() {
                    continue;
                }

                if code.is_any_dialogue() {
                    dialogue_lines.push(parameter_string.into());

                    if self.mode.is_write() {
                        dialogue_line_indices.push(item_idx);
                    }

                    in_sequence = true;
                } else if let Some(parsed) =
                    self.process_parameter(code, parameter_string)
                {
                    self.process_param(value, parsed);
                }
            }
        }
    }
}
