use super::{Base, parse_rpgm_file, string_is_only_symbols};
use crate::{
    CommentPos, ProcessedData, get_line_break,
    marshal_compat::{RpgmData, Value},
    types::{Error, RPGMFileType, Variable},
};
use regex::Regex;
use std::{borrow::Cow, sync::LazyLock};

static IS_INVALID_MULTILINE_VARIABLE_RE: LazyLock<Regex> =
    LazyLock::new(|| unsafe { Regex::new(r"^#? ?<.*>.?$|^[a-z]\d$").unwrap_unchecked() });
static IS_INVALID_VARIABLE_RE: LazyLock<Regex> =
    LazyLock::new(|| unsafe { Regex::new(r"^[+-]?$|^///|---|restrict eval").unwrap_unchecked() });

impl Base {
    /// Processes the RPG Maker other file content.
    ///
    /// # Parameters
    ///
    /// - `filename` - Filename of the file that's being processed.
    /// - `content` - Content of the file that's being processed.
    /// - `translation` - Translation file contents. Ignored with [`crate::Mode::Read`], required otherwise.
    ///
    /// # Returns
    ///
    /// - Nothing if `mode` is [`crate::Mode::Write`] and no translation exists.
    /// - [`ProcessedData`], which contains RPG Maker data if `mode` is [`crate::Mode::Write`] and translation data otherwise.
    /// - [`Error`], if unable to parse the content.
    ///
    /// # Errors
    ///
    /// - [`Error::MarshalLoad`] - if unable to load the Marshal data.
    /// - [`Error::JsonParse`] - if unable to parse the JSON data.
    /// - [`Error::NoTranslation`] - if mode is not [`crate::Mode::Read`], and no translation was passed.
    ///
    /// # Panics
    ///
    /// May panic if passed content is not `Actors`, `Armors`, `Classes`, `Enemies`, `CommonEvents`, `Troops`, `Items`, `Skills`, `States`, `Weapons`.
    ///
    /// # Example
    ///
    /// ```no_run
    /// use rvpacker_txt_rs_lib::{core::Base, Mode, EngineType, Error};
    /// use std::fs::read;
    ///
    /// fn main() -> Result<(), Box<dyn std::error::Error>> {
    ///     let mut base = Base::new(Mode::read(), EngineType::VXAce);
    ///
    ///     let other_file_content = read("C:/Game/Data/Actors.rvdata2")?;
    ///     base.process_other("Actors.rvdata2", &other_file_content, None)?;
    ///     Ok(())
    /// }
    /// ```
    pub fn process_other(
        &mut self,
        filename: &str,
        content: &[u8],
        translation: Option<&str>,
    ) -> Result<Option<ProcessedData>, Error> {
        self.process_parsed_other(filename, content, None, translation)
    }

    /// [`Base::process_other`] for a file that may already be parsed: `parsed` is `content` parsed ahead of time,
    /// or [`None`] to parse it here.
    pub(crate) fn process_parsed_other(
        &mut self,
        filename: &str,
        content: &[u8],
        parsed: Option<RpgmData>,
        translation: Option<&str>,
    ) -> Result<Option<ProcessedData>, Error> {
        self.file_type = RPGMFileType::from_filename(filename);

        self.reset();
        self.initialize_translation(translation)?;

        let mut data = match parsed {
            Some(data) => data,
            None => parse_rpgm_file(content, self.engine_type)?,
        };
        let mut processed = false;

        {
            let mut root = data.root();

            // Skipping one, because the first entry is always null.
            root.for_each_element_mut(1, |object| {
                // Corrupt files may have objects without an ID; they carry nothing to translate.
                let Some(id) = object.member("id").and_then(|id| id.as_int()) else {
                    return;
                };
                let id = id as u32;

                // Only with duplicates removed does a missing section still have lines to translate, through another section.
                let missing_section = self.mode.is_write() && !self.translation.maps.contains_key(&id);

                if self.get_translation_map(id).is_break() {
                    if self.mode.is_purge() {
                        processed = true;
                    }

                    return;
                }

                // Decoded like any other text, so a name in the read encoding counts. A missing one only loses the
                // header comment: the entry is still processed and flushed, or an append would drop its translations.
                let entry_name = object
                    .member(self.labels.name)
                    .and_then(|name| self.extract_string(&name, false).map(Cow::into_owned));

                if let Some(entry_name) = &entry_name {
                    self.update_metadata(id, &[(CommentPos::Name, entry_name)]);
                }

                let translated = if self.file_type.is_events() || self.file_type.is_troops() {
                    self.process_object(object)
                } else {
                    self.process_array(object)
                };

                self.flush_translation(id);

                // A missing section is written only if one of its lines was translated, as with maps.
                if !missing_section || translated {
                    processed = true;
                }
            });
        }

        // Text that failed to decode fails this file, before anything of it is written.

        Self::take_decode_error()?;

        if !processed {
            return Ok(None);
        }

        Ok(Some(self.finish(data)))
    }

    #[allow(clippy::collapsible_match, clippy::single_match)]
    fn process_variable(&self, variable_text: &str, variable_type: Variable) -> Option<String> {
        if string_is_only_symbols(variable_text) {
            return None;
        }

        let mut variable_text = Cow::Borrowed(variable_text);

        if !self.engine_type.is_mvmz() {
            if variable_text
                .lines()
                .all(|line| line.is_empty() || IS_INVALID_MULTILINE_VARIABLE_RE.is_match(line))
                || IS_INVALID_VARIABLE_RE.is_match(&variable_text)
            {
                return None;
            }

            if variable_text.contains('\r') {
                variable_text = Cow::Owned(variable_text.replace("\r\n", "\n"));
            }
        }

        if self.mode.is_read() {
            return Some(variable_text.into_owned());
        }

        self.get_key(&variable_text).map(|translated| {
            let mut result = translated.to_string();

            if variable_type.is_any_message() && !(variable_type.is_message_2() && self.file_type.is_skills()) {
                result = String::from(' ') + &result;
            }

            result
        })
    }

    /// Processes an object from `CommonEvents` or `Troops` file.
    /// Returns whether any line of `object` was translated.
    fn process_object(&mut self, object: &mut Value<'_>) -> bool {
        let mut walker = self.walker(self.selected_translation_map());

        if self.file_type.is_troops() {
            if let Some(mut pages) = object.member(self.labels.pages) {
                pages.for_each_element_mut(0, |page| {
                    if let Some(mut list_array) = page.member(walker.labels.list) {
                        walker.process_list(&mut list_array);
                    }
                });
            }
        } else if let Some(mut list) = object.member(self.labels.list) {
            walker.process_list(&mut list);
        }

        let translated = walker.hits.get() > 0;
        let found = walker.found;
        self.replay(found);

        translated
    }

    /// Processes an object array from `Actors`, `Armors`, `Classes`, `Enemies`, `Items`, `States`, `Weapons` files.
    ///
    /// Returns whether any of its fields was translated.
    fn process_array(&mut self, array: &mut Value<'_>) -> bool {
        let variable_pairs = [
            (self.labels.name, Variable::Name),
            (self.labels.nickname, Variable::Nickname),
            (self.labels.description, Variable::Description),
            (self.labels.message1, Variable::Message1),
            (self.labels.message2, Variable::Message2),
            (self.labels.message3, Variable::Message3),
            (self.labels.message4, Variable::Message4),
            (self.labels.note, Variable::Note),
        ];

        let mut translated = false;

        for (variable_label, variable_type) in variable_pairs {
            let Some(mut object) = array.member(variable_label) else {
                continue;
            };

            let parsed = {
                let Some(mut string) = self.extract_string(&object, true) else {
                    continue;
                };

                if self.mode.is_write() && variable_type.is_any_message() {
                    string = Cow::Owned(string.lines().map(str::trim).collect::<Vec<_>>().join("\n"));
                }

                self.process_variable(&string, variable_type)
            };

            let Some(parsed) = parsed else {
                continue;
            };

            if self.mode.is_write() {
                self.write_translated(&mut object, parsed, self.engine_type.is_mvmz());
                translated = true;
            } else {
                let mut folded = parsed
                    .lines()
                    .fold(String::with_capacity(parsed.len() * 2), |mut output, line| {
                        let trimmed = if variable_type.is_any_message() {
                            line.trim()
                        } else {
                            line
                        };

                        output.push_str(trimmed);
                        output.push_str(get_line_break());

                        output
                    });

                // Every line ended with a break; dropping the last one in place keeps the buffer instead of copying it.
                folded.truncate(folded.len().saturating_sub(get_line_break().len()));

                self.insert_string(Cow::Owned(folded));
            }
        }

        translated
    }
}
