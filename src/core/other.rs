use super::*;
use crate::{
    CommentPos, ProcessedData, get_line_break,
    types::{Error, RPGMFileType, Variable},
};
use marshal_rs::{Get, Value};
use regex::Regex;
use std::{borrow::Cow, cell::LazyCell, fmt::Write as FmtWrite};

thread_local! {
    static IS_INVALID_MULTILINE_VARIABLE_RE: LazyCell<Regex> =
        LazyCell::new(|| unsafe {
            Regex::new(r"^#? ?<.*>.?$|^[a-z]\d$").unwrap_unchecked()
        });
    static IS_INVALID_VARIABLE_RE: LazyCell<Regex> = LazyCell::new(|| unsafe {
        Regex::new(r"^[+-]?$|^///|---|restrict eval").unwrap_unchecked()
    });
}

impl Base {
    /// Processes the RPG Maker other file content.
    ///
    /// # Parameters
    ///
    /// - `filename` - Filename of the file that's being processed.
    /// - `content` - Content of the file that's being processed.
    /// - `translation` - Contents of the translation file corresponding to the file. Isn't used with [`Mode::Read`]. Requires to be set with any other [`Mode`].
    ///
    /// # Returns
    ///
    /// - Nothing if `mode` is [`Mode::Write`] and no translation exists.
    /// - [`ProcessedData`], which contains RPG Maker data if `mode` is [`Mode::Write`] and translation data otherwise.
    /// - [`Error`], if unable to parse the content.
    ///
    /// # Errors
    ///
    /// - [`Error::MarshalLoad`] - if unable to load the Marshal data.
    /// - [`Error::JsonParse`] - if unable to parse the JSON data.
    /// - [`Error::NoTranslation`] - if mode is not [`Mode::Read`], and no translation was passed.
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
        self.file_type = RPGMFileType::from_filename(filename);

        self.reset();
        self.initialize_translation(translation)?;

        let mut entry_value = parse_rpgm_file(content, self.engine_type, self.file_type)?;

        // SAFETY: All "other" entries are always arrays.
        let object_array = unsafe { entry_value.as_array_mut().unwrap_unchecked() };

        let mut processed = false;

        // Skipping one, because the first entry is always null.
        for object in object_array.iter_mut().skip(1) {
            // SAFETY: Name and ID exists on every object.
            let id = unsafe { object["id"].as_int().unwrap_unchecked() } as u16;

            if self.get_translation_map(id).is_break() {
                if self.mode.is_purge() {
                    processed = true;
                }

                continue;
            }

            processed = true;

            let event_name = unsafe { object[self.labels.name].as_str().unwrap_unchecked() };

            self.update_metadata(id, Vec::from([(CommentPos::Name, event_name)]));

            if self.file_type.is_events() || self.file_type.is_troops() {
                self.process_object(object);
            } else {
                self.process_array(object);
            }

            self.flush_translation(id);
        }

        if !processed {
            return Ok(None);
        }

        Ok(Some(self.finish(entry_value)))
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
                .all(|line| line.is_empty() || IS_INVALID_MULTILINE_VARIABLE_RE.with(|r| r.is_match(line)))
                || IS_INVALID_VARIABLE_RE.with(|r| r.is_match(&variable_text))
            {
                return None;
            }

            variable_text = Cow::Owned(variable_text.replace("\r\n", "\n"));
        }

        if self.mode.is_read() {
            return Some(variable_text.into_owned());
        }

        let translated = self.get_key(&variable_text).map(|translated| {
            let mut result = translated.to_string();

            if variable_type.is_any_message() && !(variable_type.is_message_2() && self.file_type.is_skills()) {
                result = String::from(' ') + &result;
            }

            result
        });

        translated
    }

    /// Processes an object from `CommonEvents` or `Troops` file.
    fn process_object(&mut self, object: &mut Value) {
        if self.file_type.is_troops() {
            // SAFETY: Troops always include pages.
            let pages = unsafe { object[self.labels.pages].as_array_mut().unwrap_unchecked() };

            for page in pages {
                if let Some(list_array) = page[self.labels.list].as_array_mut() {
                    self.process_list(list_array);
                }
            }
        } else {
            // SAFETY: CommonEvents always include list.
            let list = unsafe { object[self.labels.list].as_array_mut().unwrap_unchecked() };

            self.process_list(list);
        }
    }

    /// Processes an object array from `Actors`, `Armors`, `Classes`, `Enemies`, `Items`, `States`, `Weapons` files.
    fn process_array(&mut self, array: &mut Value) {
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

        for (variable_label, variable_type) in variable_pairs {
            let Some(object) = array.get(variable_label) else {
                continue;
            };

            let Some(string) = self.extract_string(object, true) else {
                continue;
            };

            let mut string = Cow::Borrowed(string);

            if self.mode.is_write() && variable_type.is_any_message() {
                string = Cow::Owned(string.lines().map(str::trim).collect::<Vec<_>>().join("\n"));
            }

            let Some(parsed) = self.process_variable(&string, variable_type) else {
                continue;
            };

            if self.mode.is_write() {
                array[variable_label] = Value::string(parsed);
            } else {
                let folded = parsed
                    .lines()
                    .fold(String::with_capacity(parsed.len() * 2), |mut output, line| {
                        let trimmed = if variable_type.is_any_message() {
                            line.trim()
                        } else {
                            line
                        };

                        let _ = write!(output, "{trimmed}{brk}", brk = get_line_break());

                        output
                    });

                let replaced = unsafe { folded.strip_suffix(get_line_break()).unwrap_unchecked() };

                self.insert_string(Cow::Borrowed(replaced));
            }
        }
    }
}
