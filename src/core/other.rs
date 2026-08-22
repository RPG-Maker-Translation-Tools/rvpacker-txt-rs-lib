use super::*;
use crate::{
    BaseFlags, CommentPos, ProcessedData,
    constants::NEW_LINE,
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

/// Base for processing other files (`Actors`, `Armors`, `Classes`, `Enemies`, `CommonEvents`, `Troops`, `Items`, `Skills`, `States`, `Weapons`).
pub struct OtherBase<'a> {
    pub base: &'a mut Base,
}

impl<'a> OtherBase<'a> {
    /// Initializes system base using [`Base`].
    /// Before calling this, you should create a base and pass it here.
    ///
    /// # Example
    ///
    /// ```
    /// use rvpacker_txt_rs_lib::{core::{Base, OtherBase}, Mode, ReadMode, EngineType};
    ///
    /// let mut base = Base::new(Mode::Read(ReadMode::Default { force: false }), EngineType::VXAce);
    /// let mut other_base = OtherBase::new(&mut base);
    /// ```
    pub fn new(base: &'a mut Base) -> Self {
        base.reset();
        base.file_type = RPGMFileType::Invalid;

        Self { base }
    }

    /// Processes the RPG Maker other file content.
    ///
    /// # Parameters
    ///
    /// - `filename` - Filename of the file that's being processed.
    /// - `content` - Content of the file that's being processed.
    /// - `translation` - Contents of the translation file corresponding to the file. Isn't used with [`ReadMode::Default`]. Requires to be set with any other [`Mode`].
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
    /// - [`Error::NoTranslation`] - if mode is not [`ReadMode::Default`], and no translation was passed.
    ///
    /// # Panics
    ///
    /// May panic if passed content is not `Actors`, `Armors`, `Classes`, `Enemies`, `CommonEvents`, `Troops`, `Items`, `Skills`, `States`, `Weapons`.
    ///
    /// # Example
    ///
    /// ```no_run
    /// use rvpacker_txt_rs_lib::{core::{Base, OtherBase}, Mode, ReadMode, EngineType, Error};
    /// use std::fs::read;
    ///
    /// fn main() -> Result<(), Box<dyn std::error::Error>> {
    ///     let mut base = Base::new(Mode::Read(ReadMode::Default { force: false }), EngineType::VXAce);
    ///     let mut other_base = OtherBase::new(&mut base);
    ///
    ///     let other_file_content = read("C:/Game/Data/Actors.rvdata2")?;
    ///     other_base.process("Actors.rvdata2", &other_file_content, None)?;
    ///     Ok(())
    /// }
    /// ```
    pub fn process(
        &mut self,
        filename: &str,
        content: &[u8],
        translation: Option<&str>,
    ) -> Result<Option<ProcessedData>, Error> {
        self.base.file_type = RPGMFileType::from_filename(filename);

        self.base.reset();
        self.base.initialize_translation(translation)?;

        let mut entry_value = parse_rpgm_file(
            content,
            self.base.engine_type,
            self.base.file_type,
        )?;

        // SAFETY: All "other" entries are always arrays.
        let object_array =
            unsafe { entry_value.as_array_mut().unwrap_unchecked() };

        let mut processed = false;

        // Skipping one, because the first entry is always null.
        for object in object_array.iter_mut().skip(1) {
            // SAFETY: Name and ID exists on every object.
            let id = unsafe { object["id"].as_int().unwrap_unchecked() } as u16;

            if self.base.get_translation_map(id).is_break() {
                if self.base.mode.is_purge() {
                    processed = true;
                }

                continue;
            }

            processed = true;

            let event_name = unsafe {
                object[self.base.labels.name].as_str().unwrap_unchecked()
            };

            self.base.update_metadata(
                id,
                Vec::from([(CommentPos::Name, event_name)]),
            );

            if self.base.file_type.is_events()
                || self.base.file_type.is_troops()
            {
                self.process_object(object);
            } else {
                self.process_array(object);
            }

            self.base.flush_translation(id);
        }

        if !processed {
            return Ok(None);
        }

        Ok(Some(self.base.finish(entry_value)))
    }

    #[allow(clippy::collapsible_match, clippy::single_match)]
    fn process_variable(
        &self,
        variable_text: &str,
        note_text: Option<&str>,
        variable_type: Variable,
    ) -> Option<String> {
        if string_is_only_symbols(variable_text) {
            return None;
        }

        let mut variable_text = Cow::Borrowed(variable_text);

        if !self.base.engine_type.is_new() {
            if variable_text.lines().all(|line| {
                line.is_empty()
                    || IS_INVALID_MULTILINE_VARIABLE_RE
                        .with(|r| r.is_match(line))
            }) || IS_INVALID_VARIABLE_RE.with(|r| r.is_match(&variable_text))
            {
                return None;
            }

            variable_text = Cow::Owned(variable_text.replace("\r\n", "\n"));
        }

        match game::process_variable(
            self.base.game_type,
            variable_text,
            variable_type,
            note_text,
            self.base.mode,
            self.base.file_type,
            self.base.translation_maps.get(&u16::MAX),
        ) {
            game::VariableOutcome::Drop => return None,
            game::VariableOutcome::Done(text) => return Some(text),
            game::VariableOutcome::Continue(text) => variable_text = text,
        }

        if self.base.mode.is_read() {
            return Some(variable_text.into_owned());
        }

        let translated = self.base.get_key(&variable_text).map(|translated| {
            let mut result = translated.to_string();

            if variable_type.is_any_message()
                && !(variable_type.is_message_2()
                    && self.base.file_type.is_skills())
            {
                result = String::from(' ') + &result;
            }

            if game::variable_needs_leading_newline(
                self.base.game_type,
                variable_type,
            ) && !result.is_empty()
                && !result.starts_with('\n')
            {
                result.insert(0, '\n');
            }

            result += game::variable_suffix(self.base.game_type, variable_type);

            result
        });

        translated
    }

    /// Processes an object from `CommonEvents` or `Troops` file.
    fn process_object(&mut self, object: &mut Value) {
        if self.base.file_type.is_troops() {
            // SAFETY: Troops always include pages.
            let pages = unsafe {
                object[self.base.labels.pages]
                    .as_array_mut()
                    .unwrap_unchecked()
            };

            for page in pages {
                if let Some(list_array) =
                    page[self.base.labels.list].as_array_mut()
                {
                    self.base.process_list(list_array);
                }
            }
        } else {
            // SAFETY: CommonEvents always include list.
            let list = unsafe {
                object[self.base.labels.list]
                    .as_array_mut()
                    .unwrap_unchecked()
            };

            self.base.process_list(list);
        }
    }

    /// Processes an object array from `Actors`, `Armors`, `Classes`, `Enemies`, `Items`, `States`, `Weapons` files.
    fn process_array(&mut self, array: &mut Value) {
        let variable_pairs = [
            (self.base.labels.name, Variable::Name),
            (self.base.labels.nickname, Variable::Nickname),
            (self.base.labels.description, Variable::Description),
            (self.base.labels.message1, Variable::Message1),
            (self.base.labels.message2, Variable::Message2),
            (self.base.labels.message3, Variable::Message3),
            (self.base.labels.message4, Variable::Message4),
            (self.base.labels.note, Variable::Note),
        ];

        for (variable_label, variable_type) in variable_pairs {
            let Some(object) = array.get(variable_label) else {
                continue;
            };

            let Some(string) = self.base.extract_string(object, true) else {
                continue;
            };

            let mut string = Cow::Borrowed(string);

            if self.base.mode.is_write() {
                string = Cow::Owned(
                    string
                        .lines()
                        .map(str::trim)
                        .collect::<Vec<_>>()
                        .join("\n"),
                );
            }

            let note_text = if game::description_absorbs_note(
                self.base.game_type,
                variable_type,
            ) {
                array[self.base.labels.note].as_str()
            } else {
                None
            };

            let Some(parsed) =
                self.process_variable(&string, note_text, variable_type)
            else {
                continue;
            };

            if self.base.mode.is_write() {
                array[variable_label] = Value::string(parsed);
            } else {
                let folded = parsed.lines().fold(
                    String::with_capacity(parsed.len() * 2),
                    |mut output, line| {
                        let trimmed = if variable_type.is_any_message()
                            || self.base.flags.contains(BaseFlags::Trim)
                        {
                            line.trim()
                        } else {
                            line
                        };

                        let _ = write!(output, "{trimmed}{NEW_LINE}");

                        output
                    },
                );

                let replaced =
                    unsafe { folded.strip_suffix(NEW_LINE).unwrap_unchecked() };

                self.base.insert_string(Cow::Borrowed(replaced));
            }
        }
    }
}
