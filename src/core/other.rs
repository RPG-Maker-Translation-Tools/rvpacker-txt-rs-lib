use super::*;
use crate::{
    BaseFlags, CommentPos, ProcessedData,
    constants::NEW_LINE,
    types::{Error, GameType, RPGMFileType, Variable},
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

    fn process_variable_termina(
        &self,
        mut variable_text: Cow<'_, str>,
        variable_type: Variable,
        note_text: Option<&str>,
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
                            let is_continuation = note_first_char == '\n'
                                && note_second_char != '\n';

                            let first_char_is_valid = note_first_char
                                .is_ascii_alphabetic()
                                || note_first_char == '"'
                                || note.starts_with("4 sticks");

                            let first_char_is_punctuation = matches!(
                                note_first_char,
                                '.' | '!' | '/' | '?'
                            );

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
                                note_string = String::from(
                                    if self.base.mode.is_write() {
                                        "\n"
                                    } else {
                                        NEW_LINE
                                    },
                                ) + left;
                            } else if self.base.mode.is_read() {
                                return None;
                            }
                        } else if note.ends_with(['.', '%', '!', '"'])
                            || note.ends_with("takes place?")
                        {
                            note_string = note.into();
                        } else if self.base.mode.is_read() {
                            return None;
                        }

                        if note_string.is_empty() {
                            if self.base.mode.is_read() {
                                return None;
                            }
                        } else {
                            variable_text = Cow::Owned(format!(
                                "{variable_text}{note_string}"
                            ));
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
                if self.base.mode.is_write() && self.base.file_type.is_items() {
                    for string in [
                        "<Menu Category: Items>",
                        "<Menu Category: Food>",
                        "<Menu Category: Healing>",
                        "<Menu Category: Body bag>",
                    ] {
                        if variable_text.rfind(string).is_some() {
                            return Some(variable_text.replace(
                                string,
                                &self.base.translation_maps[&u16::MAX][string],
                            ));
                        }
                    }
                }

                if !self.base.file_type.is_classes() {
                    return None;
                }
            }
            Variable::Name | Variable::Nickname => match self.base.file_type {
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

        match self.base.game_type {
            GameType::Termina => {
                if let Some(text) = self.process_variable_termina(
                    variable_text,
                    variable_type,
                    note_text,
                ) {
                    if self.base.mode.is_write()
                        && self.base.file_type.is_items()
                        && variable_type.is_note()
                    {
                        return Some(text);
                    }

                    variable_text = Cow::Owned(text);
                } else {
                    return None;
                }
            }
            // custom processing for other games
            _ => {}
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

            match self.base.game_type {
                GameType::Termina => match variable_type {
                    Variable::Note => {
                        if let Some(first_char) = result.chars().next() {
                            if first_char != '\n' {
                                result = String::from('\n') + &result;
                            }
                        }
                    }
                    _ => {}
                },
                _ => {}
            }

            if self.base.game_type.is_termina()
                && variable_type.is_description()
            {
                result += "\n\n\n\n";
            }

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

            let note_text = if self.base.game_type.is_termina()
                && variable_type.is_description()
            {
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
