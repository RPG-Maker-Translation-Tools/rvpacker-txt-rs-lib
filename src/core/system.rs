use super::*;
use crate::{
    CommentPos, ProcessedData,
    types::{Error, RPGMFileType, TranslationEntry},
};
use marshal_rs::{Get, Value};
use std::{borrow::Cow, mem::take};

pub struct SystemBase<'a> {
    pub base: &'a mut Base,
    game_title: String,
    system_value: Value,
}

impl<'a> SystemBase<'a> {
    /// Initializes system base using [`Base`].
    /// Before calling this, you should create a base and pass it here.
    ///
    /// # Example
    ///
    /// ```
    /// use rvpacker_txt_rs_lib::{core::{Base, SystemBase}, Mode, ReadMode, EngineType};
    ///
    /// let mut base = Base::new(Mode::Read(ReadMode::Default { force: false }), EngineType::VXAce);
    /// let mut system_base = SystemBase::new(&mut base);
    /// ```
    pub fn new(base: &'a mut Base) -> Self {
        base.reset();
        base.file_type = RPGMFileType::System;

        Self {
            base,
            game_title: String::new(),
            system_value: Value::default(),
        }
    }

    /// This function exists for compatibility with RPG Maker XP, VX and VX Ace. It should be called only when reading.
    ///
    /// RPG Maker XP/VX/VXA games may not contain game title in their respective system file. Instead, they may only contain the title in `Game.ini` file. This file is not necessarily UTF-8 encoded.
    ///
    /// Since there's no way to tell the encoding, it's user responsibility to call [`get_ini_title`], find title's encoding through trial-and-error, and pass it here.
    ///
    /// Passed title overrides automatic extraction; that means that passed title will be preferred over the title from the system file, if title even exists there.
    ///
    /// # Parameters
    ///
    /// `title` - UTF-8 encoded [`&str`] title.
    ///
    /// # Note
    ///
    /// This function is no-op if mode is not [`Mode::Read`].
    ///
    pub fn set_game_title(&mut self, title: &str) {
        if self.base.mode.is_read() {
            self.game_title = title.to_string();
        }
    }

    /// Processes the RPG Maker system file content.
    ///
    /// # Parameters
    ///
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
    /// May panic if passed content is not `System`.
    ///
    /// # Example
    ///
    /// ```no_run
    /// use rvpacker_txt_rs_lib::{core::{Base, SystemBase}, Mode, ReadMode, EngineType, Error};
    /// use std::fs::read;
    ///
    /// fn main() -> Result<(), Box<dyn std::error::Error>> {
    ///     let mut base = Base::new(Mode::Read(ReadMode::Default { force: false }), EngineType::VXAce);
    ///     let mut system_base = SystemBase::new(&mut base);
    ///
    ///     let system_file_content = read("C:/Game/Data/System.rvdata2")?;
    ///     system_base.process(&system_file_content, None)?;
    ///     Ok(())
    /// }
    /// ```
    pub fn process(
        mut self,
        content: &[u8],
        translation: Option<&str>,
    ) -> Result<Option<ProcessedData>, Error> {
        self.base.initialize_translation(translation)?;

        self.system_value = parse_rpgm_file(
            content,
            self.base.engine_type,
            self.base.file_type,
        )?;
        let mut processed = false;

        for (entry_id, entry_name) in [
            "Armor Types",
            "Elements",
            "Skill Types",
            "Weapon Types",
            "Equip Types",
            "Terms",
            "Currency Unit",
            "Game Title",
        ]
        .into_iter()
        .enumerate()
        {
            let id = entry_id as u16 + 1;

            if self.base.get_translation_map(id).is_break() {
                if self.base.mode.is_purge() {
                    processed = true;
                }

                continue;
            }

            processed = true;

            self.base.update_metadata(
                id,
                Vec::from([(CommentPos::Name, entry_name)]),
            );

            if id <= 5 {
                let label = [
                    self.base.labels.armor_types,
                    self.base.labels.elements,
                    self.base.labels.skill_types,
                    self.base.labels.weapon_types,
                    self.base.labels.equip_types,
                ][id as usize - 1];

                let Some(array) = self.system_value[label].as_array_mut()
                else {
                    continue;
                };

                for value in array {
                    Self::process_value(self.base, value);
                }
            } else if id == 6 {
                self.process_terms();
            } else if id == 7 {
                self.process_currency_unit();
            } else {
                self.process_game_title();
            }

            self.base.flush_translation(id);
        }

        if !processed {
            return Ok(None);
        }

        Ok(Some(self.base.finish(self.system_value.take())))
    }

    fn process_terms(&mut self) {
        let base = &mut *self.base;

        let Some(terms) = self.system_value[base.labels.terms].as_object_mut()
        else {
            return;
        };

        for (key, value) in terms.iter_mut() {
            if key == "messages" {
                if let Some(messages) = value.as_object_mut() {
                    for value in messages.values_mut() {
                        Self::process_value(base, value);
                    }
                }
            } else if let Some(array) = value.as_array_mut() {
                for value in array {
                    Self::process_value(base, value);
                }
            } else if value.is_bytes() || value.is_string() {
                Self::process_value(base, value);
            }
        }
    }

    /// Takes `base` rather than `&mut self`, so callers can hold a mutable borrow
    /// of the disjoint `system_value` field while calling it.
    fn process_value(base: &mut Base, value: &mut Value) {
        let Some(extracted) = base.extract_string(&*value, true) else {
            return;
        };

        if base.mode.is_read() {
            base.insert_string(Cow::Borrowed(extracted));
        } else if base.mode.is_write() {
            if let Some(translated) = base.get_key(extracted) {
                *value = Base::make_string_value(
                    translated,
                    base.engine_type.is_new(),
                );
            }
        } else {
            base.translation_map_mut()
                .insert(extracted.into(), TranslationEntry::default());
        }
    }

    fn process_currency_unit(&mut self) {
        if !self.base.engine_type.is_new() {
            let label = self.base.labels.currency_unit;
            Self::process_value(self.base, &mut self.system_value[label]);
        }
    }

    fn process_game_title(&mut self) {
        if self.base.mode.is_write() {
            if !self.game_title.is_empty() {
                self.system_value[self.base.labels.game_title] =
                    Value::string(self.game_title.as_str());
            }
        } else {
            // User previously set the game title through set_game_title
            if !self.game_title.is_empty() {
                let title = take(&mut self.game_title);
                self.base.insert_string(Cow::Owned(title));
                return;
            }

            if let Some(game_title_value) =
                self.system_value.get(self.base.labels.game_title)
            {
                let Some(game_title) =
                    self.base.extract_string(game_title_value, true)
                else {
                    return;
                };

                let game_title = game_title.to_owned();
                self.base.insert_string(Cow::Owned(game_title));
            }
        }
    }
}
