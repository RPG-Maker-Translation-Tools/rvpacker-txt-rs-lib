use super::*;
use crate::{
    CommentPos, ProcessedData,
    types::{Error, RPGMFileType, TranslationEntry},
};
use marshal_rs::{Get, Value};
use std::{borrow::Cow, mem::take};

impl Base {
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
        if self.mode.is_read() {
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
    /// use rvpacker_txt_rs_lib::{core::Base, Mode, ReadMode, EngineType, Error};
    /// use std::fs::read;
    ///
    /// fn main() -> Result<(), Box<dyn std::error::Error>> {
    ///     let mut base = Base::new(Mode::Read(ReadMode::Default { force: false }), EngineType::VXAce);
    ///
    ///     let system_file_content = read("C:/Game/Data/System.rvdata2")?;
    ///     base.process_system(&system_file_content, None)?;
    ///     Ok(())
    /// }
    /// ```
    pub fn process_system(
        &mut self,
        content: &[u8],
        translation: Option<&str>,
    ) -> Result<Option<ProcessedData>, Error> {
        self.reset();
        self.file_type = RPGMFileType::System;
        self.initialize_translation(translation)?;

        // Per-call state, kept out of `Base` so that borrowing it stays disjoint
        // from the `&mut self` the per-value helpers need.
        let mut system_value =
            parse_rpgm_file(content, self.engine_type, self.file_type)?;
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

            if self.get_translation_map(id).is_break() {
                if self.mode.is_purge() {
                    processed = true;
                }

                continue;
            }

            processed = true;

            self.update_metadata(
                id,
                Vec::from([(CommentPos::Name, entry_name)]),
            );

            if id <= 5 {
                let label = [
                    self.labels.armor_types,
                    self.labels.elements,
                    self.labels.skill_types,
                    self.labels.weapon_types,
                    self.labels.equip_types,
                ][id as usize - 1];

                let Some(array) = system_value[label].as_array_mut() else {
                    continue;
                };

                for value in array {
                    self.process_value(value);
                }
            } else if id == 6 {
                self.process_terms(&mut system_value);
            } else if id == 7 {
                self.process_currency_unit(&mut system_value);
            } else {
                self.process_game_title(&mut system_value);
            }

            self.flush_translation(id);
        }

        if !processed {
            return Ok(None);
        }

        Ok(Some(self.finish(system_value)))
    }

    fn process_terms(&mut self, system_value: &mut Value) {
        let Some(terms) = system_value[self.labels.terms].as_object_mut()
        else {
            return;
        };

        for (key, value) in terms.iter_mut() {
            if key == "messages" {
                if let Some(messages) = value.as_object_mut() {
                    for value in messages.values_mut() {
                        self.process_value(value);
                    }
                }
            } else if let Some(array) = value.as_array_mut() {
                for value in array {
                    self.process_value(value);
                }
            } else if value.is_bytes() || value.is_string() {
                self.process_value(value);
            }
        }
    }

    fn process_value(&mut self, value: &mut Value) {
        let Some(extracted) = self.extract_string(&*value, true) else {
            return;
        };

        if self.mode.is_read() {
            self.insert_string(Cow::Borrowed(extracted));
        } else if self.mode.is_write() {
            if let Some(translated) = self.get_key(extracted) {
                *value = Base::make_string_value(
                    translated,
                    self.engine_type.is_new(),
                );
            }
        } else {
            self.translation_map_mut()
                .insert(extracted.into(), TranslationEntry::default());
        }
    }

    fn process_currency_unit(&mut self, system_value: &mut Value) {
        if !self.engine_type.is_new() {
            let label = self.labels.currency_unit;
            self.process_value(&mut system_value[label]);
        }
    }

    fn process_game_title(&mut self, system_value: &mut Value) {
        if self.mode.is_write() {
            if !self.game_title.is_empty() {
                system_value[self.labels.game_title] =
                    Value::string(self.game_title.as_str());
            }
        } else {
            // User previously set the game title through set_game_title
            if !self.game_title.is_empty() {
                let title = take(&mut self.game_title);
                self.insert_string(Cow::Owned(title));
                return;
            }

            if let Some(game_title_value) =
                system_value.get(self.labels.game_title)
            {
                let Some(game_title) =
                    self.extract_string(game_title_value, true)
                else {
                    return;
                };

                let game_title = game_title.to_owned();
                self.insert_string(Cow::Owned(game_title));
            }
        }
    }
}
