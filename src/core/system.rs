use super::*;
use crate::{
    CommentPos, ProcessedData,
    marshal_compat::Value,
    types::{Error, RPGMFileType, TranslationEntry},
};
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
    /// On [`Mode::Read`] the title becomes an entry in the translation file; on
    /// [`Mode::Write`] it is written into the system file as-is. It used to be
    /// dropped on write, which left [`Base::process_system`]'s write branch
    /// unreachable and silently ignored the title callers passed.
    ///
    pub fn set_game_title(&mut self, title: &str) {
        self.game_title = title.to_string();
    }

    /// Processes the RPG Maker system file content.
    ///
    /// # Parameters
    ///
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
    /// May panic if passed content is not `System`.
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

        let mut data = parse_rpgm_file(content, self.engine_type)?;
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

            self.update_metadata(id, Vec::from([(CommentPos::Name, entry_name)]));

            let mut root = data.root();

            if id <= 5 {
                let label = [
                    self.labels.armor_types,
                    self.labels.elements,
                    self.labels.skill_types,
                    self.labels.weapon_types,
                    self.labels.equip_types,
                ][id as usize - 1];

                let Some(mut array) = root.member(label) else {
                    continue;
                };

                array.for_each_element_mut(0, |value| self.process_value(value));
            } else if id == 6 {
                self.process_terms(&mut root);
            } else if id == 7 {
                self.process_currency_unit(&mut root);
            } else {
                self.process_game_title(&mut root);
            }

            self.flush_translation(id);
        }

        if !processed {
            return Ok(None);
        }

        Ok(Some(self.finish(data)))
    }

    fn process_terms(&mut self, system_value: &mut Value<'_>) {
        let Some(mut terms) = system_value.member(self.labels.terms) else {
            return;
        };

        terms.for_each_member_mut(|key, value| {
            if key == "messages" {
                value.for_each_member_mut(|_, value| self.process_value(value));
            } else if value.is_container() {
                value.for_each_element_mut(0, |value| self.process_value(value));
            } else if value.is_bytes() || value.is_string() {
                self.process_value(value);
            }
        });
    }

    fn process_value(&mut self, value: &mut Value<'_>) {
        let Some(extracted) = self.extract_string(value, true) else {
            return;
        };

        if self.mode.is_read() {
            self.insert_string(extracted);
        } else if self.mode.is_write() {
            if let Some(translated) = self.get_key(&extracted) {
                let translated = translated.to_string();
                self.write_translated(value, translated, self.engine_type.is_mvmz());
            }
        } else {
            self.translation_map_mut()
                .insert(extracted.into_owned(), TranslationEntry::default());
        }
    }

    fn process_currency_unit(&mut self, system_value: &mut Value<'_>) {
        let label = self.labels.currency_unit;

        if let Some(mut value) = system_value.member(label) {
            self.process_value(&mut value);
        }
    }

    fn process_game_title(&mut self, system_value: &mut Value<'_>) {
        if self.mode.is_write() {
            if !self.game_title.is_empty()
                && let Some(mut value) = system_value.member(self.labels.game_title)
            {
                value.set_string(self.game_title.clone());
            }
        } else {
            // User previously set the game title through set_game_title
            if !self.game_title.is_empty() {
                let title = take(&mut self.game_title);
                self.insert_string(Cow::Owned(title));
                return;
            }

            if let Some(game_title_value) = system_value.member(self.labels.game_title) {
                let Some(game_title) = self.extract_string(&game_title_value, true) else {
                    return;
                };

                self.insert_string(game_title);
            }
        }
    }
}
