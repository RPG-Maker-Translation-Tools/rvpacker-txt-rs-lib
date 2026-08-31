use super::*;
use crate::{
    CommentPos, ProcessedData,
    constants::{COULD_NOT_SPLIT_LINE_MSG, IN_FILE_MSG},
    get_event_id_comment, get_event_name_comment, get_event_pos_comment, get_line_separator,
    marshal_compat::{RpgmData, Value, marshal_as_text},
    types::{Error, RPGMFileType, TranslationEntry, TranslationMap},
};
use marshal_rs::value::ValueRef;
use smallvec::SmallVec;
use std::borrow::Cow;

impl Base {
    /// Prepares this base to process a run of map files.
    ///
    /// Maps accumulate into one translation file, so they are processed as a run:
    /// call this, then [`Base::process_map`] per file, then [`Base::finish_maps`].
    pub fn begin_maps(&mut self) {
        self.reset();
        self.file_type = RPGMFileType::Map;
        self.mapinfos = None;
    }

    /// Returns the translation data, accumulated after processing multiple maps.
    ///
    /// Returns the actual data only with [`Mode::Read`] or [`Mode::Purge`].
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
    ///     let mapinfos = read("C:/Game/Data/MapInfos.rvdata2")?;
    ///
    ///     let map_file_content = read("C:/Game/Data/Map001.rvdata2")?;
    ///     let data = base.process_map("Map001.rvdata2", &map_file_content, &mapinfos, None)?;
    ///
    ///     let map_file_content = read("C:/Game/Data/Map002.rvdata2")?;
    ///     let data = base.process_map("Map002.rvdata2", &map_file_content, &mapinfos, None)?;
    ///
    ///     let translation_data = base.finish_maps();
    ///     Ok(())
    /// }
    /// ```
    pub fn finish_maps(&mut self) -> ProcessedData {
        self.finish(RpgmData::from_json(serde_json::Value::Null))
    }

    /// Processes the RPG Maker map file content.
    ///
    /// To get the translation data, you need to call [`Base::finish_maps`] after processing required maps.
    ///
    /// # Parameters
    ///
    /// - `filename` - Filename of the file that's being processed.
    /// - `content` - Content of the file that's being processed.
    /// - `mapinfos` - `MapInfos` file content that corresponds to the file being parsed.
    /// - `translation` - Contents of the translation file corresponding to maps. Isn't used with [`Mode::Read`]. Requires to be set with any other [`Mode`].
    ///
    /// # Returns
    ///
    /// - Nothing if map is unused (not included in Mapinfos), or mode is [`Mode::Write`] and no translation exists for the map.
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
    /// May panic if passed content is not from `Map` file.
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
    ///     let map_file_content = read("C:/Game/Data/Map001.rvdata2")?;
    ///     let mapinfos = read("C:/Game/Data/MapInfos.rvdata2")?;
    ///     let data = base.process_map("Map001.rvdata2", &map_file_content, &mapinfos, None)?;
    ///
    ///     // Required only when reading.
    ///     let translation_data = base.finish_maps();
    ///     Ok(())
    /// }
    /// ```
    pub fn process_map(
        &mut self,
        filename: &str,
        content: &[u8],
        mapinfos: &[u8],
        translation: Option<&str>,
    ) -> Result<Option<ProcessedData>, Error> {
        if self.mapinfos.is_none() {
            self.mapinfos = Some(parse_rpgm_file(mapinfos, self.engine_type)?);
        }

        self.initialize_translation(translation)?;

        let id = Self::parse_map_id(filename);
        if self.is_map_unused(id) {
            return Ok(None);
        }

        if self.get_translation_map(id).is_break() {
            return Ok(None);
        }

        let mut map_data = parse_rpgm_file(content, self.engine_type)?;
        let display_name = self.get_display_name(&mut map_data.root());

        if self.mode.is_read() {
            let map_order = self.get_map_order(id).to_string();

            // Owned, so the borrow of `self.mapinfos` ends before
            // `update_metadata` takes `&mut self`. One short allocation per map.
            let replaced_map_name = self.get_map_name(id).normalize().into_owned();

            self.update_metadata(
                id,
                Vec::from([
                    (CommentPos::Name, replaced_map_name.as_str()),
                    (CommentPos::Order, map_order.as_str()),
                    (CommentPos::DisplayName, display_name.as_str()),
                ]),
            );
        } else if !display_name.is_empty() {
            let display_name_comment_line = &self.translation.metadata[&id][2];

            let split: Vec<&str> = display_name_comment_line.split(get_line_separator()).collect();

            if split.len() >= 2 {
                let mut translation = split.into_iter().skip(1).rfind(|x| !x.is_empty()).unwrap_or_default();

                let translation_replaced = translation.denormalize();
                translation = &translation_replaced;

                if let Some(mut field) = map_data.root().member(self.labels.display_name) {
                    self.write_translated(&mut field, translation.to_owned(), self.engine_type.is_mvmz());
                }
            } else {
                log::warn!(
                    "{COULD_NOT_SPLIT_LINE_MSG} {display_name_comment_line}\n{IN_FILE_MSG}: {file}.txt",
                    file = self.file_type.to_string().to_lowercase()
                );
            }
        }

        let visited = {
            let mut root = map_data.root();
            // Previously, this assumed events are always an array/hash. This
            // isn't the case for MV/MZ - the field can also just be `false`.
            // `for_each_event_mut` returns `false` without visiting anything
            // when the field isn't array/hash shaped either way.
            let Some(mut events) = root.member(self.labels.events) else {
                return Ok(None);
            };

            events.for_each_event_mut(|event| {
                if event.is_null() {
                    return;
                }

                // Read before borrowing `pages` out of the same event.
                let event_metadata = if self.map_events {
                    Some((
                        event.member("id").and_then(|v| v.as_int()).unwrap_or_default(),
                        event
                            .member("name")
                            .and_then(|v| v.as_str().map(str::to_owned))
                            .unwrap_or_default(),
                        event.member("x").and_then(|v| v.as_int()).unwrap_or_default(),
                        event.member("y").and_then(|v| v.as_int()).unwrap_or_default(),
                    ))
                } else {
                    None
                };

                let Some(mut pages) = event.member(self.labels.pages) else {
                    return;
                };

                if let Some((event_id, event_name, event_x, event_y)) = event_metadata {
                    self.flush_translation(id);

                    self.output.accumulated.push((
                        id,
                        SmallVec::default(),
                        FlushedLines::EMPTY,
                        TranslationMap::from_iter([(
                            Cow::Borrowed(""),
                            TranslationEntry {
                                comments: vec![
                                    Cow::Owned(format!(
                                        "{comment}{sep}{event_id}",
                                        sep = get_line_separator(),
                                        comment = get_event_id_comment()
                                    )),
                                    Cow::Owned(format!(
                                        "{comment}{sep}{event_name}",
                                        sep = get_line_separator(),
                                        comment = get_event_name_comment()
                                    )),
                                    Cow::Owned(format!(
                                        "{comment}{sep}{event_x},{event_y}",
                                        sep = get_line_separator(),
                                        comment = get_event_pos_comment()
                                    )),
                                ],
                                translation: Cow::Borrowed(""),
                            },
                        )]),
                    ));
                }

                pages.for_each_element_mut(0, |page| {
                    // SAFETY: List is always in map files.
                    let mut list = unsafe { page.member(self.labels.list).unwrap_unchecked() };
                    self.process_list(&mut list);
                });
            })
        };

        if !visited {
            return Ok(None);
        }

        if self.mode.is_write() {
            Ok(Some(self.finish(map_data)))
        } else {
            self.flush_translation(id);
            Ok(None)
        }
    }

    /// Parses a map ID from a filename by extracting digits starting from position 3 and parsing them to [`u16`].
    ///
    /// # Parameters
    ///
    /// - `filename` - Filename of the map.
    ///
    /// # Returns
    ///
    /// - [`u16`] - The parsed map ID.
    ///
    pub fn parse_map_id(filename: &str) -> u16 {
        let mut id: u16 = 0;

        // Accumulated directly rather than copied into a buffer and re-parsed.
        // Not capped at three digits, because a game may have more than 999 maps.
        for &byte in &filename.as_bytes()[3..] {
            if !byte.is_ascii_digit() {
                break;
            }

            id = id * 10 + u16::from(byte - b'0');
        }

        id
    }

    /// Determines whether a map is unused based on its existence in `self.mapinfos`.
    ///
    /// # Parameters
    ///
    /// - `id` - The ID of the map to check.
    ///
    /// # Returns
    ///
    /// - [`bool`] - Whether map is unused.
    ///
    fn is_map_unused(&self, id: u16) -> bool {
        // If map ID can't be found in mapinfos, then it is unused in game.
        match self
            .mapinfos
            .as_ref()
            .expect("mapinfos is parsed before is_map_unused is ever called")
        {
            RpgmData::Json(v) => v.as_array().and_then(|a| a.get(id as usize)).is_none(),
            RpgmData::Marshal(arena) => ValueRef::root(arena)
                .entries()
                .find(|(k, _)| k.as_i64() == Some(i64::from(id)))
                .is_none(),
        }
    }

    /// Retrieves the chronological map order from `self.mapinfos`.
    ///
    /// # Parameters
    ///
    /// - `id` - The ID of the map whose order should be retrieved.
    ///
    /// # Returns
    ///
    /// - [`u16`] - The map's order.
    ///
    fn get_map_order(&self, id: u16) -> i32 {
        // SAFETY: "order" always exists in mapinfos and is always an integer.
        unsafe {
            match self.mapinfos.as_ref().unwrap_unchecked() {
                RpgmData::Json(v) => v[id as usize]["order"].as_i64().unwrap_unchecked() as i32,
                RpgmData::Marshal(arena) => ValueRef::root(arena)
                    .entries()
                    .find(|(k, _)| k.as_i64() == Some(i64::from(id)))
                    .unwrap_unchecked()
                    .1
                    .get("order")
                    .unwrap_unchecked()
                    .as_i64()
                    .unwrap_unchecked() as i32,
            }
        }
    }

    /// Retrieves the name of the map as a string slice, based on the provided map ID.
    ///
    /// # Parameters
    ///
    /// - `id` - The ID of the map whose name should be retrieved.
    ///
    /// # Returns
    ///
    /// - [`&str`] - The name of the map.
    ///
    fn get_map_name(&self, id: u16) -> &str {
        // SAFETY: "name" always exists in mapinfos and is always a string.
        unsafe {
            match self.mapinfos.as_ref().unwrap_unchecked() {
                RpgmData::Json(v) => v[id as usize]["name"].as_str().unwrap_unchecked(),
                RpgmData::Marshal(arena) => marshal_as_text(
                    ValueRef::root(arena)
                        .entries()
                        .find(|(k, _)| k.as_i64() == Some(i64::from(id)))
                        .unwrap_unchecked()
                        .1
                        .get("name")
                        .unwrap_unchecked(),
                )
                .unwrap_unchecked(),
            }
        }
    }

    /// Retrieves a display name for a map object.
    ///
    /// # Parameters
    ///
    /// - `map_object` - A [`Value`] cursor over the map object.
    ///
    /// # Returns
    ///
    /// - [`String`] - The processed display name, or an empty string if not found.
    ///
    fn get_display_name(&self, map_object: &mut Value<'_>) -> String {
        map_object
            .member(self.labels.display_name)
            .map(|display_name| {
                display_name
                    .as_str()
                    .map(|name| name.normalize().into_owned())
                    .unwrap_or_default()
            })
            .unwrap_or_default()
    }
}
