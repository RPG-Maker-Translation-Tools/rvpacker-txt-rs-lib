use super::*;
use crate::{
    CommentPos, ProcessedData,
    constants::{
        COULD_NOT_SPLIT_LINE_MSG, EVENT_ID_COMMENT, EVENT_NAME_COMMENT,
        EVENT_POS_COMMENT, IN_FILE_MSG, SEPARATOR,
    },
    types::{Error, RPGMFileType, TranslationEntry, TranslationMap},
};
use marshal_rs::{Get, Value};
use smallvec::SmallVec;

/// Newer RPG Maker versions store events in arrays while older versions use hash maps.
#[repr(u8)]
enum EventIterator<'a> {
    New(std::iter::Skip<std::slice::IterMut<'a, Value>>),
    Old(indexmap::map::ValuesMut<'a, Value, Value>),
}

impl<'a> Iterator for EventIterator<'a> {
    type Item = &'a mut Value;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            EventIterator::New(iter) => iter.next(),
            EventIterator::Old(iter) => iter.next(),
        }
    }
}

impl Base {
    /// Prepares this base to process a run of map files.
    ///
    /// Maps accumulate into one translation file, so they are processed as a run:
    /// call this, then [`Base::process_map`] per file, then [`Base::finish_maps`].
    pub fn begin_maps(&mut self) {
        self.reset();
        self.file_type = RPGMFileType::Map;
        self.mapinfos = Value::default();
    }

    /// Returns the translation data, accumulated after processing multiple maps.
    ///
    /// Returns the actual data only with [`Mode::Read`] or [`Mode::Purge`].
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
        self.finish(Value::default())
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
    /// - `translation` - Contents of the translation file corresponding to maps. Isn't used with [`ReadMode::Default`]. Requires to be set with any other [`Mode`].
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
    /// - [`Error::NoTranslation`] - if mode is not [`ReadMode::Default`], and no translation was passed.
    ///
    /// # Panics
    ///
    /// May panic if passed content is not from `Map` file.
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
        if self.mapinfos.is_null() {
            self.mapinfos =
                parse_rpgm_file(mapinfos, self.engine_type, self.file_type)?;
        }

        self.initialize_translation(translation)?;

        let id = Self::parse_map_id(filename);
        if self.is_map_unused(id) {
            return Ok(None);
        }

        if self.get_translation_map(id).is_break() {
            return Ok(None);
        }

        let mut map_object =
            parse_rpgm_file(content, self.engine_type, self.file_type)?;
        let display_name = self.get_display_name(&map_object);

        if self.mode.is_read() {
            let map_order = self.get_map_order(id).to_string();
            let engine_is_new = self.engine_type.is_new();

            // Owned, so the borrow of `self.mapinfos` ends before
            // `update_metadata` takes `&mut self`. One short allocation per map.
            let replaced_map_name =
                Self::get_map_name(&self.mapinfos, engine_is_new, id)
                    .normalize()
                    .into_owned();

            self.update_metadata(
                id,
                Vec::from([
                    (CommentPos::Name, replaced_map_name.as_str()),
                    (CommentPos::Order, map_order.as_str()),
                    (CommentPos::DisplayName, display_name.as_str()),
                ]),
            );
        } else if !display_name.is_empty() {
            let display_name_comment_line = &self.metadata[&id][2];

            let split: Vec<&str> =
                display_name_comment_line.split(SEPARATOR).collect();

            if split.len() >= 2 {
                let mut translation = split
                    .into_iter()
                    .skip(1)
                    .rfind(|x| !x.is_empty())
                    .unwrap_or_default();

                let translation_replaced = translation.denormalize();
                translation = &translation_replaced;

                map_object[self.labels.display_name] =
                    Value::string(translation);
            } else {
                log::warn!(
                    "{COULD_NOT_SPLIT_LINE_MSG} \
                     {display_name_comment_line}\n{IN_FILE_MSG}: {file}.txt",
                    file = self.file_type.to_string().to_lowercase()
                );
            }
        }

        let events = if self.engine_type.is_new() {
            // Previously, this line was using `unwrap_unchecked`, because it assumed, that events are always an array in MV/MZ.
            // This is not the case. This array can also contain just `bool`. Now, it returns, if encounters something else than an array.
            let Some(array) = map_object[self.labels.events].as_array_mut()
            else {
                return Ok(None);
            };

            EventIterator::New(array.iter_mut().skip(1))
        } else {
            // SAFETY: Always a hashmap in old maps.
            EventIterator::Old(unsafe {
                map_object[self.labels.events]
                    .as_hashmap_mut()
                    .unwrap_unchecked()
                    .values_mut()
            })
        };

        for event in events {
            if event.is_null() {
                continue;
            }

            // Read before borrowing `pages` out of the same event.
            let event_metadata = if self.map_events {
                Some((
                    event["id"].as_int().unwrap(),
                    event["name"].as_str().unwrap().to_owned(),
                    event["x"].as_int().unwrap(),
                    event["y"].as_int().unwrap(),
                ))
            } else {
                None
            };

            let Some(pages) = event[self.labels.pages].as_array_mut() else {
                continue;
            };

            if let Some((event_id, event_name, event_x, event_y)) =
                event_metadata
            {
                self.flush_translation(id);

                self.accumulated_translation.push((
                        id,
                        SmallVec::default(),
                        FlushedLines::EMPTY,
                        TranslationMap::from_iter([(String::new(), TranslationEntry {
                            comments: vec![format!(
                                "{EVENT_ID_COMMENT}{SEPARATOR}{event_id}"
                            ),
                            format!("{EVENT_NAME_COMMENT}{SEPARATOR}{event_name}"),
                            format!("{EVENT_POS_COMMENT}{SEPARATOR}{event_x},{event_y}")],
                            translation: String::new(),
                        })])
                    ));
            }

            for page in pages {
                // SAFETY: List is always in map files.
                let list = unsafe {
                    page[self.labels.list].as_array_mut().unwrap_unchecked()
                };

                self.process_list(list);
            }
        }

        if self.mode.is_write() {
            Ok(Some(self.finish(map_object)))
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
        if self.engine_type.is_new() {
            self.mapinfos.get_index(id as usize).is_none()
        } else {
            self.mapinfos.get(&Value::int(i32::from(id))).is_none()
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
            if self.engine_type.is_new() {
                &self.mapinfos[id as usize]["order"]
            } else {
                &self.mapinfos[Value::int(i32::from(id))]["order"]
            }
            .as_int()
            .unwrap_unchecked()
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
    /// Takes `mapinfos` explicitly rather than reading it through `&self`, so the
    /// returned string borrows only that field and leaves `self` free.
    fn get_map_name(mapinfos: &Value, engine_is_new: bool, id: u16) -> &str {
        // SAFETY: "name" always exists in mapinfos and is always a string.
        unsafe {
            if engine_is_new {
                &mapinfos[id as usize]["name"]
            } else {
                &mapinfos[Value::int(i32::from(id))]["name"]
            }
            .as_str()
            .unwrap_unchecked()
        }
    }

    /// Retrieves a display name for a map object.
    ///
    /// # Parameters
    ///
    /// - `map_object` - A reference to a [`Value`] representing the map object.
    ///
    /// # Returns
    ///
    /// - [`String`] - The processed display name, or an empty string if not found.
    ///
    fn get_display_name(&self, map_object: &Value) -> String {
        map_object
            .get(self.labels.display_name)
            .map(|display_name| {
                display_name
                    .as_str()
                    .map(|name| name.normalize().into_owned())
                    .unwrap_or_default()
            })
            .unwrap_or_default()
    }
}
