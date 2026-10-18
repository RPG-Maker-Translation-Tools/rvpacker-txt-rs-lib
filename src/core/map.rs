use super::{Base, CustomReplace, DISPLAY_NAME_POS, FlushedLines, parse_rpgm_file};
use crate::{
    CommentPos, ProcessedData,
    constants::{COULD_NOT_SPLIT_LINE_MSG, IN_FILE_MSG},
    get_event_id_comment, get_event_name_comment, get_event_pos_comment, get_line_separator,
    marshal_compat::{RpgmData, Value, marshal_as_text},
    types::{Error, RPGMFileType, TranslationEntry, TranslationMap},
};
use marshal_rs::{arena::Arena, value::ValueRef};
use smallvec::SmallVec;
use std::borrow::Cow;

/// What [`Base::walk_map`] found in one map, for [`Base::apply_map_walk`] to record.
pub struct MapWalk {
    display_name: String,
    /// On read, the map's text in order.
    found: Vec<String>,
    /// On read with `map_events`, where each event's text starts in `found`, and the event's header.
    events: Vec<(usize, TranslationEntry)>,
    /// Whether the map had events to walk at all.
    visited: bool,
    /// On write, how many of its lines were translated: a map with no translation of its own is written only when a
    /// line it shares with another map is one of these.
    pub(crate) lookup_hits: u32,
    /// The first text of this map that didn't decode.
    decode_error: Option<Error>,
}

impl Base {
    /// Prepares this base to process a run of map files.
    ///
    /// Maps accumulate into one translation file, so they are processed as a run:
    /// call this, then [`Base::process_map`] per file, then [`Base::finish_maps`].
    ///
    /// # Parameters
    ///
    /// - `mapinfos` - `MapInfos` file content that corresponds to the maps being processed.
    /// - `translation` - Translation file contents for maps. Ignored with [`crate::Mode::Read`], required otherwise.
    ///
    /// # Errors
    ///
    /// - [`Error::MarshalLoad`] - if unable to load the Marshal data.
    /// - [`Error::JsonParse`] - if unable to parse the JSON data.
    /// - [`Error::NoTranslation`] - if mode is not [`crate::Mode::Read`], and no translation was passed.
    pub fn begin_maps(&mut self, mapinfos: &[u8], translation: Option<&str>) -> Result<(), Error> {
        self.reset();
        self.file_type = RPGMFileType::Map;
        self.mapinfos = Some(parse_rpgm_file(mapinfos, self.engine_type)?);
        self.mapinfo_ids.clear();

        if let Some(RpgmData::Marshal(arena)) = &self.mapinfos {
            for (key, info) in ValueRef::root(arena).entries() {
                if let Some(id) = key.as_i64().and_then(|id| u32::try_from(id).ok()) {
                    // The first entry wins, as the linear search it replaces found it first.
                    self.mapinfo_ids.entry(id).or_insert_with(|| info.id());
                }
            }
        }
        self.initialize_translation(translation)?;
        Ok(())
    }

    /// Returns the translation data, accumulated after processing multiple maps.
    ///
    /// Returns the actual data only with [`crate::Mode::Read`] or [`crate::Mode::Purge`].
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
    ///     base.begin_maps(&mapinfos, None)?;
    ///
    ///     let map_file_content = read("C:/Game/Data/Map001.rvdata2")?;
    ///     let data = base.process_map("Map001.rvdata2", &map_file_content)?;
    ///
    ///     let map_file_content = read("C:/Game/Data/Map002.rvdata2")?;
    ///     let data = base.process_map("Map002.rvdata2", &map_file_content)?;
    ///
    ///     let translation_data = base.finish_maps();
    ///     Ok(())
    /// }
    /// ```
    pub fn finish_maps(&mut self) -> ProcessedData {
        self.finish_translation()
    }

    /// Processes the RPG Maker map file content.
    ///
    /// [`Base::begin_maps`] must be called first. To get the translation data, you need to call [`Base::finish_maps`]
    /// after processing required maps.
    ///
    /// # Parameters
    ///
    /// - `filename` - Filename of the file that's being processed.
    /// - `content` - Content of the file that's being processed.
    ///
    /// # Returns
    ///
    /// - Nothing if map is unused (not included in Mapinfos), or mode is [`crate::Mode::Write`] and no translation exists for the map.
    /// - [`ProcessedData`], which contains RPG Maker data if `mode` is [`crate::Mode::Write`] and translation data otherwise.
    /// - [`Error`], if unable to parse the content.
    ///
    /// # Errors
    ///
    /// - [`Error::MarshalLoad`] - if unable to load the Marshal data.
    /// - [`Error::JsonParse`] - if unable to parse the JSON data.
    /// - [`Error::MapsNotBegun`] - if [`Base::begin_maps`] was not called first.
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
    ///     let mapinfos = read("C:/Game/Data/MapInfos.rvdata2")?;
    ///     base.begin_maps(&mapinfos, None)?;
    ///
    ///     let map_file_content = read("C:/Game/Data/Map001.rvdata2")?;
    ///     let data = base.process_map("Map001.rvdata2", &map_file_content)?;
    ///
    ///     // Required only when reading.
    ///     let translation_data = base.finish_maps();
    ///     Ok(())
    /// }
    /// ```
    pub fn process_map(&mut self, filename: &str, content: &[u8]) -> Result<Option<ProcessedData>, Error> {
        let id = Self::parse_map_id(filename);
        let patched = self.process_parsed_map(id, content, None)?;

        Ok(match patched {
            Some(map_data) if self.mode.is_write() => Some(self.finish(map_data)),
            _ => None,
        })
    }

    /// Whether [`Base::process_parsed_map`] will get as far as parsing map `id`, so a caller parsing maps ahead of
    /// time can skip the rest. Read-only, so it can be asked from several threads at once.
    pub(crate) fn map_needs_parse(&self, id: u32) -> bool {
        !self.mode.is_purge()
            && !self.is_map_unused(id)
            && !self.map_unwritable(id)
            && !self.skip_maps.contains(&id)
            && !self
                .skip_events
                .get(&self.file_type)
                .is_some_and(|ids| ids.contains(&id))
    }

    /// [`Base::process_map`] for a map that may already be parsed and walked: `prepared` is `content` parsed and
    /// passed through [`Base::walk_map`] ahead of time, or [`None`] to do both here. Hands the map back once done
    /// with it - patched and still unserialized, on write - so the caller decides where the costly teardown of a
    /// big document happens.
    pub(crate) fn process_parsed_map(
        &mut self,
        id: u32,
        content: &[u8],
        prepared: Option<(RpgmData, MapWalk)>,
    ) -> Result<Option<RpgmData>, Error> {
        if self.mapinfos.is_none() {
            return Err(Error::MapsNotBegun);
        }

        if self.is_map_unused(id) {
            return Ok(None);
        }

        if self.get_translation_map(id).is_break() || self.map_unwritable(id) {
            return Ok(None);
        }

        let (map_data, mut walk) = if let Some(prepared) = prepared {
            prepared
        } else {
            let mut map_data = parse_rpgm_file(content, self.engine_type)?;
            let walk = self.walk_map(id, &mut map_data);
            (map_data, walk)
        };

        // Text that failed to decode fails this file, before anything of it is written.
        if let Some(error) = walk.decode_error.take() {
            return Err(error);
        }

        Self::take_decode_error()?;

        if self.mode.is_write() && !self.map_has_own_translation(id) && walk.lookup_hits == 0 {
            return Ok(None);
        }

        if !self.apply_map_walk(id, walk) {
            return Ok(None);
        }

        if !self.mode.is_write() {
            self.flush_translation(id);
        }

        Ok(Some(map_data))
    }

    /// Whether a write can tell before walking map `id` that there is nothing to write: with duplicates allowed, a
    /// map only looks lines up in its own section, so one with no translation of its own is never written. With
    /// duplicates removed it may still be translated through a line another map shares.
    pub(crate) fn map_unwritable(&self, id: u32) -> bool {
        self.mode.is_write() && self.duplicate_mode.is_allow() && !self.map_has_own_translation(id)
    }

    /// Whether map `id`'s section has a translation of its own: a line, or a display name. A map without one is written
    /// only for the lines it shares with another map, which its walk reports.
    pub(crate) fn map_has_own_translation(&self, id: u32) -> bool {
        let line_translated = self.translation.maps.get(&id).is_some_and(|map| !map.is_empty());
        let name_translated = self
            .translation
            .metadata
            .get(&id)
            .and_then(|metadata| metadata.get(DISPLAY_NAME_POS))
            .is_some_and(|name| !name.is_empty() && !name.ends_with(get_line_separator()));

        line_translated || name_translated
    }

    /// Walks a parsed map with `self` only borrowed, so several maps can be walked at once: patches it on write,
    /// collects its text on read. [`Base::apply_map_walk`] records the result.
    pub(crate) fn walk_map(&self, id: u32, map_data: &mut RpgmData) -> MapWalk {
        // Drop anything a task that ran earlier on this thread left behind.
        Self::take_thread_decode_error();

        let display_name = self.get_display_name(&mut map_data.root());

        if self.mode.is_write()
            && !display_name.is_empty()
            && let Some(display_name_comment_line) = self
                .translation
                .metadata
                .get(&id)
                .and_then(|metadata| metadata.get(DISPLAY_NAME_POS))
        {
            let split: Vec<&str> = display_name_comment_line.split(get_line_separator()).collect();

            if split.len() >= 2 {
                // Without a translation the name stays as the game has it.
                if let Some(translation) = split.into_iter().skip(1).rfind(|x| !x.is_empty())
                    && let Some(mut field) = map_data.root().member(self.labels.display_name)
                {
                    self.write_translated(
                        &mut field,
                        translation.denormalize().into_owned(),
                        self.engine_type.is_mvmz(),
                    );
                }
            } else {
                log::warn!(
                    "{COULD_NOT_SPLIT_LINE_MSG} {display_name_comment_line}\n{IN_FILE_MSG}: {file}.txt",
                    file = self.file_type.to_string().to_lowercase()
                );
            }
        }

        let mut walker = self.walker(self.translation.maps.get(&id));
        let mut events = Vec::new();

        let visited = {
            let mut root = map_data.root();

            // MV/MZ may store `false` instead of an array/hash here; `for_each_event_mut` returns `false` without
            // visiting anything when the field isn't array/hash shaped.
            root.member(self.labels.events).is_some_and(|mut map_events| {
                map_events.for_each_event_mut(|event| {
                    if event.is_null() {
                        return;
                    }

                    // Read before borrowing `pages` out of the same event; only reads record headers.
                    let event_metadata = (self.map_events && !self.mode.is_write()).then(|| {
                        (
                            event.member("id").and_then(|v| v.as_int()).unwrap_or_default(),
                            event
                                .member("name")
                                .and_then(|v| self.extract_string(&v, false).map(Cow::into_owned))
                                .unwrap_or_default(),
                            event.member("x").and_then(|v| v.as_int()).unwrap_or_default(),
                            event.member("y").and_then(|v| v.as_int()).unwrap_or_default(),
                        )
                    });

                    let Some(mut pages) = event.member(self.labels.pages) else {
                        return;
                    };

                    if let Some((event_id, event_name, event_x, event_y)) = event_metadata {
                        let separator = get_line_separator();

                        events.push((
                            walker.found.len(),
                            TranslationEntry {
                                comments: vec![
                                    Cow::Owned(format!("{}{separator}{event_id}", get_event_id_comment())),
                                    Cow::Owned(format!("{}{separator}{event_name}", get_event_name_comment())),
                                    Cow::Owned(format!("{}{separator}{event_x},{event_y}", get_event_pos_comment())),
                                ],
                                translation: Cow::Borrowed(""),
                            },
                        ));
                    }

                    pages.for_each_element_mut(0, |page| {
                        if let Some(mut list) = page.member(self.labels.list) {
                            walker.process_list(&mut list);
                        }
                    });
                })
            })
        };

        MapWalk {
            display_name,
            found: walker.found,
            events,
            visited,
            lookup_hits: walker.hits.get(),
            decode_error: Self::take_thread_decode_error(),
        }
    }

    /// Records what [`Base::walk_map`] found, making the same calls a walk with `&mut self` would have, in the same
    /// order. Returns whether the map had events to walk at all.
    pub(crate) fn apply_map_walk(&mut self, id: u32, walk: MapWalk) -> bool {
        let MapWalk {
            display_name,
            found,
            events,
            visited,
            ..
        } = walk;

        if self.mode.is_read() {
            let map_order = self.get_map_order(id).to_string();

            // Owned, so the borrow of `self.mapinfos` ends before
            // `update_metadata` takes `&mut self`. One short allocation per map.
            let replaced_map_name = self.get_map_name(id).normalize().into_owned();

            self.update_metadata(
                id,
                &[
                    (CommentPos::Name, replaced_map_name.as_str()),
                    (CommentPos::Order, map_order.as_str()),
                    (CommentPos::DisplayName, display_name.as_str()),
                ],
            );
        }

        self.replay_events(id, found, events);
        visited
    }

    /// Replays a map's found text, with each event's header closing the text before it as a section of its own.
    /// `events` holds where each event's text starts in `found`.
    pub(crate) fn replay_events(&mut self, id: u32, found: Vec<String>, events: Vec<(usize, TranslationEntry)>) {
        let mut found = found.into_iter();
        let mut replayed = 0;

        for (start, header) in events {
            self.replay(found.by_ref().take(start - replayed));
            replayed = start;

            self.flush_translation(id);
            self.output.accumulated.push((
                id,
                SmallVec::default(),
                FlushedLines::EMPTY,
                TranslationMap::from_iter([(Cow::Borrowed(""), header)]),
            ));
        }

        self.replay(found);
    }

    /// Parses a map ID from a filename by extracting digits starting from position 3 and parsing them to [`u32`].
    ///
    /// # Parameters
    ///
    /// - `filename` - Filename of the map.
    ///
    /// # Returns
    ///
    /// - [`u32`] - The parsed map ID.
    ///
    #[must_use]
    pub fn parse_map_id(filename: &str) -> u32 {
        let mut id: u32 = 0;

        // Accumulated directly rather than copied into a buffer and re-parsed.
        // Not capped at three digits, because a game may have more than 999 maps.
        for &byte in filename.as_bytes().get(3..).unwrap_or_default() {
            if !byte.is_ascii_digit() {
                break;
            }

            // Saturating: an absurdly long number must not overflow.
            id = id.saturating_mul(10).saturating_add(u32::from(byte - b'0'));
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
    fn is_map_unused(&self, id: u32) -> bool {
        // If map ID can't be found in mapinfos, then it is unused in game.
        match self
            .mapinfos
            .as_ref()
            .expect("mapinfos is parsed before is_map_unused is ever called")
        {
            // MV/MZ keep `null` in place of a deleted map, while its `MapNNN.json` may be left behind.
            RpgmData::Json(doc) => doc.at(doc.root(), id as usize).is_none_or(|info| doc.is_null(info)),
            RpgmData::Marshal(arena) => self.marshal_mapinfo(arena, id).is_none(),
        }
    }

    fn marshal_mapinfo<'a>(&self, arena: &'a Arena<'static>, id: u32) -> Option<ValueRef<'a, 'static>> {
        self.mapinfo_ids.get(&id).map(|&info| ValueRef::new(arena, info))
    }

    /// Retrieves the chronological map order from `self.mapinfos`.
    ///
    /// # Parameters
    ///
    /// - `id` - The ID of the map whose order should be retrieved.
    ///
    /// # Returns
    ///
    /// - [`u32`] - The map's order.
    ///
    fn get_map_order(&self, id: u32) -> i32 {
        let order = match self.mapinfos.as_ref() {
            Some(RpgmData::Json(doc)) => doc
                .at(doc.root(), id as usize)
                .and_then(|info| doc.as_i64(doc.member(info, "order")?)),
            Some(RpgmData::Marshal(arena)) => self
                .marshal_mapinfo(arena, id)
                .and_then(|info| info.get("order")?.as_i64()),
            None => None,
        };

        order.unwrap_or_default() as i32
    }

    /// Retrieves the name of the map as a string slice, based on the provided map ID.
    ///
    /// # Parameters
    ///
    /// - `id` - The ID of the map whose name should be retrieved.
    ///
    /// # Returns
    ///
    /// - [`Cow<str>`] - The name of the map, decoded like any other untagged text if it isn't UTF-8.
    ///
    fn get_map_name(&self, id: u32) -> Cow<'_, str> {
        match self.mapinfos.as_ref() {
            Some(RpgmData::Json(doc)) => Cow::Borrowed(
                doc.at(doc.root(), id as usize)
                    .and_then(|info| doc.as_str(doc.member(info, "name")?))
                    .unwrap_or_default(),
            ),
            Some(RpgmData::Marshal(arena)) => {
                let Some(name) = self.marshal_mapinfo(arena, id).and_then(|info| info.get("name")) else {
                    return Cow::Borrowed("");
                };

                // Untagged text is in the read encoding even when its bytes happen to be valid UTF-8.
                let text = if self.reads_legacy_text() {
                    None
                } else {
                    marshal_as_text(name)
                };

                text.map_or_else(
                    || Cow::Owned(self.decode_text(name.as_bytes().unwrap_or_default())),
                    Cow::Borrowed,
                )
            }
            None => Cow::Borrowed(""),
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
            .and_then(|display_name| {
                // Decoded like the map name, so a legacy codepage is honoured.
                self.extract_string(&display_name, false)
                    .map(|name| name.normalize().into_owned())
            })
            .unwrap_or_default()
    }
}
