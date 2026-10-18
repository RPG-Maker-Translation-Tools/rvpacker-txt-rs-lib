use super::Base;
use crate::{
    CommentPos, ProcessedData, get_event_id_comment, get_event_name_comment, get_event_pos_comment, get_line_separator,
    types::{Error, HashMap, HashMapExt, RPGMFileType, TranslationEntry},
};
use rm2k::{
    engine::SaveOpt,
    file,
    rpg::{MapInfo, TreeMap},
};
use std::borrow::Cow;

/// The `RPG_RT.lmt` map tree, looked up by map id.
///
/// [`Rm2kTree::indexed`] pays one pass over the tree to make every lookup a hash lookup, for a run over many maps;
/// [`Rm2kTree::linear`] scans, for the one-off [`Base::process_rm2k_map`].
pub struct Rm2kTree<'t, 'a> {
    tree: &'t TreeMap<'a>,
    /// Map id to its position in `tree.maps`, and map id to its position in `tree.tree_order`. The first of a
    /// repeated id wins, as the scan finds it first.
    index: Option<(HashMap<i64, usize>, HashMap<i32, usize>)>,
}

impl<'t, 'a> Rm2kTree<'t, 'a> {
    pub(crate) const fn linear(tree: &'t TreeMap<'a>) -> Self {
        Self { tree, index: None }
    }

    pub(crate) fn indexed(tree: &'t TreeMap<'a>) -> Self {
        let mut by_id = HashMap::with_capacity(tree.maps.len());
        let mut order = HashMap::with_capacity(tree.tree_order.len());

        for (position, info) in tree.maps.iter().enumerate() {
            by_id.entry(i64::from(info.id)).or_insert(position);
        }

        for (position, &id) in tree.tree_order.iter().enumerate() {
            order.entry(id).or_insert(position);
        }

        Self {
            tree,
            index: Some((by_id, order)),
        }
    }

    /// The tree entry of map `id` and the map's position in the tree order, or [`None`] for a map the tree doesn't list.
    fn info(&self, id: u32) -> Option<(&'t MapInfo<'a>, usize)> {
        let tree = self.tree;

        if let Some((by_id, order)) = &self.index {
            let info = &tree.maps[*by_id.get(&i64::from(id))?];
            Some((info, order.get(&info.id).copied().unwrap_or_default()))
        } else {
            let info = tree.maps.iter().find(|info| i64::from(info.id) == i64::from(id))?;
            Some((
                info,
                tree.tree_order.iter().position(|&i| i == info.id).unwrap_or_default(),
            ))
        }
    }

    pub(crate) fn lists(&self, id: u32) -> bool {
        self.info(id).is_some()
    }
}

/// What [`Base::walk_rm2k_map`] found in one map, for [`Base::process_parsed_rm2k_map`] to record.
pub struct Rm2kMapWalk {
    /// On read, the map's text in order.
    found: Vec<String>,
    /// On read with `map_events`, where each event's text starts in `found`, and the event's header.
    events: Vec<(usize, TranslationEntry)>,
    /// Whether any event had commands to walk.
    visited: bool,
    /// On write, how many of its lines were translated, as in [`MapWalk`](super::super::map::MapWalk).
    lookup_hits: u32,
    /// On write, the patched map, serialized.
    saved: Vec<u8>,
    /// The first text of this map that didn't decode.
    decode_error: Option<Error>,
}

impl Base {
    /// Prepares this base to process a run of `MapNNNN.lmu` files.
    ///
    /// Mirrors [`Base::begin_maps`] - maps share one `maps.txt`, so they're
    /// processed as a run: call this, then [`Base::process_rm2k_map`] per file,
    /// then [`Base::finish_rm2k_maps`].
    pub fn begin_rm2k_maps(&mut self) {
        self.reset();
        self.file_type = RPGMFileType::Rm2kMap;
    }

    /// Returns the translation data accumulated after processing multiple maps.
    ///
    /// Returns the actual data only with [`crate::Mode::Read`] or [`crate::Mode::Purge`].
    pub fn finish_rm2k_maps(&mut self) -> ProcessedData {
        self.finish_translation()
    }

    /// Processes one `MapNNNN.lmu` file's content.
    ///
    /// # Parameters
    ///
    /// - `filename` - Filename of the file that's being processed.
    /// - `content` - Content of the file that's being processed.
    /// - `tree` - The parsed `RPG_RT.lmt` map tree, supplying each map's name and
    ///   order - the rm2k counterpart of `MapInfos.*`.
    /// - `translation` - Contents of the translation file corresponding to maps.
    ///   Isn't used with [`crate::Mode::Read`]. Requires to be set with any other [`crate::Mode`].
    ///
    /// # Errors
    ///
    /// - [`Error::Rm2kLoad`] - if unable to load the map data.
    /// - [`Error::NoTranslation`] - if mode is not [`crate::Mode::Read`], and no translation was passed.
    pub fn process_rm2k_map(
        &mut self,
        filename: &str,
        content: &[u8],
        tree: &TreeMap<'_>,
        translation: Option<&str>,
    ) -> Result<Option<ProcessedData>, Error> {
        self.initialize_translation(translation)?;

        let id = Self::parse_map_id(filename);
        let saved = self.process_parsed_rm2k_map(id, content, &Rm2kTree::linear(tree), None)?;

        Ok(saved.map(|bytes| self.finish_rm2k(bytes)))
    }

    /// [`Base::initialize_translation`] for a run of maps a caller walks ahead of time.
    pub(crate) fn initialize_rm2k_maps(&mut self, translation: Option<&str>) -> Result<(), Error> {
        self.initialize_translation(translation)
    }

    /// Whether [`Base::process_parsed_rm2k_map`] will get as far as walking map `id`, so a caller walking maps ahead
    /// of time can skip the rest. Read-only, so it can be asked from several threads at once.
    pub(crate) fn rm2k_map_needs_walk(&self, id: u32, tree: &Rm2kTree<'_, '_>) -> bool {
        tree.lists(id)
            && !self.mode.is_purge()
            && !self.map_unwritable(id)
            && !self.skip_maps.contains(&id)
            && !self
                .skip_events
                .get(&self.file_type)
                .is_some_and(|ids| ids.contains(&id))
    }

    /// Parses and walks one map with `self` only borrowed, so several maps can be walked at once: patches and
    /// serializes it on write, collects its text on read. [`Base::process_parsed_rm2k_map`] records the result.
    pub(crate) fn walk_rm2k_map(&self, id: u32, content: &[u8]) -> Result<Rm2kMapWalk, Error> {
        // Drop anything a task that ran earlier on this thread left behind.
        Self::take_thread_decode_error();

        let loaded = file::load_map(content)?;
        let mut map = loaded.value;

        let mut walker = self.walker(self.translation.maps.get(&id));
        let mut events = Vec::new();
        let mut visited = false;

        for event in map.events.iter_mut() {
            if event.pages.iter().all(|page| page.event_commands.0.is_empty()) {
                continue;
            }

            if self.map_events && !self.mode.is_write() {
                let event_name = self.decode_text(event.name.as_bytes());
                let separator = get_line_separator();

                events.push((
                    walker.found.len(),
                    TranslationEntry {
                        comments: vec![
                            Cow::Owned(format!("{}{separator}{}", get_event_id_comment(), event.id)),
                            Cow::Owned(format!("{}{separator}{event_name}", get_event_name_comment())),
                            Cow::Owned(format!("{}{separator}{},{}", get_event_pos_comment(), event.x, event.y)),
                        ],
                        translation: Cow::Borrowed(""),
                    },
                ));
            }

            for page in event.pages.iter_mut() {
                if page.event_commands.0.is_empty() {
                    continue;
                }

                visited = true;
                walker.process_rm2k_event_list(&mut page.event_commands);
            }
        }

        let mut saved = Vec::new();

        // A map nothing will be written for (see process_parsed_rm2k_map) isn't worth serializing.
        if self.mode.is_write() && visited && (self.map_has_own_translation(id) || walker.hits.get() > 0) {
            let opt = SaveOpt { preserve_header: true };

            // `Vec<u8>` is a statically infallible `Sink`.
            file::save_map(&map, &mut saved, self.rm2k_engine, opt, loaded.header).unwrap();
        }

        Ok(Rm2kMapWalk {
            found: walker.found,
            events,
            visited,
            lookup_hits: walker.hits.get(),
            saved,
            decode_error: Self::take_thread_decode_error(),
        })
    }

    /// Records map `id`, walking it here unless `prepared` already holds its walk. Returns the patched map's bytes on
    /// write.
    pub(crate) fn process_parsed_rm2k_map(
        &mut self,
        id: u32,
        content: &[u8],
        tree: &Rm2kTree<'_, '_>,
        prepared: Option<Rm2kMapWalk>,
    ) -> Result<Option<Vec<u8>>, Error> {
        // A map absent from the tree is unused, same as a map missing from
        // `MapInfos` on MV/VX.
        let Some((map_info, order)) = tree.info(id) else {
            return Ok(None);
        };

        if self.get_translation_map(id).is_break() || self.map_unwritable(id) {
            return Ok(None);
        }

        let mut walk = match prepared {
            Some(walk) => walk,
            None => self.walk_rm2k_map(id, content)?,
        };

        if self.mode.is_read() {
            let name = self.decode_text(map_info.name.as_bytes());
            let order = order.to_string();

            self.update_metadata(
                id,
                &[(CommentPos::Name, name.as_str()), (CommentPos::Order, order.as_str())],
            );
        }

        // Text that failed to decode fails this file, before anything of it is written.
        if let Some(error) = walk.decode_error.take() {
            return Err(error);
        }

        // As for MV/MZ maps: a map without a translation of its own is written only for a line shared with another map.
        if self.mode.is_write() && !self.map_has_own_translation(id) && walk.lookup_hits == 0 {
            return Ok(None);
        }

        Self::take_decode_error()?;

        if !walk.visited {
            return Ok(None);
        }

        self.replay_events(id, walk.found, walk.events);

        if self.mode.is_write() {
            Ok(Some(walk.saved))
        } else {
            self.flush_translation(id);
            Ok(None)
        }
    }
}
