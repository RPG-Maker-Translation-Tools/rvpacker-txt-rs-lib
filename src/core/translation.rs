use super::*;
use crate::{
    BaseFlags, CommentPos, Comments, ProcessedData,
    constants::{
        AT_POSITION_MSG, COMMENT_PREFIX, COMMENT_SUFFIX,
        COULD_NOT_SPLIT_LINE_MSG, EVENT_ID_COMMENT, EVENT_NAME_COMMENT,
        EVENT_POS_COMMENT, ID_COMMENT, IN_FILE_MSG,
        MAP_DISPLAY_NAME_COMMENT_PREFIX, MAP_ORDER_COMMENT, NAME_COMMENT,
        SEPARATOR,
    },
    types::{Error, IndexMapExt, Lines, TranslationEntry, TranslationMap},
};
use gxhash::GxBuildHasher;
use indexmap::map::Entry;
use log::warn;
use smallvec::{SmallVec, smallvec};
use std::{
    mem::{replace, take, transmute},
    ops::{ControlFlow, Range},
};

/// Lines flushed for one accumulated entry.
///
/// The duplicate-removing path leaves its lines in [`Base::lines`] and records only
/// the range they occupy, so they are never copied; the duplicate-allowing path
/// drains them out and owns them.
pub(crate) enum FlushedLines {
    Owned(Vec<String>),
    Range(Range<usize>),
}

impl FlushedLines {
    pub(crate) const EMPTY: Self = Self::Range(0..0);

    pub(crate) fn len(&self) -> usize {
        match self {
            Self::Owned(lines) => lines.len(),
            Self::Range(range) => range.len(),
        }
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// `store` must be the [`Base::lines`] these were flushed from.
    pub(crate) fn get<'a>(&'a self, store: &'a Lines, index: usize) -> &'a str {
        match self {
            Self::Owned(lines) => &lines[index],
            Self::Range(range) => &store[range.start + index],
        }
    }
}

impl Base {
    /// Initializes translation by filling `self.translation_maps` with parsed maps from `translation`.
    ///
    /// # Parameters
    ///
    /// - `translation` - translation file content to parse.
    ///
    pub(super) fn initialize_translation(
        &mut self,
        translation: Option<&str>,
    ) -> Result<(), Error> {
        if self.mode.is_default() || self.translation_initialized {
            return Ok(());
        }

        let Some(translation) = translation else {
            return Err(Error::NoTranslation);
        };

        self.translation_initialized = true;

        let trim = if self.file_type.is_main() {
            self.flags.contains(BaseFlags::Trim)
        } else {
            false
        };

        let mut scratch = TranslationMap::default();
        let mut translation_lines = translation.lines().enumerate();

        if self.game_type.is_termina() && self.file_type.is_items() {
            for _ in 0..4 {
                let (_, item_category_line) =
                    unsafe { translation_lines.next().unwrap_unchecked() };

                if item_category_line.starts_with("<Menu Category") {
                    let (source, translation) = unsafe {
                        item_category_line
                            .split_once(SEPARATOR)
                            .unwrap_unchecked()
                    };

                    scratch.insert(source.into(), translation.into());
                } else {
                    panic!(
                        "items.txt in Fear & Hunger 2: Termina should start \
                         with 4 `Menu Category` entries."
                    );
                }
            }

            self.translation_maps
                .insert(u16::MAX, scratch.drain(..).collect());
        }

        let mut top_level_comments: Vec<String> = Vec::new();
        let mut comments: Comments = smallvec![String::new(); 3];
        let mut id = 0;
        let mut first = true;

        for (i, line) in translation_lines {
            if line.starts_with(ID_COMMENT) {
                if id != 0 {
                    if scratch.is_empty() {
                        let metadata_entry = self.metadata.entry(id).or_insert(
                            replace(&mut comments, smallvec![String::new(); 3]),
                        );

                        let display_name = &metadata_entry[DISPLAY_NAME_POS];

                        if self.mode.is_write()
                            && (display_name.is_empty()
                                || display_name.ends_with(SEPARATOR))
                        {
                            continue;
                        }

                        self.translation_maps
                            .entry(id)
                            .or_insert(TranslationMap::with_capacity(512));
                    }

                    self.translation_maps
                        .insert(id, scratch.drain(..).collect());
                }

                id = line
                    .strip_prefix(ID_COMMENT)
                    .and_then(|n| n.strip_prefix(SEPARATOR))
                    .and_then(|n| n.trim_end().parse::<u16>().ok())
                    .unwrap();
                first = true;
                comments = smallvec![String::new(); 3];
                top_level_comments = Vec::new();

                continue;
            }

            if line.starts_with(COMMENT_PREFIX) {
                if [EVENT_ID_COMMENT, EVENT_NAME_COMMENT, EVENT_POS_COMMENT]
                    .into_iter()
                    .any(|c| line.starts_with(c))
                {
                    continue;
                }

                if first {
                    let pos = CommentPos::from_str(line);

                    if pos == CommentPos::None {
                        top_level_comments.push(line.to_string());
                        continue;
                    }

                    if pos == CommentPos::DisplayName {
                        let suffix_pos = line.rfind(COMMENT_SUFFIX).unwrap();
                        let prefix_len = MAP_DISPLAY_NAME_COMMENT_PREFIX.len();
                        let source = &line[prefix_len..suffix_pos];
                        let translation =
                            line.rsplit_once(SEPARATOR).unwrap().1;
                        comments[pos as usize] =
                            format!("{source}{SEPARATOR}{translation}");
                    } else {
                        comments[pos as usize] =
                            line.split_once(SEPARATOR).unwrap().1.to_string();
                    }
                } else {
                    comments.push(line.to_string());
                }

                continue;
            }

            let (source, translation) = match split_translation_line(
                line,
                trim,
                self.mode.is_write(),
            ) {
                TranslationLine::Split {
                    source,
                    translation,
                } => (source, translation),
                TranslationLine::Untranslated => continue,
                TranslationLine::Malformed => {
                    warn!(
                        "{COULD_NOT_SPLIT_LINE_MSG}\n{AT_POSITION_MSG}: \
                         {i}\n{IN_FILE_MSG}: {file}.txt",
                        i = i + 1,
                        file = self.file_type.to_string().to_lowercase()
                    );
                    comments = smallvec![String::new(); 3];
                    continue;
                }
            };

            if first {
                self.top_level_comments
                    .insert(id, top_level_comments.drain(..).collect());
                self.metadata.insert(id, comments.drain(..).collect());
                first = false;
            }

            scratch.insert(
                source.into(),
                TranslationEntry {
                    // The three leading slots are positional metadata
                    // (name/order/display name) and are only ever filled for the
                    // first entry of a section, which consumes them above. For
                    // every other entry they are empty placeholders that
                    // `push_entries` would skip anyway, so dropping them here
                    // avoids allocating a Vec of empty strings per entry.
                    comments: replace(
                        &mut comments,
                        smallvec![String::new(); 3],
                    )
                    .into_iter()
                    .filter(|comment| !comment.is_empty())
                    .collect(),
                    translation: translation.into(),
                },
            );
        }

        // Flush the last parsed section at EOF.
        // Without this, the final `<!-- ID --><#>...` block is dropped if there
        // is no following ID marker to trigger the regular section flush path.
        if id != 0 {
            let mut skip_entry = false;

            if scratch.is_empty() {
                let metadata_entry = self.metadata.entry(id).or_insert(
                    replace(&mut comments, smallvec![String::new(); 3]),
                );

                let display_name = &metadata_entry[DISPLAY_NAME_POS];

                if self.mode.is_write()
                    && (display_name.is_empty()
                        || display_name.ends_with(SEPARATOR))
                {
                    skip_entry = true;
                } else {
                    self.translation_maps
                        .entry(id)
                        .or_insert(TranslationMap::with_capacity(512));
                }
            }

            if !skip_entry {
                self.translation_maps
                    .insert(id, scratch.drain(..).collect());
            }
        }

        self.build_write_lookup();

        Ok(())
    }

    /// Flattens `self.translation_maps` into `self.write_lookup`.
    ///
    /// Only relevant on write with [`DuplicateMode::Remove`], where [`Base::get_key`]
    /// has to resolve a key against *every* parsed map. Doing that by scanning is
    /// `O(maps)` per lookup, i.e. quadratic over the whole file set; flattening once
    /// makes it a single hash lookup.
    ///
    /// Entries are moved rather than cloned, so this costs no extra memory: with
    /// [`DuplicateMode::Remove`] the per-id maps are only consulted for presence on
    /// write, never for content. `u16::MAX` is left alone - it holds the Termina item
    /// category map, which is looked up by id directly.
    pub(super) fn build_write_lookup(&mut self) {
        if !self.mode.is_write() || !self.duplicate_mode.is_remove() {
            return;
        }

        let total: usize = self
            .translation_maps
            .iter()
            .filter(|(id, _)| **id != u16::MAX)
            .map(|(_, map)| map.len())
            .sum();

        self.write_lookup = TranslationMap::with_capacity(total);

        for (id, map) in &mut self.translation_maps {
            if *id == u16::MAX {
                continue;
            }

            // `or_insert` keeps the first occurrence, matching the order the
            // previous linear scan resolved duplicates in.
            for (source, translation) in map.drain(..) {
                self.write_lookup.entry(source).or_insert(translation);
            }
        }
    }

    /// Sets `self.translation_map` to the entry from `self.translation_maps`.
    ///
    /// If `self.mode` is [`Mode::Purge`], it will push entries from `self.translation_map` to `self.accumulated_translation` and break.
    ///
    /// If `self.flags` contains any of ignore flags, it will also set `self.ignore_entry`.
    ///
    /// # Parameters
    ///
    /// - `id` - ID of the entry to get.
    ///
    /// # Returns
    ///
    /// - [`ControlFlow::Break`]
    ///     - If mode is [`Mode::Write`] and `id` is not in `self.translation_maps`.
    ///     - If `id` is skipped.
    ///     - If mode is [`Mode::Purge`].
    /// - [`ControlFlow::Continue`] - In other situations.
    ///
    pub(super) fn get_translation_map(&mut self, id: u16) -> ControlFlow<()> {
        let entry = self.translation_maps.entry(id);
        let index = entry.index();

        // Select the map for `id`, creating it if we're not writing.
        match entry {
            Entry::Occupied(_) => {}
            Entry::Vacant(entry) => {
                if self.mode.is_write() {
                    return ControlFlow::Break(());
                }

                entry.insert(TranslationMap::with_capacity(512));
            }
        }

        self.translation_map_index = index;

        if self
            .skip_events
            .get(&self.file_type)
            .is_some_and(|x| x.contains(&id))
            || (self.file_type.is_map() && self.skip_maps.contains(&id))
        {
            if self.mode.is_append() || self.mode.is_purge() {
                let metadata = self.get_metadata(id);

                let map = take(self.translation_map_mut());
                self.accumulated_translation.push((
                    id,
                    metadata,
                    FlushedLines::EMPTY,
                    map,
                ));
            }

            self.total_length = self.lines.len();
            return ControlFlow::Break(());
        }

        self.get_ignore_entry(id);

        if self.mode.is_purge() {
            self.flush_translation(id);
            return ControlFlow::Break(());
        }

        ControlFlow::Continue(())
    }

    pub(super) fn get_metadata(&mut self, id: u16) -> Comments {
        let Some(mut comments) = self.metadata.remove(&id) else {
            return SmallVec::default();
        };

        comments.iter_mut().enumerate().filter(|(_, x)| !x.is_empty()).for_each(|(i, x)| {
            let pos = unsafe { transmute::<i8, CommentPos>(i as i8) };

            *x = match pos {
                CommentPos::Name => {
                    format!("{NAME_COMMENT}{SEPARATOR}{x}")
                }

                CommentPos::Order => {
                    format!("{MAP_ORDER_COMMENT}{SEPARATOR}{x}")
                }

                CommentPos::DisplayName => {
                    let (source, translation) = x.split_once(SEPARATOR).unwrap();
                    format!("{MAP_DISPLAY_NAME_COMMENT_PREFIX}{source}{COMMENT_SUFFIX}{SEPARATOR}{translation}")
                }

                CommentPos::None => unreachable!()
            }
        });

        comments
    }

    pub(super) fn finish_translation(&mut self) -> ProcessedData {
        let allow_dup =
            self.duplicate_mode.is_allow() || self.file_type.is_misc();
        let skip_events_entry = self.skip_events.get(&self.file_type);
        let ignore_index = self.ignore_entry_index;

        let additional_data = self.get_additional_data();

        // Allocate 4 MB. It makes no sense to circlejerk `accumulated_translation` to get the precise count, so we'll just take the biggest reasonable amount.
        let output_size = 4096 * 1024;
        let mut output = Vec::with_capacity(output_size);

        for &data in additional_data {
            output.extend_from_slice(data.as_bytes());
            output.extend_from_slice(SEPARATOR.as_bytes());

            if let Some(additional) = self.translation_maps.get(&u16::MAX) {
                if let Some(translation) = additional.get(data) {
                    output.extend_from_slice(translation.as_bytes());
                }
            }

            output.push(b'\n');
        }

        let mut accumulated_map: indexmap::IndexMap<
            String,
            (u16, TranslationEntry),
            gxhash::GxBuildHasher,
        > = if allow_dup {
            indexmap::IndexMap::default()
        } else {
            let len = self.translation_maps.values().fold(0, |mut acc, map| {
                acc += map.len();
                acc
            });

            self.translation_maps.drain(..).fold(
                indexmap::IndexMap::with_capacity_and_hasher(
                    len,
                    GxBuildHasher::default(),
                ),
                |mut acc, (k, v)| {
                    for (key, value) in v {
                        acc.insert(key, (k, value));
                    }
                    acc
                },
            )
        };

        // Moved out so the loop can read the rest of `self` freely. `lines_store`
        // backs every `FlushedLines::Range`.
        let mut accumulated = take(&mut self.accumulated_translation);
        let lines_store = take(&mut self.lines);
        let mut prev_id = u16::MAX;

        for i in 0..accumulated.len() {
            // Splitting gives the current entry mutably and the lookahead
            // immutably at the same time.
            let (current, rest) = accumulated[i..].split_first_mut().unwrap();
            let (id, meta, lines, map) = current;

            let skip = skip_events_entry.is_some_and(|e| e.contains(id))
                || (self.file_type.is_map() && self.skip_maps.contains(id))
                || (self.mode.is_purge()
                    && self.file_type.is_system()
                    && *id == 8);

            if skip {
                if self.mode.is_append() || self.mode.is_purge() {
                    push_metadata(&mut output, *id, meta);

                    for (source, translation) in map {
                        push_entries(&mut output, source, translation);
                    }
                }

                continue;
            }

            if let Some(comments) = self.top_level_comments.get(id) {
                for comment in comments {
                    output.extend_from_slice(comment.as_bytes());
                    output.push(b'\n');
                }
            }

            if self.mode.is_purge() {
                push_metadata(&mut output, *id, meta);

                for (mut source, translation) in take(map) {
                    if translation.is_empty() {
                        let moved = take(&mut source);

                        if self.flags.contains(BaseFlags::CreateIgnore)
                            && !moved.is_empty()
                        {
                            // Field access rather than `ignore_entry_mut`, so
                            // that `skip_events_entry`'s borrow of the disjoint
                            // `skip_events` field stays live.
                            if let Some((_, entry)) =
                                self.ignore_map.get_index_mut(ignore_index)
                            {
                                entry.insert(moved);
                            }
                        }
                    }

                    push_entries(&mut output, &source, &translation);
                }

                continue;
            }

            if *id != prev_id {
                let has_display_name =
                    meta.get(DISPLAY_NAME_POS).is_some_and(|c| !c.is_empty());

                let same_id_has_lines = !lines.is_empty()
                    || rest.iter().any(|(next_id, _, next_lines, _)| {
                        *next_id == *id && !next_lines.is_empty()
                    });

                let should_push_map = self.file_type.is_map()
                    && self.map_events
                    && (same_id_has_lines || has_display_name);

                let should_push_other = !lines.is_empty() || has_display_name;

                if should_push_map || should_push_other {
                    push_metadata(&mut output, *id, meta);
                }

                prev_id = *id;
            }

            let next_lines_empty = rest
                .first()
                .is_some_and(|(_, _, next_lines, _)| next_lines.is_empty());

            if !next_lines_empty {
                if let Some((_, entry)) = map.first() {
                    push_entries(&mut output, "", entry);
                }
            }

            for line_index in 0..lines.len() {
                let source = lines.get(&lines_store, line_index);

                let translation = match (allow_dup, self.mode.is_append()) {
                    (true, true) => map.swap_remove(source).unwrap_or_default(),
                    (false, true) => accumulated_map
                        .swap_remove(source)
                        .unzip()
                        .1
                        .unwrap_or_default(),
                    (_, false) => TranslationEntry::default(),
                };

                push_entries(&mut output, source, &translation);
            }

            if self.flags.contains(BaseFlags::SkipObsolete) {
                continue;
            }

            match (allow_dup, self.mode.is_append()) {
                (true, true) => {
                    for (source, translation) in map {
                        push_entries(&mut output, source, translation);
                    }
                }
                (false, true) => {
                    for (source, (i, translation)) in &accumulated_map {
                        if *id == *i {
                            push_entries(&mut output, source, translation);
                        }
                    }
                }
                _ => {}
            }
        }

        output.pop();
        ProcessedData::TranslationData(output)
    }

    /// Flushes current `self.translation_map` and `self.lines` to `self.accumulated_translation` along with metadata and id.
    ///
    /// It's necessary to call [`Base::finish_translation`] once we've finished flushing entries.
    ///
    /// # Parameters
    ///
    /// - `id` - ID of the entry to flush.
    ///
    pub(super) fn flush_translation(&mut self, id: u16) {
        let metadata = self.get_metadata(id);

        if self.mode.is_purge() {
            if !self.translation_map().is_empty()
                || metadata
                    .get(DISPLAY_NAME_POS)
                    .is_some_and(|x| !x.is_empty())
            {
                let map = take(self.translation_map_mut());
                self.accumulated_translation.push((
                    id,
                    metadata,
                    FlushedLines::EMPTY,
                    map,
                ));
            }
        } else if self.duplicate_mode.is_allow() || self.file_type.is_misc() {
            if self
                .skip_events
                .get(&self.file_type)
                .is_some_and(|e| e.contains(&id))
                || (self.file_type.is_map() && self.skip_maps.contains(&id))
            {
                self.lines.clear();
                self.translation_map_mut().clear();
            } else {
                let lines = FlushedLines::Owned(self.lines.drain(..).collect());

                let map = take(self.translation_map_mut());
                self.accumulated_translation
                    .push((id, metadata, lines, map));
            }
        } else {
            let total_length = self.total_length;
            let current_length = self.lines.len() - total_length;

            // Left in `self.lines`; only the range is recorded, so nothing is copied.
            let lines = FlushedLines::Range(
                total_length..total_length + current_length,
            );

            self.accumulated_translation.push((
                id,
                metadata,
                lines,
                TranslationMap::default(),
            ));

            self.total_length += current_length;
        }
    }

    pub(super) fn update_metadata(
        &mut self,
        id: u16,
        metadata_vec: Vec<(CommentPos, &str)>,
    ) {
        let metadata = self
            .metadata
            .entry(id)
            .or_insert(smallvec![String::new(); 3]);

        if metadata.len() < 3 {
            metadata.resize(3, String::new());
        }

        for (entry_id, entry) in
            metadata_vec.into_iter().filter(|(_, x)| !x.is_empty())
        {
            if entry_id == CommentPos::DisplayName {
                if self.mode.is_append() {
                    let Some((source, mut translation)) =
                        metadata[entry_id as usize].split_once(SEPARATOR)
                    else {
                        metadata[entry_id as usize] =
                            format!("{entry}{SEPARATOR}");
                        continue;
                    };

                    if source != entry {
                        translation = "";
                    }

                    metadata[entry_id as usize] =
                        format!("{entry}{SEPARATOR}{translation}");
                } else {
                    metadata[entry_id as usize] = format!("{entry}{SEPARATOR}");
                }

                continue;
            }

            metadata[entry_id as usize] = entry.to_string();
        }
    }
}
