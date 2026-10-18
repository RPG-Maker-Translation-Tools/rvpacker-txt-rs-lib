use super::base::{Ignore, Output};
use super::{Base, DISPLAY_NAME_POS, LineSplitter, SYSTEM_GAME_TITLE_ID, TranslationLine, push_entries, push_metadata};
use crate::{
    BaseFlags, CommentPos, Comments, ProcessedData,
    constants::{AT_POSITION_MSG, COULD_NOT_SPLIT_LINE_MSG, IN_FILE_MSG},
    get_line_separator,
    types::{
        DuplicateMode, Error, FastBuildHasher, HashMap, HashSet, IndexMapExt, Lines, TranslationEntry, TranslationMap,
    },
};
use crate::{
    get_comment_prefix, get_event_id_comment, get_event_name_comment, get_event_pos_comment, get_id_comment,
    get_map_display_name_comment_prefix, get_map_order_comment, get_name_comment,
};
use indexmap::map::Entry;
use log::warn;
use rayon::prelude::*;
use smallvec::{SmallVec, smallvec};
use std::{
    borrow::Cow,
    mem::{replace, take, transmute},
    ops::{ControlFlow, Range},
};

/// One section of a translation file, parsed by [`Base::parse_section`].
struct ParsedSection {
    id: u32,
    /// The comments above the first entry, and the positional metadata it took. [`None`] without entries.
    first: Option<(Vec<String>, Comments)>,
    /// The positional metadata, when no entry took it.
    pending: Comments,
    /// The comments above the section's first entry, when it has no entries to take them.
    top_level: Vec<String>,
    /// The comments after the section's last entry.
    trailing: Vec<String>,
    entries: TranslationMap,
}

/// Writes the comments that followed a section's last entry.
fn push_trailing(output: &mut Vec<u8>, comments: Option<Vec<String>>) {
    for comment in comments.into_iter().flatten() {
        output.extend_from_slice(comment.as_bytes());
        output.push(b'\n');
    }
}

/// Lines flushed for one accumulated entry.
///
/// The duplicate-removing path leaves its lines in [`Base::lines`] and records only
/// the range they occupy, so they are never copied; the duplicate-allowing path
/// drains them out and owns them.
pub enum FlushedLines {
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
    /// Initializes translation by filling `self.translation.maps` with parsed maps from `translation`.
    ///
    /// # Parameters
    ///
    /// - `translation` - translation file content to parse.
    ///
    pub(super) fn initialize_translation(&mut self, translation: Option<&str>) -> Result<(), Error> {
        let result = self.parse_translation(translation);

        // A half-parsed file must not read as initialized, or the next call would skip it and proceed with no maps.
        if result.is_err() {
            self.translation.reset();
        }

        result
    }

    fn parse_translation(&mut self, translation: Option<&str>) -> Result<(), Error> {
        if self.mode.is_default() || self.translation.initialized {
            return Ok(());
        }

        let Some(translation) = translation else {
            return Err(Error::NoTranslation);
        };

        // Notepad saves a UTF-8 byte order mark. Left in, it hides the first section's id line from the parser below.
        let translation = translation.strip_prefix('\u{feff}').unwrap_or(translation);

        self.translation.initialized = true;

        // Owning a copy here - once per translation file, not once per parsed line -
        // is what lets every `TranslationMap` entry below borrow a `&'static str`
        // slice of it instead of allocating a `String` per entry.
        //
        // SAFETY: `buf` is transmuted to `'static` so it can be stored alongside the
        // `TranslationMap`s that borrow from it, but it never actually outlives the
        // `Box<str>` it points into: `self.translation.buffer` is replaced only by
        // `Translation::reset`, which unconditionally clears `maps`/`write_lookup`
        // (every possible borrower, including `Output::accumulated`'s entries, which
        // are only ever populated from `self.translation.maps`) in the same call,
        // before the next `initialize_translation` installs a new buffer. So no
        // borrow derived from `buf` is ever observed after its buffer is gone.
        self.translation.buffer = Some(translation.into());
        let buf: &str = self.translation.buffer.as_deref().unwrap();
        let translation: &'static str = unsafe { transmute::<&str, &'static str>(buf) };

        let id_comment = get_id_comment();
        let mut bounds = Vec::new();
        let mut seen = HashSet::default();
        let mut current: Option<(u32, usize, usize)> = None;
        let mut warned = false;

        // A section is an id line and the lines up to the next one. Lines before the first id line belong to none.
        for (i, line) in translation.lines().enumerate() {
            if !line.starts_with(id_comment) {
                if current.is_none() && !warned && !line.trim().is_empty() {
                    warn!("Line {} comes before the first section and is ignored.", i + 1);
                    warned = true;
                }

                continue;
            }

            let offset = line.as_ptr() as usize - translation.as_ptr() as usize;

            if let Some((id, first_line, start)) = current.take() {
                bounds.push((id, first_line, &translation[start..offset]));
            }

            let id = line
                .strip_prefix(id_comment)
                .and_then(|n| n.strip_prefix(get_line_separator()))
                .and_then(|n| n.trim_end().parse::<u32>().ok())
                .ok_or(Error::InvalidTranslation(i + 1))?;

            // A second section would silently replace the first one's entries.
            if !seen.insert(id) {
                return Err(Error::DuplicateSection { id, line: i + 1 });
            }

            let after = offset + line.len();
            let newline = if translation[after..].starts_with("\r\n") {
                2
            } else {
                usize::from(translation[after..].starts_with('\n'))
            };

            current = Some((id, i + 1, after + newline));
        }

        if let Some((id, first_line, start)) = current {
            bounds.push((id, first_line, &translation[start..]));
        }

        // Sections parse independently, so they parse in parallel; recording them stays in file order.
        let splitter = LineSplitter::new();
        let sections = {
            let this = &*self;

            bounds
                .into_par_iter()
                .map(|(id, first_line, text)| this.parse_section(&splitter, id, first_line, text))
                .collect::<Result<Vec<_>, Error>>()?
        };

        for section in sections {
            let ParsedSection {
                id,
                first,
                pending,
                top_level,
                trailing,
                entries,
            } = section;

            if let Some((top_level_comments, metadata)) = first {
                self.translation.top_level_comments.insert(id, top_level_comments);
                self.translation.metadata.insert(id, metadata);
            }

            if !top_level.is_empty() {
                self.translation.top_level_comments.insert(id, top_level);
            }

            if !trailing.is_empty() {
                self.translation.trailing_comments.insert(id, trailing);
            }

            // A section without entries is kept for its metadata, unless writing and its display name has no
            // translation either.
            if entries.is_empty() {
                let metadata = self.translation.metadata.entry(id).or_insert(pending);
                let untranslated = metadata
                    .get(DISPLAY_NAME_POS)
                    .is_none_or(|display_name| display_name.is_empty() || display_name.ends_with(get_line_separator()));

                if self.mode.is_write() && untranslated {
                    continue;
                }
            }

            self.translation.maps.insert(id, entries);
        }

        self.build_write_lookup();

        Ok(())
    }

    /// Parses one section's lines - everything after its id line - with `self` only borrowed, so sections parse in
    /// parallel. `first_line` is the index of its first line in the file, for error positions.
    fn parse_section(
        &self,
        splitter: &LineSplitter,
        id: u32,
        first_line: usize,
        text: &'static str,
    ) -> Result<ParsedSection, Error> {
        let mut entries = TranslationMap::default();
        let mut top_level_comments: Vec<String> = Vec::new();
        let mut comments: Comments = smallvec![String::new(); 3];
        let mut first = None;

        for (offset, line) in text.lines().enumerate() {
            let i = first_line + offset;

            if line.starts_with(get_comment_prefix()) {
                if [
                    get_event_id_comment(),
                    get_event_name_comment(),
                    get_event_pos_comment(),
                ]
                .into_iter()
                .any(|c| line.starts_with(c))
                {
                    continue;
                }

                if first.is_none() {
                    let pos = CommentPos::from_str(line);

                    if pos == CommentPos::None {
                        top_level_comments.push(line.to_string());
                        continue;
                    }

                    if pos == CommentPos::DisplayName {
                        // With no closing marker the line past its prefix is
                        // already the stored form, `{source}{SEP}{translation}`.
                        comments[pos as usize] = line[get_map_display_name_comment_prefix().len()..].to_string();
                    } else {
                        comments[pos as usize] = line
                            .split_once(get_line_separator())
                            .ok_or(Error::InvalidTranslation(i + 1))?
                            .1
                            .to_string();
                    }
                } else {
                    comments.push(line.to_string());
                }

                continue;
            }

            let (source, translation) = match splitter.split(line, self.mode.is_write()) {
                TranslationLine::Split { source, translation } => (source, translation),
                TranslationLine::Untranslated => continue,
                TranslationLine::Malformed => {
                    warn!(
                        "{COULD_NOT_SPLIT_LINE_MSG}\n{AT_POSITION_MSG}: {i}\n{IN_FILE_MSG}: {file}.txt",
                        i = i + 1,
                        file = self.file_type.to_string().to_lowercase()
                    );
                    comments = smallvec![String::new(); 3];
                    continue;
                }
            };

            // The first entry takes the section's positional metadata and the comments above it.
            if first.is_none() {
                first = Some((take(&mut top_level_comments), comments.drain(..).collect()));
            }

            entries.insert(
                source,
                TranslationEntry {
                    // The three leading slots are positional metadata
                    // (name/order/display name) and are only ever filled for the
                    // first entry of a section, which consumes them above. For
                    // every other entry they are empty placeholders that
                    // `push_entries` would skip anyway, so dropping them here
                    // avoids allocating a Vec of empty strings per entry.
                    comments: replace(&mut comments, smallvec![String::new(); 3])
                        .into_iter()
                        .filter(|comment| !comment.is_empty())
                        .map(Cow::Owned)
                        .collect(),
                    translation,
                },
            );
        }

        // Past the three positional slots, what is left is the comments no entry followed.
        let trailing = if first.is_some() {
            comments.drain(3..).collect()
        } else {
            Vec::new()
        };

        Ok(ParsedSection {
            id,
            first,
            pending: comments,
            top_level: top_level_comments,
            trailing,
            entries,
        })
    }

    /// Flattens `self.translation.maps` into `self.translation.write_lookup`.
    ///
    /// Only relevant on write with [`DuplicateMode::Remove`], where [`Base::get_key`]
    /// has to resolve a key against *every* parsed map. Doing that by scanning is
    /// `O(maps)` per lookup, i.e. quadratic over the whole file set; flattening once
    /// makes it a single hash lookup.
    ///
    /// Entries are moved rather than cloned, so this costs no extra memory: with
    /// [`DuplicateMode::Remove`] the per-id maps are only consulted for presence on
    /// write, never for content.
    pub(super) fn build_write_lookup(&mut self) {
        if !self.mode.is_write() || !self.duplicate_mode.is_remove() {
            return;
        }

        let total: usize = self.translation.maps.values().map(TranslationMap::len).sum();

        self.translation.write_lookup = TranslationMap::with_capacity(total);

        for map in self.translation.maps.values_mut() {
            // `or_insert` keeps the first occurrence, matching the order the
            // previous linear scan resolved duplicates in.
            for (source, translation) in map.drain(..) {
                self.translation.write_lookup.entry(source).or_insert(translation);
            }
        }
    }

    /// Sets `self.translation_map` to the entry from `self.translation.maps`.
    ///
    /// If `self.mode` is [`Mode::Purge`], it will push entries from `self.translation_map` to `self.output.accumulated` and break.
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
    ///     - If mode is [`Mode::Write`] and `id` is not in `self.translation.maps`, unless `id` is a map: a map can be
    ///       translated by a line it shares with another map, so it is walked and decided there.
    ///     - If `id` is skipped.
    ///     - If mode is [`Mode::Purge`].
    /// - [`ControlFlow::Continue`] - In other situations.
    ///
    pub(super) fn get_translation_map(&mut self, id: u32) -> ControlFlow<()> {
        let entry = self.translation.maps.entry(id);
        let index = entry.index();

        // Select the map for `id`, creating it if we're not writing.
        match entry {
            Entry::Occupied(_) => {}
            Entry::Vacant(entry) => {
                // With duplicates removed, a line of an entry with no section of its own may still be translated
                // through an earlier section, so the entry is processed. Its caller decides whether it's written.
                let shares_lines = !self.duplicate_mode.is_allow() && !self.file_type.is_misc();

                if self.mode.is_write() && !self.file_type.is_any_map() && !shares_lines {
                    return ControlFlow::Break(());
                }

                entry.insert(TranslationMap::default());
            }
        }

        self.translation.map_index = index;

        if self.skip_events.get(&self.file_type).is_some_and(|x| x.contains(&id))
            || (self.file_type.is_any_map() && self.skip_maps.contains(&id))
        {
            if self.mode.is_append() || self.mode.is_purge() {
                let metadata = self.get_metadata(id);

                let map = take(self.translation_map_mut());
                self.output.accumulated.push((id, metadata, FlushedLines::EMPTY, map));
            }

            self.output.total_length = self.output.lines.len();
            return ControlFlow::Break(());
        }

        self.get_ignore_entry(id);

        if self.mode.is_purge() {
            self.flush_translation(id);
            return ControlFlow::Break(());
        }

        ControlFlow::Continue(())
    }

    pub(super) fn get_metadata(&mut self, id: u32) -> Comments {
        let Some(mut comments) = self.translation.metadata.remove(&id) else {
            return SmallVec::default();
        };

        comments
            .iter_mut()
            // Only the three positional slots get a prefix.
            .zip([CommentPos::Name, CommentPos::Order, CommentPos::DisplayName])
            .filter(|(x, _)| !x.is_empty())
            .for_each(|(x, pos)| {
                *x = match pos {
                    CommentPos::Name => {
                        format!(
                            "{comment}{sep}{x}",
                            comment = get_name_comment(),
                            sep = get_line_separator()
                        )
                    }

                    CommentPos::Order => {
                        format!(
                            "{comment}{sep}{x}",
                            comment = get_map_order_comment(),
                            sep = get_line_separator()
                        )
                    }

                    CommentPos::DisplayName => {
                        format!("{comment}{x}", comment = get_map_display_name_comment_prefix())
                    }

                    CommentPos::None => unreachable!(),
                }
            });

        comments
    }

    // Splitting this would rewrite working code, and the lint is about length only.
    #[allow(clippy::too_many_lines)]
    pub(super) fn finish_translation(&mut self) -> ProcessedData {
        let allow_dup = self.duplicate_mode.is_allow() || self.file_type.is_misc();
        let skip_events_entry = self.skip_events.get(&self.file_type);

        // Each id's sources in file order, so its leftovers are found without scanning every entry of every id.
        let mut leftover_keys: HashMap<u32, Vec<Cow<'static, str>>> = HashMap::default();

        let mut accumulated_map: indexmap::IndexMap<Cow<'static, str>, (u32, TranslationEntry), FastBuildHasher> =
            if allow_dup {
                indexmap::IndexMap::default()
            } else {
                let len = self.translation.maps.values().fold(0, |mut acc, map| {
                    acc += map.len();
                    acc
                });

                self.translation.maps.drain(..).fold(
                    indexmap::IndexMap::with_capacity_and_hasher(len, FastBuildHasher::default()),
                    |mut acc, (k, v)| {
                        let keys = leftover_keys.entry(k).or_default();

                        for (key, value) in v {
                            keys.push(key.clone());
                            acc.insert(key, (k, value));
                        }
                        acc
                    },
                )
            };

        // `output` is consumed here while the rest of `self` is only read, so
        // destructuring makes the two borrows disjoint. `lines` backs every
        // `FlushedLines::Range`.
        let Self {
            output: Output { accumulated, lines, .. },
            ignore,
            ..
        } = self;

        // Exact on a plain read; a floor once translations or comments are appended, and the vector grows from there.
        let separator_len = get_line_separator().len();
        let output_size: usize = accumulated
            .iter()
            .map(|(_, meta, flushed, map)| {
                let metadata: usize = meta.iter().map(|comment| comment.len() + 1).sum();
                let sources: usize = (0..flushed.len())
                    .map(|index| flushed.get(lines, index).len() + separator_len + 2)
                    .sum();
                let entries: usize = map
                    .iter()
                    .map(|(source, entry)| source.len() + entry.translation.len() + separator_len + 2)
                    .sum();

                metadata + sources + entries
            })
            .sum();
        let mut output = Vec::with_capacity(output_size);

        let mut prev_id = u32::MAX;

        // A map with events is flushed once per event, so only the first of an
        // id's blocks carries the translations parsed for that id. They are
        // carried across the whole run instead: appending a map with
        // `map_events` set used to resolve the first event's lines and leave
        // every later event untranslated.
        let mut carried = TranslationMap::default();
        // Entries already handed to a line. A source can appear under two
        // events of the same map, and both want the one translation the map
        // holds for it, so a consumed entry stays reachable until the id ends.
        let mut consumed = TranslationMap::default();
        let mut carry_id = u32::MAX;

        for i in 0..accumulated.len() {
            // Splitting gives the current entry mutably and the lookahead
            // immutably at the same time.
            let (current, rest) = accumulated[i..].split_first_mut().unwrap();
            let (id, meta, flushed, map) = current;

            if *id != carry_id {
                carried.clear();
                consumed.clear();
                carry_id = *id;
            }

            // Leftovers belong at the end of the id's run, not after each of
            // its blocks.
            let last_of_id = rest.first().is_none_or(|(next_id, ..)| *next_id != *id);

            let skip = skip_events_entry.is_some_and(|e| e.contains(id))
                || (self.file_type.is_any_map() && self.skip_maps.contains(id))
                || (self.mode.is_purge() && self.file_type.is_system() && *id == SYSTEM_GAME_TITLE_ID);

            if skip {
                if self.mode.is_append() || self.mode.is_purge() {
                    push_metadata(
                        &mut output,
                        *id,
                        meta,
                        &self.translation.top_level_comments.remove(id).unwrap_or_default(),
                    );

                    for (source, translation) in map {
                        push_entries(&mut output, source, translation);
                    }

                    if last_of_id {
                        push_trailing(&mut output, self.translation.trailing_comments.remove(id));
                    }
                }

                continue;
            }

            if self.mode.is_purge() {
                push_metadata(
                    &mut output,
                    *id,
                    meta,
                    &self.translation.top_level_comments.remove(id).unwrap_or_default(),
                );

                for (mut source, translation) in take(map) {
                    if translation.is_empty() {
                        let moved = take(&mut source);

                        if self.flags.contains(BaseFlags::CreateIgnore) && !moved.is_empty() {
                            // Keyed on the id being written, not on whichever
                            // one was selected last - every purged line used to
                            // land in the final section, so nothing matched on
                            // the next read. Always per id, as `parse_ignore`
                            // loads a purge's file: with duplicates removed this
                            // key would collapse, and the loaded sections would
                            // be left next to a second one.
                            ignore
                                .entry_for(Ignore::key(self.file_type, *id, self.flags, DuplicateMode::Allow))
                                .insert(moved.into_owned());
                        }
                    }

                    push_entries(&mut output, &source, &translation);
                }

                if last_of_id {
                    push_trailing(&mut output, self.translation.trailing_comments.remove(id));
                }

                continue;
            }

            if *id != prev_id {
                let has_display_name = meta.get(DISPLAY_NAME_POS).is_some_and(|c| !c.is_empty());

                // What the file holds that the game no longer has still needs its header, or it would be written
                // under the previous section's.
                let keeps_user_data = self.mode.is_append()
                    && (self.translation.top_level_comments.contains_key(id)
                        || self.translation.trailing_comments.contains_key(id));

                let has_leftovers = self.mode.is_append()
                    && !self.flags.contains(BaseFlags::SkipObsolete)
                    && if allow_dup {
                        !map.is_empty()
                    } else {
                        leftover_keys.get(id).is_some_and(|keys| {
                            keys.iter()
                                .any(|key| accumulated_map.get(key).is_some_and(|(owner, _)| *owner == *id))
                        })
                    };

                let same_id_has_lines = !flushed.is_empty()
                    // An id's blocks are pushed back to back, so the lookahead stops at the next id.
                    || rest
                        .iter()
                        .take_while(|(next_id, ..)| *next_id == *id)
                        .any(|(next_id, _, next_lines, _)| *next_id == *id && !next_lines.is_empty());

                let should_push_map =
                    self.file_type.is_any_map() && self.map_events && (same_id_has_lines || has_display_name);

                let should_push_other = !flushed.is_empty() || has_display_name || keeps_user_data || has_leftovers;

                if should_push_map || should_push_other {
                    push_metadata(
                        &mut output,
                        *id,
                        meta,
                        &self.translation.top_level_comments.remove(id).unwrap_or_default(),
                    );
                }

                prev_id = *id;
            }

            // An event header is smuggled in as an entry with an empty source
            // whose comments carry the id, name and position lines. It is only
            // written when the event that follows actually has text, and it is
            // taken out of the map so the append pass below cannot print it
            // again. Keying on the empty source matters: this used to fire for
            // whatever entry happened to be first, so an append emitted a real
            // translation with no source beside it.
            if let Some(entry) = map.shift_remove("") {
                let next_lines_empty = rest.first().is_some_and(|(_, _, next_lines, _)| next_lines.is_empty());

                if !next_lines_empty {
                    push_entries(&mut output, "", &entry);
                }
            }

            if !map.is_empty() {
                carried = take(map);
            }

            let empty = TranslationEntry::default();

            for line_index in 0..flushed.len() {
                let source = flushed.get(lines, line_index);
                let deduplicated;

                let translation = match (allow_dup, self.mode.is_append()) {
                    (true, true) => {
                        if let Some((key, entry)) = carried.swap_remove_entry(source) {
                            consumed.insert(key, entry);
                        }

                        consumed.get(source).unwrap_or(&empty)
                    }
                    (false, true) => {
                        deduplicated = accumulated_map.swap_remove(source).unzip().1.unwrap_or_default();

                        &deduplicated
                    }
                    (_, false) => &empty,
                };

                push_entries(&mut output, source, translation);
            }

            if last_of_id && !self.flags.contains(BaseFlags::SkipObsolete) {
                match (allow_dup, self.mode.is_append()) {
                    (true, true) => {
                        for (source, translation) in carried.drain(..) {
                            push_entries(&mut output, &source, &translation);
                        }
                    }
                    (false, true) => {
                        for key in leftover_keys.remove(id).unwrap_or_default() {
                            // A source repeated under a later id belongs to that id.
                            if accumulated_map.get(&key).is_some_and(|(i, _)| i == id)
                                && let Some((_, translation)) = accumulated_map.swap_remove(&key)
                            {
                                push_entries(&mut output, &key, &translation);
                            }
                        }
                    }
                    _ => {}
                }
            }

            if last_of_id {
                push_trailing(&mut output, self.translation.trailing_comments.remove(id));
            }
        }

        if self.mode.is_append() && !self.flags.contains(BaseFlags::SkipObsolete) {
            self.push_unvisited_sections(&mut output, allow_dup, &mut leftover_keys, &mut accumulated_map);
        }

        output.pop();
        ProcessedData::TranslationData(output)
    }

    /// The ids for the entries of a file whose entries have none of their own, such as `Scripts` and `plugins.js`, given
    /// their `names` in game order.
    ///
    /// An entry takes the id of the translation section with its name - the first such section for the first entry of
    /// that name, and so on - so an entry inserted into the game doesn't shift the translations after it. An entry
    /// with no such section gets a new id past all the others. Without a translation that is the position, from 1.
    pub(super) fn ids_by_name<'a>(&self, names: impl IntoIterator<Item = &'a str>) -> Vec<u32> {
        let translation = &self.translation;

        let mut known: Vec<(u32, &str)> = translation
            .metadata
            .iter()
            .filter_map(|(id, comments)| {
                comments
                    .get(CommentPos::Name as usize)
                    .filter(|name| !name.is_empty())
                    .map(|name| (*id, name.as_str()))
            })
            .collect();

        // Descending, so that popping gives the lowest id first.
        known.sort_unstable_by_key(|(id, _)| std::cmp::Reverse(*id));

        let mut by_name: HashMap<&str, Vec<u32>> = HashMap::default();

        for (id, name) in known {
            by_name.entry(name).or_default().push(id);
        }

        let mut last = translation
            .metadata
            .keys()
            .chain(translation.maps.keys())
            .copied()
            .max()
            .unwrap_or(0);

        names
            .into_iter()
            .map(|name| {
                by_name.get_mut(name).and_then(Vec::pop).unwrap_or_else(|| {
                    last += 1;
                    last
                })
            })
            .collect()
    }

    /// Writes the sections of the translation file that no entry of the game was processed for - an entry the game
    /// removed, a map it left without events - after the processed ones, by id. They are obsolete, but dropping a whole
    /// section would throw its translations away, which only [`BaseFlags::SkipObsolete`] asks for.
    fn push_unvisited_sections(
        &mut self,
        output: &mut Vec<u8>,
        allow_dup: bool,
        leftover_keys: &mut HashMap<u32, Vec<Cow<'static, str>>>,
        accumulated_map: &mut indexmap::IndexMap<Cow<'static, str>, (u32, TranslationEntry), FastBuildHasher>,
    ) {
        let translation = &self.translation;
        let mut ids: Vec<u32> = translation
            .metadata
            .keys()
            .chain(translation.top_level_comments.keys())
            .chain(translation.trailing_comments.keys())
            .chain(
                translation
                    .maps
                    .iter()
                    .filter(|(_, map)| !map.is_empty())
                    .map(|(id, _)| id),
            )
            .chain(leftover_keys.keys())
            .copied()
            .collect();

        ids.sort_unstable();
        ids.dedup();

        for id in ids {
            let entries: Vec<(Cow<'static, str>, TranslationEntry)> = if allow_dup {
                self.translation
                    .maps
                    .get_mut(&id)
                    .map(|map| map.drain(..).collect())
                    .unwrap_or_default()
            } else {
                leftover_keys
                    .remove(&id)
                    .unwrap_or_default()
                    .into_iter()
                    .filter_map(|key| {
                        // A source repeated under a later id belongs to that id.
                        if accumulated_map.get(&key).is_some_and(|(owner, _)| *owner == id) {
                            accumulated_map
                                .swap_remove(&key)
                                .map(|(_, translation)| (key, translation))
                        } else {
                            None
                        }
                    })
                    .collect()
            };

            // The name and order alone are not worth a section: a processed id leaves them behind whenever it had
            // nothing to flush. A translated display name is.
            let display_name_translated = self
                .translation
                .metadata
                .get(&id)
                .and_then(|metadata| metadata.get(DISPLAY_NAME_POS))
                .is_some_and(|name| !name.is_empty() && !name.ends_with(get_line_separator()));

            let metadata = self.get_metadata(id);
            let top_level = self.translation.top_level_comments.remove(&id).unwrap_or_default();
            let trailing = self.translation.trailing_comments.remove(&id);

            if entries.is_empty() && top_level.is_empty() && trailing.is_none() && !display_name_translated {
                continue;
            }

            push_metadata(output, id, &metadata, &top_level);

            for (source, translation) in &entries {
                push_entries(output, source, translation);
            }

            push_trailing(output, trailing);
        }
    }

    /// Flushes current `self.translation_map` and `self.output.lines` to `self.output.accumulated` along with metadata and id.
    ///
    /// It's necessary to call [`Base::finish_translation`] once we've finished flushing entries.
    ///
    /// # Parameters
    ///
    /// - `id` - ID of the entry to flush.
    ///
    pub(super) fn flush_translation(&mut self, id: u32) {
        let metadata = self.get_metadata(id);

        if self.mode.is_purge() {
            if !self.translation_map().is_empty() || metadata.get(DISPLAY_NAME_POS).is_some_and(|x| !x.is_empty()) {
                let map = take(self.translation_map_mut());
                self.output.accumulated.push((id, metadata, FlushedLines::EMPTY, map));
            }
        } else if self.duplicate_mode.is_allow() || self.file_type.is_misc() {
            if self.skip_events.get(&self.file_type).is_some_and(|e| e.contains(&id))
                || (self.file_type.is_any_map() && self.skip_maps.contains(&id))
            {
                self.output.lines.clear();
                self.translation_map_mut().clear();
            } else {
                let lines = FlushedLines::Owned(self.output.lines.drain(..).collect());

                let map = take(self.translation_map_mut());
                self.output.accumulated.push((id, metadata, lines, map));
            }
        } else {
            let total_length = self.output.total_length;
            let current_length = self.output.lines.len() - total_length;

            // Left in `self.output.lines`; only the range is recorded, so nothing is copied.
            let lines = FlushedLines::Range(total_length..total_length + current_length);

            self.output
                .accumulated
                .push((id, metadata, lines, TranslationMap::default()));

            self.output.total_length += current_length;
        }
    }

    pub(super) fn update_metadata(&mut self, id: u32, metadata_vec: &[(CommentPos, &str)]) {
        let metadata = self
            .translation
            .metadata
            .entry(id)
            .or_insert_with(|| smallvec![String::new(); 3]);

        if metadata.len() < 3 {
            metadata.resize(3, String::new());
        }

        for &(entry_id, entry) in metadata_vec.iter().filter(|(_, x)| !x.is_empty()) {
            if entry_id == CommentPos::DisplayName {
                if self.mode.is_append() {
                    let Some((source, mut translation)) = metadata[entry_id as usize].split_once(get_line_separator())
                    else {
                        metadata[entry_id as usize] = format!("{entry}{sep}", sep = get_line_separator());
                        continue;
                    };

                    if source != entry {
                        translation = "";
                    }

                    metadata[entry_id as usize] = format!("{entry}{sep}{translation}", sep = get_line_separator());
                } else {
                    metadata[entry_id as usize] = format!("{entry}{sep}", sep = get_line_separator());
                }

                continue;
            }

            metadata[entry_id as usize] = entry.to_string();
        }
    }
}
