use super::*;
use crate::{
    BaseFlags, Comments, IndexSetExt, ProcessedData,
    constants::{IGNORE_ENTRY_COMMENT, INSTANCE_VAR_PREFIX, SEPARATOR},
    types::{
        DuplicateMode, EngineType, IgnoreMap, IndexMapExt, IndexMapGx, Labels,
        Lines, Mode, RPGMFileType, TranslationEntry, TranslationMap,
    },
};
use gxhash::{HashMap, HashMapExt, HashSet};
use marshal_rs::{Value, dump};
use serde_json::{Value as SerdeValue, to_vec};
use std::{borrow::Cow, fmt::Write as FmtWrite};

/// Everything parsed out of a translation `.txt`.
///
/// Built once by [`Base::initialize_translation`] and read from there on; the only
/// mutation during processing is selecting the current section and draining it.
pub(super) struct Translation {
    pub(super) initialized: bool,

    pub(super) maps: IndexMapGx<u16, TranslationMap>,

    /// Index of the currently selected map in `maps`, or `usize::MAX` when none is
    /// selected. An index rather than a reference because the map it points at is
    /// owned by this same struct; `maps` is only ever appended to while a selection
    /// is live, so indices stay valid.
    pub(super) map_index: usize,

    /// Flattened `maps`, built once on write with [`DuplicateMode::Remove`].
    /// See [`Base::build_write_lookup`].
    pub(super) write_lookup: TranslationMap,

    pub(super) metadata: HashMap<u16, Comments>,
    pub(super) top_level_comments: HashMap<u16, Vec<String>>,
}

impl Default for Translation {
    fn default() -> Self {
        Self {
            initialized: false,
            maps: IndexMapGx::default(),
            // Sentinel: no section selected. Deriving `Default` would make this 0,
            // which is a valid index.
            map_index: usize::MAX,
            write_lookup: TranslationMap::default(),
            metadata: HashMap::default(),
            top_level_comments: HashMap::default(),
        }
    }
}

impl Default for Ignore {
    fn default() -> Self {
        Self {
            map: IgnoreMap::default(),
            // Sentinel: no entry selected.
            entry_index: usize::MAX,
        }
    }
}

impl Ignore {
    /// The currently selected entry, or [`None`] if none was selected.
    pub(super) fn entry(&self) -> Option<&IgnoreEntry> {
        self.map.get_index(self.entry_index).map(|(_, entry)| entry)
    }

    /// Mutable counterpart of [`Ignore::entry`].
    pub(super) fn entry_mut(&mut self) -> Option<&mut IgnoreEntry> {
        self.map
            .get_index_mut(self.entry_index)
            .map(|(_, entry)| entry)
    }
}

impl Translation {
    fn reset(&mut self) {
        self.initialized = false;
        self.maps.clear();
        self.map_index = usize::MAX;
        self.write_lookup.clear();
        self.metadata.clear();
    }
}

/// Everything accumulated while processing, drained by [`Base::finish_translation`].
#[derive(Default)]
pub(super) struct Output {
    pub(super) lines: Lines,

    /// How much of `lines` has already been claimed by a flushed section, so the
    /// next flush can record the range it added.
    pub(super) total_length: usize,

    pub(super) accumulated: Vec<(u16, Comments, FlushedLines, TranslationMap)>,
}

impl Output {
    fn reset(&mut self) {
        self.lines.clear();
        self.total_length = 0;
        self.accumulated.clear();
    }
}

/// Entries to skip on read, or to collect on purge.
pub struct Ignore {
    pub map: IgnoreMap,

    /// Index of the currently selected entry in `map`, or `usize::MAX` when none is
    /// selected. Only set when the ignore flags are on.
    pub(super) entry_index: usize,
}

pub struct Base {
    pub mode: Mode,
    pub flags: BaseFlags,
    pub engine_type: EngineType,
    pub duplicate_mode: DuplicateMode,

    pub skip_maps: HashSet<u16>,
    pub skip_events: HashMap<RPGMFileType, HashSet<u16>>,
    pub map_events: bool,

    pub ignore: Ignore,

    pub(super) translation: Translation,
    pub(super) output: Output,

    /// `MapInfos` for the current run of maps, parsed once by
    /// [`Base::process_map`] and reused across the run.
    pub(super) mapinfos: Value,

    /// Title override set through [`Base::set_game_title`], for engines that keep
    /// it in `Game.ini` rather than the system file.
    pub(super) game_title: String,

    pub(super) file_type: RPGMFileType,
    pub(super) labels: Labels,
}

impl Default for Base {
    fn default() -> Self {
        Self {
            mode: Mode::read(),
            flags: BaseFlags::empty(),
            engine_type: EngineType::New,
            duplicate_mode: DuplicateMode::Remove,

            ignore: Ignore::default(),
            translation: Translation::default(),
            output: Output::default(),

            map_events: false,
            mapinfos: Value::default(),
            game_title: String::new(),
            file_type: RPGMFileType::Invalid,
            labels: Labels::default(),

            skip_maps: HashSet::default(),
            skip_events: HashMap::default(),
        }
    }
}

impl Base {
    /// Creates new base from mode and engine type.
    ///
    /// # Parameters
    ///
    /// - `mode` - [`Mode`] to use.
    /// - `engine_type` - [`EngineType`] to use.
    ///
    #[must_use]
    pub fn new(mode: Mode, engine_type: EngineType) -> Self {
        Self {
            mode,
            engine_type,
            labels: Labels::new(engine_type),

            translation: Translation {
                maps: IndexMapGx::with_capacity(1024),
                metadata: HashMap::with_capacity(1024),
                ..Default::default()
            },

            output: Output {
                lines: Lines::with_capacity(512),
                ..Default::default()
            },

            ..Default::default()
        }
    }

    /// Clears all the underlying collections, and makes this base ready to be used in the next base.
    ///
    /// This function is used by file-specific bases' constructors, so you generally mustn't call it manually.
    pub fn reset(&mut self) {
        self.translation.reset();
        self.output.reset();
    }

    /// Inserts `string` to `self.output.lines` if `self.mode`.
    ///
    /// Will skip inserting if `self.mode` is not [`Mode::Write`] or `self.flags` contain [`BaseFlags::Ignore`] and `self.ignore_entry` contains the string.
    ///
    /// # Parameters
    ///
    /// - `string` - String to insert in `self.output.lines`.
    ///
    pub(super) fn insert_string(&mut self, string: Cow<'_, str>) {
        if self.mode.is_write()
            || (self.flags.contains(BaseFlags::Ignore)
                && self
                    .ignore_entry()
                    .is_some_and(|entry| entry.contains(string.as_ref())))
        {
            return;
        }

        self.output.lines.insert(string.into_owned());
    }

    /// Gets ignore entry from `self.ignore.map` by `id`.
    ///
    /// Skips getting an entry if `self.flags` do not contain [`BaseFlags::Ignore`] or [`BaseFlags::CreateIgnore`].
    ///
    /// # Parameters
    ///
    /// - `id` - ID of the entry to get.
    ///
    pub(super) fn get_ignore_entry(&mut self, id: u16) {
        if !self
            .flags
            .intersects(BaseFlags::CreateIgnore | BaseFlags::Ignore)
        {
            return;
        }

        // Built in one pass. This previously formatted the name, truncated it at
        // the ':', then formatted the whole key again - two allocations per map
        // and per event.
        let mut key = String::with_capacity(
            IGNORE_ENTRY_COMMENT.len() + SEPARATOR.len() + 24,
        );

        key.push_str(IGNORE_ENTRY_COMMENT);
        key.push_str(SEPARATOR);
        let _ = write!(key, "{file}", file = self.file_type);

        // With duplicates removed the whole file shares a single entry, so the id
        // is left off.
        if !(self.flags.contains(BaseFlags::Ignore)
            && self.duplicate_mode.is_remove())
        {
            let _ = write!(key, ": {id}");
        }

        let entry = self.ignore.map.entry(key);

        self.ignore.entry_index = entry.index();
        entry.or_default();
    }

    /// The currently selected ignore entry, or [`None`] if the ignore flags are off
    /// and no entry was ever selected.
    pub(super) fn ignore_entry(&self) -> Option<&IgnoreEntry> {
        self.ignore.entry()
    }

    /// The currently selected translation map.
    ///
    /// # Panics
    ///
    /// Panics if no map is selected, i.e. if called before [`Base::get_translation_map`].
    pub(super) fn translation_map(&self) -> &TranslationMap {
        &self.translation.maps[self.translation.map_index]
    }

    /// Mutable counterpart of [`Base::translation_map`].
    ///
    /// # Panics
    ///
    /// Panics if no map is selected, i.e. if called before [`Base::get_translation_map`].
    pub(super) fn translation_map_mut(&mut self) -> &mut TranslationMap {
        &mut self.translation.maps[self.translation.map_index]
    }

    /// Wraps string in a [`Value`].
    ///
    /// If `literal` argument is set, this wraps string in a [`Value`] of [`ValueType::String`] type.
    /// Else, this wraps string in a [`Value`] of [`ValueType::Bytes`] type.
    ///
    /// # Parameters
    ///
    /// - `string` - String to wrap in a [`Value`].
    /// - `literal` - Whether to wrap `string` as [`ValueType::String`] or as [`ValueType::Bytes`].
    ///
    /// # Returns
    ///
    /// - [`Value`] - wrapped string.
    ///
    pub(super) fn make_string_value(string: &str, literal: bool) -> Value {
        if literal {
            Value::string(string)
        } else {
            Value::bytes(string.as_bytes())
        }
    }

    /// Extracts string from [`Value`].
    ///
    /// Will always return [`None`] if [`Value`] is not of [`ValueType::String`] or [`ValueType::Bytes`].
    ///
    /// # Parameters
    ///
    /// - `value` - Value from which string will be extracted.
    /// - `fail_if_empty` - Whether to return if extracted string happens to be empty.
    ///
    /// # Returns
    ///
    /// - Nothing if [`Value`] is not of [`ValueType::String`] or [`ValueType::Bytes`], or `fail_if_empty` is set and `string` is empty.
    /// - [`&str`] - Parsed string.
    ///
    /// The returned string borrows `value`, not `self` - keeping the lifetimes
    /// separate lets the caller drop the `&self` borrow immediately.
    pub(super) fn extract_string<'v>(
        &self,
        value: &'v Value,
        fail_if_empty: bool,
    ) -> Option<&'v str> {
        let string = value.as_str().or_else(|| {
            std::str::from_utf8(value.as_byte_vec().unwrap_or_default()).ok()
        })?;

        let trimmed = string.trim();

        if trimmed.is_empty() && fail_if_empty {
            return None;
        }

        Some(if self.flags.contains(BaseFlags::Trim) {
            trimmed
        } else {
            string
        })
    }

    /// ONLY CALLED ON WRITE.
    ///
    /// Gets the [`TranslationEntry`] corresponding to the `key` from translation.
    ///
    /// This will return [`TranslationEntry`] corresponding to the `key` from `self.translation_map`, and also will seek it in maps `self.translation.maps` if `self.duplicate_mode` is [`DuplicateMode::Remove`].
    ///
    /// # Parameters
    ///
    /// - `key` - key to get.
    ///
    /// # Returns
    ///
    /// - Nothing if key wasn't found in translation.
    /// - [`&TranslationEntry`] - entry corresponding to the `key`.
    ///
    pub(super) fn get_key(&self, key: &str) -> Option<&TranslationEntry> {
        if self.duplicate_mode.is_allow() {
            self.translation_map().get(key)
        } else {
            self.translation.write_lookup.get(key)
        }
    }

    /// Returns the RPG Maker data if `self.mode` is [`Mode::Write`], else returns translation data.
    ///
    /// # Parameters
    ///
    /// - `value` - [`Value`] to use on write.
    ///
    /// # Returns
    ///
    /// - [`ProcessedData::RPGMData`] if `self.mode` is [`Mode::Write`].
    /// - [`ProcessedData::TranslationData`] otherwise.
    ///
    pub(super) fn finish(&mut self, value: Value) -> ProcessedData {
        if self.mode.is_write() {
            ProcessedData::RPGMData(if self.file_type.is_plugins() {
                let plugins_bytes = unsafe {
                    to_vec(&SerdeValue::from(value)).unwrap_unchecked()
                };

                ["var $plugins =\n".as_bytes(), &plugins_bytes].concat()
            } else if self.engine_type.is_new() {
                unsafe { to_vec(&SerdeValue::from(value)).unwrap_unchecked() }
            } else {
                dump(
                    value,
                    if self.file_type.is_scripts() {
                        None
                    } else {
                        INSTANCE_VAR_PREFIX
                    },
                )
            })
        } else {
            self.finish_translation()
        }
    }
}
