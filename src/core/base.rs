use super::*;
use crate::{
    BaseFlags, Comments, IndexSetExt, ProcessedData,
    get_ignore_entry_comment, get_line_separator,
    marshal_compat::{RpgmData, Value},
    types::{
        DuplicateMode, EngineType, IgnoreMap, IndexMapExt, IndexMapGx, Labels, Lines, Mode, RPGMFileType,
        TranslationEntry, TranslationMap,
    },
};
use gxhash::{HashMap, HashMapExt, HashSet};
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
    /// The section key one file's entries live under.
    ///
    /// With duplicates removed on a read the whole file shares a single entry,
    /// so the id is left off; every other combination keys on the entry id.
    ///
    /// System, Scripts and Plugins are exempt from the collapse: each is a
    /// single section already (there is nothing to deduplicate), and
    /// `parse_ignore` keeps their id for exactly that reason - it must match
    /// what this function builds, or a `.rvpacker-ignore` entry written under
    /// one of these three never matches at read time. This used to collapse
    /// them anyway, so ignore entries silently had no effect on System and
    /// Plugins under `DuplicateMode::Remove`.
    pub(super) fn key(file_type: RPGMFileType, id: u16, flags: BaseFlags, duplicate_mode: DuplicateMode) -> String {
        // Built in one pass. This previously formatted the name, truncated it at
        // the ':', then formatted the whole key again - two allocations per map
        // and per event.
        let mut key = String::with_capacity(get_ignore_entry_comment().len() + get_line_separator().len() + 24);

        key.push_str(get_ignore_entry_comment());
        key.push_str(get_line_separator());
        let _ = write!(key, "{file_type}");

        if file_type.is_misc() || !(flags.contains(BaseFlags::Ignore) && duplicate_mode.is_remove()) {
            let _ = write!(key, ": {id}");
        }

        key
    }

    /// The currently selected entry, or [`None`] if none was selected.
    pub(super) fn entry(&self) -> Option<&IgnoreEntry> {
        self.map.get_index(self.entry_index).map(|(_, entry)| entry)
    }

    /// The entry for `key`, created if it is not there yet.
    ///
    /// Purging collects its ignore entries in [`Base::finish_translation`], by
    /// which point the selected entry is whichever id happened to be processed
    /// last - so that pass has to name the id it is writing rather than rely on
    /// the selection.
    pub(super) fn entry_for(&mut self, key: String) -> &mut IgnoreEntry {
        self.map.entry(key).or_default()
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
    pub(super) mapinfos: Option<RpgmData>,

    /// Title override set through [`Base::set_game_title`], for engines that keep
    /// it in `Game.ini` rather than the system file.
    pub(super) game_title: String,

    pub(super) file_type: RPGMFileType,
    pub(super) labels: Labels,

    /// Forces *decoding* of untagged source text to a specific codepage, set
    /// through [`Base::set_read_encoding`].
    ///
    /// XP/VX (pre-1.9 Ruby) and RM2K text carries no reliable in-file encoding
    /// indicator, so the caller may know the codepage (e.g. from `RPG_RT.ini`)
    /// better than the multi-encoding fallback guess ([`Base::decode_scripts`]
    /// and the RM2K `DbStr` decoder both consult this). Deliberately separate
    /// from [`Base::write_encoding`] - the source game's codepage and the
    /// codepage a translation should be written back in are almost never the
    /// same value (a `Shift_JIS` game translated into Russian can't be
    /// re-encoded as `Shift_JIS` at all).
    pub(super) read_encoding: Option<&'static encoding_rs::Encoding>,

    /// Forces *encoding* of translated text written back to the source
    /// format, set through [`Base::set_write_encoding`]. `None` (the
    /// default) always writes plain UTF-8, which is the only choice that
    /// cannot silently corrupt a translation into a different script than
    /// the source - see the "Text encoding" section of the crate
    /// documentation for when to override this.
    pub(super) write_encoding: Option<&'static encoding_rs::Encoding>,

    /// Which RPG Maker 2000/2003 build the source targets, detected from the
    /// database's `system.ldb_id` by [`Base::process_rm2k_database`] (or the
    /// caller, through [`Base::set_rm2k_engine`]) and consulted when
    /// re-serializing `.lmu`/`.ldb`/`.lmt` files - some fields only exist on
    /// one of the two.
    pub(super) rm2k_engine: rm2k::engine::Engine,
}

impl Default for Base {
    fn default() -> Self {
        Self {
            mode: Mode::read(),
            flags: BaseFlags::empty(),
            engine_type: EngineType::MVMZ,
            duplicate_mode: DuplicateMode::Remove,

            ignore: Ignore::default(),
            translation: Translation::default(),
            output: Output::default(),

            map_events: false,
            mapinfos: None,
            game_title: String::new(),
            file_type: RPGMFileType::Invalid,
            labels: Labels::default(),
            read_encoding: None,
            write_encoding: None,
            rm2k_engine: rm2k::engine::Engine::R2K,

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

    /// Forces *decoding* of untagged source text to `encoding`, instead of
    /// guessing it.
    ///
    /// Affects [`Base::process_scripts`] (XP/VX/VX Ace), the RM2K database/map
    /// processing, and any VX Ace field whose declared encoding isn't
    /// resolvable - all of which otherwise fall back to trying a fixed list of
    /// common codepages. Pass [`None`] to restore the guessing behavior.
    ///
    /// This only affects reading the source game's own text - see
    /// [`Base::set_write_encoding`] for the independent, write-side setting.
    /// They are almost never the same value: a game written in `Shift_JIS`
    /// being translated into Russian cannot be re-encoded as `Shift_JIS` at
    /// all, so reusing this for both directions would silently corrupt the
    /// translation.
    ///
    /// # Parameters
    ///
    /// - `encoding` - Codepage to decode text with, or [`None`] to guess it.
    pub fn set_read_encoding(&mut self, encoding: Option<&'static encoding_rs::Encoding>) {
        self.read_encoding = encoding;
    }

    /// Forces *encoding* of translated text written back to the source
    /// format, instead of always writing plain UTF-8.
    ///
    /// Defaulting to UTF-8 is deliberate: it is the only choice that can
    /// never corrupt a translation into a different script than the source
    /// game's. Only override this when targeting an engine build that has
    /// no Unicode-aware text renderer (RM2K/2003, XP, VX all render through
    /// the OS's legacy ANSI codepage rather than decoding UTF-8) *and* the
    /// translation stays within a script that codepage can represent - the
    /// player then also needs their system (or a locale emulator) set to
    /// that same codepage to see it correctly. See the "Text encoding"
    /// section of the crate documentation.
    ///
    /// # Parameters
    ///
    /// - `encoding` - Codepage to encode translated text with, or [`None`]
    ///   to always write UTF-8.
    pub fn set_write_encoding(&mut self, encoding: Option<&'static encoding_rs::Encoding>) {
        self.write_encoding = encoding;
    }

    /// Overrides which RPG Maker 2000/2003 build to target when re-serializing.
    ///
    /// Left at its default ([`rm2k::engine::Engine::R2K`]) unless the caller
    /// sets it - typically from the loaded database's `system.ldb_id`, via
    /// [`rm2k::engine::Engine::from_ldb_id`] - as [`crate::Processor::process`] does.
    pub fn set_rm2k_engine(&mut self, engine: rm2k::engine::Engine) {
        self.rm2k_engine = engine;
    }

    /// Decodes `bytes` as text, honoring [`Base::set_read_encoding`] if set.
    ///
    /// Falls back to trying a fixed list of common codepages (UTF-8, Windows-1252,
    /// Windows-1251, Shift-JIS, GB18030) in order and taking the first one that
    /// decodes without errors, when no override was set.
    pub(super) fn decode_with_fallback(&self, bytes: &[u8]) -> String {
        Self::decode_bytes_with(bytes, self.read_encoding)
    }

    /// The encoding logic behind [`Base::decode_with_fallback`], taking the
    /// override explicitly instead of reading it off `self` - for callers
    /// (namely [`Base::decode_scripts`]) that decode more than one blob per
    /// call and would otherwise have to thread `&self` through a static
    /// method just to reach one field.
    pub(super) fn decode_bytes_with(bytes: &[u8], encoding: Option<&'static encoding_rs::Encoding>) -> String {
        if let Some(encoding) = encoding {
            return encoding.decode(bytes).0.into_owned();
        }

        let encodings = [
            encoding_rs::UTF_8,
            encoding_rs::WINDOWS_1252,
            encoding_rs::WINDOWS_1251,
            encoding_rs::SHIFT_JIS,
            encoding_rs::GB18030,
        ];

        for encoding in encodings {
            let (decoded, _, had_errors) = encoding.decode(bytes);

            if !had_errors {
                return decoded.into_owned();
            }
        }

        // None of the fixed candidates decoded cleanly - fall back to UTF-8's
        // lossy replacement rather than silently dropping the text.
        encoding_rs::UTF_8.decode(bytes).0.into_owned()
    }

    /// Encodes `text` back into bytes using the codepage set through
    /// [`Base::set_write_encoding`], or plain UTF-8 if none was set.
    pub(super) fn encode_with_fallback(&self, text: &str) -> Vec<u8> {
        match self.write_encoding {
            Some(encoding) => encoding.encode(text).0.into_owned(),
            None => text.as_bytes().to_vec(),
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
                && self.ignore_entry().is_some_and(|entry| entry.contains(string.as_ref())))
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
        if !self.flags.intersects(BaseFlags::CreateIgnore | BaseFlags::Ignore) {
            return;
        }

        let key = Ignore::key(self.file_type, id, self.flags, self.duplicate_mode);

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

    /// Writes `text` into `value`'s own slot.
    ///
    /// If `literal` is set, writes it as a UTF-8 string - [`Value::set_string`]
    /// always tags a fresh Marshal write `E => true`, so this is unconditionally
    /// safe. Otherwise, `text` is encoded per [`Base::set_write_encoding`]
    /// (plain UTF-8 if none was set - deliberately *not* whatever encoding the
    /// source field happened to declare, since a translation is not generally
    /// representable in the source script's codepage - see
    /// [`Base::set_write_encoding`]'s docs).
    ///
    /// On VX Ace, the encoded bytes are still tagged with whichever encoding
    /// was actually used (rather than left as an untagged, implicitly
    /// `ASCII-8BIT` blob): Ruby 1.9's interpreter raises
    /// `Encoding::CompatibilityError` at runtime when a script concatenates an
    /// `ASCII-8BIT` string containing genuine non-ASCII bytes against a real
    /// `UTF-8` string (common in default RGSS3 scripts, e.g. level-up message
    /// interpolation) - tagging the field with its true encoding avoids that.
    /// XP/VX/RM2K have no such runtime enforcement, so they stay untagged,
    /// matching the source file's own shape.
    ///
    /// # Parameters
    ///
    /// - `value` - Cursor to write into.
    /// - `text` - Text to write.
    /// - `literal` - Whether to write `text` as a string or as bytes.
    pub(super) fn write_translated(&self, value: &mut Value<'_>, text: String, literal: bool) {
        if literal {
            value.set_string(text);
            return;
        }

        let encoded = self.encode_with_fallback(&text);

        if self.engine_type.is_vx_ace() {
            let name = self.write_encoding.map_or("UTF-8", |encoding| encoding.name());
            value.set_bytes_with_encoding(encoded, name.as_bytes());
        } else {
            value.set_bytes(encoded);
        }
    }

    /// Extracts string from a [`Value`] cursor.
    ///
    /// Will always return [`None`] if the cursor isn't a string or bytes value.
    ///
    /// # Parameters
    ///
    /// - `value` - Value from which string will be extracted.
    /// - `fail_if_empty` - Whether to return if extracted string happens to be empty.
    ///
    /// # Returns
    ///
    /// - Nothing if the value isn't string/bytes, or `fail_if_empty` is set and `string` is empty.
    /// - [`Cow<str>`] - Parsed string.
    ///
    /// A UTF-8 value borrows `value` rather than `self`, letting the caller drop
    /// the `&self` borrow immediately. A non-UTF-8 value is decoded into an
    /// owned string, in one of two ways:
    ///
    /// - VX Ace's Ruby 1.9+ Marshal format tags a `Str` with the `E`/`encoding`
    ///   ivar it actually declared ([`Value::declared_encoding`]) - trust that
    ///   over any guess. Used directly via [`encoding_rs::Encoding::for_label`]
    ///   if it resolves to a known encoding.
    /// - XP/VX's older Ruby 1.8 format never wrote that ivar at all (nothing to
    ///   probe), same as RM2K - decoded per [`Base::set_read_encoding`], or the
    ///   same fallback guess RM2K uses if none was set, via [`Base::decode_with_fallback`].
    pub(super) fn extract_string<'v>(&self, value: &'v Value<'_>, fail_if_empty: bool) -> Option<Cow<'v, str>> {
        let string = if let Some(s) = value.as_str() {
            Cow::Borrowed(s)
        } else {
            let bytes = value.as_byte_vec()?;

            let decoded = value
                .declared_encoding()
                .and_then(encoding_rs::Encoding::for_label)
                .map_or_else(|| self.decode_with_fallback(bytes), |encoding| encoding.decode(bytes).0.into_owned());

            Cow::Owned(decoded)
        };

        let trimmed = string.trim();

        if trimmed.is_empty() && fail_if_empty {
            return None;
        }

        Some(string)
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
    /// - `data` - [`RpgmData`] to serialize on write.
    ///
    /// # Returns
    ///
    /// - [`ProcessedData::RPGMData`] if `self.mode` is [`Mode::Write`].
    /// - [`ProcessedData::TranslationData`] otherwise.
    ///
    pub(super) fn finish(&mut self, data: RpgmData) -> ProcessedData {
        if self.mode.is_write() {
            let is_json = data.is_json();
            let bytes = data.into_bytes();

            ProcessedData::RPGMData(if is_json && self.file_type.is_plugins() {
                ["var $plugins =\n".as_bytes(), &bytes].concat()
            } else {
                bytes
            })
        } else {
            self.finish_translation()
        }
    }
}
