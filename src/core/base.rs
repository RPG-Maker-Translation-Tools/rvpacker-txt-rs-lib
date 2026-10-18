use super::{FlushedLines, IgnoreEntry};
use crate::{
    BaseFlags, Comments, IndexSetExt, ProcessedData, get_ignore_entry_comment, get_line_separator,
    marshal_compat::{RpgmData, Value},
    types::{
        DuplicateMode, EngineType, Error, HashMap, HashMapExt, HashSet, IgnoreMap, IndexMapExt, IndexMapGx, Labels,
        Lines, Mode, RPGMFileType, TranslationEntry, TranslationMap,
    },
};
use std::{borrow::Cow, fmt::Write as FmtWrite};

thread_local! {
    /// The first text that didn't decode since the last take, per thread. See [`Base::decode_text`].
    static DECODE_ERROR: std::cell::RefCell<Option<Error>> = const { std::cell::RefCell::new(None) };
}

/// Everything parsed out of a translation `.txt`.
///
/// Built once by [`Base::initialize_translation`] and read from there on; the only
/// mutation during processing is selecting the current section and draining it.
pub(super) struct Translation {
    pub(super) initialized: bool,

    pub(super) maps: IndexMapGx<u32, TranslationMap>,

    /// Index of the currently selected map in `maps`, or `usize::MAX` when none is
    /// selected. An index rather than a reference because the map it points at is
    /// owned by this same struct; `maps` is only ever appended to while a selection
    /// is live, so indices stay valid.
    pub(super) map_index: usize,

    /// Flattened `maps`, built once on write with [`DuplicateMode::Remove`].
    /// See [`Base::build_write_lookup`].
    pub(super) write_lookup: TranslationMap,

    pub(super) metadata: HashMap<u32, Comments>,
    pub(super) top_level_comments: HashMap<u32, Vec<String>>,

    /// The comments after each section's last entry, which no entry carries.
    pub(super) trailing_comments: HashMap<u32, Vec<String>>,

    /// Owns the raw text of the current translation `.txt` file, so that
    /// [`TranslationMap`] entries can borrow `&'static str` slices out of it instead
    /// of allocating a copy per parsed line.
    ///
    /// SAFETY invariant this crate must uphold: every `Cow::Borrowed` reachable from
    /// `maps`/`write_lookup` (and from [`Output::accumulated`]) is a slice of
    /// *this* `Box<str>` - a stable heap allocation that doesn't move when replaced -
    /// and every one of those borrowers is cleared by [`Translation::reset`] in the
    /// same call that replaces this field (see [`Base::initialize_translation`]),
    /// so a `'static`-transmuted borrow is never observed after its buffer is gone.
    pub(super) buffer: Option<Box<str>>,
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
            trailing_comments: HashMap::default(),
            buffer: None,
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
    /// System, Scripts and Plugins always keep their id, since each is a single section. `parse_ignore` must match.
    pub(super) fn key(file_type: RPGMFileType, id: u32, flags: BaseFlags, duplicate_mode: DuplicateMode) -> String {
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

    /// The entry for `key`, created if missing. Purging names its id explicitly, since the selection is the last one processed.
    pub(super) fn entry_for(&mut self, key: String) -> &mut IgnoreEntry {
        self.map.entry(key).or_default()
    }
}

impl Translation {
    pub(super) fn reset(&mut self) {
        self.initialized = false;
        self.maps.clear();
        self.map_index = usize::MAX;
        self.write_lookup.clear();
        self.metadata.clear();
        self.top_level_comments.clear();
        self.trailing_comments.clear();
        // Every borrower of the old buffer was just cleared above; drop the buffer
        // itself before `initialize_translation` installs the next one.
        self.buffer = None;
    }
}

/// Everything accumulated while processing, drained by [`Base::finish_translation`].
#[derive(Default)]
pub(super) struct Output {
    pub(super) lines: Lines,

    /// How much of `lines` has already been claimed by a flushed section, so the
    /// next flush can record the range it added.
    pub(super) total_length: usize,

    pub(super) accumulated: Vec<(u32, Comments, FlushedLines, TranslationMap)>,
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

/// The state every file kind shares: mode, flags, the parsed translation and the output being built.
///
/// Create one with [`Base::new`], then call the `process_*` methods. [`Processor`](crate::Processor) does this for you.
pub struct Base {
    /// The operation to run.
    pub mode: Mode,
    /// Ignore and purge options. See [`BaseFlags`].
    pub flags: BaseFlags,
    /// The engine the files belong to.
    pub engine_type: EngineType,
    /// What to do with repeated lines. Must match the read when writing or purging. [`Base::new`] sets [`DuplicateMode::Allow`].
    pub duplicate_mode: DuplicateMode,

    /// Map ids to leave untouched on read. Their current text is written back unchanged.
    pub skip_maps: HashSet<u32>,
    /// Per-file entry ids to leave untouched on read, keyed by file kind.
    pub skip_events: HashMap<RPGMFileType, HashSet<u32>>,
    /// Whether to record each event's id, name and coordinates ahead of its text.
    pub map_events: bool,

    /// Entries read from a `.rvpacker-ignore` file, or collected on purge.
    pub ignore: Ignore,

    pub(super) translation: Translation,
    pub(super) output: Output,

    /// `MapInfos` for the current run of maps, parsed once by
    /// [`Base::process_map`] and reused across the run.
    pub(super) mapinfos: Option<RpgmData>,

    /// `MapInfos` entries by map id, built by [`Base::begin_maps`] so lookups don't scan linearly.
    pub(super) mapinfo_ids: HashMap<u32, marshal_rs::arena::ValueId>,

    /// Title override set through [`Base::set_game_title`], for engines that keep
    /// it in `Game.ini` rather than the system file.
    pub(super) game_title: String,

    pub(super) file_type: RPGMFileType,
    pub(super) labels: Labels,

    /// Codepage for decoding untagged source text, set through [`Base::set_read_encoding`]. `None` means UTF-8.
    ///
    /// Kept separate from [`Base::write_encoding`]: the source codepage and the translation's codepage are rarely the same.
    pub(super) read_encoding: Option<&'static encoding_rs::Encoding>,

    /// Codepage for encoding translated text, set through [`Base::set_write_encoding`]. `None` writes UTF-8.
    pub(super) write_encoding: Option<&'static encoding_rs::Encoding>,

    /// RPG Maker 2000/2003 build to re-serialize for. Some fields exist on only one build.
    pub(super) rm2k_engine: rm2k::engine::Engine,
}

impl Default for Base {
    fn default() -> Self {
        Self {
            mode: Mode::read(),
            flags: BaseFlags::empty(),
            engine_type: EngineType::MVMZ,
            duplicate_mode: DuplicateMode::Allow,

            ignore: Ignore::default(),
            translation: Translation::default(),
            output: Output::default(),

            map_events: false,
            mapinfos: None,
            mapinfo_ids: HashMap::default(),
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

    /// Decodes untagged source text in `encoding` instead of UTF-8.
    ///
    /// Text that isn't valid in the codepage fails the file with [`Error::InvalidText`]. Independent of
    /// [`Base::set_write_encoding`].
    ///
    /// # Parameters
    ///
    /// - `encoding` - Codepage to decode text with, or [`None`] for UTF-8.
    pub const fn set_read_encoding(&mut self, encoding: Option<&'static encoding_rs::Encoding>) {
        self.read_encoding = encoding;
    }

    /// Forces *encoding* of translated text written back to the source format, instead of always writing UTF-8.
    ///
    /// XP and VX normally read UTF-8 (RGSS sets `$KCODE` to `"UTF8"`), though there can be exceptions. RM2K/2003 reads
    /// the OS's legacy codepage. Only override for a game that reads a legacy codepage, and only when the translation
    /// fits it. The player's system must then use the same codepage. See the "Text encoding" section of the crate
    /// documentation.
    ///
    /// # Parameters
    ///
    /// - `encoding` - Codepage to encode translated text with, or [`None`] to always write UTF-8.
    ///
    /// # Errors
    ///
    /// - [`Error::UnsupportedWriteEncoding`] - if `encoding` can't be encoded into, such as UTF-16.
    pub fn set_write_encoding(&mut self, encoding: Option<&'static encoding_rs::Encoding>) -> Result<(), Error> {
        Self::check_write_encoding(encoding)?;

        self.write_encoding = encoding;
        Ok(())
    }

    /// Fails with [`Error::UnsupportedWriteEncoding`] for an encoding that can't be encoded into: UTF-16 and the
    /// replacement encoding only decode, and `encode` writes UTF-8 for them.
    pub(crate) fn check_write_encoding(encoding: Option<&'static encoding_rs::Encoding>) -> Result<(), Error> {
        match encoding {
            Some(encoding) if encoding.output_encoding() != encoding => Err(Error::UnsupportedWriteEncoding {
                encoding: encoding.name(),
            }),
            _ => Ok(()),
        }
    }

    /// Encodes `text` in `encoding`, UTF-8 if [`None`], failing on any character it doesn't have.
    ///
    /// # Errors
    ///
    /// - [`Error::UnsupportedWriteEncoding`] - if `encoding` can't be encoded into.
    /// - [`Error::UnmappableText`] - if `text` has a character `encoding` can't represent.
    pub(crate) fn encode_bytes_with(
        text: &str,
        encoding: Option<&'static encoding_rs::Encoding>,
    ) -> Result<Vec<u8>, Error> {
        Self::check_write_encoding(encoding)?;

        let Some(encoding) = encoding else {
            return Ok(text.as_bytes().to_vec());
        };

        let (bytes, _, unmappable) = encoding.encode(text);

        if unmappable {
            return Err(Error::UnmappableText {
                encoding: encoding.name(),
                text: text.chars().take(64).collect(),
                file: String::new(),
            });
        }

        Ok(bytes.into_owned())
    }

    /// Overrides which RPG Maker 2000/2003 build to target when re-serializing.
    ///
    /// Left at its default ([`rm2k::engine::Engine::R2K`]) unless the caller
    /// sets it - typically from the loaded database's `system.ldb_id`, via
    /// [`rm2k::engine::Engine::from_ldb_id`] - as [`crate::Processor::process`] does.
    pub const fn set_rm2k_engine(&mut self, engine: rm2k::engine::Engine) {
        self.rm2k_engine = engine;
    }

    /// Whether Marshal text must be decoded in the read encoding, rather than moved out as it is on the grounds that its
    /// bytes happen to be valid UTF-8. Never for MV/MZ, whose text is JSON.
    pub(super) const fn reads_legacy_text(&self) -> bool {
        self.read_encoding.is_some() && !self.engine_type.is_mvmz()
    }

    /// Decodes untagged text with [`Base::set_read_encoding`]'s codepage, UTF-8 if none was set.
    ///
    /// Called deep inside traversals that can't return an error, so text that doesn't decode is recorded instead:
    /// the first such error fails the file being processed once it is done (see [`Base::take_decode_error`]), and
    /// the lossy text returned meanwhile is never written anywhere.
    pub(super) fn decode_text(&self, bytes: &[u8]) -> String {
        Self::decode_text_in(bytes, self.read_encoding)
    }

    /// [`Base::decode_text`] in a codepage of the caller's choosing.
    pub(super) fn decode_text_in(bytes: &[u8], encoding: Option<&'static encoding_rs::Encoding>) -> String {
        Self::decode_bytes_with(bytes, encoding).unwrap_or_else(|error| {
            // Only the first error is kept; later ones are usually the same problem.
            DECODE_ERROR.with_borrow_mut(|slot| {
                slot.get_or_insert(error);
            });
            encoding
                .unwrap_or(encoding_rs::UTF_8)
                .decode_without_bom_handling(bytes)
                .0
                .into_owned()
        })
    }

    /// Fails with the first text [`Base::decode_text`] couldn't decode since the last call.
    pub(super) fn take_decode_error() -> Result<(), Error> {
        Self::take_thread_decode_error().map_or(Ok(()), Err)
    }

    /// The first decode error recorded on this thread. A walk runs start to finish on one thread, so taking it at the
    /// end of the walk attributes the error to that walk's file even when several walks run at once.
    pub(super) fn take_thread_decode_error() -> Option<Error> {
        DECODE_ERROR.with_borrow_mut(Option::take)
    }

    /// Decodes `bytes` in `encoding`, UTF-8 if [`None`], failing on any byte sequence that isn't valid in it.
    ///
    /// # Errors
    ///
    /// - [`Error::InvalidText`] - if `bytes` isn't valid in `encoding`.
    pub(crate) fn decode_bytes_with(
        bytes: &[u8],
        encoding: Option<&'static encoding_rs::Encoding>,
    ) -> Result<String, Error> {
        let encoding = encoding.unwrap_or(encoding_rs::UTF_8);

        // Without BOM handling: a leading `FF FE` in a legacy string must not switch it to UTF-16.
        encoding
            .decode_without_bom_handling_and_without_replacement(bytes)
            .map(Cow::into_owned)
            .ok_or_else(|| Error::InvalidText {
                encoding: encoding.name(),
                text: encoding.decode_without_bom_handling(bytes).0.chars().take(64).collect(),
                file: String::new(),
            })
    }

    /// Encodes `text` back into bytes using the codepage set through
    /// [`Base::set_write_encoding`], or plain UTF-8 if none was set.
    pub(super) fn encode_text(&self, text: &str) -> Vec<u8> {
        match self.write_encoding {
            Some(encoding) => {
                let (bytes, _, unmappable) = encoding.encode(text);

                // Recorded like a decode error: this is called where an error can't be returned, and the file
                // fails once it is done, before anything of it is written.
                if unmappable {
                    DECODE_ERROR.with_borrow_mut(|slot| {
                        slot.get_or_insert_with(|| Error::UnmappableText {
                            encoding: encoding.name(),
                            text: text.chars().take(64).collect(),
                            file: String::new(),
                        });
                    });
                }

                bytes.into_owned()
            }
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
    /// Will skip inserting if `self.mode` is [`Mode::Write`], or `self.flags` contain [`BaseFlags::Ignore`] and `self.ignore_entry` contains the string.
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

        match string {
            // Checked first, so a repeated line isn't copied only to be dropped.
            Cow::Borrowed(borrowed) => {
                if !self.output.lines.contains(borrowed) {
                    self.output.lines.insert(borrowed.to_owned());
                }
            }
            Cow::Owned(owned) => {
                self.output.lines.insert(owned);
            }
        }
    }

    /// Gets ignore entry from `self.ignore.map` by `id`.
    ///
    /// Skips getting an entry if `self.flags` do not contain [`BaseFlags::Ignore`] or [`BaseFlags::CreateIgnore`].
    ///
    /// # Parameters
    ///
    /// - `id` - ID of the entry to get.
    ///
    pub(super) fn get_ignore_entry(&mut self, id: u32) {
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
    /// `literal` writes a UTF-8 string. Otherwise `text` is encoded per [`Base::set_write_encoding`], not the source
    /// field's declared encoding. On VX Ace the bytes are tagged with the encoding used, since Ruby 1.9 raises
    /// `Encoding::CompatibilityError` on an untagged non-ASCII blob concatenated with UTF-8.
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

        let encoded = self.encode_text(&text);

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
    /// A UTF-8 value borrows `value`. A non-UTF-8 value is decoded into an owned string: with the encoding the VX Ace
    /// `Str` declared ([`Value::declared_encoding`]) if it resolves, else per [`Base::decode_text`].
    pub(super) fn extract_string<'v>(&self, value: &'v Value<'_>, fail_if_empty: bool) -> Option<Cow<'v, str>> {
        // Untagged Marshal text (XP/VX) is in the read encoding even when its bytes happen to be valid UTF-8, so it only
        // takes the borrowing path without one. JSON has no raw bytes, and a tagged string declares its own encoding.
        let string = if let Some(s) = value.utf8_str()
            && (self.read_encoding.is_none() || value.as_raw_bytes().is_none() || value.declared_encoding().is_some())
        {
            Cow::Borrowed(s)
        } else {
            let bytes = value.as_raw_bytes()?;

            // A VX Ace string declares its own encoding; an untagged one is in the read encoding.
            let encoding = value
                .declared_encoding()
                .and_then(encoding_rs::Encoding::for_label)
                .or(self.read_encoding);

            Cow::Owned(Self::decode_text_in(bytes, encoding))
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
        self.lookup(self.selected_translation_map(), key)
    }

    /// The translation map [`Base::get_translation_map`] selected, if any.
    pub(super) fn selected_translation_map(&self) -> Option<&TranslationMap> {
        self.translation
            .maps
            .get_index(self.translation.map_index)
            .map(|(_, map)| map)
    }

    /// [`Base::get_key`] in `map` rather than the selected one, for walks that don't select.
    pub(super) fn lookup<'a>(&'a self, map: Option<&'a TranslationMap>, key: &str) -> Option<&'a TranslationEntry> {
        if self.duplicate_mode.is_allow() {
            map?.get(key)
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
            ProcessedData::RPGMData(data.into_bytes())
        } else {
            self.finish_translation()
        }
    }
}
