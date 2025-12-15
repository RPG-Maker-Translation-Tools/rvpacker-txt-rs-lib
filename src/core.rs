#[allow(unused_imports)]
use crate::types::ReadMode;
use crate::{
    BaseFlags, CommentPos, Comments, IndexSetExt, ProcessedData,
    constants::{
        AT_POSITION_MSG, COMMENT_PREFIX, COMMENT_SUFFIX,
        COULD_NOT_SPLIT_LINE_MSG, EVENT_ID_COMMENT, EVENT_NAME_COMMENT,
        EVENT_POS_COMMENT, ID_COMMENT, IGNORE_ENTRY_COMMENT, IN_FILE_MSG,
        INSTANCE_VAR_PREFIX, MAP_DISPLAY_NAME_COMMENT_PREFIX,
        MAP_ORDER_COMMENT, NAME_COMMENT, NEW_LINE, SEPARATOR, SYMBOLS,
    },
    types::{
        Code, DuplicateMode, EachLine, EngineType, Error, GameType,
        IgnoreEntry, IgnoreMap, IndexMapExt, IndexMapGx, Labels, Lines, Mode,
        RPGMFileType, Scripts, TranslationEntry, TranslationMap, Variable,
    },
};
use flate2::{Compression, read::ZlibDecoder, write::ZlibEncoder};
use gxhash::{HashMap, HashMapExt, HashSet, HashSetExt};
use indexmap::map::Entry;
use log::warn;
use marshal_rs::{Get, Value, ValueType, dump, load_binary, load_utf8};
use regex::Regex;
use serde_json::{Value as SerdeValue, from_str, to_vec};
use smallvec::{SmallVec, smallvec};
use std::{
    borrow::Cow,
    cell::LazyCell,
    fmt::Write as FmtWrite,
    fs::DirEntry,
    io::{Read, Write},
    mem::{take, transmute},
    ops::{ControlFlow, Range},
    path::Path,
};

const DISPLAY_NAME_POS: usize = CommentPos::DisplayName as usize;

macro_rules! mutable {
    ($var:expr, $t:ty) => {{
        #[allow(invalid_reference_casting)]
        unsafe {
            &mut *std::ptr::from_ref::<$t>($var).cast_mut()
        }
    }};
}

const BOM: &[u8] = &[0xEF, 0xBB, 0xBF];

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

thread_local! {
    static IS_INVALID_MULTILINE_VARIABLE_RE: LazyCell<Regex> =
        LazyCell::new(|| unsafe {
            Regex::new(r"^#? ?<.*>.?$|^[a-z]\d$").unwrap_unchecked()
        });
    static IS_INVALID_VARIABLE_RE: LazyCell<Regex> = LazyCell::new(|| unsafe {
        Regex::new(r"^[+-]?$|^///|---|restrict eval").unwrap_unchecked()
    });
    static PLUGINS_REGEXPS: LazyCell<[Regex; 11]> = LazyCell::new(|| unsafe {
        [
            Regex::new(r"^(name|description|Window Width|Window Height|ATTENTION!!!|Shown Elements|Width|Outline Color|Command Alignment|Command Position|Command Rows|Chinese Font|Korean Font|Default Font|Text Align|Scenes To Draw|displacementImage|Turn Alignment|Buff Formula|Counter Alignment|Default Width|Face Indent|Fast Forward Key|Font Name|Font Name CH|Font Name KR|Name Box Padding|Name Box Added Text|Critical Rate Formula|Critical Multplier Formula|Flat Critical Formula|Default SE|---List---|Button Events List|Kill Switch|Ex Turn Image|Ex Turn Name Color|Non Ex Turn Name Color|Option menu entry|Add to options|Default Ambient Light|Reset Lights|Gab Font Name|Escape Ratio|Translated Format|Default Sound|Action Speed|Default System|Untranslated Format|Default Format|Victory Screen Level Sound|Warning Side Battle UI|Weapon Swap Text Hit|Weapon Swap Text Critical|Weapon Swap Command|Weapon Swap Text Evasion|alwaysDash|renderingMode|Attributes Command|Attributes Column 1|Attributes Column 2|Attributes Column 3|Warning OTB|</span> Minimum Damage</span></td>|Present Settings)$").unwrap_unchecked(),
            Regex::new(r"^Folder.*\w$").unwrap_unchecked(),
            Regex::new(r"[XY]$").unwrap_unchecked(),
            Regex::new(r"BGM").unwrap_unchecked(),
            Regex::new(r"Label").unwrap_unchecked(),
            Regex::new(r"^Custom \w").unwrap_unchecked(),
            Regex::new(r"^outlineColor").unwrap_unchecked(),
            Regex::new(r"^(Menu|Item|Skill|Equip|Status|Save|Options|End).*(Background|Motion)$").unwrap_unchecked(),
            Regex::new(r"^Menu \w").unwrap_unchecked(),
            Regex::new(r"^(MHP|MMP|ATK|DEF|MAT|MDF|AGI|LUK).*(Formula|Maximum|Minimum|Effect|Color)$").unwrap_unchecked(),
            Regex::new(r"^Damage\w*$").unwrap_unchecked(),
        ]
    });
    static IS_ONLY_SYMBOLS_RE: LazyCell<Regex> = LazyCell::new(|| unsafe {
        Regex::new(r#"^[,.()+\-:;\[\]^~%&!№$@`*\/→×？?ｘ％▼|♥♪！：〜『』「」〽。…‥＝゠、，【】［］｛｝（）〔〕｟｠〘〙〈〉《》・\\#<>=_ー※▶ⅠⅰⅡⅱⅢⅲⅣⅳⅤⅴⅥⅵⅦⅶⅧⅷⅨⅸⅩⅹⅪⅺⅫⅻⅬⅼⅭⅽⅮⅾⅯⅿ\s\d"']+$"#).unwrap_unchecked()
    });
    static LINE_BREAKS_RE: LazyCell<Regex> = LazyCell::new(|| unsafe {
        Regex::new(r"\r|\n|\r\n").unwrap_unchecked()
    });
    static NEW_LINE_RE: LazyCell<Regex> = LazyCell::new(|| unsafe {
        Regex::new(r"\\#").unwrap_unchecked()
    });
}

trait CustomReplace {
    /// Normalizes RPG Maker line break symbols (`\n`, `\r`, `\r\n`) to the format that the library uses (`\#`).
    fn normalize(&self) -> Cow<'_, str>;

    /// Denormalizes library line break symbols to the format that RPG Maker uses (`\n`).
    fn denormalize(&self) -> Cow<'_, str>;
}

impl CustomReplace for str {
    fn normalize(&self) -> Cow<'_, str> {
        LINE_BREAKS_RE.with(|re| re.replace_all(self, NEW_LINE))
    }

    fn denormalize(&self) -> Cow<'_, str> {
        NEW_LINE_RE.with(|re| re.replace_all(self, "\n"))
    }
}

/// Filters entries of [`std::fs::ReadDir`] and returns iterator of only `Map` entries.
///
/// # Parameters
///
/// - `entries` - Entries read with [`std::fs::read_dir`].
/// - `engine_extension` - [`&str`] corresponding to the extension of read entries.
///
/// # Returns
///
/// Filtered iterator containing only `Map` entries.
///
pub fn filter_maps<'a>(
    entries: impl Iterator<Item = &'a DirEntry>,
    engine_extension: &'a str,
) -> impl Iterator<Item = &'a DirEntry> {
    entries.filter_map(move |entry| {
        if !entry.file_type().ok()?.is_file() {
            return None;
        }

        let filename = entry.file_name();
        let extension = Path::new(&filename).extension()?;
        let filename_str = filename.to_str()?;

        if filename_str.starts_with("Map")
            && filename_str.as_bytes().get(3)?.is_ascii_digit()
            && extension == engine_extension
        {
            return Some(entry);
        }

        None
    })
}

/// Filters entries of [`std::fs::ReadDir`] and returns iterator of only other entries.
///
/// # Parameters
///
/// - `entries` - Entries read with [`std::fs::read_dir`].
/// - `engine_extension` - [`&str`] corresponding to the extension of read entries.
/// - `game_type` - [`GameType`] of entries.
///
/// # Returns
///
/// Filtered iterator containing only other entries.
///
pub fn filter_other<'a>(
    entries: impl Iterator<Item = &'a DirEntry>,
    engine_extension: &'a str,
    game_type: GameType,
) -> impl Iterator<Item = &'a DirEntry> {
    entries.filter_map(move |entry| {
        if !entry.file_type().ok()?.is_file() {
            return None;
        }

        let filename = entry.file_name();
        let filename_path = Path::new(&filename);
        let basename = filename_path
            .file_stem()
            .and_then(|basename| basename.to_str())?;
        let extension = filename_path.extension()?;

        let file_type = RPGMFileType::from_filename(basename);

        if extension == engine_extension && file_type.is_other() {
            if game_type.is_termina() && file_type.is_states() {
                return None;
            }

            return Some(entry);
        }

        None
    })
}

/// Returns the RPG Maker file extension that corresponds to given [`EngineType`].
#[must_use]
pub const fn get_engine_extension(engine_type: EngineType) -> &'static str {
    match engine_type {
        EngineType::New => "json",
        EngineType::VXAce => "rvdata2",
        EngineType::VX => "rvdata",
        EngineType::XP => "rxdata",
    }
}

/// Parses ignore file contents to [`IgnoreMap`].
///
/// # Parameters
///
/// - `ignore_file_path` - Path to the `.rvpacker-ignore` file.
/// - `duplicate_mode` - [`DuplicateMode`], which was used during read.
/// - `read` - Parse for reading or purging.
///
/// # Returns
///
/// Parsed [`IgnoreMap`].
///
#[must_use]
pub fn parse_ignore(
    ignore_file_content: &str,
    duplicate_mode: DuplicateMode,
    read: bool,
) -> IgnoreMap {
    let mut ignore_map = IgnoreMap::default();
    let mut ignore_file_lines = ignore_file_content.lines();

    let Some(mut first_entry_comment) = ignore_file_lines.next() else {
        return ignore_map;
    };

    if read
        && duplicate_mode.is_remove()
        && !(first_entry_comment.contains("<#>System")
            || first_entry_comment.contains("<#>Scripts")
            || first_entry_comment.contains("<#>Plugins"))
    {
        // If duplicates are removed, we should group all ignore entries
        // that correspond to a single file into one ignore entry.
        first_entry_comment = &first_entry_comment
            [..unsafe { first_entry_comment.find(':').unwrap_unchecked() }];
    }

    ignore_map.reserve_exact(256);
    ignore_map.insert(
        first_entry_comment.to_string(),
        IgnoreEntry::with_capacity(128),
    );

    let mut ignore_entry =
        unsafe { ignore_map.last_mut().unwrap_unchecked().1 };

    for mut line in ignore_file_lines.filter(|line| !line.is_empty()) {
        if let Some(mid) = line.strip_prefix(IGNORE_ENTRY_COMMENT) {
            // If duplicates are allowed, we should group all ignore entries
            // that correspond to a single file into one ignore entry.
            if read
                && duplicate_mode.is_remove()
                && !(mid.starts_with("<#>System")
                    || mid.starts_with("<#>Scripts")
                    || mid.starts_with("<#>Plugins"))
            {
                line = &mid[..unsafe { mid.find(':').unwrap_unchecked() }];
            }

            ignore_map
                .entry(line.into())
                .or_insert(IgnoreEntry::with_capacity(128));
            ignore_entry =
                unsafe { ignore_map.last_mut().unwrap_unchecked().1 };
        } else {
            ignore_entry.insert(line.into());
        }
    }

    ignore_map
}

/// Extracts the game title from a `Game.ini` file's content.
///
/// # Parameters
///
/// - `ini_file_content` - raw byte content of the INI file to parse.
///
/// # Returns
///
/// - [`Vec<u8>`] - vector of extracted title's bytes on success. Title may not be UTF-8.
/// - [`Error`] - otherwise.
///
/// # Errors
///
/// - [`Error::NoTitle`] - if no "Title" entry is found in the INI file.
///
/// # Example
///
/// ```no_run
/// use rvpacker_txt_rs_lib::{get_ini_title, Error};
/// use std::fs::read;
///
/// fn main() -> Result<(), Box<dyn std::error::Error>> {
///     let ini_content = read("C:/Game/Game.ini")?;
///     let title = get_ini_title(&ini_content)?;
///     Ok(())
/// }
/// ```
pub fn get_ini_title(ini_file_content: &[u8]) -> Result<Vec<u8>, Error> {
    fn trim_bytes(bytes: &[u8]) -> &[u8] {
        let start = bytes.iter().position(|&b| !is_space(b)).unwrap_or(0);
        let end = bytes
            .iter()
            .rposition(|&b| !is_space(b))
            .map_or(0, |i| i + 1);
        &bytes[start..end]
    }

    fn is_space(b: u8) -> bool {
        b == b' ' || b == b'\t' || b == b'\r'
    }

    fn split_lines(data: &[u8]) -> SmallVec<[&[u8]; 4]> {
        let mut lines = SmallVec::with_capacity(4);
        let mut start = 0;
        let mut i = 0;

        while i < data.len() {
            if data[i] == b'\n' {
                lines.push(&data[start..i]);
                i += 1;
                start = i;
            } else if data[i] == b'\r' {
                lines.push(&data[start..i]);

                if data.get(i + 1).is_some_and(|ch| *ch == b'\n') {
                    i += 2;
                } else {
                    i += 1;
                }

                start = i;
            } else {
                i += 1;
            }
        }

        if start < data.len() {
            lines.push(&data[start..]);
        }

        lines
    }

    for line in split_lines(ini_file_content) {
        if line.starts_with(b"Title") {
            if let Some(pos) = line.iter().position(|&b| b == b'=') {
                let right = &line[pos + 1..];
                let trimmed = trim_bytes(right);
                return Ok(trimmed.to_vec());
            }
        }
    }

    Err(Error::NoTitle)
}

/// Extracts the game title from a `System.json` file's content.
///
/// # Parameters
///
/// - `system_file_content` - JSON string content of the system file
///
/// # Returns
///
/// - [`String`] game title extracted from the "gameTitle" field if successful.
/// - [`Error`] otherwise.
///
/// # Errors
///
/// - [`Error::JsonParse`] - if parsing `system_file_content` failed.
/// - [`Error::NoTitle`] - if the parsed JSON doesn't contain "gameTitle" key.
///
/// # Example
///
/// ```no_run
/// use rvpacker_txt_rs_lib::{get_system_title, Error};
/// use std::fs::read_to_string;
///
/// fn main() -> Result<(), Box<dyn std::error::Error>> {
///     let system_file_content = read_to_string("C:/Game/www/data/System.json")?;
///     let title = get_system_title(&system_file_content)?;
///     Ok(())
/// }
/// ```
pub fn get_system_title(
    mut system_file_content: &str,
) -> Result<String, Error> {
    // MZ includes Byte Order Mark in files.
    if system_file_content.as_bytes().starts_with(BOM) {
        system_file_content = &system_file_content[BOM.len()..];
    }

    let system_file_value: SerdeValue = from_str(system_file_content)?;

    system_file_value["gameTitle"]
        .as_str()
        .map(Into::into)
        .ok_or(Error::NoTitle)
}

pub struct Base {
    pub mode: Mode,
    pub flags: BaseFlags,
    pub game_type: GameType,
    pub engine_type: EngineType,
    pub duplicate_mode: DuplicateMode,

    pub skip_maps: HashSet<u16>,
    pub skip_events: HashMap<RPGMFileType, HashSet<u16>>,
    pub map_events: bool,

    pub ignore_map: IgnoreMap,
    ignore_entry: &'static mut IgnoreEntry,

    translation_initialized: bool,

    lines: Lines,
    total_length: usize,

    metadata: HashMap<u16, Comments>,

    translation_maps: IndexMapGx<u16, TranslationMap>,
    leaked_translation_map_reference: &'static mut TranslationMap,
    translation_map: &'static mut TranslationMap,

    accumulated_translation:
        Vec<(u16, Comments, Vec<Cow<'static, str>>, TranslationMap)>,

    file_type: RPGMFileType,
    labels: Labels,
}

impl Default for Base {
    fn default() -> Self {
        let leaked_translation_map =
            Box::leak(Box::new(TranslationMap::default()));

        Self {
            mode: Mode::Read(ReadMode::Default(false)),
            flags: BaseFlags::empty(),
            game_type: GameType::None,
            engine_type: EngineType::New,
            duplicate_mode: DuplicateMode::Remove,

            ignore_map: IgnoreMap::default(),
            ignore_entry: unsafe { &mut *(16 as *mut IgnoreEntry) },

            translation_initialized: false,

            lines: Lines::default(),
            total_length: 0,

            metadata: HashMap::default(),

            translation_map: mutable!(leaked_translation_map, TranslationMap),
            leaked_translation_map_reference: leaked_translation_map,
            translation_maps: IndexMapGx::default(),

            accumulated_translation: Vec::new(),

            map_events: false,
            file_type: RPGMFileType::Invalid,
            labels: Labels::default(),

            skip_maps: HashSet::default(),
            skip_events: HashMap::default(),
        }
    }
}

impl<'a> Base {
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
            lines: Lines::with_capacity(512),

            metadata: HashMap::with_capacity(1024),
            translation_map: Box::leak(Box::new(
                TranslationMap::with_capacity(512),
            )),
            translation_maps: IndexMapGx::with_capacity(1024),

            // SAFETY: If `flags` contain neither `BaseFlags::Ignore` or `BaseFlags::CreateIgnore`, this entry is simply unused.
            // Also we're dereferncing from 16 because Rust fucking prevents null dereferencing in debug mode (who asked for this?)
            ignore_entry: unsafe { &mut *(16 as *mut IgnoreEntry) },

            ..Default::default()
        }
    }

    /// Clears all the underlying collections, and makes this base ready to be used in the next base.
    ///
    /// This function is used by file-specific bases' constructors, so you generally mustn't call it manually.
    pub fn reset(&mut self) {
        self.translation_initialized = false;

        self.lines.clear();
        self.total_length = 0;

        self.metadata.clear();

        self.reset_translation_map();
        self.translation_maps.clear();
        self.accumulated_translation.clear();
    }

    pub fn reset_translation_map(&mut self) {
        self.translation_map =
            mutable!(self.leaked_translation_map_reference, TranslationMap);
    }

    fn process_parameter(
        &mut self,
        code: Code,
        mut parameter: &str,
    ) -> Option<String> {
        if Self::string_is_only_symbols(parameter) {
            return None;
        }

        let mut extra_strings: SmallVec<[(&str, bool); 4]> =
            SmallVec::with_capacity(4);

        match self.game_type {
            GameType::Termina => {
                if parameter.chars().all(|c| {
                    c.is_ascii_lowercase()
                        || (c.is_ascii_punctuation() && c != '"')
                }) {
                    return None;
                }

                if code.is_system()
                    && !parameter.starts_with("Gab")
                    && (!parameter.starts_with("choice_text")
                        || parameter.ends_with("????"))
                {
                    return None;
                }
            }
            GameType::LisaRPG => {
                if code.is_any_dialogue() {
                    if let Some(i) = Self::find_lisa_prefix_index(parameter) {
                        if Self::string_is_only_symbols(&parameter[i..]) {
                            return None;
                        }

                        if self.mode.is_write() {
                            extra_strings.push((&parameter[..i], false));
                        }

                        if !parameter.starts_with(r"\et") {
                            parameter = &parameter[i..];
                        }
                    }
                }
            }
            // custom processing for other games
            GameType::None => {}
        }

        if !self.engine_type.is_new() {
            if let Some(i) = Self::ends_with_if_index(parameter) {
                if self.mode.is_write() {
                    extra_strings.push((&parameter[..i], true));
                }

                parameter = &parameter[..i];
            }

            if code.is_shop() {
                if !parameter.contains("shop_talk") {
                    return None;
                }

                // SAFETY: At this point, shop parameter should always contain '='.
                let (_, mut actual_string) =
                    unsafe { parameter.split_once('=').unwrap_unchecked() };
                actual_string = actual_string.trim();

                if actual_string.len() < 2 {
                    return None;
                }

                let without_quotes = &actual_string[1..actual_string.len() - 1];

                if without_quotes.is_empty()
                    || Self::string_is_only_symbols(without_quotes)
                {
                    return None;
                }

                parameter = without_quotes;
            }
        }

        if self.mode.is_write() {
            self.get_key(parameter).map(|t| {
                let mut translation = String::new();

                for (string, append) in extra_strings {
                    if append {
                        translation = t.to_string() + string;
                    } else {
                        translation = format!("{string}{t}", t = t.translation);
                    }
                }

                translation
            })
        } else {
            Some(if self.flags.contains(BaseFlags::Romanize) {
                Self::romanize_string(parameter).into_owned()
            } else {
                parameter.to_string()
            })
        }
    }

    fn process_param(
        &mut self,
        value: &mut Value,
        code: Code,
        parameter: &str,
    ) {
        let parameter = if self.flags.contains(BaseFlags::Romanize) {
            Self::romanize_string(parameter)
        } else {
            Cow::Borrowed(parameter)
        };

        let Some(mut parsed) = self.process_parameter(code, &parameter) else {
            return;
        };

        if self.mode.is_write() {
            if code.is_shop() {
                if let Some((left, _)) = parameter.split_once('=') {
                    parsed = format!("{left}=\"{parsed}\"");
                }
            }

            *value =
                Self::make_string_value(&parsed, self.engine_type.is_new());
        } else {
            self.insert_string(Cow::Owned(parsed));
        }
    }

    /// Inserts `string` to `self.lines` if `self.mode`.
    ///
    /// Will skip inserting if `self.mode` is not [`Mode::Write`] or `self.flags` contain [`BaseFlags::Ignore`] and `self.ignore_entry` contains the string.
    ///
    /// # Parameters
    ///
    /// - `string` - String to insert in `self.lines`.
    ///
    fn insert_string(&mut self, string: Cow<'_, str>) {
        if self.mode.is_write()
            || (self.flags.contains(BaseFlags::Ignore)
                && self.ignore_entry.contains(string.as_ref()))
        {
            return;
        }

        self.lines.insert(string.into_owned());
    }

    fn join_dialogue_lines(
        &mut self,
        list: &mut [Value],
        dialogue_lines: &mut SmallVec<[String; 4]>,
        dialogue_line_indices: &mut SmallVec<[usize; 4]>,
        write_string_literally: bool,
    ) {
        let mut joined =
            Cow::Owned(dialogue_lines.join(if self.mode.is_write() {
                "\n"
            } else {
                NEW_LINE
            }));

        if self.mode.is_write() {
            let old_joined = take(&mut joined);

            if self.flags.contains(BaseFlags::Romanize) {
                joined = Self::romanize_string(&old_joined);
            } else {
                joined = old_joined;
            }

            let Some(translation) =
                self.process_parameter(Code::Dialogue, &joined)
            else {
                return;
            };

            let translation_lines: Vec<&str> = translation.lines().collect();
            let split_line_count = translation_lines.len();
            let dialogue_line_count = dialogue_lines.len();

            for (i, &index) in dialogue_line_indices.iter().enumerate() {
                list[index][self.labels.parameters][0] = if i < split_line_count
                {
                    Self::make_string_value(
                        translation_lines[i],
                        write_string_literally,
                    )
                } else {
                    // Overwrite leftover source text
                    Value::string(" ")
                }
            }

            if split_line_count > dialogue_line_count {
                let remaining =
                    translation_lines[dialogue_line_count - 1..].join("\n");

                // SAFETY: We checked that `dialogue_lines` are not empty before calling this.
                list[unsafe {
                    *dialogue_line_indices.last().unwrap_unchecked()
                }][self.labels.parameters][0] = Value::string(remaining);
            }
        } else {
            self.process_param(&mut Value::default(), Code::Dialogue, &joined);
        }
    }

    /// Processes the list of objects found in `Map`, `CommonEvents` and `Troops` files.
    ///
    /// # Parameters
    ///
    /// - `list` - list of [`Value`]s.
    ///
    fn process_list(&mut self, list: &mut Vec<Value>) {
        let mut in_sequence = false;
        let mut write_string_literally = self.engine_type.is_new();
        let mut dialogue_lines = SmallVec::with_capacity(4);
        let mut dialogue_line_indices = SmallVec::with_capacity(4);

        for (item_idx, item) in
            mutable!(list, Vec<Value>).iter_mut().enumerate()
        {
            // SAFETY: Each item must contain code.
            let code = Code::from(unsafe {
                item[self.labels.code].as_int().unwrap_unchecked()
            } as u16);

            let code = if code.is_dialogue_start() && !self.engine_type.is_xp()
            {
                Code::Bad
            } else {
                code
            };

            if self.mode.is_write() && !self.engine_type.is_new() {
                // SAFETY: Each item must contain parameters.
                let parameters = unsafe {
                    item[self.labels.parameters].as_array().unwrap_unchecked()
                };

                if !parameters.is_empty() {
                    write_string_literally = !match code {
                        Code::ChoiceArray => parameters[0][0].is_bytes(),
                        Code::Misc1 | Code::Misc2 | Code::Choice => {
                            parameters[1].is_bytes()
                        }
                        _ => parameters[0].is_bytes(),
                    }
                }
            }

            if in_sequence
                && (!self.engine_type.is_xp() && !code.is_any_dialogue())
                || (code.is_dialogue_start() && !dialogue_lines.is_empty())
            {
                if !dialogue_lines.is_empty() {
                    self.join_dialogue_lines(
                        list,
                        &mut dialogue_lines,
                        &mut dialogue_line_indices,
                        write_string_literally,
                    );
                    dialogue_lines.clear();
                    dialogue_line_indices.clear();
                }

                in_sequence = false;
            }

            if code.is_bad() {
                continue;
            }

            // SAFETY: Each item must contain parameters.
            let parameters = unsafe {
                item[self.labels.parameters]
                    .as_array_mut()
                    .unwrap_unchecked()
            };

            if parameters.is_empty() {
                continue;
            }

            let value_index =
                usize::from(code.is_any_misc() || code.is_choice());

            let value = &mut parameters[value_index];

            if code.is_choice_array() {
                // SAFETY: We have just checked - it's an array.
                for value in unsafe { value.as_array_mut().unwrap_unchecked() }
                {
                    let Some(string) = mutable!(self, Self)
                        .extract_string(mutable!(value, Value), true)
                    else {
                        continue;
                    };

                    self.process_param(value, code, string);
                }
            } else {
                let Some(parameter_string) = mutable!(self, Self)
                    .extract_string(mutable!(value, Value), false)
                else {
                    continue;
                };

                if !code.is_credit() && parameter_string.is_empty() {
                    continue;
                }

                if code.is_any_dialogue() {
                    dialogue_lines.push(parameter_string.into());

                    if self.mode.is_write() {
                        dialogue_line_indices.push(item_idx);
                    }

                    in_sequence = true;
                } else {
                    self.process_param(value, code, parameter_string);
                }
            }
        }
    }

    /// Gets ignore entry from `self.ignore_map` by `id`.
    ///
    /// Skips getting an entry if `self.flags` do not contain [`BaseFlags::Ignore`] or [`BaseFlags::CreateIgnore`].
    ///
    /// # Parameters
    ///
    /// - `id` - ID of the entry to get.
    ///
    fn get_ignore_entry(&mut self, id: u16) {
        if self
            .flags
            .intersects(BaseFlags::CreateIgnore | BaseFlags::Ignore)
        {
            let mut entry_name: &str =
                &format!("{file}: {id}", file = self.file_type);

            if self.flags.contains(BaseFlags::Ignore)
                && self.duplicate_mode.is_remove()
            {
                entry_name = &entry_name
                    [..unsafe { entry_name.find(':').unwrap_unchecked() }];
            }

            // SAFETY: We're bypassing lifetime and ownership rules here, converting `&'a IgnoreEntry` to `&'static IgnoreEntry`
            // Because compiler doesn't understand that the access to `self.ignore_entry` is optional based on `self.flags`.
            let static_entry = mutable!(
                self.ignore_map
                    .entry(format!(
                        "{IGNORE_ENTRY_COMMENT}{SEPARATOR}{entry_name}"
                    ))
                    .or_default(),
                IgnoreEntry
            );

            self.ignore_entry = static_entry;
        }
    }

    /// Parses RPG Maker file from passed content.
    ///
    /// This function determines how to parse the content by `self.engine_type`, and assumes it always set correctly.
    ///
    /// # Parameters
    ///
    /// - `content` - Content of file to parse.
    ///
    /// # Returns
    ///
    /// - [`Value`] - if file was parsed successfully.
    /// - [`Error`] - if unable to deserialize the file.
    ///
    /// # Errors
    ///
    /// - [`Error::MarshalLoad`] - if unable to load the Marshal data.
    /// - [`Error::JsonParse`] - if unable to parse the JSON data.
    ///
    fn parse_rpgm_file(&mut self, mut content: &[u8]) -> Result<Value, Error> {
        if self.engine_type.is_new() {
            // MZ includes Byte Order Mark in files.
            if content.starts_with(BOM) {
                content = &content[3..];
            }

            // SAFETY: JSON is always valid UTF-8.
            let parsed = from_str::<SerdeValue>(unsafe {
                std::str::from_utf8_unchecked(content)
            })?;

            Ok(Value::from(parsed))
        } else {
            let loaded = if self.file_type.is_scripts() {
                load_binary(content, INSTANCE_VAR_PREFIX)
            } else {
                load_utf8(content, INSTANCE_VAR_PREFIX)
            }?;

            Ok(loaded)
        }
    }

    /// Initializes translation by filling `self.translation_maps` with parsed maps from `translation`.
    ///
    /// # Parameters
    ///
    /// - `translation` - translation file content to parse.
    ///
    fn initialize_translation(
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

        self.translation_maps.clear();

        let mut translation_lines = translation.lines().enumerate().peekable();

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

                    self.translation_map
                        .insert(source.into(), translation.into());
                } else {
                    panic!(
                        "items.txt in Fear & Hunger 2: Termina should start with 4 `Menu Category` entries."
                    );
                }
            }

            self.translation_maps
                .insert(u16::MAX, self.translation_map.drain(..).collect());
        }

        loop {
            let Some((_, next)) = translation_lines.next() else {
                // Translation drained
                break;
            };

            let mut comments: Comments = smallvec![String::new(); 3];

            let id = next
                .strip_prefix(ID_COMMENT)
                .and_then(|n| n.strip_prefix(SEPARATOR))
                .and_then(|n| n.trim_end().parse::<u16>().ok())
                .unwrap();

            let mut first = true;

            while let Some((_, line)) = translation_lines.peek() {
                if line.starts_with(ID_COMMENT) {
                    break;
                }

                // SAFETY: We just peeked at this item.
                let (i, line) =
                    unsafe { translation_lines.next().unwrap_unchecked() };

                if line.starts_with(COMMENT_PREFIX) {
                    if [EVENT_ID_COMMENT, EVENT_NAME_COMMENT, EVENT_POS_COMMENT]
                        .into_iter()
                        .any(|c| line.starts_with(c))
                    {
                        continue;
                    }

                    if first {
                        let pos = CommentPos::from_str(line);

                        // TODO: Handle top-level comments that don't correspond to required here
                        if pos == CommentPos::None {
                            continue;
                        }

                        if pos == CommentPos::DisplayName {
                            let suffix_pos =
                                line.rfind(COMMENT_SUFFIX).unwrap();
                            let prefix_len =
                                MAP_DISPLAY_NAME_COMMENT_PREFIX.len();
                            let source = &line[prefix_len..suffix_pos];
                            let translation =
                                line.rsplit_once(SEPARATOR).unwrap().1;
                            comments[pos as usize] =
                                format!("{source}{SEPARATOR}{translation}");
                        } else {
                            comments[pos as usize] = line
                                .split_once(SEPARATOR)
                                .unwrap()
                                .1
                                .to_string();
                        }
                    } else {
                        comments.push(line.to_string());
                    }

                    continue;
                }

                // This split is essentially free, since we're not cloning to String
                let split: Vec<&str> = line.split(SEPARATOR).collect();

                if split.len() < 2 {
                    warn!(
                        "{COULD_NOT_SPLIT_LINE_MSG}\n{AT_POSITION_MSG}: {i}\n{IN_FILE_MSG}: {file}.txt",
                        i = i + 1,
                        file = self.file_type.to_string().to_lowercase()
                    );
                    comments.clear();
                    continue;
                }

                // SAFETY: We just checked for split length.
                let source =
                    Cow::Borrowed(*unsafe { split.first().unwrap_unchecked() });

                let translation = Cow::Borrowed(
                    split
                        .into_iter()
                        .skip(1)
                        .rfind(|x| !x.is_empty())
                        .unwrap_or_default(),
                );

                let (source, translation) = if trim {
                    (
                        Cow::Borrowed(source.trim()),
                        Cow::Borrowed(translation.trim()),
                    )
                } else {
                    (source, translation)
                };

                let (source, translation) = if self.mode.is_write() {
                    // Discard lines with empty translation, those are unused on write
                    if translation.is_empty() {
                        continue;
                    }

                    (source.denormalize(), translation.denormalize())
                } else {
                    (source, translation)
                };

                if first {
                    self.metadata.insert(id, comments.drain(..).collect());
                    first = false;
                }

                self.translation_map.insert(
                    source.into(),
                    TranslationEntry {
                        comments: comments.drain(..).collect(),
                        translation: translation.into(),
                    },
                );
            }

            if self.translation_map.is_empty() {
                let metadata_entry = self
                    .metadata
                    .entry(id)
                    .or_insert(comments.drain(..).collect());

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
                .insert(id, self.translation_map.drain(..).collect());
        }

        Ok(())
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
    fn get_translation_map(&mut self, id: u16) -> ControlFlow<()> {
        let entry = self.translation_maps.entry(id);

        // Move a map from `translation_maps` to `translation_map`.
        self.translation_map = mutable!(
            match entry {
                Entry::Occupied(entry) => entry.into_mut(),
                Entry::Vacant(entry) => {
                    if self.mode.is_write() {
                        return ControlFlow::Break(());
                    }

                    entry.insert(TranslationMap::with_capacity(512))
                }
            },
            TranslationMap
        );

        if self
            .skip_events
            .get(&self.file_type)
            .is_some_and(|x| x.contains(&id))
            || (self.file_type.is_map() && self.skip_maps.contains(&id))
        {
            if self.mode.is_append() || self.mode.is_purge() {
                let metadata = self.get_metadata(id);

                self.accumulated_translation.push((
                    id,
                    metadata,
                    Vec::new(),
                    take(self.translation_map),
                ));
            }

            self.total_length += self.lines.len();
            return ControlFlow::Break(());
        }

        self.get_ignore_entry(id);

        if self.mode.is_purge() {
            self.flush_translation(id);
            return ControlFlow::Break(());
        }

        ControlFlow::Continue(())
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
    fn make_string_value(string: &str, literal: bool) -> Value {
        if literal {
            Value::string(string)
        } else {
            Value::bytes(string.as_bytes())
        }
    }

    /// Replaces Eastern symbols in string to their Western (or sort of Western) equivalents.
    ///
    /// # Parameters
    ///
    /// - `string` - String to romanize.
    ///
    /// # Returns
    ///
    /// - [`Cow<str>`] - as owned if replacements occured, as borrowed otherwise.
    ///
    fn romanize_string(string: &str) -> Cow<'_, str> {
        let mut result: Option<String> = None;

        for (i, char) in string.chars().enumerate() {
            let replacement = match char {
                '。' => ".",
                '、' | '，' => ",",
                '・' | '※' => "·",
                '゠' => "–",
                '＝' | 'ー' => "—",
                '「' | '」' | '〈' | '〉' => "'",
                '『' | '』' | '《' | '》' => "\"",
                '（' | '〔' | '｟' | '〘' => "(",
                '）' | '〕' | '｠' | '〙' => ")",
                '｛' => "{",
                '｝' => "}",
                '［' | '【' | '〖' | '〚' => "[",
                '］' | '】' | '〗' | '〛' => "]",
                '〜' => "~",
                '？' => "?",
                '！' => "!",
                '：' => ":",
                '…' | '‥' => "...",
                '　' => " ",
                'Ⅰ' => "I",
                'ⅰ' => "i",
                'Ⅱ' => "II",
                'ⅱ' => "ii",
                'Ⅲ' => "III",
                'ⅲ' => "iii",
                'Ⅳ' => "IV",
                'ⅳ' => "iv",
                'Ⅴ' => "V",
                'ⅴ' => "v",
                'Ⅵ' => "VI",
                'ⅵ' => "vi",
                'Ⅶ' => "VII",
                'ⅶ' => "vii",
                'Ⅷ' => "VIII",
                'ⅷ' => "viii",
                'Ⅸ' => "IX",
                'ⅸ' => "ix",
                'Ⅹ' => "X",
                'ⅹ' => "x",
                'Ⅺ' => "XI",
                'ⅺ' => "xi",
                'Ⅻ' => "XII",
                'ⅻ' => "xii",
                'Ⅼ' => "L",
                'ⅼ' => "l",
                'Ⅽ' => "C",
                'ⅽ' => "c",
                'Ⅾ' => "D",
                'ⅾ' => "d",
                'Ⅿ' => "M",
                'ⅿ' => "m",
                _ => {
                    if let Some(s) = &mut result {
                        s.push(char);
                    }
                    continue;
                }
            };

            if result.is_none() {
                let mut s = String::with_capacity(string.len());
                s.push_str(&string[..string.char_indices().nth(i).unwrap().0]);
                result = Some(s);
            }

            result.as_mut().unwrap().push_str(replacement);
        }

        match result {
            Some(s) => Cow::Owned(s),
            None => Cow::Borrowed(string),
        }
    }

    fn string_is_only_symbols(string: &str) -> bool {
        !string.chars().any(|c| !SYMBOLS.contains(&c))
    }

    // TODO: Check when starts with if
    //* This is breaking
    fn ends_with_if_index(string: &str) -> Option<usize> {
        if !string.ends_with(')') {
            return None;
        }

        let mut stage: u8 = 0;
        let char_indices = string.char_indices().rev().skip(1);

        for (i, char) in char_indices {
            match stage {
                0 => {
                    if char == '(' {
                        stage = 1;
                    }
                }
                1 => {
                    if char == 'f' {
                        stage = 2;
                    } else {
                        return None;
                    }
                }
                2 => {
                    if char == 'i' {
                        return Some(i);
                    }
                }
                _ => unreachable!(),
            }
        }

        None
    }

    fn find_lisa_prefix_index(string: &str) -> Option<usize> {
        if string.starts_with(r"\et[") {
            let mut index = r"\et[".len() + 1;

            loop {
                let char = string.as_bytes()[index];

                if char == b']' {
                    return Some(index + 1);
                }

                index += 1;

                if index == 10 {
                    return None;
                }
            }
        } else if string.starts_with(r"\nbt") {
            Some(r"\nbt".len())
        } else if string.starts_with(r"\nblt") {
            Some(r"\nblt".len())
        } else {
            None
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
    fn extract_string(
        &'a self,
        value: &'a Value,
        fail_if_empty: bool,
    ) -> Option<&'a str> {
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

    /// Checks if `key` is present in translation.
    ///
    /// This will check if `key` is present in `self.translation_map`, and also will seek it in maps of `self.translation_maps` if `self.duplicate_mode` is [`DuplicateMode::Remove`].
    ///
    /// # Parameters
    ///
    /// - `key` - key to find in translation.
    ///
    /// # Returns
    ///
    /// - [`bool`] - whether the key is present in translation.
    ///
    fn contains_key(&self, key: &str) -> bool {
        let contains = self.translation_map.contains_key(key);

        if contains {
            return contains;
        }

        if self.duplicate_mode.is_remove() {
            for translation_map in self.translation_maps.values() {
                let contains = translation_map.contains_key(key);

                if contains {
                    return contains;
                }
            }
        }

        false
    }

    /// Gets the [`TranslationEntry`] corresponding to the `key` from translation.
    ///
    /// This will return [`TranslationEntry`] corresponding to the `key` from `self.translation_map`, and also will seek it in maps `self.translation_maps` if `self.duplicate_mode` is [`DuplicateMode::Remove`].
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
    fn get_key(&self, key: &str) -> Option<&TranslationEntry> {
        let value = self.translation_map.get(key);

        if value.is_some() {
            return value;
        }

        if self.duplicate_mode.is_remove() {
            for translation_map in self.translation_maps.values() {
                let option = translation_map.get(key);

                if option.is_some() {
                    return option;
                }
            }
        }

        None
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
    fn finish(&mut self, value: Value) -> ProcessedData {
        let output_content = if self.mode.is_write() {
            ProcessedData::RPGMData(if self.file_type.is_plugins() {
                ["var $plugins =\n".as_bytes(), unsafe {
                    &to_vec(&SerdeValue::from(value)).unwrap_unchecked()
                }]
                .concat()
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
        };

        output_content
    }

    fn get_metadata(&mut self, id: u16) -> Comments {
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

    fn push_metadata(output: &mut Vec<u8>, id: u16, metadata: &[String]) {
        output.extend_from_slice(ID_COMMENT.as_bytes());
        output.extend_from_slice(SEPARATOR.as_bytes());
        output.extend_from_slice(id.to_string().as_bytes());
        output.push(b'\n');

        for comment in metadata.iter().filter(|c| !c.is_empty()) {
            output.extend_from_slice(comment.as_bytes());
            output.push(b'\n');
        }
    }

    fn push_entries(
        output: &mut Vec<u8>,
        source: &str,
        translation: &TranslationEntry,
    ) {
        for comment in translation.comments.iter().filter(|c| !c.is_empty()) {
            output.extend_from_slice(comment.as_bytes());
            output.push(b'\n');
        }

        if !source.is_empty() {
            output.extend_from_slice(source.as_bytes());
            output.extend_from_slice(SEPARATOR.as_bytes());
        }

        if !translation.is_empty() {
            output.extend_from_slice(translation.as_bytes());
        }

        if !source.is_empty() || !translation.is_empty() {
            output.push(b'\n');
        }
    }

    fn finish_translation(&mut self) -> ProcessedData {
        let allow_dup =
            self.duplicate_mode.is_allow() || self.file_type.is_misc();
        let skip_events_entry = self.skip_events.get(&self.file_type);

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

        let mut accumulated_map = if allow_dup {
            TranslationMap::default()
        } else {
            let len = self.translation_maps.values().fold(0, |mut acc, map| {
                acc += map.len();
                acc
            });

            self.translation_maps.drain(..).map(|(_, m)| m).fold(
                TranslationMap::with_capacity(len),
                |mut acc, map| {
                    acc.extend(map);
                    acc
                },
            )
        };

        let iter = mutable!(self, Self)
            .accumulated_translation
            .iter_mut()
            .enumerate();
        let mut prev_id = u16::MAX;

        for (i, (id, meta, lines, map)) in iter {
            let skip = skip_events_entry.is_some_and(|e| e.contains(id))
                || (self.file_type.is_map() && self.skip_maps.contains(id))
                || (self.mode.is_purge()
                    && self.file_type.is_system()
                    && *id == 8);

            if skip {
                if self.mode.is_append() || self.mode.is_purge() {
                    Self::push_metadata(&mut output, *id, meta);

                    for (source, translation) in map {
                        Self::push_entries(&mut output, source, translation);
                    }
                }

                continue;
            }

            if self.mode.is_purge() {
                Self::push_metadata(&mut output, *id, meta);

                for (mut source, translation) in take(map) {
                    if translation.is_empty() {
                        let moved = take(&mut source);

                        if self.flags.contains(BaseFlags::CreateIgnore)
                            && !moved.is_empty()
                        {
                            self.ignore_entry.insert(moved);
                        }
                    }

                    Self::push_entries(&mut output, &source, &translation);
                }

                continue;
            }

            if *id != prev_id {
                let has_display_name =
                    meta.get(DISPLAY_NAME_POS).is_some_and(|c| !c.is_empty());

                let should_push_map = self.file_type.is_map()
                    && self.map_events
                    && (self.accumulated_translation[i..].iter().any(
                        |(next_id, _, lines, _)| {
                            *next_id == *id && !lines.is_empty()
                        },
                    ) || has_display_name);

                let should_push_other = !lines.is_empty() || has_display_name;

                if should_push_map || should_push_other {
                    Self::push_metadata(&mut output, *id, meta);
                }

                prev_id = *id;
            }

            let next_lines_empty = self
                .accumulated_translation
                .get(i + 1)
                .is_some_and(|(_, _, next_lines, _)| next_lines.is_empty());

            if !next_lines_empty {
                if let Some((_, entry)) = map.first() {
                    Self::push_entries(&mut output, "", entry);
                }
            }

            for source in lines {
                let translation = match (allow_dup, self.mode.is_append()) {
                    (true, true) => {
                        map.swap_remove(source.as_ref()).unwrap_or_default()
                    }
                    (false, true) => accumulated_map
                        .swap_remove(source.as_ref())
                        .unwrap_or_default(),
                    (_, false) => TranslationEntry::default(),
                };

                Self::push_entries(&mut output, source, &translation);
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
    fn flush_translation(&mut self, id: u16) {
        let metadata = self.get_metadata(id);

        if self.mode.is_purge() {
            if !self.translation_map.is_empty()
                || metadata
                    .get(DISPLAY_NAME_POS)
                    .is_some_and(|x| !x.is_empty())
            {
                self.accumulated_translation.push((
                    id,
                    metadata,
                    Vec::new(),
                    take(self.translation_map),
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
                self.translation_map.clear();
            } else {
                let lines =
                    self.lines.drain(..).map(Cow::Owned).collect::<Vec<_>>();

                self.accumulated_translation.push((
                    id,
                    metadata,
                    lines,
                    take(self.translation_map),
                ));
            }
        } else {
            let total_length = self.total_length;
            let current_length = self.lines.len() - total_length;

            let lines = self.lines[total_length..]
                .iter()
                .map(|x| Cow::Borrowed(mutable!(x.as_str(), str)))
                .collect::<Vec<_>>();

            self.accumulated_translation.push((
                id,
                metadata,
                lines,
                TranslationMap::default(),
            ));

            self.total_length += current_length;
        }
    }

    fn update_metadata(
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

    /// Returns some additional data, that needs to be inserted at the start of the output translation data.
    ///
    /// Right now, this function returns the slice of source entries that need to be inserted on read. This may change at any moment.
    ///
    /// # Returns
    ///
    /// - [`&[&str]`] - slice of source entries.
    ///
    #[must_use]
    fn get_additional_data(&self) -> &[&str] {
        if !self.mode.is_write()
            && self.game_type.is_termina()
            && self.file_type.is_items()
        {
            return &[
                "<Menu Category: Items>",
                "<Menu Category: Food>",
                "<Menu Category: Healing>",
                "<Menu Category: Body bag>",
            ];
        }

        &[]
    }
}

/// Base for processing `Map` files.
pub struct MapBase<'a> {
    pub base: &'a mut Base,
    mapinfos: Value,
}

impl<'a> MapBase<'a> {
    /// Initializes system base using [`Base`].
    /// Before calling this, you should create a base and pass it here.
    ///
    /// # Example
    ///
    /// ```
    /// use rvpacker_txt_rs_lib::{core::{Base, MapBase}, Mode, ReadMode, EngineType};
    ///
    /// let mut base = Base::new(Mode::Read(ReadMode::Default(false)), EngineType::VXAce);
    /// let mut map_base = MapBase::new(&mut base);
    /// ```
    pub fn new(base: &'a mut Base) -> Self {
        base.reset();
        base.file_type = RPGMFileType::Map;

        Self {
            base,
            mapinfos: Value::default(),
        }
    }

    /// Returns the translation data, accumulated after processing multiple maps.
    ///
    /// Returns the actual data only with [`Mode::Read`] or [`Mode::Purge`].
    ///
    /// # Example
    ///
    /// ```no_run
    /// use rvpacker_txt_rs_lib::{core::{Base, MapBase}, Mode, ReadMode, EngineType, Error};
    /// use std::fs::read;
    ///
    /// fn main() -> Result<(), Box<dyn std::error::Error>> {
    ///     let mut base = Base::new(Mode::Read(ReadMode::Default(false)), EngineType::VXAce);
    ///     let mut map_base = MapBase::new(&mut base);
    ///
    ///     let mapinfos = read("C:/Game/Data/MapInfos.rvdata2")?;
    ///
    ///     let map_file_content = read("C:/Game/Data/Map001.rvdata2")?;
    ///     let data = map_base.process("Map001.rvdata2", &map_file_content, &mapinfos, None)?;
    ///
    ///     let map_file_content = read("C:/Game/Data/Map002.rvdata2")?;
    ///     let data = map_base.process("Map002.rvdata2", &map_file_content, &mapinfos, None)?;
    ///
    ///     let translation_data = map_base.translation();
    ///     Ok(())
    /// }
    /// ```
    pub fn translation(&mut self) -> ProcessedData {
        self.base.finish(Value::default())
    }

    /// Processes the RPG Maker map file content.
    ///
    /// To get the translation data, you need to call [`MapBase::translation`] after processing required maps.
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
    /// use rvpacker_txt_rs_lib::{core::{Base, MapBase}, Mode, ReadMode, EngineType, Error};
    /// use std::fs::read;
    ///
    /// fn main() -> Result<(), Box<dyn std::error::Error>> {
    ///     let mut base = Base::new(Mode::Read(ReadMode::Default(false)), EngineType::VXAce);
    ///     let mut map_base = MapBase::new(&mut base);
    ///
    ///     let map_file_content = read("C:/Game/Data/Map001.rvdata2")?;
    ///     let mapinfos = read("C:/Game/Data/MapInfos.rvdata2")?;
    ///     let data = map_base.process("Map001.rvdata2", &map_file_content, &mapinfos, None)?;
    ///
    ///     // Required only when reading.
    ///     let translation_data = map_base.translation();
    ///     Ok(())
    /// }
    /// ```
    pub fn process(
        &mut self,
        filename: &str,
        content: &[u8],
        mapinfos: &[u8],
        translation: Option<&str>,
    ) -> Result<Option<ProcessedData>, Error> {
        if self.mapinfos.is_null() {
            self.mapinfos = self.base.parse_rpgm_file(mapinfos)?;
        }

        self.base.initialize_translation(translation)?;

        let id = Self::parse_map_id(filename);
        if self.is_map_unused(id) {
            return Ok(None);
        }

        if self.base.get_translation_map(id).is_break() {
            return Ok(None);
        }

        let mut map_object = self.base.parse_rpgm_file(content)?;
        let display_name = self.get_display_name(&map_object);

        if self.base.mode.is_read() {
            let map_order = self.get_map_order(id).to_string();
            let map_name = mutable!(self, Self).get_map_name(id);
            let replaced_map_name = map_name.normalize();

            self.base.update_metadata(
                id,
                Vec::from([
                    (CommentPos::Name, replaced_map_name.as_ref()),
                    (CommentPos::Order, &map_order),
                    (CommentPos::DisplayName, &display_name),
                ]),
            );

            let events = if self.base.engine_type.is_new() {
                // SAFETY: Always an array in new maps.
                EventIterator::New(unsafe {
                    map_object[self.base.labels.events]
                        .as_array_mut()
                        .unwrap_unchecked()
                        .iter_mut()
                        .skip(1)
                })
            } else {
                // SAFETY: Always a hashmap in old maps.
                EventIterator::Old(unsafe {
                    map_object[self.base.labels.events]
                        .as_hashmap_mut()
                        .unwrap_unchecked()
                        .values_mut()
                })
            };

            for event in events {
                if event.is_null() {
                    continue;
                }

                let Some(pages) = mutable!(event, Value)
                    [self.base.labels.pages]
                    .as_array_mut()
                else {
                    continue;
                };

                if self.base.map_events {
                    self.base.flush_translation(id);

                    let event_id = event["id"].as_int().unwrap();
                    let event_name = event["name"].as_str().unwrap();
                    let event_x = event["x"].as_int().unwrap();
                    let event_y = event["y"].as_int().unwrap();

                    self.base.accumulated_translation.push((
                        id,
                        SmallVec::default(),
                        Vec::new(),
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
                        page[self.base.labels.list]
                            .as_array_mut()
                            .unwrap_unchecked()
                    };

                    self.base.process_list(list);
                }
            }
        } else if !display_name.is_empty() {
            let display_name_comment_line = &self.base.metadata[&id][2];

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

                map_object[self.base.labels.display_name] =
                    Value::string(translation);
            } else {
                log::warn!(
                    "{COULD_NOT_SPLIT_LINE_MSG} {display_name_comment_line}\n{IN_FILE_MSG}: {file}.txt",
                    file = self.base.file_type.to_string().to_lowercase()
                );
            }
        }

        if self.base.mode.is_write() {
            Ok(Some(self.base.finish(map_object)))
        } else {
            self.base.flush_translation(id);
            Ok(None)
        }
    }

    /// Parses a map ID from a filename by extracting the substring at positions 3 to 5 and converting parsing it to [`u16`].
    ///
    /// # Parameters
    ///
    /// - `filename` - Filename of the map.
    ///
    /// # Returns
    ///
    /// - [`u16`] - The parsed map ID.
    ///
    fn parse_map_id(filename: &str) -> u16 {
        // SAFETY: We discarded all files, which don't contain a digit at index 3.
        unsafe { filename[3..=5].parse::<u16>().unwrap_unchecked() }
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
        if self.base.engine_type.is_new() {
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
            if self.base.engine_type.is_new() {
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
    fn get_map_name(&self, id: u16) -> &str {
        // SAFETY: "name" always exists in mapinfos and is always a string.
        unsafe {
            if self.base.engine_type.is_new() {
                &self.mapinfos[id as usize]["name"]
            } else {
                &self.mapinfos[Value::int(i32::from(id))]["name"]
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
            .get(self.base.labels.display_name)
            .map(|display_name| {
                display_name
                    .as_str()
                    .map(|name| {
                        let name_replaced = name.normalize();

                        if self.base.flags.contains(BaseFlags::Romanize) {
                            Base::romanize_string(&name_replaced)
                        } else {
                            name_replaced
                        }
                        .into_owned()
                    })
                    .unwrap_or_default()
            })
            .unwrap_or_default()
    }
}

/// Base for processing other files (`Actors`, `Armors`, `Classes`, `Enemies`, `CommonEvents`, `Troops`, `Items`, `Skills`, `States`, `Weapons`).
pub struct OtherBase<'a> {
    pub base: &'a mut Base,
}

impl<'a> OtherBase<'a> {
    /// Initializes system base using [`Base`].
    /// Before calling this, you should create a base and pass it here.
    ///
    /// # Example
    ///
    /// ```
    /// use rvpacker_txt_rs_lib::{core::{Base, OtherBase}, Mode, ReadMode, EngineType};
    ///
    /// let mut base = Base::new(Mode::Read(ReadMode::Default(false)), EngineType::VXAce);
    /// let mut other_base = OtherBase::new(&mut base);
    /// ```
    pub fn new(base: &'a mut Base) -> Self {
        base.reset();
        base.file_type = RPGMFileType::Invalid;

        Self { base }
    }

    /// Processes the RPG Maker other file content.
    ///
    /// # Parameters
    ///
    /// - `filename` - Filename of the file that's being processed.
    /// - `content` - Content of the file that's being processed.
    /// - `translation` - Contents of the translation file corresponding to the file. Isn't used with [`ReadMode::Default`]. Requires to be set with any other [`Mode`].
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
    /// - [`Error::NoTranslation`] - if mode is not [`ReadMode::Default`], and no translation was passed.
    ///
    /// # Panics
    ///
    /// May panic if passed content is not `Actors`, `Armors`, `Classes`, `Enemies`, `CommonEvents`, `Troops`, `Items`, `Skills`, `States`, `Weapons`.
    ///
    /// # Example
    ///
    /// ```no_run
    /// use rvpacker_txt_rs_lib::{core::{Base, OtherBase}, Mode, ReadMode, EngineType, Error};
    /// use std::fs::read;
    ///
    /// fn main() -> Result<(), Box<dyn std::error::Error>> {
    ///     let mut base = Base::new(Mode::Read(ReadMode::Default(false)), EngineType::VXAce);
    ///     let mut other_base = OtherBase::new(&mut base);
    ///
    ///     let other_file_content = read("C:/Game/Data/Actors.rvdata2")?;
    ///     other_base.process("Actors.rvdata2", &other_file_content, None)?;
    ///     Ok(())
    /// }
    /// ```
    pub fn process(
        &mut self,
        filename: &str,
        content: &[u8],
        translation: Option<&str>,
    ) -> Result<Option<ProcessedData>, Error> {
        self.base.file_type = RPGMFileType::from_filename(filename);

        self.base.reset();
        self.base.initialize_translation(translation)?;

        let mut entry_value = self.base.parse_rpgm_file(content)?;

        // SAFETY: All "other" entries are always arrays.
        let object_array =
            unsafe { entry_value.as_array_mut().unwrap_unchecked() };

        let mut processed = false;

        // Skipping one, because the first entry is always null.
        for object in object_array.iter_mut().skip(1) {
            // SAFETY: Name and ID exists on every object.
            let id = unsafe { object["id"].as_int().unwrap_unchecked() } as u16;

            if self.base.get_translation_map(id).is_break() {
                if self.base.mode.is_purge() {
                    processed = true;
                }

                continue;
            }

            processed = true;

            let event_name = unsafe {
                object[self.base.labels.name].as_str().unwrap_unchecked()
            };

            self.base.update_metadata(
                id,
                Vec::from([(CommentPos::Name, event_name)]),
            );

            if self.base.file_type.is_events()
                || self.base.file_type.is_troops()
            {
                self.process_object(object);
            } else {
                self.process_array(object);
            }

            self.base.flush_translation(id);
        }

        if !processed {
            return Ok(None);
        }

        Ok(Some(self.base.finish(entry_value)))
    }

    fn process_variable_termina(
        &mut self,
        mut variable_text: Cow<'_, str>,
        variable_type: Variable,
        note_text: Option<&str>,
    ) -> Option<String> {
        if variable_text.starts_with("///") || variable_text.contains("---") {
            return None;
        }

        match variable_type {
            Variable::Description => {
                if let Some(note) = note_text {
                    let mut note_is_continuation = false;

                    if !note.starts_with("flesh puppetry") {
                        let mut note_chars = note.chars();

                        if let Some((note_first_char, note_second_char)) =
                            note_chars.next().zip(note_chars.next())
                        {
                            let is_continuation = note_first_char == '\n'
                                && note_second_char != '\n';

                            let first_char_is_valid = note_first_char
                                .is_ascii_alphabetic()
                                || note_first_char == '"'
                                || note.starts_with("4 sticks");

                            let first_char_is_punctuation = matches!(
                                note_first_char,
                                '.' | '!' | '/' | '?'
                            );

                            if (is_continuation || first_char_is_valid)
                                && !first_char_is_punctuation
                            {
                                note_is_continuation = true;
                            }
                        }
                    }

                    if note_is_continuation {
                        let mut note_string = String::from(note);

                        if let Some((mut left, _)) =
                            note.trim_start().split_once('\n')
                        {
                            left = left.trim();

                            if left.ends_with(['.', '%', '!', '"']) {
                                note_string = String::from(
                                    if self.base.mode.is_write() {
                                        "\n"
                                    } else {
                                        NEW_LINE
                                    },
                                ) + left;
                            } else if self.base.mode.is_read() {
                                return None;
                            }
                        } else if note.ends_with(['.', '%', '!', '"'])
                            || note.ends_with("takes place?")
                        {
                            note_string = note.into();
                        } else if self.base.mode.is_read() {
                            return None;
                        }

                        if note_string.is_empty() {
                            if self.base.mode.is_read() {
                                return None;
                            }
                        } else {
                            variable_text = Cow::Owned(format!(
                                "{variable_text}{note_string}"
                            ));
                        }
                    }
                }
            }
            Variable::Message1
            | Variable::Message2
            | Variable::Message3
            | Variable::Message4 => {
                return None;
            }
            Variable::Note => {
                if self.base.mode.is_write() && self.base.file_type.is_items() {
                    for string in [
                        "<Menu Category: Items>",
                        "<Menu Category: Food>",
                        "<Menu Category: Healing>",
                        "<Menu Category: Body bag>",
                    ] {
                        if variable_text.rfind(string).is_some() {
                            return Some(variable_text.replace(
                                string,
                                &self.base.translation_maps[&u16::MAX][string],
                            ));
                        }
                    }
                }

                if !self.base.file_type.is_classes() {
                    return None;
                }
            }
            Variable::Name | Variable::Nickname => match self.base.file_type {
                RPGMFileType::Actors => {
                    if ![
                        "Levi",
                        "Marina",
                        "Daan",
                        "Abella",
                        "O'saa",
                        "Blood golem",
                        "Black Kalev",
                        "Marcoh",
                        "Karin",
                        "Olivia",
                        "Ghoul",
                        "Villager",
                        "August",
                        "Caligura",
                        "Henryk",
                        "Pav",
                        "Tanaka",
                        "Samarie",
                    ]
                    .contains(&variable_text.as_ref())
                    {
                        return None;
                    }
                }
                RPGMFileType::Armors => {
                    if variable_text.starts_with("test_armor") {
                        return None;
                    }
                }
                RPGMFileType::Classes => {
                    if [
                        "Girl",
                        "Kid demon",
                        "Captain",
                        "Marriage",
                        "Marriage2",
                        "Baby demon",
                        "Buckman",
                        "Nas'hrah",
                        "Skeleton",
                    ]
                    .contains(&variable_text.as_ref())
                    {
                        return None;
                    }
                }
                RPGMFileType::Enemies => {
                    if ["Spank Tank", "giant", "test"]
                        .contains(&variable_text.as_ref())
                    {
                        return None;
                    }
                }
                RPGMFileType::Items => {
                    if [
                        "Torch",
                        "Flashlight",
                        "Stick",
                        "Quill",
                        "Empty scroll",
                        "Soul stone_NOT_USE",
                        "Cube of depths",
                        "Worm juice",
                        "Silver shilling",
                        "Coded letter #1 - UNUSED",
                        "Black vial",
                        "Torturer's notes 1",
                        "Purple vial",
                        "Orange vial",
                        "Red vial",
                        "Green vial",
                        "Pinecone pig instructions",
                        "Grilled salmonsnake meat",
                        "Empty scroll",
                        "Water vial",
                        "Blood vial",
                        "Devil's Grass",
                        "Stone",
                        "Codex #1",
                        "The Tale of the Pocketcat I",
                        "The Tale of the Pocketcat II",
                    ]
                    .contains(&variable_text.as_ref())
                        || variable_text.starts_with("The Fellowship")
                        || variable_text.starts_with("Studies of")
                        || variable_text.starts_with("Blueish")
                        || variable_text.starts_with("Skeletal")
                        || variable_text.ends_with("soul")
                        || variable_text.ends_with("schematics")
                    {
                        return None;
                    }
                }
                RPGMFileType::Weapons => {
                    if variable_text == "makeshift2" {
                        return None;
                    }
                }
                _ => {}
            },
        }

        Some(variable_text.into_owned())
    }

    #[allow(clippy::collapsible_match, clippy::single_match)]
    fn process_variable(
        &mut self,
        variable_text: &str,
        note_text: Option<&str>,
        variable_type: Variable,
    ) -> Option<String> {
        if Base::string_is_only_symbols(variable_text) {
            return None;
        }

        let mut variable_text = Cow::Borrowed(variable_text);

        if !self.base.engine_type.is_new() {
            if variable_text.lines().all(|line| {
                line.is_empty()
                    || IS_INVALID_MULTILINE_VARIABLE_RE
                        .with(|r| r.is_match(line))
            }) || IS_INVALID_VARIABLE_RE.with(|r| r.is_match(&variable_text))
            {
                return None;
            }

            variable_text = Cow::Owned(variable_text.replace("\r\n", "\n"));
        }

        let remaining_strings: SmallVec<[(String, bool); 4]> =
            SmallVec::with_capacity(4);

        match self.base.game_type {
            GameType::Termina => {
                if let Some(text) = self.process_variable_termina(
                    variable_text,
                    variable_type,
                    note_text,
                ) {
                    if self.base.mode.is_write()
                        && self.base.file_type.is_items()
                        && variable_type.is_note()
                    {
                        return Some(text);
                    }

                    variable_text = Cow::Owned(text);
                } else {
                    return None;
                }
            }
            // custom processing for other games
            _ => {}
        }

        let old_variable_text = take(&mut variable_text);

        if self.base.flags.contains(BaseFlags::Romanize) {
            variable_text = Base::romanize_string(&old_variable_text);
        } else {
            variable_text = old_variable_text;
        }

        if self.base.mode.is_read() {
            return Some(variable_text.into_owned());
        }

        let translated = self.base.get_key(&variable_text).map(|translated| {
            let mut result = translated.to_string();

            for (string, position) in remaining_strings {
                if position {
                    result += &string;
                } else {
                    result = string + &result;
                }
            }

            if variable_type.is_any_message()
                && !(variable_type.is_message_2()
                    && self.base.file_type.is_skills())
            {
                result = String::from(' ') + &result;
            }

            match self.base.game_type {
                GameType::Termina => match variable_type {
                    Variable::Note => {
                        if let Some(first_char) = result.chars().next() {
                            if first_char != '\n' {
                                result = String::from('\n') + &result;
                            }
                        }
                    }
                    _ => {}
                },
                _ => {}
            }

            if self.base.game_type.is_termina()
                && variable_type.is_description()
            {
                result += "\n\n\n\n";
            }

            result
        });

        translated
    }

    /// Processes an object from `CommonEvents` or `Troops` file.
    fn process_object(&mut self, object: &mut Value) {
        if self.base.file_type.is_troops() {
            // SAFETY: Troops always include pages.
            let pages = unsafe {
                object[self.base.labels.pages]
                    .as_array_mut()
                    .unwrap_unchecked()
            };

            for page in pages {
                if let Some(list_array) =
                    page[self.base.labels.list].as_array_mut()
                {
                    self.base.process_list(list_array);
                }
            }
        } else {
            // SAFETY: CommonEvents always include list.
            let list = unsafe {
                object[self.base.labels.list]
                    .as_array_mut()
                    .unwrap_unchecked()
            };

            self.base.process_list(list);
        }
    }

    /// Processes an object array from `Actors`, `Armors`, `Classes`, `Enemies`, `Items`, `States`, `Weapons` files.
    fn process_array(&mut self, array: &mut Value) {
        let variable_pairs = [
            (self.base.labels.name, Variable::Name),
            (self.base.labels.nickname, Variable::Nickname),
            (self.base.labels.description, Variable::Description),
            (self.base.labels.message1, Variable::Message1),
            (self.base.labels.message2, Variable::Message2),
            (self.base.labels.message3, Variable::Message3),
            (self.base.labels.message4, Variable::Message4),
            (self.base.labels.note, Variable::Note),
        ];

        for (variable_label, variable_type) in variable_pairs {
            let Some(object) = array.get(variable_label) else {
                continue;
            };

            let Some(string) = self.base.extract_string(object, true) else {
                continue;
            };

            let mut string = if self.base.mode.is_write()
                && self.base.flags.contains(BaseFlags::Romanize)
            {
                Base::romanize_string(string)
            } else {
                Cow::Borrowed(string)
            };

            if self.base.mode.is_write() {
                string = Cow::Owned(
                    string
                        .lines()
                        .map(str::trim)
                        .collect::<Vec<_>>()
                        .join("\n"),
                );
            }

            let note_text = if self.base.game_type.is_termina()
                && variable_type.is_description()
            {
                array[self.base.labels.note].as_str()
            } else {
                None
            };

            let Some(parsed) = mutable!(self, Self).process_variable(
                &string,
                note_text,
                variable_type,
            ) else {
                continue;
            };

            if self.base.mode.is_write() {
                array[variable_label] = Value::string(parsed);
            } else {
                let folded = parsed.lines().fold(
                    String::with_capacity(parsed.len() * 2),
                    |mut output, line| {
                        let trimmed = if variable_type.is_any_message()
                            || self.base.flags.contains(BaseFlags::Trim)
                        {
                            line.trim()
                        } else {
                            line
                        };

                        let _ = write!(output, "{trimmed}{NEW_LINE}");

                        output
                    },
                );

                let replaced =
                    unsafe { folded.strip_suffix(NEW_LINE).unwrap_unchecked() };

                self.base.insert_string(Cow::Borrowed(replaced));
            }
        }
    }
}

pub struct SystemBase<'a> {
    pub base: &'a mut Base,
    game_title: String,
    system_value: Value,
}

impl<'a> SystemBase<'a> {
    /// Initializes system base using [`Base`].
    /// Before calling this, you should create a base and pass it here.
    ///
    /// # Example
    ///
    /// ```
    /// use rvpacker_txt_rs_lib::{core::{Base, SystemBase}, Mode, ReadMode, EngineType};
    ///
    /// let mut base = Base::new(Mode::Read(ReadMode::Default(false)), EngineType::VXAce);
    /// let mut system_base = SystemBase::new(&mut base);
    /// ```
    pub fn new(base: &'a mut Base) -> Self {
        base.reset();
        base.file_type = RPGMFileType::System;

        Self {
            base,
            game_title: String::new(),
            system_value: Value::default(),
        }
    }

    /// Processes the RPG Maker system file content.
    ///
    /// # Parameters
    ///
    /// - `content` - Content of the file that's being processed.
    /// - `translation` - Contents of the translation file corresponding to the file. Isn't used with [`ReadMode::Default`]. Requires to be set with any other [`Mode`].
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
    /// - [`Error::NoTranslation`] - if mode is not [`ReadMode::Default`], and no translation was passed.
    ///
    /// # Panics
    ///
    /// May panic if passed content is not `System`.
    ///
    /// # Example
    ///
    /// ```no_run
    /// use rvpacker_txt_rs_lib::{core::{Base, SystemBase}, Mode, ReadMode, EngineType, Error};
    /// use std::fs::read;
    ///
    /// fn main() -> Result<(), Box<dyn std::error::Error>> {
    ///     let mut base = Base::new(Mode::Read(ReadMode::Default(false)), EngineType::VXAce);
    ///     let mut system_base = SystemBase::new(&mut base);
    ///
    ///     let system_file_content = read("C:/Game/Data/System.rvdata2")?;
    ///     system_base.process(&system_file_content, None)?;
    ///     Ok(())
    /// }
    /// ```
    pub fn process(
        mut self,
        content: &[u8],
        translation: Option<&str>,
    ) -> Result<Option<ProcessedData>, Error> {
        self.base.initialize_translation(translation)?;

        self.system_value = self.base.parse_rpgm_file(content)?;
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

            if self.base.get_translation_map(id).is_break() {
                if self.base.mode.is_purge() {
                    processed = true;
                }

                continue;
            }

            processed = true;

            self.base.update_metadata(
                id,
                Vec::from([(CommentPos::Name, entry_name)]),
            );

            if id <= 5 {
                let label = [
                    self.base.labels.armor_types,
                    self.base.labels.elements,
                    self.base.labels.skill_types,
                    self.base.labels.weapon_types,
                    self.base.labels.equip_types,
                ][id as usize - 1];

                let Some(array) =
                    mutable!(&self, Self).system_value[label].as_array_mut()
                else {
                    continue;
                };

                for value in array {
                    self.process_value(value);
                }
            } else if id == 6 {
                self.process_terms();
            } else if id == 7 {
                self.process_currency_unit();
            } else {
                self.process_game_title();
            }

            self.base.flush_translation(id);
        }

        if !processed {
            return Ok(None);
        }

        Ok(Some(self.base.finish(self.system_value.take())))
    }

    fn process_terms(&mut self) {
        let Some(terms) = mutable!(self, Self).system_value
            [self.base.labels.terms]
            .as_object_mut()
        else {
            return;
        };

        for (key, value) in terms.iter_mut() {
            if key == "messages" {
                if let Some(messages) = value.as_object_mut() {
                    for value in messages.values_mut() {
                        self.process_value(value);
                    }
                }
            } else if let Some(array) = value.as_array_mut() {
                for value in array {
                    self.process_value(value);
                }
            } else if value.is_bytes() || value.is_string() {
                self.process_value(value);
            }
        }
    }

    fn process_value(&mut self, value: &mut Value) {
        let Some(extracted) =
            mutable!(self, Self).base.extract_string(value, true)
        else {
            return;
        };

        let extracted = if self.base.flags.contains(BaseFlags::Romanize) {
            Base::romanize_string(extracted)
        } else {
            Cow::Borrowed(extracted)
        };

        if self.base.mode.is_read() {
            self.base.insert_string(extracted);
        } else if self.base.mode.is_write() {
            if let Some(translated) = self.base.get_key(&extracted) {
                *value = Base::make_string_value(
                    translated,
                    self.base.engine_type.is_new(),
                );
            }
        } else {
            self.base
                .translation_map
                .insert(extracted.into(), TranslationEntry::default());
        }
    }

    fn process_currency_unit(&mut self) {
        if !self.base.engine_type.is_new() {
            mutable!(self, Self).process_value(
                &mut self.system_value[self.base.labels.currency_unit],
            );
        }
    }

    fn process_game_title(&mut self) {
        if self.base.mode.is_write() {
            if !self.game_title.is_empty() {
                self.system_value[self.base.labels.game_title] =
                    Value::string(self.game_title.as_str());
            }
        } else if let Some(game_title_value) =
            self.system_value.get(self.base.labels.game_title)
        {
            let Some(game_title) =
                self.base.extract_string(game_title_value, true)
            else {
                return;
            };

            let game_title = if self.base.flags.contains(BaseFlags::Romanize) {
                Base::romanize_string(game_title)
            } else {
                Cow::Borrowed(game_title)
            };

            mutable!(self, Self).base.insert_string(game_title);
        }
    }
}

pub struct ScriptBase<'a> {
    pub base: &'a mut Base,
}

impl<'a> ScriptBase<'a> {
    /// Initializes system base using [`Base`].
    /// Before calling this, you should create a base and pass it here.
    ///
    /// # Example
    ///
    /// ```
    /// use rvpacker_txt_rs_lib::{core::{Base, ScriptBase}, Mode, ReadMode, EngineType};
    ///
    /// let mut base = Base::new(Mode::Read(ReadMode::Default(false)), EngineType::VXAce);
    /// let mut script_base = ScriptBase::new(&mut base);
    /// ```
    pub fn new(base: &'a mut Base) -> Self {
        base.reset();
        base.file_type = RPGMFileType::Scripts;

        Self { base }
    }

    /// Processes the RPG Maker scripts file content.
    ///
    /// # Parameters
    ///
    /// - `content` - Content of the file that's being processed.
    /// - `translation` - Contents of the translation file corresponding to the file. Isn't used with [`ReadMode::Default`]. Requires to be set with any other [`Mode`].
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
    /// - [`Error::NoTranslation`] - if mode is not [`ReadMode::Default`], and no translation was passed.
    ///
    /// # Panics
    ///
    /// May panic if passed content is not `Scripts`.
    ///
    /// # Example
    ///
    /// ```no_run
    /// use rvpacker_txt_rs_lib::{core::{Base, ScriptBase}, Mode, ReadMode, EngineType, Error};
    /// use std::fs::read;
    ///
    /// fn main() -> Result<(), Box<dyn std::error::Error>> {
    ///     let mut base = Base::new(Mode::Read(ReadMode::Default(false)), EngineType::VXAce);
    ///     let mut script_base = ScriptBase::new(&mut base);
    ///
    ///     let script_file_content = read("C:/Game/Data/Scripts.rvdata2")?;
    ///     script_base.process(&script_file_content, None)?;
    ///     Ok(())
    /// }
    /// ```
    pub fn process(
        self,
        content: &[u8],
        translation: Option<&str>,
    ) -> Result<Option<ProcessedData>, Error> {
        self.base.initialize_translation(translation)?;

        // SAFETY: Scripts are always array.
        let mut scripts_array = unsafe {
            self.base
                .parse_rpgm_file(content)?
                .into_array()
                .unwrap_unchecked()
        };
        let mut scripts = Self::decode_scripts(&scripts_array);

        // SAFETY: These regexes are valid, 100% no shit.
        let regexes = unsafe {
            [
                Regex::new(r"(Graphics|Data|Audio|Movies|System)\/.*\/?").unwrap_unchecked(),
                Regex::new(r"r[xv]data2?$").unwrap_unchecked(),
                Regex::new(r".*\(").unwrap_unchecked(),
                Regex::new(r"^([d\d\p{P}+-]*|[d\p{P}+-]&*)$").unwrap_unchecked(),
                Regex::new(r"^(Actor<id>|ExtraDropItem|EquipLearnSkill|GameOver|Iconset|Window|true|false|MActor%d|[wr]b|\\f|\\n|\[[A-Z]*\])$")
                    .unwrap_unchecked(),
            ]
        };

        let mut processed = false;

        for (((script_id, script), script_name), mut code) in scripts_array
            .iter_mut()
            .enumerate()
            .zip(take(&mut scripts.names))
            .zip(take(&mut scripts.contents))
        {
            let id = script_id as u16 + 1;

            if self.base.get_translation_map(id).is_break() {
                if self.base.mode.is_purge() {
                    processed = true;
                }

                continue;
            }

            processed = true;

            self.base.update_metadata(
                id,
                Vec::from([(CommentPos::Name, script_name.as_str())]),
            );
            let (extracted_strings, ranges) = self.extract_strings(&code);

            if self.base.mode.is_write() {
                let mut code_changed = false;

                for (mut extracted, range) in extracted_strings
                    .into_iter()
                    .zip(ranges)
                    .filter(|(s, _)| !s.trim().is_empty())
                    .rev()
                    .map(|(s, r)| (Cow::Owned(s), r))
                {
                    let old_extracted = take(&mut extracted);

                    if self.base.flags.contains(BaseFlags::Romanize) {
                        extracted = Base::romanize_string(&old_extracted);
                    } else {
                        extracted = old_extracted;
                    }

                    if let Some(translated) = self.base.get_key(&extracted) {
                        code.replace_range(range, translated);
                        code_changed = true;
                    }
                }

                if code_changed {
                    let mut buf = Vec::with_capacity(code.len());

                    ZlibEncoder::new(&mut buf, Compression::default())
                        .write_all(code.as_bytes())
                        .unwrap();

                    script[2] = Value::bytes(&buf);
                }
            } else {
                for mut extracted in extracted_strings
                    .into_iter()
                    .filter(|s| !s.trim().is_empty())
                    .map(Cow::Owned)
                {
                    if Base::string_is_only_symbols(&extracted)
                        || extracted.contains("@window")
                        || extracted.contains(r"\$game")
                        || extracted.starts_with(r"\\e")
                        || extracted.contains("ALPHAC")
                        || extracted.contains('_')
                        || regexes.iter().any(|re| re.is_match(&extracted))
                    {
                        continue;
                    }

                    let old_extracted = take(&mut extracted);

                    if self.base.flags.contains(BaseFlags::Romanize) {
                        extracted = Base::romanize_string(&old_extracted);
                    } else {
                        extracted = old_extracted;
                    }

                    self.base.insert_string(extracted);
                }

                self.base.flush_translation(id);
            }
        }

        if !processed {
            return Ok(None);
        }

        Ok(Some(self.base.finish(Value::array(scripts_array))))
    }

    fn is_escaped(index: usize, string: &str) -> bool {
        let mut backslash_count: u8 = 0;

        for char in string[..index].chars().rev() {
            if char != '\\' {
                break;
            }

            backslash_count += 1;
        }

        backslash_count % 2 == 1
    }

    fn extract_strings(&self, ruby_code: &str) -> (Lines, Vec<Range<usize>>) {
        let mut strings = Lines::default();
        let mut ranges = Vec::new();
        let mut inside_string = false;
        let mut inside_multiline_comment = false;
        let mut string_start_index = 0;
        let mut current_quote_type = '\0';
        let mut global_index = 0;

        for line in ruby_code.each_line() {
            let trimmed = line.trim();

            if !inside_string {
                if trimmed.starts_with('#') {
                    global_index += line.len();
                    continue;
                }

                if trimmed.starts_with("=begin") {
                    inside_multiline_comment = true;
                } else if trimmed.starts_with("=end") {
                    inside_multiline_comment = false;
                }
            }

            if inside_multiline_comment {
                global_index += line.len();
                continue;
            }

            let char_indices = line.char_indices();

            for (i, char) in char_indices {
                if !inside_string && char == '#' {
                    break;
                }

                if !inside_string && (char == '"' || char == '\'') {
                    inside_string = true;
                    string_start_index = global_index + i;
                    current_quote_type = char;
                } else if inside_string
                    && char == current_quote_type
                    && !Self::is_escaped(i, &line)
                {
                    let range = string_start_index + 1..global_index + i;

                    let extracted_string = ruby_code[range.clone()].normalize();

                    if !extracted_string.is_empty()
                        && !strings.contains(extracted_string.as_ref())
                    {
                        strings.insert(extracted_string.into_owned());

                        if self.base.mode.is_write() {
                            ranges.push(range);
                        }
                    }

                    inside_string = false;
                    current_quote_type = '\0';
                }
            }

            global_index += line.len();
        }

        (strings, ranges)
    }

    /// Decodes an array of script entries into [`Scripts`] struct that holds `numbers`, `scripts` and `names` fields.
    ///
    /// # Parameters
    ///
    /// - `scripts_array`: Slice of script entries.
    ///
    /// # Returns
    ///
    /// A [`Scripts`] struct that holds `numbers`, `scripts` and `names` fields.
    ///
    /// # Panics
    ///
    /// May panic if decoder gets interrupted.
    ///
    #[must_use]
    pub fn decode_scripts(scripts_array: &[Value]) -> Scripts {
        let mut numbers = Vec::with_capacity(scripts_array.len());
        let mut contents = Vec::with_capacity(scripts_array.len());
        let mut names = Vec::with_capacity(scripts_array.len());

        for script in scripts_array {
            // SAFETY: Scripts always have a layout like this. `0` is magic number, `1` is name and `2` is actual script data.
            let script_number =
                unsafe { script[0].as_int().unwrap_unchecked() };
            let script_name_data =
                unsafe { script[1].as_byte_vec().unwrap_unchecked() };
            let script_data =
                unsafe { script[2].as_byte_vec().unwrap_unchecked() };

            let mut decoded_script = Vec::with_capacity(script_data.len());
            ZlibDecoder::new(script_data)
                .read_to_end(&mut decoded_script)
                .unwrap();

            for encoding in [
                encoding_rs::UTF_8,
                encoding_rs::WINDOWS_1252,
                encoding_rs::WINDOWS_1251,
                encoding_rs::SHIFT_JIS,
                encoding_rs::GB18030,
            ] {
                let (content_cow, _, had_errors) =
                    encoding.decode(&decoded_script);
                let (name_cow, _, _) = encoding.decode(script_name_data);

                if !had_errors {
                    numbers.push(script_number);
                    contents.push(content_cow.into());
                    names.push(name_cow.into());
                    break;
                }
            }
        }

        Scripts::new(numbers, contents, names)
    }

    /// Encodes decoded [`Scripts`] struct back to [`Vec<Value>`].
    ///
    /// # Parameters
    ///
    /// - [`Scripts`] struct to encode.
    ///
    /// # Returns
    ///
    /// - [`Vec<Value>`] of encoded script entries.
    ///
    /// # Panics
    ///
    /// May panic if encoder gets interrupted.
    ///
    #[must_use]
    pub fn encode_scripts(scripts: &Scripts) -> Vec<Value> {
        let mut scripts_array = Vec::with_capacity(scripts.contents.len());

        for ((content, name), number) in scripts
            .contents
            .iter()
            .zip(scripts.names.iter())
            .zip(scripts.numbers.iter())
        {
            let mut encoder =
                ZlibEncoder::new(Vec::new(), Compression::default());
            encoder.write_all(content.as_bytes()).unwrap();
            let compressed_content = encoder.finish().unwrap();

            scripts_array.push(Value::array(vec![
                Value::int(*number),
                Value::string(name),
                Value::bytes(&compressed_content),
            ]));
        }

        scripts_array
    }
}

pub struct PluginBase<'a> {
    pub base: &'a mut Base,
}

impl<'a> PluginBase<'a> {
    /// Initializes system base using [`Base`].
    /// Before calling this, you should create a base and pass it here.
    ///
    /// # Example
    ///
    /// ```
    /// use rvpacker_txt_rs_lib::{core::{Base, PluginBase}, Mode, ReadMode, EngineType};
    ///
    /// let mut base = Base::new(Mode::Read(ReadMode::Default(false)), EngineType::New);
    /// let mut plugin_base = PluginBase::new(&mut base);
    /// ```
    pub fn new(base: &'a mut Base) -> Self {
        base.reset();
        base.file_type = RPGMFileType::Plugins;

        Self { base }
    }

    /// Processes the RPG Maker plugins file content.
    ///
    /// # Parameters
    ///
    /// - `content` - Content of the file that's being processed.
    /// - `translation` - Contents of the translation file corresponding to the file. Isn't used with [`ReadMode::Default`]. Requires to be set with any other [`Mode`].
    ///
    /// # Returns
    ///
    /// - Nothing if `mode` is [`Mode::Write`] and no translation exists.
    /// - [`ProcessedData`], which contains RPG Maker data if `mode` is [`Mode::Write`] and translation data otherwise.
    /// - [`Error`], if unable to parse the content.
    ///
    /// # Errors
    ///
    /// - [`Error::JsonParse`] - if parsing plugin JSON content fails.
    /// - [`Error::NoTranslation`] - if mode is not [`ReadMode::Default`], and no translation was passed.
    ///
    /// # Panics
    ///
    /// May panic if passed content is not `plugins.js`.
    ///
    /// # Example
    ///
    /// ```no_run
    /// use rvpacker_txt_rs_lib::{core::{Base, PluginBase}, Mode, ReadMode, EngineType, Error};
    /// use std::fs::read;
    ///
    /// fn main() -> Result<(), Box<dyn std::error::Error>> {
    ///     let mut base = Base::new(Mode::Read(ReadMode::Default(false)), EngineType::New);
    ///     let mut plugin_base = PluginBase::new(&mut base);
    ///
    ///     let plugins_file_content = read("plugins.js")?;
    ///     plugin_base.process(&plugins_file_content, None)?;
    ///     Ok(())
    /// }
    /// ```
    pub fn process(
        mut self,
        content: &[u8],
        translation: Option<&str>,
    ) -> Result<Option<ProcessedData>, Error> {
        self.base.initialize_translation(translation)?;

        // SAFETY: Plugins content should always be like `plugins = [...]`, and JSON is always valid UTF-8.
        let plugins_array_str = unsafe {
            std::str::from_utf8_unchecked(content)
                .split_once('=')
                .unwrap_unchecked()
                .1
                .trim_end_matches([';', '\r', '\n'])
        };

        // SAFETY: Plugins are always array.
        let mut plugins_array = unsafe {
            Value::from(from_str::<SerdeValue>(plugins_array_str)?)
                .into_array()
                .unwrap_unchecked()
        };

        let mut processed = false;

        for (plugin_id, plugin_object) in plugins_array.iter_mut().enumerate() {
            let id = plugin_id as u16 + 1;

            if self.base.get_translation_map(id).is_break() {
                if self.base.mode.is_purge() {
                    processed = true;
                }

                continue;
            }

            processed = true;

            // SAFETY: Each plugin always contains name.
            let plugin_name =
                unsafe { plugin_object["name"].as_str().unwrap_unchecked() };

            self.base.update_metadata(
                id,
                Vec::from([(CommentPos::Name, plugin_name)]),
            );
            self.parse_plugin(None, plugin_object);
            self.base.flush_translation(id);
        }

        if !processed {
            return Ok(None);
        }

        Ok(Some(self.base.finish(Value::array(plugins_array))))
    }

    fn parse_plugin(&mut self, key: Option<&str>, value: &mut Value) {
        let is_invalid_key = |key: Option<&str>| {
            let Some(key_string) = key else {
                return false;
            };

            if key_string.starts_with("LATIN") {
                false
            } else {
                PLUGINS_REGEXPS
                    .with(|r| r.iter().any(|re| re.is_match(key_string)))
            }
        };

        match &mut **value {
            ValueType::String(value_string) => {
                if is_invalid_key(key) {
                    return;
                }

                if !(value_string.trim().is_empty()
                    || IS_ONLY_SYMBOLS_RE.with(|r| r.is_match(value_string))
                    || ["true", "false", "none", "time", "off"]
                        .contains(&value_string.as_str())
                    || value_string.starts_with("this.")
                        && value_string
                            .chars()
                            .nth(5)
                            .is_some_and(char::is_alphabetic)
                        && value_string.ends_with(')')
                    || value_string.starts_with("rgba"))
                    || key.is_some_and(|x| x.starts_with("LATIN"))
                {
                    let mut string = value_string.normalize();
                    let old_string = take(&mut string);

                    if self.base.flags.contains(BaseFlags::Romanize) {
                        string = Base::romanize_string(&old_string);
                    } else {
                        string = old_string;
                    }

                    if self.base.mode.is_write() {
                        if !self.base.contains_key(&string) {
                            return;
                        }

                        if let Some(translated) = self.base.get_key(&string) {
                            *value = Value::string(translated.as_str());
                        }
                    } else {
                        self.base.insert_string(string);
                    }
                }
            }
            ValueType::Object(obj) => {
                for (key, value) in obj.iter_mut() {
                    self.parse_plugin(Some(key), value);
                }
            }
            ValueType::Array(arr) => {
                for value in arr {
                    self.parse_plugin(None, value);
                }
            }
            _ => {}
        }
    }
}
