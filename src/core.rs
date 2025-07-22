use crate::{constants::*, types::*};
use flate2::{Compression, read::ZlibDecoder, write::ZlibEncoder};
use gxhash::HashSetExt;
use indexmap::map::Entry;
use marshal_rs::{Get, Value, ValueType, dump, load_binary, load_utf8};
use regex::Regex;
use serde_json::{from_str, to_vec};
use smallvec::SmallVec;
use std::{
    borrow::Cow,
    cell::LazyCell,
    fs::DirEntry,
    io::{Read, Write},
    mem::{replace, take},
    ops::{ControlFlow, Range},
    path::Path,
};

macro_rules! mutable {
    ($var:expr, $t:ty) => {{
        #[allow(invalid_reference_casting)]
        unsafe {
            &mut *($var as *const $t as *mut $t)
        }
    }};
}

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
        Regex::new(r"\#").unwrap_unchecked()
    });
}

trait CustomReplace {
    fn replace_line_breaks(&self) -> Cow<'_, str>;
    fn replace_new_line(&self) -> Cow<'_, str>;
}

impl CustomReplace for str {
    fn replace_line_breaks(&self) -> Cow<'_, str> {
        LINE_BREAKS_RE.with(|re| re.replace_all(self, NEW_LINE))
    }

    fn replace_new_line(&self) -> Cow<'_, str> {
        LINE_BREAKS_RE.with(|re| re.replace_all(self, "\n"))
    }
}

/// Filters entries of `ReadDir` and returns iterator of only `Map` entries.
///
/// # Arguments
/// - `entries` - Entries read with `read_dir`.
/// - `engine_type` - [`EngineType`] of entries.
///
/// # Returns
/// Filtered iterator containing only `Map` entries.
pub fn filter_maps(
    entries: impl Iterator<Item = DirEntry>,
    engine_type: EngineType,
) -> impl Iterator<Item = DirEntry> {
    entries.filter_map(move |entry| {
        if !entry.file_type().ok()?.is_file() {
            return None;
        };

        let filename = entry.file_name();
        let extension = Path::new(&filename).extension()?;
        let filename_str = filename.to_str()?;

        if filename_str.starts_with("Map")
            && filename_str.as_bytes().get(3)?.is_ascii_digit()
            && extension == get_engine_extension(engine_type)
        {
            return Some(entry);
        }
        None
    })
}

/// Filters entries of `ReadDir` and returns iterator of only other entries.
///
/// # Arguments
/// - `entries` - Entries read with `read_dir`.
/// - `engine_type` - [`EngineType`] of entries.
/// - `game_type` - [`GameType`] of entries.
///
/// # Returns
/// Filtered iterator containing only other entries.
pub fn filter_other(
    entries: impl Iterator<Item = DirEntry>,
    engine_type: EngineType,
    game_type: GameType,
) -> impl Iterator<Item = DirEntry> {
    entries.filter_map(move |entry| {
        if !entry.file_type().ok()?.is_file() {
            return None;
        };

        let filename = entry.file_name();
        let filename_path = Path::new(&filename);
        let basename = filename_path
            .file_stem()
            .and_then(|basename| basename.to_str())?;
        let extension = filename_path.extension()?;

        let file_type = RPGMFileType::from_filename(basename);

        if extension == get_engine_extension(engine_type)
            && file_type.is_other()
        {
            if game_type.is_termina() && file_type.is_states() {
                return None;
            }

            return Some(entry);
        }

        None
    })
}

/// Returns the RPG Maker file extension that corresponds to given `EngineType`.
pub const fn get_engine_extension(engine_type: EngineType) -> &'static str {
    match engine_type {
        EngineType::New => "json",
        EngineType::VXAce => "rvdata2",
        EngineType::VX => "rvdata",
        EngineType::XP => "rxdata",
    }
}

/// Parses ignore file contents to `IgnoreMap`.
///
/// # Arguments
/// - `ignore_file_path` - Path to the `.rvpacker-ignore` file.
/// - `duplicate_mode` - [`DuplicateMode`], which was used during read.
/// - `read` - Parse for reading or purging.
///
/// # Returns
/// Parsed [`IgnoreMap`].
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

    ignore_map.reserve(256);
    ignore_map
        .insert(first_entry_comment.into(), IgnoreEntry::with_capacity(128));

    let mut ignore_entry =
        unsafe { ignore_map.last_mut().unwrap_unchecked().1 };

    for mut line in ignore_file_lines {
        if line.is_empty() {
            continue;
        }

        if line.starts_with(IGNORE_ENTRY_COMMENT) {
            // If duplicates are allowed, we should group all ignore entries
            // that correspond to a single file into one ignore entry.
            if read
                && duplicate_mode.is_remove()
                && !(line.contains("<#>System")
                    || line.contains("<#>Scripts")
                    || line.contains("<#>Plugins"))
            {
                line = &line[..unsafe { line.find(':').unwrap_unchecked() }];
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
/// # Arguments
/// * `ini_file_content` - Raw byte content of the INI file to parse
///
/// # Returns
/// * `Ok(Vec<u8>)` - Vector of extracted title's bytes. Title may not be UTF-8.
/// * `Err(Error::NoTitle)` - If no "Title" entry is found in the INI file
///
/// # Example
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

    fn split_lines(data: &[u8]) -> Vec<&[u8]> {
        let mut lines = Vec::with_capacity(4);
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
/// # Arguments
/// * `system_file_content` - JSON string content of the system file
///
/// # Returns
/// * `Ok(String)` - The game title extracted from the "gameTitle" field
/// * `Err(Error)` - If the JSON parsing fails or the "gameTitle" field is missing/invalid
///
/// # Example
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
pub fn get_system_title(system_file_content: &str) -> Result<String, Error> {
    let system_file_value: serde_json::Value = from_str(system_file_content)?;

    system_file_value["gameTitle"]
        .as_str()
        .map(Into::into)
        .ok_or(Error::NoTitle)
}

#[derive(Default)]
pub struct Base {
    pub mode: Mode,
    pub game_type: GameType,
    engine_type: EngineType,
    file_type: RPGMFileType,

    pub romanize: bool,
    pub trim: bool,

    pub read_mode: ReadMode,
    pub duplicate_mode: DuplicateMode,
    pub ignore: bool,

    pub create_ignore: bool,
    pub ignore_map: IgnoreMap,
    ignore_entry: IgnoreEntry,

    lines: Lines,
    translation_map: TranslationMap,
    translation_maps: IndexMapGx<String, TranslationMap>,
    translation_duplicate_map: TranslationDuplicateMap,
    lines_lengths: Vec<usize>,

    labels: Labels,
}

impl<'a> Base {
    /// Creates new base from mode and engine type.
    ///
    /// # Arguments
    /// - `mode` - [`Mode`] to use.
    /// - `engine_type` - [`EngineType`] to use.
    pub fn new(mode: Mode, engine_type: EngineType) -> Self {
        Self {
            mode,
            engine_type,
            labels: Labels::new(engine_type),
            ..Default::default()
        }
    }

    /// Sets engine type for the base.
    ///
    /// # Arguments
    /// - `engine_type` - [`EngineType`] to use.
    pub fn set_engine_type(&mut self, engine_type: EngineType) {
        self.engine_type = engine_type;
        self.labels = Labels::new(engine_type);
    }

    /// Clears all the underlying collections, and makes this base ready to be used in the next base.
    ///
    /// This function is used by file-specific bases' constructors, so you generally mustn't call it manually.
    pub fn reset(&mut self) {
        self.lines.clear();
        self.translation_map.clear();
        self.translation_maps.clear();
        self.translation_duplicate_map.clear();
        self.lines_lengths.clear();
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
            _ => {} // custom processing for other games
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

                // At this point, shop parameter should always contain '='.
                // Panic is unlikely.
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
            self.get_key(parameter).map(|translation| {
                let mut translation = translation.to_string();

                for (string, append) in extra_strings {
                    if append {
                        translation.push_str(string);
                    } else {
                        translation = format!("{string}{translation}");
                    }
                }

                translation
            })
        } else {
            Some(if self.romanize {
                Self::romanize_string(parameter)
            } else {
                parameter.to_string()
            })
        }
    }

    /// Returns the RPG Maker data if `self.mode` is [`Mode::Write`], else returns translation data.
    ///
    /// # Arguments
    /// - `value` - [`Value`] to use on write.
    ///
    /// # Returns
    /// RPG Maker data if `self.mode` is [`Mode::Write`], else returns translation data.
    fn finish(&mut self, value: Value) -> Vec<u8> {
        self.flush_translation(true);

        let output_content = if self.mode.is_write() {
            if self.file_type.is_plugins() {
                ["var $plugins =\n".as_bytes(), unsafe {
                    &to_vec(&serde_json::Value::from(value)).unwrap_unchecked()
                }]
                .concat()
            } else {
                if self.engine_type.is_new() {
                    unsafe {
                        to_vec(&serde_json::Value::from(value))
                            .unwrap_unchecked()
                    }
                } else {
                    dump(
                        value,
                        if self.file_type.is_scripts() {
                            None
                        } else {
                            INSTANCE_VAR_PREFIX
                        },
                    )
                }
            }
        } else {
            let mut output_content = self
                .translation_duplicate_map
                .iter()
                .map(|(source, translation_entry)| {
                    let (translation, comments) = translation_entry.parts();
                    let mut string = String::with_capacity(
                        source.len()
                            + SEPARATOR.len()
                            + translation.len()
                            + comments.iter().map(|c| c.len()).sum::<usize>()
                            + 1,
                    );

                    for comment in comments {
                        string.push_str(comment);
                        string.push('\n');
                    }

                    if !source.is_empty() {
                        string.push_str(source);
                        string.push_str(SEPARATOR);
                    }

                    if !translation.is_empty() {
                        string.push_str(translation);
                    }

                    if !source.is_empty() || !translation.is_empty() {
                        string.push('\n');
                    }

                    string
                })
                .collect::<String>()
                .into_bytes();
            output_content.pop();
            output_content
        };

        self.lines.clear();
        self.translation_duplicate_map.clear();
        output_content
    }

    fn process_param(
        &mut self,
        value: &mut Value,
        code: Code,
        parameter: &str,
    ) {
        let parameter = if self.romanize {
            Cow::Owned(Self::romanize_string(parameter))
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
            self.insert_string(parsed);
        }
    }

    /// Inserts `string` to `self.lines` if `self.mode`.
    ///
    /// Will skip inserting if `self.mode` is not [`Mode::Write`] or `self.ignore` is `true` and `self.ignore_entry` contains the string.
    ///
    /// # Arguments
    /// - `string` - String to insert in `self.lines`.
    fn insert_string(&mut self, string: String) {
        if !self.mode.is_write() {
            if self.ignore && self.ignore_entry.contains(&string) {
                return;
            }

            self.lines.insert(string);
        }
    }

    fn join_dialogue_lines(
        &mut self,
        list: &mut [Value],
        dialogue_lines: &mut SmallVec<[String; 4]>,
        dialogue_line_indices: &mut SmallVec<[usize; 4]>,
        write_string_literally: bool,
    ) {
        let mut joined = dialogue_lines.join(if self.mode.is_write() {
            "\n"
        } else {
            NEW_LINE
        });

        if self.mode.is_write() {
            if self.romanize {
                joined = Self::romanize_string(&joined);
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

                // We checked if `dialogue_lines` not empty before calling this.
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
    /// # Arguments
    ///
    /// - `list` - list of [`Value`]s.
    fn process_list(&mut self, list: &mut Vec<Value>) {
        let mut in_sequence = false;
        let mut write_string_literally = self.engine_type.is_new();
        let mut dialogue_lines = SmallVec::with_capacity(4);
        let mut dialogue_line_indices = SmallVec::with_capacity(4);

        for (item_idx, item) in
            mutable!(list, Vec<Value>).iter_mut().enumerate()
        {
            // Each item must contain code, panic is unlikely.
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
                // Each item must contain parameters, panic is unlikely.
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

            // Each item must contain parameters, panic is unlikely.
            let parameters = unsafe {
                item[self.labels.parameters]
                    .as_array_mut()
                    .unwrap_unchecked()
            };

            if parameters.is_empty() {
                continue;
            }

            let value_index = if code.is_any_misc() || code.is_choice() {
                1
            } else {
                0
            };

            let value = &mut parameters[value_index];

            if code.is_choice_array() {
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

    /// Gets ignore entry from `self.ignore_map` by `entry_name`.
    ///
    /// Skips getting an entry if `self.ignore` or `self.create_ignore` are set to `false`.
    ///
    /// # Arguments
    /// - `entry_name` - Name of the entry to get.
    ///
    /// # Note
    /// This function moves ignore entry from `self.ignore_map`, so it must be moved back with `reset_ignore_entry` later.
    fn get_ignore_entry(&mut self, entry_name: &str) {
        if self.ignore || self.create_ignore {
            let mut entry_name: &str =
                &format!("{file}: {entry_name}", file = self.file_type);

            if self.ignore && self.duplicate_mode.is_remove() {
                entry_name = &entry_name
                    [..unsafe { entry_name.find(':').unwrap_unchecked() }];
            }

            self.ignore_entry = take(
                self.ignore_map
                    .entry(format!(
                        "{IGNORE_ENTRY_COMMENT}{SEPARATOR}{entry_name}"
                    ))
                    .or_default(),
            );
        }
    }

    /// Moves ignore entry owned by `self.ignore_entry` back to `self.ignore_map`.
    ///
    /// # Arguments
    /// - `entry_name` - Name of the entry to get.
    fn reset_ignore_entry(&mut self, entry_name: &str) {
        self.ignore_map
            .entry(format!("{IGNORE_ENTRY_COMMENT}{SEPARATOR}{entry_name}"))
            .and_modify(|entry| *entry = take(&mut self.ignore_entry));
    }

    /// Parses RPG Maker file from passed content.
    ///
    /// This function determines how to parse the content by `self.engine_type`, and assumes it always set correctly.
    ///
    /// # Arguments
    /// - `content` - Content of file to parse.
    ///
    /// # Returns
    /// - `Ok(Value)` - if file was parsed correctly.
    /// - `Err(Error)` - if was unabled to deserialize the file.
    fn parse_rpgm_file(&mut self, mut content: &[u8]) -> Result<Value, Error> {
        if self.engine_type.is_new() {
            const BOM: [u8; 3] = [0xEF, 0xBB, 0xBF];

            // MZ includes Byte Order Mark in files.
            if content.starts_with(&BOM) {
                content = &content[3..]
            }

            let parsed = from_str::<serde_json::Value>(unsafe {
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
    /// # Arguments
    /// - `translation` - translation file content to parse.
    fn initialize_translation(&mut self, translation: &str) {
        if !self.mode.is_read() || self.read_mode.is_append() {
            self.translation_maps.clear();

            let mut translation_lines =
                translation.lines().enumerate().peekable();

            let map_start_comment_prefix = if self.file_type.is_map() {
                MAP_ID_COMMENT
            } else if self.file_type.is_other() {
                if self.game_type.is_termina() && self.file_type.is_items() {
                    for _ in 0..4 {
                        let (_, item_category_line) = unsafe {
                            translation_lines.next().unwrap_unchecked()
                        };

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

                    self.translation_maps.insert(
                        ADDITIONAL_HASHMAP_LABEL.into(),
                        take(&mut self.translation_map),
                    );
                }

                EVENT_ID_COMMENT
            } else if self.file_type.is_system() {
                SYSTEM_ENTRY_COMMENT
            } else if self.file_type.is_plugins() {
                PLUGIN_ID_COMMENT
            } else {
                SCRIPT_ID_COMMENT
            };

            loop {
                let Some((_, next)) = translation_lines.next() else {
                    return;
                };

                // Push the first line in iterator to comments
                // If it's not a comment, then something is wrong.
                let mut comments = Vec::with_capacity(4);
                comments.push(next.into());

                while let Some((_, line)) = translation_lines.peek() {
                    if line.starts_with(map_start_comment_prefix) {
                        break;
                    }

                    let (i, line) =
                        unsafe { translation_lines.next().unwrap_unchecked() };

                    if line.starts_with(COMMENT_PREFIX) {
                        comments.push(line.to_string());
                        continue;
                    }

                    let split: Vec<&str> = line.split(SEPARATOR).collect();

                    if split.len() < 2 {
                        log::warn!(
                            "{COULD_NOT_SPLIT_LINE_MSG}\n{AT_POSITION_MSG}: {i}\n{IN_FILE_MSG}: {file}.txt",
                            i = i + 1,
                            file = self.file_type.to_string().to_lowercase()
                        );
                        comments.clear();
                        continue;
                    }

                    let source = Cow::Borrowed(*unsafe {
                        split.first().unwrap_unchecked()
                    });

                    let translation = Cow::Borrowed(
                        split
                            .into_iter()
                            .skip(1)
                            .rfind(|x| !x.is_empty())
                            .unwrap_or_default(),
                    );

                    let (source, translation) = if self.trim {
                        (
                            Cow::Borrowed(source.trim()),
                            Cow::Borrowed(translation.trim()),
                        )
                    } else {
                        (source, translation)
                    };

                    let (source, translation) = if self.mode.is_write() {
                        if translation.is_empty() {
                            continue;
                        }

                        (
                            source.replace_new_line(),
                            translation.replace_new_line(),
                        )
                    } else {
                        (source, translation)
                    };

                    self.translation_map.insert(
                        source.into(),
                        TranslationEntry::new(
                            translation.into(),
                            take(&mut comments),
                        ),
                    );
                }

                if self.translation_map.is_empty() {
                    if self.mode.is_write() {
                        continue;
                    }

                    self.translation_map.insert(
                        String::new(),
                        TranslationEntry::new(
                            String::new(),
                            take(&mut comments),
                        ),
                    );
                }

                let comments = self.translation_map.comments();

                if comments.is_empty() {
                    continue;
                }

                self.translation_maps.insert(
                    unsafe {
                        comments[0].rsplit_once(SEPARATOR).unwrap_unchecked()
                    }
                    .1
                    .into(),
                    replace(
                        &mut self.translation_map,
                        TranslationMap::with_capacity(512),
                    ),
                );
            }
        }
    }

    /// Sets `self.translation_map` to the entry from `self.translation_maps`.
    ///
    /// # Arguments
    /// - `entry_name` - Name of the entry to get.
    ///
    /// # Returns
    /// - `ControlFlow::Break` - If `self.mode` is write, and entry corresponding to the `entry_name` does not exist.
    /// - `ControlFlow::Continue` - In other situations.
    ///
    /// # Note
    /// This function moves map from `self.translation_maps`, so it must be moved back with `reset_translation_map` later.
    fn get_translation_map(&mut self, entry_name: &str) -> ControlFlow<()> {
        let entry = self.translation_maps.entry(entry_name.into());

        // Move a map from `translation_maps` to `translation_map`.
        self.translation_map = take(match entry {
            Entry::Occupied(entry) => entry.into_mut(),
            Entry::Vacant(entry) => {
                if self.mode.is_write() {
                    return ControlFlow::Break(());
                }

                entry.insert(Default::default())
            }
        });

        ControlFlow::Continue(())
    }

    /// Moves map owned by `self.translation_map` back to `self.translation_maps`.
    ///
    /// # Arguments
    /// - `entry_name` - Name of the entry to get.
    fn reset_translation_map(&mut self, entry_name: &str) {
        // Move the map back to `translation_maps`.
        self.translation_maps
            .entry(entry_name.into())
            .and_modify(|entry| *entry = take(&mut self.translation_map));
    }

    /// Wraps string in [`Value`].
    ///
    /// If `literal` argument is set, this wraps string in [`Value`] of [`ValueType::String`] type.
    /// Else, this wraps string in [`Value`] of [`ValueType::Bytes`] type.
    ///
    /// # Arguments
    /// - `string` - String to wrap in [`Value`].
    /// - `literal` - Whether to wrap `string` as [`ValueType::String`] or as [`ValueType::Bytes`].
    ///
    /// # Returns
    /// - [`Value`] - wrapped string.
    fn make_string_value(string: &str, literal: bool) -> Value {
        if literal {
            Value::string(string)
        } else {
            Value::bytes(string.as_bytes())
        }
    }

    /// Replaces Eastern symbols in string to their Western (or sort of Western) equivalents.
    ///
    /// # Arguments
    /// - `string` - String to romanize.
    ///
    /// # Returns
    /// - `String` - Romanized string.
    fn romanize_string(string: &str) -> String {
        let mut result = String::with_capacity(string.len());

        for char in string.chars() {
            let replacement = match char {
                '。' => ".",
                '、' | '，' => ",",
                '・' => "·",
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
                '※' => "·",
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
                    result.push(char);
                    continue;
                }
            };

            result.push_str(replacement);
        }

        result
    }

    fn string_is_only_symbols(string: &str) -> bool {
        !string.chars().any(|c| !SYMBOLS.contains(&c))
    }

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
                        stage = 3;
                    } else {
                        return None;
                    }
                }
                3 => {
                    if char == ' ' {
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
                let char = &string[index..index + 1];

                if char != "]" {
                    index += 1;
                } else {
                    return Some(index + 1);
                }

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
    /// Will always return `None` if [`Value`] is not of [`ValueType::String`] or [`ValueType::Bytes`].
    ///
    /// # Arguments
    /// - `value` - Value from which string will be extracted.
    /// - `ret` - Whether to return if extracted string happens to be empty.
    ///
    /// # Returns
    /// - `None` - If [`Value`] is not of [`ValueType::String`] or [`ValueType::Bytes`], or `ret` is set and `string` is empty.
    /// - `Some(&str)` - Parsed string.
    fn extract_string(
        &'a self,
        value: &'a Value,
        ret: bool,
    ) -> Option<&'a str> {
        let string = value.as_str().or_else(|| {
            std::str::from_utf8(value.as_byte_vec().unwrap_or_default()).ok()
        })?;

        let trimmed = string.trim();

        if trimmed.is_empty() && ret {
            return None;
        }

        Some(if self.trim { trimmed } else { string })
    }

    /// Checks if `key` is present in translation.
    ///
    /// This will check if `key` is present in `self.translation_map`, and also will seek it in maps of `self.translation_maps` if `self.duplicate_mode` is [`DuplicateMode::Remove`].
    ///
    /// # Arguments
    /// - `key` - key to find in translation.
    ///
    /// # Returns
    /// - `bool` - whether the key is present in translation.
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
    /// # Arguments
    /// - `key` - key to get.
    ///
    /// # Returns
    /// - `None` - if key wasn't found in translation.
    /// - `Some(&TranslationEntry)` - entry corresponding to the `key`.
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

    fn push_comments(&mut self, map: &TranslationMap) {
        if map.first().is_some_and(|x| x.0.is_empty()) {
            self.translation_duplicate_map.extend(
                map.comments()
                    .iter()
                    .map(|k| (String::new(), k.clone().into())),
            );
        } else {
            for (k, v) in map
                .iter()
                .take_while(|(k, _)| k.starts_with(COMMENT_PREFIX))
            {
                self.translation_duplicate_map.push((k.clone(), v.clone()));
            }
        }
    }

    /// Purges entries with empty translation from `self.translation_map`, and drains the resulting entries to `self.translation_duplicate_map`.
    ///
    /// Will add purged entries to `self.ignore_entry` if `self.create_ignore` is true.
    fn purge_empty_translation(&mut self) {
        let len_limit = if self.file_type.is_system() {
            self.translation_map.len().wrapping_sub(1)
        } else {
            self.translation_map.len()
        };

        self.translation_duplicate_map
            .reserve(self.translation_map.len());
        self.translation_duplicate_map.extend(
            self.translation_map.drain(..).enumerate().map(
                |(i, (mut k, v))| {
                    if i < len_limit
                        && !k.starts_with(COMMENT_PREFIX)
                        && v.is_empty()
                    {
                        let moved = take(&mut k);

                        if self.create_ignore && !moved.is_empty() {
                            self.ignore_entry.insert(moved);
                        }
                    }

                    (k, v)
                },
            ),
        );
    }

    /// Flushes translation from `self.translation_map` and `self.lines` to `self.translation_duplicate_map`.
    ///
    /// # Arguments
    /// - `finish` - whether this function is called from `finish` function.
    fn flush_translation(&mut self, finish: bool) {
        if self.duplicate_mode.is_allow()
            || !(self.file_type.is_map() || self.file_type.is_other())
        {
            if self.read_mode.is_append() {
                self.translation_duplicate_map
                    .reserve(self.lines.capacity() + 5);
                mutable!(self, Self).push_comments(&self.translation_map);

                for key in take(&mut self.lines) {
                    let val = self
                        .translation_map
                        .swap_remove(&key)
                        .unwrap_or_default();
                    self.translation_duplicate_map.push((key, val));
                }
            } else {
                self.translation_duplicate_map
                    .reserve(self.translation_map.len() + self.lines.len());
                self.translation_duplicate_map.extend(
                    self.translation_map.drain(..).chain(
                        take(&mut self.lines)
                            .into_iter()
                            .map(|k| (k, TranslationEntry::default())),
                    ),
                );
            }
        } else if finish && self.mode.is_read() {
            if self.read_mode.is_append() {
                self.translation_duplicate_map.reserve(
                    self.lines.capacity() + (self.translation_maps.len() * 5),
                );

                let mut map_index = 0;

                for key in take(&mut self.lines) {
                    let mut val = TranslationEntry::default();

                    for (i, map) in
                        self.translation_maps.values_mut().enumerate()
                    {
                        if let Some(entry) = map.swap_remove(&key) {
                            while i > map_index {
                                mutable!(self, Self).push_comments(
                                    &self.translation_maps[map_index],
                                );
                                map_index += 1;
                            }

                            val = entry;
                            break;
                        }
                    }

                    self.translation_duplicate_map.push((key, val));
                }

                if map_index < self.translation_maps.len() - 1 {
                    if let Some(kv) = self.translation_maps.last() {
                        mutable!(self, Self).push_comments(kv.1);
                    }
                }
            } else {
                self.translation_duplicate_map.reserve(
                    self.translation_maps
                        .values()
                        .map(|m| m.len())
                        .sum::<usize>()
                        + self.lines_lengths.iter().sum::<usize>(),
                );
                let mut lines_iter = take(&mut self.lines).into_iter();

                for (i, map) in
                    take(&mut self.translation_maps).into_values().enumerate()
                {
                    self.translation_duplicate_map.extend(map);

                    for _ in 0..self.lines_lengths[i] {
                        self.translation_duplicate_map.push((
                            unsafe { lines_iter.next().unwrap_unchecked() },
                            TranslationEntry::default(),
                        ));
                    }
                }
            }
        }
    }

    fn process_comments(&mut self, id: &str, name: &str) {
        if !self.mode.is_read() {
            return;
        }

        let (id_comment, name_comment) = match self.file_type {
            RPGMFileType::Plugins => (PLUGIN_ID_COMMENT, PLUGIN_NAME_COMMENT),
            RPGMFileType::Scripts => (SCRIPT_ID_COMMENT, SCRIPT_NAME_COMMENT),
            // Covers only other, map and system don't call this function.
            _ => (EVENT_ID_COMMENT, EVENT_NAME_COMMENT),
        };

        if self.read_mode.is_append() {
            self.translation_map.comments_mut()[1] =
                format!("{name_comment}{SEPARATOR}{name}");
        } else {
            self.translation_map.extend([
                (id_comment.into(), id.into()),
                (name_comment.into(), name.into()),
            ]);
        }
    }

    /// Appends additional data directly to `self.translation_duplicate_map` based on some criteria.
    fn append_additional_data(&mut self) {
        if !self.mode.is_write()
            && self.game_type.is_termina()
            && self.file_type.is_items()
        {
            self.translation_duplicate_map.extend([
                (
                    String::from("<Menu Category: Items>"),
                    TranslationEntry::default(),
                ),
                (
                    String::from("<Menu Category: Food>"),
                    TranslationEntry::default(),
                ),
                (
                    String::from("<Menu Category: Healing>"),
                    TranslationEntry::default(),
                ),
                (
                    String::from("<Menu Category: Body bag>"),
                    TranslationEntry::default(),
                ),
            ]);
        }
    }
}

/// Base for processing `Map` files.
pub struct MapBase<'a> {
    pub base: &'a mut Base,
    mapinfos: Value,
}

impl<'a> MapBase<'a> {
    /// Initializes system base using `Base`.
    /// Before calling this, you should create a base and pass it here.
    ///
    /// # Example
    /// ```
    /// use rvpacker_txt_rs_lib::{core::{Base, MapBase}, Mode, EngineType};
    ///
    /// let mut base = Base::new(Mode::Read, EngineType::VXAce);
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

    /// Initializes map info from `Mapinfos` file.
    ///
    /// Required before calling `process` function.
    ///
    /// # Returns
    /// Returns [`Error`] if passed content is not RPG Maker file.
    ///
    /// # Example
    /// ```no_run
    /// use rvpacker_txt_rs_lib::{core::{Base, MapBase}, Mode, EngineType, Error};
    /// use std::fs::read;
    ///
    /// fn main() -> Result<(), Box<dyn std::error::Error>> {
    ///     let mut base = Base::new(Mode::Read, EngineType::VXAce);
    ///     let mut map_base = MapBase::new(&mut base);
    ///
    ///     let mapinfos = read("C:/Game/Data/Mapinfos.rvdata2")?;
    ///     map_base.initialize_mapinfos(&mapinfos)?;
    ///     Ok(())
    /// }
    /// ```
    pub fn initialize_mapinfos(&mut self, content: &[u8]) -> Result<(), Error> {
        self.mapinfos = self.base.parse_rpgm_file(content)?;
        Ok(())
    }

    /// Initializes the translation from `.txt` file contents.
    ///
    /// Required when reading with `Append` read mode, or when writing/purging.
    ///
    /// # Example
    /// ```no_run
    /// use rvpacker_txt_rs_lib::{core::{Base, MapBase}, Mode, EngineType, Error};
    /// use std::fs::read_to_string;
    ///
    /// fn main() -> Result<(), Box<dyn std::error::Error>> {
    ///     let mut base = Base::new(Mode::Read, EngineType::VXAce);
    ///     let mut map_base = MapBase::new(&mut base);
    ///
    ///     let translation_file_content = read_to_string("maps.txt")?;
    ///     map_base.initialize_translation(&translation_file_content);
    ///     Ok(())
    /// }
    /// ```
    pub fn initialize_translation(&mut self, translation: &str) {
        self.base.initialize_translation(translation);
    }

    /// Returns the translation data, accumulated after processing multiple maps.
    ///
    /// Returns the actual data only when reading or purging.
    ///
    /// # Example
    /// ```no_run
    /// use rvpacker_txt_rs_lib::{core::{Base, MapBase}, Mode, EngineType, Error};
    /// use std::fs::read;
    ///
    /// fn main() -> Result<(), Box<dyn std::error::Error>> {
    ///     let mut base = Base::new(Mode::Read, EngineType::VXAce);
    ///     let mut map_base = MapBase::new(&mut base);
    ///
    ///     let map_file_content = read("C:/Game/Data/Map001.rvdata2")?;
    ///     let data = map_base.process("Map001.rvdata2", &map_file_content)?;
    ///
    ///     let map_file_content = read("C:/Game/Data/Map002.rvdata2")?;
    ///     let data = map_base.process("Map002.rvdata2", &map_file_content)?;
    ///
    ///     let translation_data = map_base.translation();
    ///     Ok(())
    /// }
    /// ```
    pub fn translation(&mut self) -> Vec<u8> {
        self.base.finish(Value::default())
    }

    /// Processes the RPG Maker file content and returns:
    /// - Ok(None) if map is unused (not included in Mapinfos), or mode is [`Mode::Write`] and no translation exists for the map.
    /// - RPG Maker data with applied translation, if `mode` is [`Mode::Write`].
    ///
    /// To get the translation data, you need to call `translation` after processing required maps.
    ///
    /// If you use [`Mode::Write`], [`Mode::Purge`] or [`Mode::Read`] with [`ReadMode::Append`],
    /// you should initialize the translation using `initialize_translation` function.
    ///
    /// # Returns
    /// - RPG Maker data with applied translation, if `mode` is [`Mode::Write`].
    /// - Translation file data in other cases.
    /// - [`Error`], if unable to parse the content.
    ///
    /// # Panics
    /// May panic if passed content is not from `Map` file.
    ///
    /// # Example
    /// ```no_run
    /// use rvpacker_txt_rs_lib::{core::{Base, MapBase}, Mode, EngineType, Error};
    /// use std::fs::read;
    ///
    /// fn main() -> Result<(), Box<dyn std::error::Error>> {
    ///     let mut base = Base::new(Mode::Read, EngineType::VXAce);
    ///     let mut map_base = MapBase::new(&mut base);
    ///
    ///     let map_file_content = read("C:/Game/Data/Map001.rvdata2")?;
    ///     let data = map_base.process("Map001.rvdata2", &map_file_content)?;
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
    ) -> Result<Option<Vec<u8>>, Error> {
        let map_id = Self::parse_map_id(filename);
        if self.is_map_unused(map_id) {
            return Ok(None);
        }

        let map_id_string = map_id.to_string();
        let flow = self.base.get_translation_map(&map_id_string);
        if flow.is_break() {
            return Ok(None);
        }

        self.base.get_ignore_entry(&map_id_string);

        let result = if self.base.mode.is_purge() {
            self.base.purge_empty_translation();
            Ok(None)
        } else {
            let mut map_object = self.base.parse_rpgm_file(content)?;

            let display_name = self.get_display_name(&map_object);
            let display_name_comment = format!(
                "{MAP_DISPLAY_NAME_COMMENT_PREFIX}{display_name}{COMMENT_SUFFIX}"
            );

            if self.base.mode.is_read() {
                let mut comments_inserted = true;

                let map_order = self.get_map_order(map_id).to_string();
                let mut map_name = mutable!(self, Self).get_map_name(map_id);

                let replaced_map_name = map_name.replace_line_breaks();
                map_name = &replaced_map_name;

                if self.base.read_mode.is_append() {
                    if self.base.translation_map.first().is_some() {
                        let start_comments =
                            self.base.translation_map.comments_mut();
                        let mut slots = vec![String::new(); 4];

                        for comment in start_comments.iter() {
                            if comment.starts_with(MAP_ID_COMMENT) {
                                slots[0] = comment.clone();
                            } else if comment.starts_with(MAP_ORDER_COMMENT) {
                                slots[1] = comment.clone();
                            } else if comment.starts_with(MAP_NAME_COMMENT) {
                                slots[2] = comment.clone();
                            } else if comment
                                .starts_with(MAP_DISPLAY_NAME_COMMENT_PREFIX)
                            {
                                slots[3] = comment.clone();
                            }
                        }

                        *start_comments = slots;

                        let map_name_start_comment = &mut start_comments[2];
                        *map_name_start_comment =
                            format!("{MAP_NAME_COMMENT}{SEPARATOR}{map_name}");

                        let display_name_start_comment = &mut start_comments[3];

                        let translation = display_name_start_comment
                            .get(
                                display_name_start_comment
                                    .rfind(SEPARATOR)
                                    .unwrap_or_default()
                                    + SEPARATOR.len()..,
                            )
                            .unwrap_or_default();

                        *display_name_start_comment = format!(
                            "{display_name_comment}{SEPARATOR}{translation}"
                        );
                    } else {
                        comments_inserted = false;
                    }
                } else {
                    comments_inserted = false;
                }

                if !comments_inserted {
                    self.base.translation_map.extend([
                        (MAP_ID_COMMENT.into(), map_id_string.as_str().into()),
                        (MAP_ORDER_COMMENT.into(), map_order.into()),
                        (MAP_NAME_COMMENT.into(), map_name.into()),
                        (display_name_comment, TranslationEntry::default()),
                    ]);
                }
            } else if !display_name.is_empty() {
                let display_name_comment_line =
                    &self.base.translation_map.comments()[3];

                let split: Vec<&str> =
                    display_name_comment_line.split(SEPARATOR).collect();

                if split.len() >= 2 {
                    let mut translation = split
                        .into_iter()
                        .skip(1)
                        .rfind(|x| !x.is_empty())
                        .unwrap_or_default();

                    let translation_replaced = translation.replace_new_line();
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

            let events = if self.base.engine_type.is_new() {
                // Always an array in new maps, unlikely to panic.
                EventIterator::New(unsafe {
                    map_object[self.base.labels.events]
                        .as_array_mut()
                        .unwrap_unchecked()
                        .iter_mut()
                        .skip(1)
                })
            } else {
                // Always a hashmap in old maps, unlikely to panic.
                EventIterator::Old(unsafe {
                    map_object[self.base.labels.events]
                        .as_hashmap_mut()
                        .unwrap_unchecked()
                        .values_mut()
                })
            };

            for event in events {
                // TODO: Parse event name
                if event.is_null() {
                    continue;
                }

                let Some(pages) = event[self.base.labels.pages].as_array_mut()
                else {
                    continue;
                };

                for page in pages {
                    // List is always in map files, unlikely to panic.
                    let list = unsafe {
                        page[self.base.labels.list]
                            .as_array_mut()
                            .unwrap_unchecked()
                    };

                    self.base.process_list(list);
                }
            }

            if self.base.mode.is_write() {
                Ok(Some(self.base.finish(map_object)))
            } else {
                if self.base.duplicate_mode.is_remove()
                    && !self.base.read_mode.is_append()
                {
                    self.base.lines_lengths.push(
                        self.base.lines.len()
                            - self.base.lines_lengths.iter().sum::<usize>(),
                    );
                }

                self.base.flush_translation(false);
                Ok(None)
            }
        };

        self.base.reset_ignore_entry(&map_id_string);
        self.base.reset_translation_map(&map_id_string);
        result
    }

    /// Parses a map ID from a filename by extracting the substring at positions 3 to 5 and converting parsing it to `i32`.
    ///
    /// # Arguments
    /// * `filename` - Filename of the map.
    ///
    /// # Returns
    /// * `i32` - The parsed map ID.
    fn parse_map_id(filename: &str) -> i32 {
        // We discarded all files, which don't contain a digit at index 3, so
        // panic is unlikely.
        unsafe { filename[3..=5].parse::<i32>().unwrap_unchecked() }
    }

    /// Determines whether a map is unused based on its existance in `self.mapinfos`.
    ///
    /// # Arguments
    /// * `id` - The ID of the map to check.
    ///
    /// # Returns
    /// * `bool` - Whether map is unused.
    fn is_map_unused(&self, id: i32) -> bool {
        // If map ID can't be found in mapinfos, then it is unused in game.
        if self.base.engine_type.is_new() {
            self.mapinfos.get_index(id as usize).is_none()
        } else {
            self.mapinfos.get(&Value::int(id)).is_none()
        }
    }

    /// Retrieves the chronological map order from `self.mapinfos`.
    ///
    /// # Arguments
    /// * `id` - The ID of the map whose order should be retrieved.
    ///
    /// # Returns
    /// * `i32` - The map's order.
    fn get_map_order(&self, id: i32) -> i32 {
        unsafe {
            if self.base.engine_type.is_new() {
                &self.mapinfos[id as usize]["order"]
            } else {
                &self.mapinfos[Value::int(id)]["order"]
            }
            .as_int()
            .unwrap_unchecked()
        }
    }

    /// Retrieves the name of the map as a string slice, based on the provided map ID.
    ///
    /// # Arguments
    /// * `id` - The ID of the map whose name should be retrieved.
    ///
    /// # Returns
    /// * `&str` - The name of the map.
    fn get_map_name(&self, id: i32) -> &str {
        unsafe {
            if self.base.engine_type.is_new() {
                &self.mapinfos[id as usize]["name"]
            } else {
                &self.mapinfos[Value::int(id)]["name"]
            }
            .as_str()
            .unwrap_unchecked()
        }
    }

    /// Retrieves a display name for a map object.
    ///
    /// # Arguments
    /// * `map_object` - A reference to a `Value` representing the map object.
    ///
    /// # Returns
    /// * `String` - The processed display name, or an empty string if not found.
    fn get_display_name(&self, map_object: &Value) -> String {
        map_object
            .get(self.base.labels.display_name)
            .map(|display_name| {
                display_name
                    .as_str()
                    .map(|name| {
                        let name_replaced = name.replace_line_breaks();

                        if self.base.romanize {
                            Base::romanize_string(&name_replaced)
                        } else {
                            name_replaced.into_owned()
                        }
                    })
                    .unwrap_or_default()
            })
            .unwrap_or_default()
    }
}

/// Base for processing other files (`Actors`, `Armors`, `Classes`, `Enemies`,` `CommonEvents`, `Troops`, `Items`, `Skills`, `States`, `Weapons`).
pub struct OtherBase<'a> {
    pub base: &'a mut Base,
}

impl<'a> OtherBase<'a> {
    /// Initializes system base using `Base`.
    /// Before calling this, you should create a base and pass it here.
    ///
    /// # Example
    /// ```
    /// use rvpacker_txt_rs_lib::{core::{Base, OtherBase}, Mode, EngineType};
    ///
    /// let mut base = Base::new(Mode::Read, EngineType::VXAce);
    /// let mut other_base = OtherBase::new(&mut base);
    /// ```
    pub fn new(base: &'a mut Base) -> Self {
        base.reset();
        base.file_type = RPGMFileType::Invalid;

        Self { base }
    }

    /// Initializes the translation from `.txt` file contents.
    ///
    /// Required when reading with `Append` read mode, or when writing/purging.
    ///
    /// # Example
    /// ```no_run
    /// use rvpacker_txt_rs_lib::{core::{Base, OtherBase}, Mode, EngineType, Error};
    /// use std::fs::read_to_string;
    ///
    /// fn main() -> Result<(), Box<dyn std::error::Error>> {
    ///     let mut base = Base::new(Mode::Read, EngineType::VXAce);
    ///     let mut other_base = OtherBase::new(&mut base);
    ///
    ///     let translation_file_content = read_to_string("actors.txt")?;
    ///     other_base.initialize_translation("actors.txt", &translation_file_content);
    ///     Ok(())
    /// }
    /// ```
    pub fn initialize_translation(
        &mut self,
        filename: &str,
        translation: &str,
    ) {
        self.base.file_type = RPGMFileType::from_filename(filename);
        self.base.initialize_translation(translation);
    }

    /// Processes the RPG Maker file content and returns:
    /// - RPG Maker data with applied translation, if `mode` is [`Mode::Write`].
    /// - Translation file data in other cases.
    ///
    /// If you use [`Mode::Write`], [`Mode::Purge`] or [`Mode::Read`] with [`ReadMode::Append`],
    /// you should initialize the translation using `initialize_translation` function.
    ///
    /// # Example
    /// ```no_run
    /// use rvpacker_txt_rs_lib::{core::{Base, OtherBase}, Mode, EngineType, Error};
    /// use std::fs::read;
    ///
    /// fn main() -> Result<(), Box<dyn std::error::Error>> {
    ///     let mut base = Base::new(Mode::Read, EngineType::VXAce);
    ///     let mut other_base = OtherBase::new(&mut base);
    ///
    ///     let other_file_content = read("C:/Game/Data/Actors.rvdata2")?;
    ///     other_base.process("Actors.rvdata2", &other_file_content)?;
    ///     Ok(())
    /// }
    /// ```
    /// # Returns
    /// - RPG Maker data with applied translation, if `mode` is [`Mode::Write`].
    /// - Translation file data in other cases.
    /// - [`Error`], if unable to parse the content.
    ///
    /// # Panics
    /// May panic if passed content is not `Actors`, `Armors`, `Classes`, `Enemies`,` `CommonEvents`, `Troops`, `Items`, `Skills`, `States`, `Weapons`.
    pub fn process(
        &mut self,
        filename: &str,
        content: &[u8],
    ) -> Result<Vec<u8>, Error> {
        self.base.file_type = RPGMFileType::from_filename(filename);

        self.base.append_additional_data();
        let mut entry_value = self.base.parse_rpgm_file(content)?;
        self.base.lines_lengths.clear();

        // All "other" entries are always arrays, unlikely to panic.
        let object_array =
            unsafe { entry_value.as_array_mut().unwrap_unchecked() };

        // Skipping one, because the first entry is always null.
        for object in object_array.iter_mut().skip(1) {
            // Name and ID exists on every object, unlikely to panic.
            let event_name = unsafe {
                object[self.base.labels.name].as_str().unwrap_unchecked()
            };
            let event_id =
                unsafe { object["id"].as_int().unwrap_unchecked().to_string() };

            let flow = self.base.get_translation_map(&event_id);
            if flow.is_break() {
                continue;
            }

            self.base.get_ignore_entry(&event_id);

            if self.base.mode.is_purge() {
                self.base.purge_empty_translation();
            } else {
                self.base.process_comments(&event_id, event_name);

                if self.base.file_type.is_events()
                    || self.base.file_type.is_troops()
                {
                    self.process_object(object);
                } else {
                    self.process_array(object);
                }

                if self.base.duplicate_mode.is_remove()
                    && !self.base.read_mode.is_append()
                {
                    self.base.lines_lengths.push(
                        self.base.lines.len()
                            - self.base.lines_lengths.iter().sum::<usize>(),
                    );
                }

                self.base.flush_translation(false);
            }

            self.base.reset_ignore_entry(&event_id);
            self.base.reset_translation_map(&event_id);
        }

        Ok(self.base.finish(entry_value))
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
                            } else if !self.base.mode.is_write() {
                                return None;
                            }
                        } else if note.ends_with(['.', '%', '!', '"'])
                            || note.ends_with("takes place?")
                        {
                            note_string = note.into();
                        } else if !self.base.mode.is_write() {
                            return None;
                        }

                        if note_string.is_empty() {
                            if !self.base.mode.is_write() {
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
                                &self.base.translation_maps
                                    [ADDITIONAL_HASHMAP_LABEL][string],
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
                    } else {
                        variable_text = Cow::Owned(text);
                    }
                } else {
                    return None;
                }
            }
            _ => {} // custom processing for other games
        }

        if self.base.romanize {
            variable_text = Cow::Owned(Base::romanize_string(&variable_text));
        }

        if !self.base.mode.is_write() {
            return Some(variable_text.into_owned());
        }

        let translated = self.base.get_key(&variable_text).map(|translated| {
            let mut result = translated.to_string();

            for (string, position) in remaining_strings.into_iter() {
                match position {
                    true => result += &string,
                    false => result = string + &result,
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
                                result = String::from('\n') + &result
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

    fn process_object(&mut self, object: &mut Value) {
        if self.base.file_type.is_troops() {
            // Troops always include pages, panic is unlikely.
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
            // Commonevents always include list, panic is unlikely.
            let list = unsafe {
                object[self.base.labels.list]
                    .as_array_mut()
                    .unwrap_unchecked()
            };

            self.base.process_list(list);
        }
    }

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
            let Some(value) = array.get(variable_label) else {
                continue;
            };

            let Some(string) = self.base.extract_string(value, true) else {
                continue;
            };

            let mut string = if self.base.mode.is_write() && self.base.romanize
            {
                Cow::Owned(Base::romanize_string(string))
            } else {
                Cow::Borrowed(string)
            };

            string = if !self.base.mode.is_read() {
                Cow::Owned(
                    string
                        .lines()
                        .map(str::trim)
                        .collect::<Vec<_>>()
                        .join("\n"),
                )
            } else {
                string
            };

            let note_text = if self.base.game_type.is_termina()
                && variable_type.is_description()
            {
                array[self.base.labels.note].as_str()
            } else {
                None
            };

            let this = mutable!(self, Self);
            let Some(parsed) =
                this.process_variable(&string, note_text, variable_type)
            else {
                if variable_type.is_name() {
                    return;
                }

                continue;
            };

            if self.base.mode.is_write() {
                array[variable_label] = Value::string(parsed);
                continue;
            } else {
                let replaced = unsafe {
                    parsed
                        .lines()
                        .map(|line| {
                            let trimmed = if variable_type.is_any_message()
                                || self.base.trim
                            {
                                line.trim()
                            } else {
                                line
                            };

                            format!("{trimmed}{NEW_LINE}")
                        })
                        .collect::<String>()
                        .strip_suffix(NEW_LINE)
                        .unwrap_unchecked()
                }
                .into();

                self.base.insert_string(replaced);
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
    /// Initializes system base using `Base`.
    /// Before calling this, you should create a base and pass it here.
    ///
    /// # Example
    /// ```
    /// use rvpacker_txt_rs_lib::{core::{Base, SystemBase}, Mode, EngineType};
    ///
    /// let mut base = Base::new(Mode::Read, EngineType::VXAce);
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

    /// Initializes the translation from `.txt` file contents.
    ///
    /// Required when reading with `Append` read mode, or when writing/purging.
    ///
    /// # Example
    /// ```no_run
    /// use rvpacker_txt_rs_lib::{core::{Base, SystemBase}, Mode, EngineType, Error};
    /// use std::fs::read_to_string;
    ///
    /// fn main() -> Result<(), Box<dyn std::error::Error>> {
    ///     let mut base = Base::new(Mode::Read, EngineType::VXAce);
    ///     let mut system_base = SystemBase::new(&mut base);
    ///
    ///     let translation_file_content = read_to_string("system.txt")?;
    ///     system_base.initialize_translation(&translation_file_content);
    ///     Ok(())
    /// }
    /// ```
    pub fn initialize_translation(&mut self, translation: &str) {
        self.base.initialize_translation(translation);
    }

    /// Processes the RPG Maker file content and returns:
    /// - RPG Maker data with applied translation, if `mode` is [`Mode::Write`].
    /// - Translation file data in other cases.
    ///
    /// If you use [`Mode::Write`], [`Mode::Purge`] or [`Mode::Read`] with [`ReadMode::Append`],
    /// you should initialize the translation using `initialize_translation` function.
    ///
    /// # Example
    /// ```no_run
    /// use rvpacker_txt_rs_lib::{core::{Base, SystemBase}, Mode, EngineType, Error};
    /// use std::fs::read;
    ///
    /// fn main() -> Result<(), Box<dyn std::error::Error>> {
    ///     let mut base = Base::new(Mode::Read, EngineType::VXAce);
    ///     let mut system_base = SystemBase::new(&mut base);
    ///
    ///     let system_file_content = read("C:/Game/Data/System.rvdata2")?;
    ///     system_base.process(&system_file_content)?;
    ///     Ok(())
    /// }
    /// ```
    /// # Returns
    /// - RPG Maker data with applied translation, if `mode` is [`Mode::Write`].
    /// - Translation file data in other cases.
    /// - [`Error`], if unable to parse the content.
    ///
    /// # Panics
    /// May panic if passed content is not `System`.
    pub fn process(mut self, content: &[u8]) -> Result<Vec<u8>, Error> {
        self.system_value = self.base.parse_rpgm_file(content)?;

        for (entry_id, entry) in [
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
            let flow = self.base.get_translation_map(entry);
            if flow.is_break() {
                continue;
            }

            self.base.get_ignore_entry(entry);

            if self.base.mode.is_purge() {
                self.base.purge_empty_translation();
            } else {
                if self.base.mode.is_read() {
                    self.base
                        .translation_map
                        .insert(SYSTEM_ENTRY_COMMENT.into(), entry.into());
                }

                if entry_id <= 4 {
                    let label = [
                        self.base.labels.armor_types,
                        self.base.labels.elements,
                        self.base.labels.skill_types,
                        self.base.labels.weapon_types,
                        self.base.labels.equip_types,
                    ][entry_id];

                    let Some(array) = mutable!(&self, Self).system_value[label]
                        .as_array_mut()
                    else {
                        continue;
                    };

                    for value in array {
                        self.process_value(value);
                    }
                } else if entry_id == 5 {
                    self.process_terms();
                } else if entry_id == 6 {
                    self.process_currency_unit();
                } else {
                    self.process_game_title();
                }

                self.base.flush_translation(false);
            }

            self.base.reset_ignore_entry(entry);
            self.base.reset_translation_map(entry);
        }

        Ok(self.base.finish(self.system_value.take()))
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
            } else {
                if let Some(array) = value.as_array_mut() {
                    for value in array {
                        self.process_value(value);
                    }
                } else if value.is_bytes() || value.is_string() {
                    self.process_value(value);
                }
            }
        }
    }

    fn process_value(&mut self, value: &mut Value) {
        let Some(extracted) = self.base.extract_string(value, true) else {
            return;
        };

        let extracted = if self.base.romanize {
            Cow::Owned(Base::romanize_string(extracted))
        } else {
            Cow::Borrowed(extracted)
        };

        if self.base.mode.is_read() {
            self.base.insert_string(extracted.into());
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

            let game_title = if self.base.romanize {
                Cow::Owned(Base::romanize_string(game_title))
            } else {
                Cow::Borrowed(game_title)
            };

            self.base.insert_string(game_title.into());
        }
    }
}

pub struct ScriptBase<'a> {
    pub base: &'a mut Base,
    scripts_array: Vec<Value>,
    script_names: Vec<String>,
    scripts_content: Vec<String>,
}

impl<'a> ScriptBase<'a> {
    /// Initializes system base using `Base`.
    /// Before calling this, you should create a base and pass it here.
    ///
    /// # Example
    /// ```
    /// use rvpacker_txt_rs_lib::{core::{Base, ScriptBase}, Mode, EngineType};
    ///
    /// let mut base = Base::new(Mode::Read, EngineType::VXAce);
    /// let mut script_base = ScriptBase::new(&mut base);
    /// ```
    pub fn new(base: &'a mut Base) -> Self {
        base.reset();
        base.file_type = RPGMFileType::Scripts;

        Self {
            base,
            scripts_array: Vec::new(),
            script_names: Vec::new(),
            scripts_content: Vec::new(),
        }
    }

    /// Initializes the translation from `.txt` file contents.
    ///
    /// Required when reading with `Append` read mode, or when writing/purging.
    ///
    /// # Example
    /// ```no_run
    /// use rvpacker_txt_rs_lib::{core::{Base, ScriptBase}, Mode, EngineType, Error};
    /// use std::fs::read_to_string;
    ///
    /// fn main() -> Result<(), Box<dyn std::error::Error>> {
    ///     let mut base = Base::new(Mode::Read, EngineType::VXAce);
    ///     let mut script_base = ScriptBase::new(&mut base);
    ///
    ///     let translation_file_content = read_to_string("scripts.txt")?;
    ///     script_base.initialize_translation(&translation_file_content);
    ///     Ok(())
    /// }
    /// ```
    pub fn initialize_translation(&mut self, translation: &str) {
        self.base.initialize_translation(translation);
    }

    /// Processes the RPG Maker file content and returns:
    /// - RPG Maker data with applied translation, if `mode` is [`Mode::Write`].
    /// - Translation file data in other cases.
    ///
    /// If you use [`Mode::Write`], [`Mode::Purge`] or [`Mode::Read`] with [`ReadMode::Append`],
    /// you should initialize the translation using `initialize_translation` function.
    ///
    /// # Example
    /// ```no_run
    /// use rvpacker_txt_rs_lib::{core::{Base, ScriptBase}, Mode, EngineType, Error};
    /// use std::fs::read;
    ///
    /// fn main() -> Result<(), Box<dyn std::error::Error>> {
    ///     let mut base = Base::new(Mode::Read, EngineType::VXAce);
    ///     let mut script_base = ScriptBase::new(&mut base);
    ///
    ///     let script_file_content = read("C:/Game/Data/Scripts.rvdata2")?;
    ///     script_base.process(&script_file_content)?;
    ///     Ok(())
    /// }
    /// ```
    /// # Returns
    /// - RPG Maker data with applied translation, if `mode` is [`Mode::Write`].
    /// - Translation file data in other cases.
    /// - [`Error`], if unable to parse the content.
    ///
    /// # Panics
    /// May panic if passed content is not `Scripts`.
    pub fn process(mut self, content: &[u8]) -> Result<Vec<u8>, Error> {
        self.scripts_array = unsafe {
            self.base
                .parse_rpgm_file(content)?
                .into_array()
                .unwrap_unchecked()
        };
        self.decode_scripts();

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

        for (((script_id, script), script_name), mut code) in
            mutable!(&self, Self)
                .scripts_array
                .iter_mut()
                .enumerate()
                .zip(take(&mut self.script_names))
                .zip(take(&mut self.scripts_content))
        {
            let script_id = script_id.to_string();
            let flow = self.base.get_translation_map(&script_id);
            if flow.is_break() {
                continue;
            }

            self.base.get_ignore_entry(&script_id);

            if self.base.mode.is_purge() {
                self.base.purge_empty_translation();
            } else {
                self.base.process_comments(&script_id, &script_name);
                let (extracted_strings, ranges) = self.extract_strings(&code);

                if self.base.mode.is_write() {
                    let mut buf = Vec::with_capacity(1024);

                    unsafe {
                        ZlibEncoder::new(&mut buf, Compression::default())
                            .write_all(code.as_bytes())
                            .unwrap_unchecked()
                    };

                    script[2] = Value::bytes(&buf);

                    for (mut extracted, range) in extracted_strings
                        .into_iter()
                        .zip(ranges)
                        .filter(|(s, _)| !s.trim().is_empty())
                        .rev()
                    {
                        if self.base.romanize {
                            extracted = Base::romanize_string(&extracted);
                        }

                        if let Some(translated) = self.base.get_key(&extracted)
                        {
                            code.replace_range(range, translated);
                        }
                    }
                } else {
                    for mut extracted in extracted_strings
                        .into_iter()
                        .filter(|s| !s.trim().is_empty())
                    {
                        if Base::string_is_only_symbols(&extracted)
                            || extracted.contains("@window")
                            || extracted.contains(r"\$game")
                            || extracted.starts_with(r"\\e")
                            || extracted.contains("ALPHAC")
                            || extracted.contains("_")
                            || regexes.iter().any(|re| re.is_match(&extracted))
                        {
                            continue;
                        }

                        if self.base.romanize {
                            extracted = Base::romanize_string(&extracted);
                        }

                        self.base.insert_string(extracted);
                    }

                    self.base.flush_translation(false);
                }
            }

            self.base.reset_ignore_entry(&script_id);
            self.base.reset_translation_map(&script_id);
        }

        Ok(self.base.finish(Value::array(self.scripts_array)))
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

                    let extracted_string =
                        ruby_code[range.clone()].replace_line_breaks();

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

    fn decode_scripts(&mut self) {
        self.scripts_content.reserve_exact(self.scripts_array.len());
        self.script_names.reserve_exact(self.scripts_array.len());

        for script in self.scripts_array.iter() {
            // These always exist in scripts, panic is unlikely.
            let script_name_data =
                unsafe { script[1].as_byte_vec().unwrap_unchecked() };
            let script_data =
                unsafe { script[2].as_byte_vec().unwrap_unchecked() };

            let mut decoded_script = Vec::new();

            // `script_data` is always zlib encoded, panic is unlikely.
            unsafe {
                ZlibDecoder::new(script_data)
                    .read_to_end(&mut decoded_script)
                    .unwrap_unchecked()
            };

            for encoding in [
                encoding_rs::UTF_8,
                encoding_rs::WINDOWS_1252,
                encoding_rs::WINDOWS_1251,
                encoding_rs::SHIFT_JIS,
                encoding_rs::GB18030,
            ] {
                let (script_cow, _, had_errors) =
                    encoding.decode(&decoded_script);
                let (script_name_cow, _, _) = encoding.decode(script_name_data);

                if !had_errors {
                    self.scripts_content.push(script_cow.into());
                    self.script_names.push(script_name_cow.into());
                    break;
                }
            }
        }
    }
}

pub struct PluginBase<'a> {
    pub base: &'a mut Base,
}

impl<'a> PluginBase<'a> {
    /// Initializes system base using `Base`.
    /// Before calling this, you should create a base and pass it here.
    ///
    /// # Example
    /// ```
    /// use rvpacker_txt_rs_lib::{core::{Base, PluginBase}, Mode, EngineType};
    ///
    /// let mut base = Base::new(Mode::Read, EngineType::New);
    /// let mut plugin_base = PluginBase::new(&mut base);
    /// ```
    pub fn new(base: &'a mut Base) -> Self {
        base.reset();
        base.file_type = RPGMFileType::Plugins;

        Self { base }
    }

    /// Initializes the translation from `.txt` file contents.
    ///
    /// Required when reading with `Append` read mode, or when writing/purging.
    ///
    /// # Example
    /// ```no_run
    /// use rvpacker_txt_rs_lib::{core::{Base, PluginBase}, Mode, EngineType, Error};
    /// use std::fs::read_to_string;
    ///
    /// fn main() -> Result<(), Box<dyn std::error::Error>> {
    ///     let mut base = Base::new(Mode::Read, EngineType::New);
    ///     let mut plugin_base = PluginBase::new(&mut base);
    ///
    ///     let translation_file_content = read_to_string("plugins.txt")?;
    ///     plugin_base.initialize_translation(&translation_file_content);
    ///     Ok(())
    /// }
    /// ```
    pub fn initialize_translation(&mut self, translation: &str) {
        self.base.initialize_translation(translation);
    }

    /// Processes the RPG Maker file content and returns:
    /// - RPG Maker data with applied translation, if `mode` is [`Mode::Write`].
    /// - Translation file data in other cases.
    ///
    /// If you use [`Mode::Write`], [`Mode::Purge`] or [`Mode::Read`] with [`ReadMode::Append`],
    /// you should initialize the translation using `initialize_translation` function.
    ///
    /// # Example
    /// ```no_run
    /// use rvpacker_txt_rs_lib::{core::{Base, PluginBase}, Mode, EngineType, Error};
    /// use std::fs::read;
    ///
    /// fn main() -> Result<(), Box<dyn std::error::Error>> {
    ///     let mut base = Base::new(Mode::Read, EngineType::New);
    ///     let mut plugin_base = PluginBase::new(&mut base);
    ///
    ///     let plugins_file_content = read("plugins.js")?;
    ///     plugin_base.process(&plugins_file_content)?;
    ///     Ok(())
    /// }
    /// ```
    /// # Returns
    /// - RPG Maker data with applied translation, if `mode` is [`Mode::Write`].
    /// - Translation file data in other cases.
    /// - [`Error`], if unable to parse the content.
    ///
    /// # Panics
    /// May panic if passed content is not `plugins.js`.
    pub fn process(mut self, content: &[u8]) -> Result<Vec<u8>, Error> {
        let mut plugins_array = unsafe {
            Value::from(from_str::<serde_json::Value>(
                std::str::from_utf8_unchecked(content)
                    .split_once('=')
                    .unwrap_unchecked()
                    .1
                    .trim_end_matches([';', '\r', '\n']),
            )?)
            .into_array()
            .unwrap_unchecked()
        };

        for (plugin_id, plugin_object) in plugins_array.iter_mut().enumerate() {
            // Each plugin always contain name, panic is unlikely.
            let plugin_name =
                unsafe { plugin_object["name"].as_str().unwrap_unchecked() };
            let plugin_id = plugin_id.to_string();

            let flow = self.base.get_translation_map(&plugin_id);
            if flow.is_break() {
                continue;
            }

            self.base.get_ignore_entry(&plugin_id);

            if self.base.mode.is_purge() {
                self.base.purge_empty_translation();
            } else {
                self.base.process_comments(&plugin_id, plugin_name);
                self.parse_plugin(None, plugin_object);
                self.base.flush_translation(false);
            }

            self.base.reset_ignore_entry(&plugin_id);
            self.base.reset_translation_map(&plugin_id);
        }

        Ok(self.base.finish(Value::array(plugins_array)))
    }

    fn parse_plugin(&mut self, key: Option<&str>, value: &mut Value) {
        let is_invalid_key = |key: &Option<&str>| {
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
                if is_invalid_key(&key) {
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
                            .is_some_and(|c| c.is_alphabetic())
                        && value_string.ends_with(')')
                    || value_string.starts_with("rgba"))
                    || key.is_some_and(|x| x.starts_with("LATIN"))
                {
                    let mut string = value_string.replace_line_breaks();

                    string = if self.base.romanize {
                        Cow::Owned(Base::romanize_string(&string))
                    } else {
                        string
                    };

                    if self.base.mode.is_write() {
                        if !self.base.contains_key(&string) {
                            return;
                        }

                        if let Some(translated) = self.base.get_key(&string) {
                            *value = Value::string(translated.as_str());
                        }
                    } else {
                        self.base.insert_string(string.into());
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
