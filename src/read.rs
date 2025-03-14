#[cfg(feature = "log")]
use crate::println;
use crate::{
    determine_extension,
    functions::{
        extract_strings, filter_maps, filter_other, get_maps_labels, get_object_data, get_other_labels,
        get_system_labels, is_allowed_code, is_bad_code, parse_ignore, parse_map_number, parse_rpgm_file,
        parse_translation, process_parameter, process_variable, romanize_string, string_is_only_symbols, traverse_json,
    },
    statics::{
        localization::{FILES_ARE_NOT_PARSED_MSG, FILE_ALREADY_EXISTS_MSG, PARSED_FILE_MSG},
        ENCODINGS, LINES_SEPARATOR, NEW_LINE,
    },
    types::{
        Code, EngineType, GameType, IgnoreEntry, IgnoreMap, IndexMapGx, IndexSetGx, MapsProcessingMode, OptionExt,
        ProcessingMode, ResultExt, TrimReplace, Variable,
    },
};
use flate2::read::ZlibDecoder;
use gxhash::GxBuildHasher;
use indexmap::{IndexMap, IndexSet};
use marshal_rs::{load, StringMode};
use regex::Regex;
use smallvec::SmallVec;
use sonic_rs::{from_str, from_value, prelude::*, Array, Value};
use std::{
    cell::UnsafeCell,
    collections::VecDeque,
    fs::{read, read_dir, read_to_string, write},
    io::Read,
    mem::{take, transmute},
    path::{Path, PathBuf},
};

#[allow(clippy::too_many_arguments)]
#[inline(always)]
fn parse_list<'a>(
    list: &Array,
    romanize: bool,
    game_type: Option<GameType>,
    engine_type: EngineType,
    processing_mode: ProcessingMode,
    (code_label, parameters_label): (&str, &str),
    map: &'a mut IndexMapGx,
    set: &'a mut IndexSetGx,
    mut translation_map_vec: Option<&mut Vec<(String, String)>>,
    lines_pos: &mut usize,
    maps_processing_mode: Option<MapsProcessingMode>,
    ignore_entry: Option<&IgnoreEntry>,
    trim: bool,
) {
    let mut in_sequence: bool = false;

    let mut lines_vec: SmallVec<[&str; 4]> = SmallVec::with_capacity(4);
    let buf: UnsafeCell<SmallVec<[Vec<u8>; 4]>> = UnsafeCell::new(SmallVec::with_capacity(4));

    let mut process_parameter = |code: Code, parameter: &str| {
        if let Some(parsed) = process_parameter(code, parameter, game_type, engine_type, romanize, None, None, false) {
            if let Some(entry) = ignore_entry {
                if entry.contains(&parsed) {
                    return;
                }
            }

            if maps_processing_mode == Some(MapsProcessingMode::Preserve) {
                let vec: &mut &mut Vec<(String, String)> = unsafe { translation_map_vec.as_mut().unwrap_unchecked() };
                let pos: usize = *lines_pos;

                if processing_mode.is_append() {
                    if vec.get(pos).is_some_and(|(o, _)| *o != parsed) {
                        vec.insert(pos, (parsed, String::new()));
                    }
                } else {
                    vec.push((parsed, String::new()))
                }

                *lines_pos += 1;
            } else {
                let absent: bool = set.insert(parsed.clone());
                if maps_processing_mode == Some(MapsProcessingMode::Default) && absent {
                    *lines_pos += 1;
                }

                if processing_mode.is_append() {
                    if map.contains_key(&parsed) {
                        let map_index: usize = unsafe { map.get_index_of(&parsed).unwrap_unchecked() };
                        let mut set_index: usize = if maps_processing_mode == Some(MapsProcessingMode::Separate)
                            || maps_processing_mode.is_none()
                        {
                            unsafe { set.get_index_of(&parsed).unwrap_unchecked() }
                        } else {
                            if !absent {
                                return;
                            }

                            *lines_pos
                        };

                        if maps_processing_mode == Some(MapsProcessingMode::Separate) {
                            // counting starting comments, three to four
                            set_index += map.iter().take(4).filter(|(k, _)| k.starts_with("<!--")).count();
                        }

                        // just prevent the fucking panic
                        let map_len: usize = map.len();
                        if set_index >= map_len {
                            set_index = map_len - 1;
                        }

                        map.swap_indices(set_index, map_index);
                    } else if (maps_processing_mode == Some(MapsProcessingMode::Separate)
                        || maps_processing_mode.is_none())
                        && !set.contains(&parsed)
                    {
                        let pos: usize = set.len() - 1;
                        map.shift_insert(pos, parsed, String::new());
                    } else if maps_processing_mode == Some(MapsProcessingMode::Default) && absent {
                        let pos: usize = *lines_pos - 1;
                        map.shift_insert(pos, parsed, String::new());
                    }
                } else if maps_processing_mode == Some(MapsProcessingMode::Default) {
                    if absent {
                        map.insert(parsed, String::new());
                    }
                } else {
                    map.insert(parsed, String::new());
                }
            }
        }
    };

    for item in list {
        let code: u16 = item[code_label].as_u64().unwrap_log() as u16;

        let code: Code = if is_allowed_code(code) {
            let code: Code = unsafe { transmute(code) };

            if is_bad_code(code, engine_type) {
                Code::Bad
            } else {
                code
            }
        } else {
            Code::Bad
        };

        if in_sequence
            && (!code.is_any_dialogue() || (engine_type.is_xp() && code.is_dialogue_start() && !lines_vec.is_empty()))
        {
            if !lines_vec.is_empty() {
                let joined: String = lines_vec.join(NEW_LINE);

                process_parameter(Code::Dialogue, &joined);

                lines_vec.clear();
                unsafe { (*buf.get()).clear() };
            }

            in_sequence = false;
        }

        if code.is_bad() {
            continue;
        }

        let parameters: &Array = item[parameters_label].as_array().unwrap_log();

        let value_index: usize = if code.is_any_misc() { 1 } else { 0 };
        let value: &Value = &parameters[value_index];

        if code.is_choice_array() {
            for i in 0..value.as_array().unwrap_log().len() {
                let mut buf: Vec<u8> = Vec::new();

                let subparameter_string: &str = value[i].as_str().unwrap_or_else(|| match value[i].as_object() {
                    Some(obj) => {
                        buf = get_object_data(obj);
                        unsafe { std::str::from_utf8_unchecked(&buf) }
                    }
                    None => unreachable!(),
                });

                let trimmed: &str = subparameter_string.trim();

                if trimmed.is_empty() {
                    continue;
                }

                process_parameter(code, if trim { trimmed } else { subparameter_string });
            }
        } else {
            let parameter_string: &str = value
                .as_str()
                .unwrap_or_else(|| match value.as_object() {
                    Some(obj) => unsafe {
                        (*buf.get()).push(get_object_data(obj));
                        std::str::from_utf8_unchecked(&(*buf.get())[lines_vec.len()])
                    },
                    None => "",
                })
                .trim();

            if !code.is_credit() && parameter_string.is_empty() {
                continue;
            }

            if code.is_any_dialogue() {
                lines_vec.push(parameter_string);
                in_sequence = true;
            } else {
                process_parameter(code, parameter_string);
            }
        }
    }
}

#[inline]
fn get_order_number(mapinfos: &Value, entry: &str, map_number: u16, engine_type: EngineType) -> String {
    if engine_type.is_new() {
        &mapinfos[map_number as usize]["order"]
    } else {
        &mapinfos[entry]["__symbol__order"]
    }
    .as_u64()
    .unwrap_log()
    .to_string()
}

#[inline]
fn get_map_name(mapinfos: &Value, entry: &str, map_number: u16, engine_type: EngineType) -> String {
    if engine_type.is_new() {
        &mapinfos[map_number as usize]["name"]
    } else {
        &mapinfos[&entry]["__symbol__name"]
    }
    .as_str()
    .unwrap_log()
    .to_string()
}

#[inline]
fn find_starting_comment<'a, I: Iterator<Item = (&'a String, &'a String)>>(
    iterable: I,
    n: usize,
    str: &str,
) -> Option<usize> {
    iterable.skip(n).take(4).position(|(k, _)| k.starts_with(str))
}

#[inline]
fn process_map_comments(
    translation_map: &mut IndexMapGx,
    map_number_comment_index: &mut usize,
    lines_pos: &mut usize,
    map_name_comment: String,
    map_display_name_comment: String,
) {
    if !map_name_comment.is_empty() && !translation_map.contains_key(&map_name_comment) {
        if let Some(index) = find_starting_comment(translation_map.iter(), *lines_pos, "<!-- Map Name") {
            translation_map.shift_remove_index(index);
        }

        *map_number_comment_index += 1;
        translation_map.shift_insert(*map_number_comment_index, map_name_comment.to_string(), String::new());
        *lines_pos += 1;
    }

    if !map_display_name_comment.is_empty() && !translation_map.contains_key(&map_display_name_comment) {
        let mut translation: String = String::new();

        if let Some(index) = find_starting_comment(translation_map.iter(), *lines_pos, "<!-- In-game") {
            if let Some((_, t)) = translation_map.shift_remove_index(index) {
                translation = t
            }
        }

        *map_number_comment_index += 1;
        translation_map.shift_insert(
            *map_number_comment_index,
            map_display_name_comment.to_string(),
            translation,
        );
        *lines_pos += 1;
    }
}

#[inline]
fn process_map_comments_vec(
    translation_map_vec: &mut Vec<(String, String)>,
    map_number_comment_index: &mut usize,
    lines_pos: &mut usize,
    map_name_comment: String,
    map_display_name_comment: String,
) {
    if !map_name_comment.is_empty() {
        if let Some(index) = find_starting_comment(
            translation_map_vec.iter().map(|(a, b)| (a, b)),
            *lines_pos,
            "<!-- Map Name",
        ) {
            translation_map_vec.remove(index);
        }

        *map_number_comment_index += 1;
        translation_map_vec.insert(*map_number_comment_index, (map_name_comment, String::new()));
        *lines_pos += 1;
    }

    if !map_display_name_comment.is_empty() {
        let mut translation: String = String::new();

        if let Some(index) = find_starting_comment(
            translation_map_vec.iter().map(|(a, b)| (a, b)),
            *lines_pos,
            "<!-- In-game",
        ) {
            if translation_map_vec.get(index).is_some() {
                let (_, t) = translation_map_vec.remove(index);
                translation = t;
            }
        }

        *map_number_comment_index += 1;
        translation_map_vec.insert(*map_number_comment_index, (map_display_name_comment, translation));
        *lines_pos += 1;
    }
}

/// A struct for reading map files and parsing them into `.txt` files.
///
/// This reader extracts translatable text from RPG Maker map files and writes them to a
/// structured text file that can be used for translation purposes. It handles different
/// engine types and processing modes.
///
/// # Fields
///
/// - `original_path` - Path to the directory containing the original map files
/// - `output_path` - Path to the directory where the output `.txt` files will be written
/// - `maps_processing_mode` - Controls how maps are processed
/// - `romanize` - Whether to romanize non-Latin text
/// - `logging` - Whether to log processing information
/// - `game_type` - Optional specific game type for specialized processing
/// - `engine_type` - The RPG Maker engine type
/// - `processing_mode` - Controls how files are processed
/// - `ignore` - Whether to ignore entries specified in `.rvpacker-ignore`
/// - `trim` - Whether to trim whitespace from extracted strings
pub struct MapReader<P: AsRef<Path>> {
    original_path: P,
    output_path: P,
    maps_processing_mode: MapsProcessingMode,
    romanize: bool,
    logging: bool,
    game_type: Option<GameType>,
    engine_type: EngineType,
    processing_mode: ProcessingMode,
    ignore: bool,
    trim: bool,
}

impl<P: AsRef<Path>> MapReader<P> {
    /// Creates a new MapReader with default values.
    ///
    /// # Parameters
    ///
    /// - `original_path` - Path to the directory containing the original map files
    /// - `output_path` - Path to the directory where the output `.txt` files will be written
    /// - `engine_type` - The RPG Maker engine type
    pub fn new(original_path: P, output_path: P, engine_type: EngineType) -> Self {
        Self::default(original_path, output_path, engine_type)
    }

    /// Creates a new MapReader with default values.
    ///
    /// Default values are:
    /// - `maps_processing_mode`: `MapsProcessingMode::Default`
    /// - `romanize`: `false`
    /// - `logging`: `false`
    /// - `game_type`: `None`
    /// - `processing_mode`: `ProcessingMode::Default`
    /// - `ignore`: `false`
    /// - `trim`: `false`
    ///
    /// # Parameters
    ///
    /// - `original_path` - Path to the directory containing the original map files
    /// - `output_path` - Path to the directory where the output `.txt` files will be written
    /// - `engine_type` - The RPG Maker engine type
    pub fn default(original_path: P, output_path: P, engine_type: EngineType) -> Self {
        Self {
            original_path,
            output_path,
            maps_processing_mode: MapsProcessingMode::Default,
            romanize: false,
            logging: false,
            game_type: None,
            engine_type,
            processing_mode: ProcessingMode::Default,
            ignore: false,
            trim: false,
        }
    }

    /// Sets the maps processing mode.
    ///
    /// This controls how maps are processed:
    /// - `Default`: Eliminate all duplicates across all maps
    /// - `Separate`: Eliminate only inner duplicates of each map
    /// - `Preserve`: Preserve all duplicates
    pub fn maps_processing_mode(mut self, maps_processing_mode: MapsProcessingMode) -> Self {
        self.maps_processing_mode = maps_processing_mode;
        self
    }

    /// Sets whether to romanize text.
    ///
    /// When enabled, non-Latin text (like Japanese, Chinese, etc.) will be
    /// converted to Latin characters where possible.
    pub fn romanize(mut self, romanize: bool) -> Self {
        self.romanize = romanize;
        self
    }

    /// Sets whether to log processing information.
    ///
    /// When enabled, the reader will log information about the files being processed.
    pub fn logging(mut self, logging: bool) -> Self {
        self.logging = logging;
        self
    }

    /// Sets the game type for specialized processing.
    ///
    /// Some games may require special handling. This parameter allows specifying
    /// a particular game type for customized processing.
    pub fn game_type(mut self, game_type: Option<GameType>) -> Self {
        self.game_type = game_type;
        self
    }

    /// Sets the processing mode.
    ///
    /// This controls how files are processed:
    /// - `Default`: Create new files, but don't overwrite existing ones
    /// - `Force`: Always create new files, overwriting existing ones
    /// - `Append`: Add new content to existing files
    pub fn processing_mode(mut self, processing_mode: ProcessingMode) -> Self {
        self.processing_mode = processing_mode;
        self
    }

    /// Sets whether to ignore entries specified in `.rvpacker-ignore`.
    ///
    /// When enabled, the reader will skip entries that match patterns in the
    /// `.rvpacker-ignore` file.
    pub fn ignore(mut self, ignore: bool) -> Self {
        self.ignore = ignore;
        self
    }

    /// Sets whether to trim whitespace from extracted strings.
    ///
    /// When enabled, leading and trailing whitespace will be removed from
    /// extracted strings.
    ///
    /// In some cases, could lead to non-working writing, or incorrect displaying of text in-game.
    pub fn trim(mut self, trim: bool) -> Self {
        self.trim = trim;
        self
    }

    /// This method reads all map files from the original path, extracts translatable
    /// text, and writes it to a structured text file in the output path. The behavior
    /// is controlled by the various settings configured on the MapReader instance.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use std::path::Path;
    /// use rvpacker_txt_rs_lib::{read::MapReader, types::EngineType};
    ///
    /// let reader = MapReader::new(
    ///     Path::new("data"),
    ///     Path::new("translation"),
    ///     EngineType::New
    /// );
    ///
    /// reader.read();
    /// ```
    #[inline(always)]
    pub fn read(self) {
        let txt_output_path: &Path = &self.output_path.as_ref().join("maps.txt");

        if self.processing_mode.is_default() && txt_output_path.exists() {
            println!("maps.txt {FILE_ALREADY_EXISTS_MSG}");
            return;
        }

        // Allocated when maps processing mode is DEFAULT or SEPARATE.
        let mut lines_set: IndexSetGx = IndexSet::default();

        // Allocated when maps processing mode is DEFAULT or SEPARATE.
        // Reads the translation from existing `.txt` file and then appends new lines.
        let mut translation_map: &mut IndexMapGx = &mut IndexMap::default();

        // Allocated when maps processing mode is SEPARATE.
        let mut translation_maps: IndexMap<u16, IndexMapGx> = IndexMap::new();

        // Allocated when maps processing mode is PRESERVE or SEPARATE.
        let mut translation_map_vec: Vec<(String, String)> = Vec::new(); // This map is implemented via Vec<Tuple> because required to preserve duplicates.

        // Used when maps processing mode is PRESERVE.
        let mut lines_pos: usize = 0;

        let mut ignore_map: IndexMap<String, IgnoreEntry, GxBuildHasher> = IndexMap::default();

        if self.processing_mode.is_append() {
            if txt_output_path.exists() {
                if self.ignore {
                    ignore_map = parse_ignore(self.output_path.as_ref().join(".rvpacker-ignore"));
                }

                let translation: String = read_to_string(txt_output_path).unwrap_log();
                let parsed_translation: Box<dyn Iterator<Item = (String, String)>> =
                    parse_translation(&translation, "maps.txt", false, true);

                match self.maps_processing_mode {
                    MapsProcessingMode::Default | MapsProcessingMode::Separate => {
                        translation_maps.reserve(512);

                        let mut map: IndexMapGx = IndexMap::default();
                        let mut map_number: u16 = u16::MAX;
                        let mut prev_map: String = String::new();

                        for (original, translation) in parsed_translation {
                            if original == "<!-- Map -->" {
                                if map.is_empty() {
                                    if translation != prev_map {
                                        translation_maps.insert(map_number, take(&mut map));
                                    }
                                } else {
                                    translation_maps.insert(map_number, take(&mut map));
                                }

                                map_number = parse_map_number(&translation);
                                prev_map = translation.clone();
                            }

                            map.insert(original, translation);
                        }

                        if !map.is_empty() {
                            translation_maps.insert(map_number, map);
                        }
                    }
                    MapsProcessingMode::Preserve => translation_map_vec.extend(parsed_translation),
                }
            } else {
                println!("{FILES_ARE_NOT_PARSED_MSG}");
                return;
            }
        };

        let (display_name_label, events_label, pages_label, list_label, code_label, parameters_label) =
            get_maps_labels(self.engine_type);

        let mapinfos_path: PathBuf = self
            .original_path
            .as_ref()
            .join("MapInfos".to_owned() + determine_extension(self.engine_type));
        let mapinfos: Value = parse_rpgm_file(&mapinfos_path, self.engine_type);

        let maps_iter = read_dir(&self.original_path)
            .unwrap_log()
            .filter_map(|entry| filter_maps(entry, self.engine_type));

        for (filename, path) in maps_iter {
            let obj: Value = parse_rpgm_file(&path, self.engine_type);
            let map_number: u16 = parse_map_number(&filename);
            let map_number_string: String = map_number.to_string();

            let ignore_entry: Option<&IgnoreEntry> = ignore_map.get(&format!("<!-- File: map{map_number} -->"));

            let map_number_comment: String = String::from("<!-- Map -->");
            let mut map_name_comment: String = String::new();

            let entry: String = format!("__integer__{map_number}");

            let order_number: String = get_order_number(&mapinfos, &entry, map_number, self.engine_type);
            let map_name: String = get_map_name(&mapinfos, &entry, map_number, self.engine_type);

            if !map_name.is_empty() {
                map_name_comment = format!("<!-- Map Name: {map_name} -->");
            }

            let mut map_display_name_comment: String = String::new();

            if let Some(display_name) = obj[display_name_label].as_str() {
                if !display_name.is_empty() {
                    let mut display_name_string: String = display_name.to_owned();

                    if self.romanize {
                        display_name_string = romanize_string(display_name_string);
                    }

                    map_display_name_comment = format!("<!-- In-game Displayed Name: {display_name_string} -->");
                }
            }

            let map_display_name_comment_clone: String = map_display_name_comment.clone();

            let order: String = String::from("<!-- Order -->");

            match (self.processing_mode, self.maps_processing_mode) {
                (
                    ProcessingMode::Default | ProcessingMode::Force,
                    MapsProcessingMode::Default | MapsProcessingMode::Separate,
                ) => {
                    translation_map.insert(map_number_comment, map_number_string);
                    translation_map.insert(map_name_comment, String::new());

                    if !map_display_name_comment.is_empty() {
                        translation_map.insert(map_display_name_comment, String::new());
                    }

                    translation_map.insert(order, order_number);
                    translation_maps.insert(map_number, take(translation_map));

                    if self.maps_processing_mode == MapsProcessingMode::Separate {
                        lines_set.clear();
                    }
                }
                (ProcessingMode::Default | ProcessingMode::Force, MapsProcessingMode::Preserve) => {
                    translation_map_vec.push((map_number_comment, map_number_string));
                    lines_pos += 1;

                    if !map_name_comment.is_empty() {
                        translation_map_vec.push((map_name_comment, String::new()));
                        lines_pos += 1;
                    }

                    if !map_display_name_comment.is_empty() {
                        translation_map_vec.push((map_display_name_comment, String::new()));
                        lines_pos += 1;
                    }

                    translation_map_vec.push((order, order_number));
                    lines_pos += 1;
                }
                (ProcessingMode::Append, MapsProcessingMode::Default | MapsProcessingMode::Separate) => {
                    match self.maps_processing_mode {
                        MapsProcessingMode::Separate => lines_set.clear(),
                        _ => lines_pos = 0,
                    }

                    translation_map = unsafe { &mut *(&mut translation_maps as *mut IndexMap<u16, IndexMapGx>) }
                        .entry(map_number)
                        .or_insert_with(|| {
                            let mut new_map = IndexMap::default();
                            new_map.insert(map_number_comment, map_number_string);
                            new_map
                        });

                    let mut map_number_comment_index: usize = translation_map
                        .iter()
                        .take(4)
                        .position(|(k, _)| k == "<!-- Map -->")
                        .unwrap_log();
                    lines_pos += 1;

                    process_map_comments(
                        translation_map,
                        &mut map_number_comment_index,
                        &mut lines_pos,
                        map_name_comment,
                        map_display_name_comment,
                    );

                    if let Some(x) = translation_map.get(&order) {
                        if *x != order_number {
                            translation_map.insert(order, order_number);
                        }
                    } else {
                        translation_map.shift_insert(map_number_comment_index + 1, order, order_number);
                    }
                    lines_pos += 1;
                }
                (ProcessingMode::Append, MapsProcessingMode::Preserve) => {
                    let mut map_number_comment_index: usize = translation_map_vec
                        .iter()
                        .skip(lines_pos)
                        .take(4)
                        .position(|(k, _)| k == "<!-- Map -->")
                        .unwrap_log();
                    lines_pos += 1;

                    process_map_comments_vec(
                        &mut translation_map_vec,
                        &mut map_number_comment_index,
                        &mut lines_pos,
                        map_name_comment,
                        map_display_name_comment,
                    );

                    translation_map_vec.insert(map_number_comment_index + 1, (order, order_number));
                    lines_pos += 1;
                }
            }

            if self.maps_processing_mode != MapsProcessingMode::Preserve && !map_display_name_comment_clone.is_empty() {
                lines_set.insert(map_display_name_comment_clone);
                lines_pos += 1;
            }

            let events_arr: Box<dyn Iterator<Item = &Value>> = if self.engine_type.is_new() {
                Box::new(obj[events_label].as_array().unwrap_log().iter().skip(1))
            } else {
                Box::new(
                    obj[events_label]
                        .as_object()
                        .unwrap_log()
                        .iter()
                        .map(|(_, value)| value),
                )
            };

            for event in events_arr {
                if !event[pages_label].is_array() {
                    continue;
                }

                for page in event[pages_label].as_array().unwrap_log() {
                    parse_list(
                        page[list_label].as_array().unwrap_log(),
                        self.romanize,
                        self.game_type,
                        self.engine_type,
                        self.processing_mode,
                        (code_label, parameters_label),
                        translation_map,
                        &mut lines_set,
                        Some(&mut translation_map_vec),
                        &mut lines_pos,
                        Some(self.maps_processing_mode),
                        ignore_entry,
                        self.trim,
                    );
                }
            }

            if self.logging {
                println!("{PARSED_FILE_MSG} {filename}");
            }
        }

        let mut output_content: String = match self.maps_processing_mode {
            MapsProcessingMode::Default | MapsProcessingMode::Separate => {
                translation_maps.sort_unstable_keys();

                String::from_iter(
                    translation_maps
                        .into_iter()
                        .flat_map(|hashmap| hashmap.1.into_iter())
                        .map(|(original, translation)| format!("{original}{LINES_SEPARATOR}{translation}\n")),
                )
            }
            MapsProcessingMode::Preserve => String::from_iter(
                translation_map_vec
                    .into_iter()
                    .map(|(original, translation)| format!("{original}{LINES_SEPARATOR}{translation}\n")),
            ),
        };

        output_content.pop();
        write(txt_output_path, output_content).unwrap_log();
    }
}

/// A struct for reading non-map files and parsing them into `.txt` files.
///
/// This reader extracts translatable text from RPG Maker data files (like `Items`, `Actors`,
/// `CommonEvents`, etc.) and writes them to structured text files that can be used for
/// translation purposes. It handles different engine types and processing modes.
///
/// # Fields
///
/// - `original_path` - Path to the directory containing the original data files
/// - `output_path` - Path to the directory where the output `.txt` files will be written
/// - `romanize` - Whether to romanize non-Latin text
/// - `logging` - Whether to log processing information
/// - `game_type` - Optional specific game type for specialized processing
/// - `processing_mode` - Controls how files are processed
/// - `engine_type` - The RPG Maker engine type
/// - `ignore` - Whether to ignore entries specified in `.rvpacker-ignore`
/// - `trim` - Whether to trim whitespace from extracted strings
pub struct OtherReader<P: AsRef<Path>> {
    original_path: P,
    output_path: P,
    romanize: bool,
    logging: bool,
    game_type: Option<GameType>,
    processing_mode: ProcessingMode,
    engine_type: EngineType,
    ignore: bool,
    trim: bool,
}

impl<P: AsRef<Path>> OtherReader<P> {
    /// Creates a new `OtherReader` with default values.
    ///
    /// # Parameters
    ///
    /// - `original_path` - Path to the directory containing the original data files
    /// - `output_path` - Path to the directory where the output `.txt` files will be written
    /// - `engine_type` - The RPG Maker engine type
    pub fn new(original_path: P, output_path: P, engine_type: EngineType) -> Self {
        Self::default(original_path, output_path, engine_type)
    }

    /// Creates a new `OtherReader` with default values.
    ///
    /// Default values are:
    /// - `romanize`: `false`
    /// - `logging`: `false`
    /// - `game_type`: `None`
    /// - `processing_mode`: `ProcessingMode::Default`
    /// - `ignore`: `false`
    /// - `trim`: `false`
    ///
    /// # Parameters
    ///
    /// - `original_path` - Path to the directory containing the original data files
    /// - `output_path` - Path to the directory where the output `.txt` files will be written
    /// - `engine_type` - The RPG Maker engine type
    pub fn default(original_path: P, output_path: P, engine_type: EngineType) -> Self {
        Self {
            original_path,
            output_path,
            romanize: false,
            logging: false,
            game_type: None,
            processing_mode: ProcessingMode::Default,
            engine_type,
            ignore: false,
            trim: false,
        }
    }

    /// Sets whether to romanize text.
    ///
    /// When enabled, non-Latin text (like Japanese, Chinese, etc.) will be
    /// converted to Latin characters where possible.
    pub fn romanize(mut self, romanize: bool) -> Self {
        self.romanize = romanize;
        self
    }

    /// Sets whether to log processing information.
    ///
    /// When enabled, the reader will log information about the files being processed.
    pub fn logging(mut self, logging: bool) -> Self {
        self.logging = logging;
        self
    }

    /// Sets the game type for specialized processing.
    ///
    /// Some games may require special handling. This parameter allows specifying
    /// a particular game type for customized processing.
    pub fn game_type(mut self, game_type: Option<GameType>) -> Self {
        self.game_type = game_type;
        self
    }

    /// Sets the processing mode.
    ///
    /// This controls how files are processed:
    /// - `Default`: Create new files, but don't overwrite existing ones
    /// - `Force`: Always create new files, overwriting existing ones
    /// - `Append`: Add new content to existing files
    pub fn processing_mode(mut self, processing_mode: ProcessingMode) -> Self {
        self.processing_mode = processing_mode;
        self
    }

    /// Sets whether to ignore entries specified in `.rvpacker-ignore`.
    ///
    /// When enabled, the reader will skip entries that match patterns in the
    /// `.rvpacker-ignore` file.
    pub fn ignore(mut self, ignore: bool) -> Self {
        self.ignore = ignore;
        self
    }

    /// Sets whether to trim whitespace from extracted strings.
    ///
    /// When enabled, leading and trailing whitespace will be removed from
    /// extracted strings.
    ///
    /// In some cases, could lead to non-working writing, or incorrect displaying of text in-game.
    pub fn trim(mut self, trim: bool) -> Self {
        self.trim = trim;
        self
    }

    /// This method reads all data files from the original path (like `Items`, `Actors`,
    /// `CommonEvents`, etc.), extracts translatable text, and writes it to structured
    /// text files in the output path. The behavior is controlled by the various
    /// settings configured on the `OtherReader` instance.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use std::path::Path;
    /// use rvpacker_txt_rs_lib::{read::OtherReader, types::EngineType};
    ///
    /// let reader = OtherReader::new(
    ///     Path::new("data"),
    ///     Path::new("translation"),
    ///     EngineType::New
    /// );
    ///
    /// reader.read();
    /// ```
    #[inline(always)]
    pub fn read(self) {
        let (
            name_label,
            nickname_label,
            description_label,
            message1_label,
            message2_label,
            message3_label,
            message4_label,
            note_label,
            pages_label,
            list_label,
            code_label,
            parameters_label,
        ) = get_other_labels(self.engine_type);

        let mut ignore_map: IgnoreMap = IndexMap::default();

        if self.ignore {
            ignore_map = parse_ignore(self.output_path.as_ref().join(".rvpacker-ignore"));
        }

        for (filename, path) in read_dir(&self.original_path)
            .unwrap_log()
            .filter_map(|entry| filter_other(entry, self.engine_type, self.game_type))
        {
            let basename: String = filename.rsplit_once('.').unwrap_log().0.to_owned().to_lowercase();
            let txt_filename: String = basename.clone() + ".txt";
            let txt_output_path: &Path = &self.output_path.as_ref().join(txt_filename.clone());

            if self.processing_mode.is_default() && txt_output_path.exists() {
                println!("{txt_filename} {FILE_ALREADY_EXISTS_MSG}");
                continue;
            }

            let ignore_entry: Option<&IgnoreEntry> = ignore_map.get(&format!("<!-- File: {basename} -->"));

            let mut lines_set: IndexSetGx = IndexSet::default();
            let lines_mut_ref: &mut IndexSetGx = unsafe { &mut *(&mut lines_set as *mut IndexSetGx) };

            let mut translation_map: IndexMapGx = IndexMap::default();

            if self.processing_mode.is_append() {
                if txt_output_path.exists() {
                    let translation: String = read_to_string(txt_output_path).unwrap_log();
                    translation_map.extend(parse_translation(&translation, &txt_filename, false, true));
                } else {
                    println!("{FILES_ARE_NOT_PARSED_MSG}");
                    continue;
                }
            }

            let obj_arr: Value = parse_rpgm_file(&path, self.engine_type);

            // Other files except CommonEvents and Troops have the structure that consists
            // of name, nickname, description and note
            if !filename.starts_with("Co") && !filename.starts_with("Tr") {
                if self.game_type == Some(GameType::Termina) && filename.starts_with("It") {
                    lines_mut_ref.extend([
                        String::from("<Menu Category: Items>"),
                        String::from("<Menu Category: Food>"),
                        String::from("<Menu Category: Healing>"),
                        String::from("<Menu Category: Body bag>"),
                    ]);
                }

                if let Some(obj_real_arr) = obj_arr.as_array() {
                    'obj: for obj in obj_real_arr {
                        for (variable_label, variable_type) in [
                            (name_label, Variable::Name),
                            (nickname_label, Variable::Nickname),
                            (description_label, Variable::Description),
                            (message1_label, Variable::Message1),
                            (message2_label, Variable::Message2),
                            (message3_label, Variable::Message3),
                            (message4_label, Variable::Message4),
                            (note_label, Variable::Note),
                        ] {
                            let value: Option<&Value> = obj.get(variable_label);

                            let string: String = {
                                let mut buf: Vec<u8> = Vec::new();

                                let str: &str = value.as_str().unwrap_or_else(|| match value.as_object() {
                                    Some(obj) => {
                                        buf = get_object_data(obj);
                                        unsafe { std::str::from_utf8_unchecked(&buf) }
                                    }
                                    None => "",
                                });

                                let trimmed: &str = str.trim();

                                if trimmed.is_empty() {
                                    continue;
                                }

                                if self.trim { trimmed } else { str }.to_owned()
                            };

                            let note_text: Option<&str> =
                                if self.game_type == Some(GameType::Termina) && variable_type.is_desc() {
                                    match obj.get(note_label) {
                                        Some(value) => value.as_str(),
                                        None => None,
                                    }
                                } else {
                                    None
                                };

                            let parsed: Option<String> = process_variable(
                                string,
                                note_text,
                                variable_type,
                                &filename,
                                self.game_type,
                                self.engine_type,
                                self.romanize,
                                None,
                                false,
                            );

                            if let Some(parsed) = parsed {
                                let mut replaced: String =
                                    String::from_iter(parsed.split('\n').map(
                                        |x: &str| if self.trim { x.trim_replace() } else { x.to_owned() } + NEW_LINE,
                                    ));

                                replaced.pop();
                                replaced.pop();

                                if let Some(entry) = ignore_entry {
                                    if entry.contains(&replaced) {
                                        continue;
                                    }
                                }

                                lines_mut_ref.insert(replaced);
                                let string_ref: &str = unsafe { lines_mut_ref.last().unwrap_unchecked() }.as_str();

                                if self.processing_mode.is_append() {
                                    if translation_map.contains_key(string_ref) {
                                        translation_map.swap_indices(
                                            translation_map.get_index_of(string_ref).unwrap(),
                                            lines_mut_ref.len() - 1,
                                        );
                                    } else {
                                        translation_map.shift_insert(
                                            lines_mut_ref.len() - 1,
                                            string_ref.to_owned(),
                                            String::new(),
                                        );
                                    }
                                }
                            } else if variable_type.is_name() {
                                continue 'obj;
                            }
                        }
                    }
                }
            }
            // Other files have the structure somewhat similar to Maps files
            else {
                // Skipping first element in array as it is null
                for obj in obj_arr.as_array().unwrap_log().iter().skip(1) {
                    let commonevent_name: &str = obj[&name_label].as_str().unwrap_log();

                    if !commonevent_name.is_empty() {
                        let event_name_comment: String = format!("<!-- Event Name: {commonevent_name} -->");
                        lines_set.insert(event_name_comment.clone());
                        let pos: usize = lines_set.len() - 1;

                        if self.processing_mode.is_append() && !translation_map.contains_key(&event_name_comment) {
                            if pos <= translation_map.len() {
                                translation_map.shift_insert(pos, event_name_comment, String::new());
                            } else {
                                translation_map.insert(event_name_comment, String::new());
                            }
                        }
                    }

                    // CommonEvents doesn't have pages, so we can just check if it's Troops
                    let pages_length: usize = if filename.starts_with("Tr") {
                        obj[pages_label].as_array().unwrap_log().len()
                    } else {
                        1
                    };

                    for i in 0..pages_length {
                        let list: &Value = if pages_length != 1 {
                            &obj[pages_label][i][list_label]
                        } else {
                            &obj[list_label]
                        };

                        if !list.is_array() {
                            continue;
                        }

                        parse_list(
                            list.as_array().unwrap_log(),
                            self.romanize,
                            self.game_type,
                            self.engine_type,
                            self.processing_mode,
                            (code_label, parameters_label),
                            unsafe { &mut *(&mut translation_map as *mut IndexMapGx) },
                            lines_mut_ref,
                            None,
                            &mut 0,
                            None,
                            ignore_entry,
                            self.trim,
                        );
                    }
                }
            }

            let mut output_content: String = match self.processing_mode {
                ProcessingMode::Append => String::from_iter(
                    translation_map
                        .into_iter()
                        .map(|(original, translated)| format!("{original}{LINES_SEPARATOR}{translated}\n")),
                ),
                _ => String::from_iter(
                    lines_set
                        .into_iter()
                        .map(|line: String| format!("{line}{LINES_SEPARATOR}\n")),
                ),
            };

            output_content.pop();

            write(txt_output_path, output_content).unwrap_log();

            if self.logging {
                println!("{PARSED_FILE_MSG} {filename}");
            }
        }
    }
}

/// A struct for reading the system file and parsing it into a `.txt` file.
///
/// This reader extracts translatable text from the RPG Maker `System` file (which contains
/// game terms, vocabulary, and other system-level text) and writes it to a structured
/// text file that can be used for translation purposes.
///
/// # Fields
///
/// - `system_file_path` - Path to the `System` file
/// - `output_path` - Path to the directory where the output `.txt` file will be written
/// - `romanize` - Whether to romanize non-Latin text
/// - `logging` - Whether to log processing information
/// - `processing_mode` - Controls how files are processed
/// - `engine_type` - The RPG Maker engine type
/// - `ignore` - Whether to ignore entries specified in `.rvpacker-ignore`
/// - `trim` - Whether to trim whitespace from extracted strings
pub struct SystemReader<P: AsRef<Path>> {
    system_file_path: P,
    output_path: P,
    romanize: bool,
    logging: bool,
    processing_mode: ProcessingMode,
    engine_type: EngineType,
    ignore: bool,
    trim: bool,
}

impl<P: AsRef<Path>> SystemReader<P> {
    /// Creates a new `SystemReader` with default values.
    ///
    /// # Parameters
    ///
    /// - `system_file_path` - Path to the `System` file
    /// - `output_path` - Path to the directory where the output `.txt` file will be written
    /// - `engine_type` - The RPG Maker engine type
    pub fn new(system_file_path: P, output_path: P, engine_type: EngineType) -> Self {
        Self::default(system_file_path, output_path, engine_type)
    }

    /// Creates a new `SystemReader` with default values.
    ///
    /// Default values are:
    /// - `romanize`: `false`
    /// - `logging`: `false`
    /// - `processing_mode`: `ProcessingMode::Default`
    /// - `ignore`: `false`
    /// - `trim`: `false`
    ///
    /// # Parameters
    ///
    /// - `system_file_path` - Path to the `System` file
    /// - `output_path` - Path to the directory where the output `.txt` file will be written
    /// - `engine_type` - The RPG Maker engine type
    pub fn default(system_file_path: P, output_path: P, engine_type: EngineType) -> Self {
        Self {
            system_file_path,
            output_path,
            romanize: false,
            logging: false,
            processing_mode: ProcessingMode::Default,
            engine_type,
            ignore: false,
            trim: false,
        }
    }

    /// Sets whether to romanize text.
    ///
    /// When enabled, non-Latin text (like Japanese, Chinese, etc.) will be
    /// converted to Latin characters where possible.
    pub fn romanize(mut self, romanize: bool) -> Self {
        self.romanize = romanize;
        self
    }

    /// Sets whether to log processing information.
    ///
    /// When enabled, the reader will log information about the files being processed.
    pub fn logging(mut self, logging: bool) -> Self {
        self.logging = logging;
        self
    }

    /// Sets the processing mode.
    ///
    /// This controls how files are processed:
    /// - `Default`: Create new files, but don't overwrite existing ones
    /// - `Append`: Add new content to existing files
    /// - `Force`: Always create new files, overwriting existing ones
    pub fn processing_mode(mut self, processing_mode: ProcessingMode) -> Self {
        self.processing_mode = processing_mode;
        self
    }

    /// Sets whether to ignore entries specified in `.rvpacker-ignore`.
    ///
    /// When enabled, the reader will skip entries that match patterns in the
    /// `.rvpacker-ignore` file.
    pub fn ignore(mut self, ignore: bool) -> Self {
        self.ignore = ignore;
        self
    }

    /// Sets whether to trim whitespace from extracted strings.
    ///
    /// When enabled, leading and trailing whitespace will be removed from
    /// extracted strings.
    ///
    /// In some cases, could lead to non-working writing, or incorrect displaying of text in-game.
    pub fn trim(mut self, trim: bool) -> Self {
        self.trim = trim;
        self
    }

    /// This method reads the `System` file, extracts translatable text (like game terms,
    /// vocabulary, and other system-level text), and writes it to a structured text file
    /// in the output path. The behavior is controlled by the various settings configured
    /// on the `SystemReader` instance.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use std::path::Path;
    /// use rvpacker_txt_rs_lib::{read::SystemReader, types::EngineType};
    ///
    /// let reader = SystemReader::new(
    ///     Path::new("data/System.json"),
    ///     Path::new("translation"),
    ///     EngineType::New
    /// );
    ///
    /// reader.read();
    /// ```
    #[inline(always)]
    pub fn read(self) {
        let txt_output_path: &Path = &self.output_path.as_ref().join("system.txt");

        if self.processing_mode.is_default() && txt_output_path.exists() {
            println!("system.txt {FILE_ALREADY_EXISTS_MSG}");
            return;
        }

        let mut ignore_map: IgnoreMap = IndexMap::default();

        let mut lines_set: IndexSetGx = IndexSet::default();
        let lines_mut_ref: &mut IndexSetGx = unsafe { &mut *(&mut lines_set as *mut IndexSetGx) };
        let lines_ref: &IndexSetGx = unsafe { &*(&lines_set as *const IndexSetGx) };

        let mut translation_map: IndexMapGx = IndexMap::default();

        if self.processing_mode.is_append() {
            if txt_output_path.exists() {
                if self.ignore {
                    ignore_map = parse_ignore(self.output_path.as_ref().join(".rvpacker-ignore"));
                }

                let translation: String = read_to_string(txt_output_path).unwrap_log();
                translation_map.extend(parse_translation(&translation, "system.txt", false, true));
            } else {
                println!("{FILES_ARE_NOT_PARSED_MSG}");
                return;
            }
        }

        let ignore_entry: Option<&IgnoreEntry> = ignore_map.get("<!-- File: system -->");

        let mut parse_str = |value: &Value| {
            let mut string: String = {
                let mut buf: Vec<u8> = Vec::new();

                let str: &str = value.as_str().unwrap_or_else(|| match value.as_object() {
                    Some(obj) => {
                        buf = get_object_data(obj);
                        unsafe { std::str::from_utf8_unchecked(&buf) }
                    }
                    None => "",
                });

                let trimmed: &str = str.trim();

                if trimmed.is_empty() {
                    return;
                }

                if self.trim { trimmed } else { str }.to_owned()
            };

            if self.romanize {
                string = romanize_string(string)
            }

            if let Some(entry) = ignore_entry {
                if entry.contains(&string) {
                    return;
                }
            }

            lines_mut_ref.insert(string);
            let string_ref: &str = unsafe { lines_ref.last().unwrap_unchecked() }.as_str();

            if self.processing_mode.is_append() && !translation_map.contains_key(string_ref) {
                translation_map.shift_insert(lines_ref.len() - 1, string_ref.to_owned(), String::new());
            }
        };

        let (armor_types_label, elements_label, skill_types_label, terms_label, weapon_types_label, game_title_label) =
            get_system_labels(self.engine_type);

        let obj: Value = parse_rpgm_file(self.system_file_path.as_ref(), self.engine_type);

        // Armor types and elements - mostly system strings, but may be required for some purposes
        for label in [
            armor_types_label,
            elements_label,
            skill_types_label,
            weapon_types_label,
            "equipTypes",
        ] {
            if let Some(arr) = obj[label].as_array() {
                for value in arr {
                    parse_str(value)
                }
            }
        }

        // Game terms vocabulary
        for (key, value) in obj[terms_label].as_object().unwrap_log() {
            if !self.engine_type.is_new() && !key.starts_with("__symbol__") {
                continue;
            }

            if key != "messages" {
                if let Some(arr) = value.as_array() {
                    for value in arr {
                        parse_str(value);
                    }
                } else if (value.is_object() && value["__type"].as_str() == Some("bytes")) || value.is_str() {
                    parse_str(value)
                }
            } else {
                if !value.is_object() {
                    continue;
                }

                for (_, value) in value.as_object().unwrap_log().iter() {
                    parse_str(value);
                }
            }
        }

        if !self.engine_type.is_new() {
            parse_str(&obj["__symbol__currency_unit"]);
        }

        // Game title - Translators may add something like "ELFISH TRANSLATION v1.0.0" to the title
        {
            let mut game_title_string: String = {
                let mut buf: Vec<u8> = Vec::new();

                let game_title: &str =
                    obj[game_title_label]
                        .as_str()
                        .unwrap_or_else(|| match obj[game_title_label].as_object() {
                            Some(obj) => {
                                buf = get_object_data(obj);
                                unsafe { std::str::from_utf8_unchecked(&buf) }
                            }
                            None => "",
                        });

                if self.trim {
                    game_title.trim_replace()
                } else {
                    game_title.to_owned()
                }
            };

            // We aren't checking if game_title_string is empty because VX and XP don't include game title in `System` file, and we still need it last

            if self.romanize {
                game_title_string = romanize_string(game_title_string)
            }

            lines_mut_ref.insert(game_title_string);
            let string_ref: &str = unsafe { lines_ref.last().unwrap_unchecked() }.as_str();

            if self.processing_mode.is_append() {
                if translation_map.contains_key(string_ref) {
                    translation_map.swap_indices(
                        translation_map.get_index_of(string_ref).unwrap(),
                        lines_mut_ref.len() - 1,
                    );
                } else {
                    translation_map.shift_insert(lines_ref.len() - 1, string_ref.to_owned(), String::new());
                }
            }
        }

        let mut output_content: String = match self.processing_mode {
            ProcessingMode::Append => String::from_iter(
                translation_map
                    .into_iter()
                    .map(|(original, translated)| format!("{original}{LINES_SEPARATOR}{translated}\n")),
            ),
            _ => String::from_iter(lines_set.into_iter().map(|line: String| line + LINES_SEPARATOR + "\n")),
        };

        output_content.pop();

        write(txt_output_path, output_content).unwrap_log();

        if self.logging {
            println!("{PARSED_FILE_MSG} System{}", determine_extension(self.engine_type))
        }
    }
}

/// A struct for reading `Scripts` file and parsing it into a `.txt` file.
///
/// This reader extracts translatable text from RPG Maker script files (which contain
/// Ruby code in older RPG Maker versions) and writes it to a structured
/// text file that can be used for translation purposes.
///
/// # Fields
///
/// - `scripts_file_path` - Path to the `Scripts` file
/// - `output_path` - Path to the directory where the output `.txt` file will be written
/// - `romanize` - Whether to romanize non-Latin text
/// - `logging` - Whether to log processing information
/// - `engine_type` - The RPG Maker engine type
/// - `processing_mode` - Controls how files are processed
/// - `ignore` - Whether to ignore entries specified in `.rvpacker-ignore`
pub struct ScriptReader<P: AsRef<Path>> {
    scripts_file_path: P,
    output_path: P,
    romanize: bool,
    logging: bool,
    processing_mode: ProcessingMode,
    ignore: bool,
}

impl<P: AsRef<Path>> ScriptReader<P> {
    /// Creates a new `ScriptReader` with default values.
    ///
    /// # Parameters
    ///
    /// - `scripts_file_path` - Path to the `Scripts` file
    /// - `output_path` - Path to the directory where the output `.txt` file will be written
    /// - `engine_type` - The RPG Maker engine type
    pub fn new(scripts_file_path: P, output_path: P) -> Self {
        Self::default(scripts_file_path, output_path)
    }

    /// Creates a new `ScriptReader` with default values.
    ///
    /// Default values are:
    /// - `romanize`: `false`
    /// - `logging`: `false`
    /// - `processing_mode`: `ProcessingMode::Default`
    /// - `ignore`: `false`
    ///
    /// # Parameters
    ///
    /// - `scripts_file_path` - Path to the `Scripts` file
    /// - `output_path` - Path to the directory where the output `.txt` file will be written
    /// - `engine_type` - The RPG Maker engine type
    pub fn default(scripts_file_path: P, output_path: P) -> Self {
        Self {
            scripts_file_path,
            output_path,
            romanize: false,
            logging: false,
            processing_mode: ProcessingMode::Default,
            ignore: false,
        }
    }

    /// Sets whether to romanize text.
    ///
    /// When enabled, non-Latin text (like Japanese, Chinese, etc.) will be
    /// converted to Latin characters where possible.
    pub fn romanize(mut self, romanize: bool) -> Self {
        self.romanize = romanize;
        self
    }

    /// Sets whether to log processing information.
    ///
    /// When enabled, the reader will log information about the files being processed.
    pub fn logging(mut self, logging: bool) -> Self {
        self.logging = logging;
        self
    }

    /// Sets the processing mode.
    ///
    /// This controls how files are processed:
    /// - `Default`: Create new files, but don't overwrite existing ones
    /// - `Force`: Always create new files, overwriting existing ones
    /// - `Append`: Add new content to existing files
    pub fn processing_mode(mut self, processing_mode: ProcessingMode) -> Self {
        self.processing_mode = processing_mode;
        self
    }

    /// Sets whether to ignore entries specified in `.rvpacker-ignore`.
    ///
    /// When enabled, the reader will skip entries that match patterns in the
    /// `.rvpacker-ignore` file.
    pub fn ignore(mut self, ignore: bool) -> Self {
        self.ignore = ignore;
        self
    }

    /// This method reads the `Scripts` file, extracts translatable text from the Ruby code,
    /// and writes it to a structured text file in the output path. The behavior is controlled
    /// by the various settings configured on the ScriptReader instance.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use std::path::Path;
    /// use rvpacker_txt_rs_lib::{read::ScriptReader, types::EngineType};
    ///
    /// let reader = ScriptReader::new(
    ///     Path::new("Data/Scripts.rvdata"),
    ///     Path::new("translation")
    /// );
    ///
    /// reader.read();
    /// ```
    #[inline(always)]
    pub fn read(self) {
        let txt_output_path: &Path = &self.output_path.as_ref().join("scripts.txt");

        if self.processing_mode.is_default() && txt_output_path.exists() {
            println!("scripts.txt {FILE_ALREADY_EXISTS_MSG}");
            return;
        }

        let mut ignore_map: IgnoreMap = IndexMap::default();

        let mut lines_vec: Vec<String> = Vec::new();
        let mut translation_map: Vec<(String, String)> = Vec::new();

        if self.processing_mode.is_append() {
            if txt_output_path.exists() {
                if self.ignore {
                    ignore_map = parse_ignore(self.output_path.as_ref().join(".rvpacker-ignore"));
                }

                let translation: String = read_to_string(txt_output_path).unwrap_log();
                translation_map.extend(parse_translation(&translation, "scripts.txt", false, false));
            } else {
                println!("{FILES_ARE_NOT_PARSED_MSG}");
                return;
            }
        }

        let ignore_entry: Option<&IgnoreEntry> = ignore_map.get("<!-- File: `Scripts` -->");

        let scripts_entries: Value = load(
            &read(self.scripts_file_path.as_ref()).unwrap_log(),
            Some(StringMode::Binary),
            None,
        )
        .unwrap_log();

        let scripts_entries_array: &Array = scripts_entries.as_array().unwrap_log();
        let mut codes_content: Vec<String> = Vec::with_capacity(scripts_entries_array.len());

        for code in scripts_entries_array {
            let bytes_stream: Vec<u8> = from_value(&code[2]["data"]).unwrap_log();

            let mut inflated: Vec<u8> = Vec::new();
            ZlibDecoder::new(&*bytes_stream).read_to_end(&mut inflated).unwrap_log();

            let mut code: String = String::new();

            for encoding in ENCODINGS {
                let (cow, _, had_errors) = encoding.decode(&inflated);

                if !had_errors {
                    code = cow.into_owned();
                    break;
                }
            }

            codes_content.push(code);
        }

        let codes_text: String = codes_content.join("");
        let extracted_strings: IndexSetGx = extract_strings(&codes_text, false).0;

        let regexes: [Regex; 5] = unsafe {
            [
            Regex::new(r"(Graphics|Data|Audio|Movies|System)\/.*\/?").unwrap_unchecked(),
            Regex::new(r"r[xv]data2?$").unwrap_unchecked(),
            Regex::new(r".*\(").unwrap_unchecked(),
            Regex::new(r"^([d\d\p{P}+-]*|[d\p{P}+-]&*)$").unwrap_unchecked(),
            Regex::new(r"^(Actor<id>|ExtraDropItem|EquipLearnSkill|GameOver|Iconset|Window|true|false|MActor%d|[wr]b|\\f|\\n|\[[A-Z]*\])$")
                .unwrap_unchecked(),
        ]
        };

        lines_vec.reserve_exact(extracted_strings.len());

        if self.processing_mode.is_append() {
            translation_map.reserve_exact(extracted_strings.len());
        }

        for mut extracted in extracted_strings {
            if extracted.is_empty() {
                continue;
            }

            if string_is_only_symbols(&extracted)
                || extracted.contains("@window")
                || extracted.contains(r"\$game")
                || extracted.starts_with(r"\\e")
                || extracted.contains("ALPHAC")
                || extracted.contains("_")
                || regexes.iter().any(|re| re.is_match(&extracted))
            {
                continue;
            }

            if self.romanize {
                extracted = romanize_string(extracted);
            }

            if let Some(entry) = ignore_entry {
                if entry.contains(&extracted) {
                    return;
                }
            }

            lines_vec.push(extracted);
            let last: &String = unsafe { lines_vec.last().unwrap_unchecked() };
            let pos: usize = lines_vec.len() - 1;

            if self.processing_mode.is_append() && translation_map.get(pos).is_some_and(|x| *last != x.0) {
                translation_map.insert(pos, (last.to_owned(), String::new()));
            }
        }

        let mut output_content: String = match self.processing_mode {
            ProcessingMode::Append => String::from_iter(
                translation_map
                    .into_iter()
                    .map(|(original, translated)| format!("{original}{LINES_SEPARATOR}{translated}\n")),
            ),
            _ => String::from_iter(lines_vec.into_iter().map(|line: String| line + LINES_SEPARATOR + "\n")),
        };

        output_content.pop();

        write(txt_output_path, output_content).unwrap_log();

        if self.logging {
            println!(
                "{PARSED_FILE_MSG} Scripts.{}",
                self.scripts_file_path.as_ref().extension().unwrap().to_str().unwrap()
            )
        }
    }
}

/// A struct for reading plugins file and parsing it into a `.txt` file.
///
/// This reader extracts translatable text from RPG Maker MV/MZ `plugins.js` file
/// (which contains JavaScript plugin configurations) and writes it to a structured
/// text file that can be used for translation purposes.
///
/// # Fields
///
/// - `plugins_file_path` - Path to the `plugins.js` file
/// - `output_path` - Path to the directory where the output `.txt` file will be written
/// - `romanize` - Whether to romanize non-Latin text
/// - `logging` - Whether to log processing information
/// - `processing_mode` - Controls how files are processed
/// - `ignore` - Whether to ignore entries specified in `.rvpacker-ignore`
pub struct PluginReader<P: AsRef<Path>> {
    plugins_file_path: P,
    output_path: P,
    romanize: bool,
    logging: bool,
    processing_mode: ProcessingMode,
    ignore: bool,
}

impl<P: AsRef<Path>> PluginReader<P> {
    /// Creates a new `PluginReader` with default values.
    ///
    /// # Parameters
    ///
    /// - `plugins_file_path` - Path to the `plugins.js` file
    /// - `output_path` - Path to the directory where the output `.txt` file will be written
    pub fn new(plugins_file_path: P, output_path: P) -> Self {
        Self::default(plugins_file_path, output_path)
    }

    /// Creates a new `PluginReader` with default values.
    ///
    /// Default values are:
    /// - `romanize`: `false`
    /// - `logging`: `false`
    /// - `processing_mode`: `ProcessingMode::Default`
    /// - `ignore`: `false`
    ///
    /// # Parameters
    ///
    /// - `plugins_file_path` - Path to the `plugins.js` file
    /// - `output_path` - Path to the directory where the output `.txt` file will be written
    pub fn default(plugins_file_path: P, output_path: P) -> Self {
        Self {
            plugins_file_path,
            output_path,
            romanize: false,
            logging: false,
            processing_mode: ProcessingMode::Default,
            ignore: false,
        }
    }

    /// Sets whether to romanize text.
    ///
    /// When enabled, non-Latin text (like Japanese, Chinese, etc.) will be
    /// converted to Latin characters where possible.
    pub fn romanize(mut self, romanize: bool) -> Self {
        self.romanize = romanize;
        self
    }

    /// Sets whether to log processing information.
    ///
    /// When enabled, the reader will log information about the files being processed.
    pub fn logging(mut self, logging: bool) -> Self {
        self.logging = logging;
        self
    }

    /// Sets the processing mode.
    ///
    /// This controls how files are processed:
    /// - `Default`: Create new files, but don't overwrite existing ones
    /// - `Force`: Always create new files, overwriting existing ones
    /// - `Append`: Add new content to existing files
    pub fn processing_mode(mut self, processing_mode: ProcessingMode) -> Self {
        self.processing_mode = processing_mode;
        self
    }

    /// Sets whether to ignore entries specified in `.rvpacker-ignore`.
    ///
    /// When enabled, the reader will skip entries that match patterns in the
    /// `.rvpacker-ignore` file.
    pub fn ignore(mut self, ignore: bool) -> Self {
        self.ignore = ignore;
        self
    }

    /// This method reads the `plugins.js` file, extracts translatable text from the
    /// JavaScript plugin configurations, and writes it to a structured text file
    /// in the output path. The behavior is controlled by the various settings
    /// configured on the `PluginReader` instance.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use std::path::Path;
    /// use rvpacker_txt_rs_lib::read::PluginReader;
    ///
    /// let reader = PluginReader::new(
    ///     Path::new("js/plugins.js"),
    ///     Path::new("translation")
    /// );
    ///
    /// reader.read();
    /// ```
    #[inline(always)]
    pub fn read(self) {
        let txt_output_path: &Path = &self.output_path.as_ref().join("plugins.txt");

        if self.processing_mode.is_default() && txt_output_path.exists() {
            println!("scripts.txt {FILE_ALREADY_EXISTS_MSG}");
            return;
        }

        let mut ignore_map: IgnoreMap = IndexMap::default();

        let mut translation_map: VecDeque<(String, String)> = VecDeque::new();
        let translation: String;

        if self.processing_mode.is_append() {
            if self.ignore {
                ignore_map = parse_ignore(self.output_path.as_ref().join(".rvpacker-ignore"));
            }

            if txt_output_path.exists() {
                translation = read_to_string(txt_output_path).unwrap_log();
                translation_map.extend(parse_translation(&translation, "plugins.txt", false, false));
            } else {
                println!("{FILES_ARE_NOT_PARSED_MSG}");
                return;
            }
        }

        let ignore_entry: Option<&IgnoreEntry> = ignore_map.get("<!-- File: plugins -->");

        let plugins_content: String = read_to_string(self.plugins_file_path.as_ref()).unwrap_log();

        let plugins_object: &str = plugins_content
            .split_once('=')
            .unwrap_log()
            .1
            .trim_end_matches([';', '\n']);

        let mut plugins_json: Value = from_str(plugins_object).unwrap_log();
        let mut lines_vec: Vec<String> = Vec::new();

        traverse_json(
            None,
            &mut plugins_json,
            &mut Some(&mut lines_vec),
            &mut Some(&mut translation_map),
            None,
            false,
            self.romanize,
            self.processing_mode,
            ignore_entry,
        );

        let mut output_content: String = match self.processing_mode {
            ProcessingMode::Append => String::from_iter(
                translation_map
                    .into_iter()
                    .map(|(original, translated)| format!("{original}{LINES_SEPARATOR}{translated}\n")),
            ),
            _ => String::from_iter(lines_vec.into_iter().map(|line: String| line + LINES_SEPARATOR + "\n")),
        };

        output_content.pop();

        write(txt_output_path, output_content).unwrap_log();

        if self.logging {
            println!("{PARSED_FILE_MSG} plugins.js")
        }
    }
}
