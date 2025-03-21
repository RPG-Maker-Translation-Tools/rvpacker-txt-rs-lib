#[cfg(feature = "log")]
use crate::println;
use crate::{
    functions::{
        extract_strings, filter_maps, filter_other, get_maps_labels, get_object_data, get_other_labels,
        get_system_labels, is_allowed_code, is_bad_code, parse_map_number, parse_rpgm_file, parse_translation,
        process_parameter, process_variable, romanize_string, string_is_only_symbols, traverse_json,
    },
    statics::{localization::PURGED_FILE_MSG, ENCODINGS, LINES_SEPARATOR, NEW_LINE},
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
    collections::HashSet,
    fs::{read, read_dir, read_to_string, write},
    io::Read,
    mem::{take, transmute},
    path::Path,
};

/// Writes the ignore map to a `.rvpacker-ignore` file.
///
/// This function takes an `ignore_map` containing file patterns and lines to ignore
/// during translation processing, and writes it to a `.rvpacker-ignore` file at the
/// specified output path. Use to output the `.rvpacker-ignore` file after performing
/// purges with `create_ignore` argument.
///
/// # Parameters
///
/// - `ignore_map` - A map of file patterns to sets of lines to ignore
/// - `translation_path` - The directory where the `.rvpacker-ignore` file will be written
#[inline]
pub fn write_ignore<P: AsRef<Path>>(ignore_map: IgnoreMap, translation_path: P) {
    use std::fmt::Write;

    let contents: String = ignore_map.into_iter().fold(String::new(), |mut output, (file, lines)| {
        let _ = write!(
            output,
            "{}\n{}",
            file,
            String::from_iter(lines.into_iter().map(|mut x| {
                x.push('\n');
                x
            }))
        );

        output
    });

    write(translation_path.as_ref().join(".rvpacker-ignore"), contents).unwrap();
}

/// Writes statistics about purged lines to a `stat.txt` file.
///
/// This function takes a vector of tuples containing original text and its translation,
/// and writes them to a `stat.txt` file at the specified output path. Use to output
/// the `stat.txt` file after performing purges with `stat` argument.
///
/// # Parameters
///
/// - `stat_vec` - A vector of tuples containing (original, translation) pairs
/// - `translation_path` - The directory where the `stat.txt` file will be written
#[inline]
pub fn write_stat<P: AsRef<Path>>(stat_vec: Vec<(String, String)>, translation_path: P) {
    write(
        translation_path.as_ref().join("stat.txt"),
        String::from_iter(
            stat_vec
                .into_iter()
                .map(|(original, translation)| format!("{original}{LINES_SEPARATOR}{translation}\n")),
        ),
    )
    .unwrap_log();
}

#[allow(clippy::too_many_arguments)]
#[inline(always)]
fn parse_list(
    list: &Array,
    romanize: bool,
    game_type: Option<GameType>,
    engine_type: EngineType,
    (code_label, parameters_label): (&str, &str),
    set_mut_ref: &mut IndexSetGx,
    mut translation_map_vec: Option<&mut Vec<(String, String)>>,
    maps_processing_mode: Option<MapsProcessingMode>,
    trim: bool,
) {
    let mut in_sequence: bool = false;

    let mut lines: SmallVec<[&str; 4]> = SmallVec::with_capacity(4);
    let buf: UnsafeCell<SmallVec<[Vec<u8>; 4]>> = UnsafeCell::new(SmallVec::with_capacity(4));

    let mut process_parameter = |code: Code, parameter: &str| {
        if let Some(parsed) = process_parameter(code, parameter, game_type, engine_type, romanize, None, None, false) {
            if maps_processing_mode == Some(MapsProcessingMode::Preserve) {
                let vec: &mut &mut Vec<(String, String)> = unsafe { translation_map_vec.as_mut().unwrap_unchecked() };
                vec.push((parsed, String::new()));
            } else {
                set_mut_ref.insert(parsed);
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
            && (!code.is_any_dialogue() || (engine_type.is_xp() && code.is_dialogue_start() && !lines.is_empty()))
        {
            if !lines.is_empty() {
                let joined: String = lines.join(NEW_LINE);

                process_parameter(Code::Dialogue, &joined);

                lines.clear();
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

                let trimmed = subparameter_string.trim();

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
                        std::str::from_utf8_unchecked(&(*buf.get())[lines.len()])
                    },
                    None => "",
                })
                .trim();

            if !code.is_credit() && parameter_string.is_empty() {
                continue;
            }

            if code.is_any_dialogue() {
                lines.push(parameter_string);
                in_sequence = true;
            } else {
                process_parameter(code, parameter_string);
            }
        }
    }
}

/// A struct for purging unused or empty translation from map files.
///
/// `MapPurger` analyzes RPG Maker map files and their corresponding translation files,
/// removing translation that are no longer needed or are empty. This helps keep
/// translation files clean and focused only on text that actually appears in the game.
///
/// # Fields
///
/// - `original_path` - Path to the directory containing the original map files
/// - `translation_path` - Path to the directory containing the translation files
/// - `maps_processing_mode` - Controls how maps are processed
/// - `romanize` - Whether to romanize non-Latin text
/// - `logging` - Whether to log processing information
/// - `game_type` - Optional specific game type for specialized processing
/// - `engine_type` - The RPG Maker engine type
/// - `stat` - Whether to generate statistics instead of modifying files
/// - `leave_filled` - Whether to leave filled translation even if they're unused
/// - `purge_empty` - Whether to purge empty translation
/// - `create_ignore` - Whether to create an ignore file for purged entries
/// - `trim` - Whether to trim whitespace from strings
pub struct MapPurger<P: AsRef<Path>> {
    original_path: P,
    translation_path: P,
    maps_processing_mode: MapsProcessingMode,
    romanize: bool,
    logging: bool,
    game_type: Option<GameType>,
    engine_type: EngineType,
    stat: bool,
    leave_filled: bool,
    purge_empty: bool,
    create_ignore: bool,
    trim: bool,
}

impl<P: AsRef<Path>> MapPurger<P> {
    /// Creates a new `MapPurger` with default values.
    ///
    /// # Parameters
    ///
    /// - `original_path` - Path to the directory containing the original map files
    /// - `translation_path` - Path to the directory containing the translation files
    /// - `engine_type` - The RPG Maker engine type
    pub fn new(original_path: P, translation_path: P, engine_type: EngineType) -> Self {
        Self::default(original_path, translation_path, engine_type)
    }

    /// Creates a new `MapPurger` with default values.
    ///
    /// Default values are:
    /// - `maps_processing_mode`: `MapsProcessingMode::Default`
    /// - `romanize`: `false`
    /// - `logging`: `false`
    /// - `game_type`: `None`
    /// - `stat`: `false`
    /// - `leave_filled`: `false`
    /// - `create_ignore`: `false`
    /// - `purge_empty`: `false`
    /// - `trim`: `false`
    ///
    /// # Parameters
    ///
    /// - `original_path` - Path to the directory containing the original map files
    /// - `translation_path` - Path to the directory containing the translation files
    /// - `engine_type` - The RPG Maker engine type
    pub fn default(original_path: P, translation_path: P, engine_type: EngineType) -> Self {
        Self {
            original_path,
            translation_path,
            engine_type,
            romanize: false,
            logging: false,
            game_type: None,
            maps_processing_mode: MapsProcessingMode::Default,
            stat: false,
            leave_filled: false,
            create_ignore: false,
            purge_empty: false,
            trim: false,
        }
    }

    /// Sets whether to romanize text.
    ///
    /// Must be the same value, as in previous read.
    pub fn romanize(mut self, romanize: bool) -> Self {
        self.romanize = romanize;
        self
    }

    /// Sets whether to log processing information.
    ///
    /// When enabled, the purger will log information about the files being processed.
    pub fn logging(mut self, logging: bool) -> Self {
        self.logging = logging;
        self
    }

    /// Sets whether to trim whitespace from strings.
    ///
    /// Must be the same value, as in previous read.
    pub fn trim(mut self, trim: bool) -> Self {
        self.trim = trim;
        self
    }

    /// Sets the game type for specialized processing.
    ///
    /// Must be the same value, as in previous read.
    pub fn game_type(mut self, game_type: Option<GameType>) -> Self {
        self.game_type = game_type;
        self
    }

    /// Sets the maps processing mode.
    ///
    /// Must be the same value, as in previous read.
    pub fn maps_processing_mode(mut self, maps_processing_mode: MapsProcessingMode) -> Self {
        self.maps_processing_mode = maps_processing_mode;
        self
    }

    /// Sets whether to generate statistics instead of modifying files.
    ///
    /// When enabled, the purger will generate statistics about what would be purged
    /// without actually modifying any files.
    pub fn stat(mut self, stat: bool) -> Self {
        self.stat = stat;
        self
    }

    /// Sets whether to leave filled translation even if they're unused.
    ///
    /// When enabled, translation fields that have content will not be purged even if
    /// they're no longer used in the game.
    pub fn leave_filled(mut self, leave_filled: bool) -> Self {
        self.leave_filled = leave_filled;
        self
    }

    /// Sets whether to create an ignore file for purged entries.
    ///
    /// When enabled, the purger will create a `.rvpacker-ignore` file containing
    /// entries for all purged entries.
    pub fn create_ignore(mut self, create_ignore: bool) -> Self {
        self.create_ignore = create_ignore;
        self
    }

    /// Sets whether to purge empty translation.
    ///
    /// When enabled, only translation fields that are empty will be purged.
    pub fn purge_empty(mut self, purge_empty: bool) -> Self {
        self.purge_empty = purge_empty;
        self
    }

    /// This method analyzes the map files and their translation, removing unused
    /// or empty translation based on the configured settings.
    ///
    /// # Parameters
    ///
    /// - `ignore_map` - Optional map to store ignored entries
    /// - `stat_vec` - Optional vector to store statistics
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use std::path::Path;
    /// use rvpacker_txt_rs_lib::purge::MapPurger;
    /// use rvpacker_txt_rs_lib::types::EngineType;
    ///
    /// let purger = MapPurger::new(
    ///     "data",
    ///     "translation",
    ///     EngineType::New
    /// )
    /// .logging(true)
    /// .purge_empty(true);
    ///
    /// purger.purge(None, None);
    /// ```
    #[inline(always)]
    pub fn purge(self, mut ignore_map: Option<&mut IgnoreMap>, mut stat_vec: Option<&mut Vec<(String, String)>>) {
        let txt_file_path: &Path = &self.translation_path.as_ref().join("maps.txt");

        // Allocated when maps processing mode is DEFAULT or SEPARATE.
        let mut lines_set: IndexSetGx = IndexSet::default();

        // Allocated when maps processing mode is DEFAULT or SEPARATE.
        // Reads the translation from existing `.txt` file and then appends new lines.
        let mut translation_map: &mut IndexMapGx = &mut IndexMap::default();

        // Allocated when maps processing mode is SEPARATE.
        let mut translation_maps: IndexMap<u16, IndexMapGx> = IndexMap::new();

        // Allocated when maps processing mode is PRESERVE or SEPARATE.
        let mut translation_map_vec: Vec<(String, String)> = Vec::new(); // This map is implemented via Vec<Tuple> because required to preserve duplicates.

        let mut new_translation_map_vec: Vec<(String, String)> = Vec::new();

        let translation: String = read_to_string(txt_file_path).unwrap_log();
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
                        if map.is_empty() && map_number != u16::MAX {
                            if translation != prev_map {
                                translation_maps.insert(map_number, take(&mut map));
                            }

                            map.insert(original.clone(), translation.clone());
                        } else {
                            translation_maps.insert(map_number, take(&mut map));
                            map.insert(original.clone(), translation.clone());
                        }

                        map_number = parse_map_number(&translation);
                        prev_map = translation;
                    } else {
                        map.insert(original, translation);
                    }
                }

                if !map.is_empty() {
                    translation_maps.insert(map_number, map);
                }
            }
            MapsProcessingMode::Preserve => translation_map_vec.extend(parsed_translation),
        }

        let mut skip_indices: HashSet<usize, GxBuildHasher> = HashSet::default();

        if self.purge_empty {
            if self.maps_processing_mode == MapsProcessingMode::Preserve {
                for (i, (original, translation)) in translation_map_vec.iter().enumerate() {
                    if !original.starts_with("<!--") && translation.is_empty() {
                        skip_indices.insert(i);
                    }
                }
            } else {
                for (map_number, map) in translation_maps.iter_mut() {
                    if self.stat {
                        stat_vec
                            .as_mut()
                            .unwrap()
                            .push((String::from("<!-- Map -->"), map_number.to_string()))
                    };

                    let mut ignore_entry: Option<&mut IgnoreEntry> = if self.create_ignore {
                        Some(
                            ignore_map
                                .as_mut()
                                .unwrap()
                                .entry(format!("<!-- File: map{map_number} -->"))
                                .or_default(),
                        )
                    } else {
                        None
                    };

                    for (i, (original, translation)) in map.iter().enumerate() {
                        if !original.starts_with("<!--") && translation.is_empty() {
                            if self.stat {
                                stat_vec
                                    .as_mut()
                                    .unwrap()
                                    .push((original.to_owned(), translation.to_owned()));
                            } else {
                                skip_indices.insert(i);

                                if self.create_ignore {
                                    ignore_entry.as_mut().unwrap().insert(original.to_owned());
                                }
                            }
                        }
                    }
                }
            }
        } else {
            let (_, events_label, pages_label, list_label, code_label, parameters_label) =
                get_maps_labels(self.engine_type);

            let obj_vec_iter = read_dir(self.original_path)
                .unwrap_log()
                .filter_map(|entry| filter_maps(entry, self.engine_type));

            for (filename, path) in obj_vec_iter {
                let obj: Value = parse_rpgm_file(&path, self.engine_type);
                let map_number: u16 = parse_map_number(&filename);

                if self.stat {
                    stat_vec
                        .as_mut()
                        .unwrap()
                        .push((String::from("<!-- Map -->"), map_number.to_string()));
                }
                let mut ignore_entry: Option<&mut IgnoreEntry> = if self.create_ignore {
                    Some(
                        ignore_map
                            .as_mut()
                            .unwrap()
                            .entry(format!("<!-- File: map{map_number} -->"))
                            .or_default(),
                    )
                } else {
                    None
                };

                if self.maps_processing_mode != MapsProcessingMode::Preserve {
                    translation_map = translation_maps.get_mut(&map_number).unwrap_log();
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

                lines_set.clear();

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
                            (code_label, parameters_label),
                            unsafe { &mut *(&mut lines_set as *mut IndexSetGx) },
                            Some(&mut new_translation_map_vec),
                            Some(self.maps_processing_mode),
                            self.trim,
                        );
                    }
                }

                if self.maps_processing_mode != MapsProcessingMode::Preserve {
                    for (i, (original, translation)) in translation_map.iter().enumerate() {
                        if self.leave_filled && !translation.is_empty() {
                            continue;
                        }

                        if !original.starts_with("<!--") && !lines_set.contains(original) {
                            if self.stat {
                                stat_vec
                                    .as_mut()
                                    .unwrap()
                                    .push((original.to_owned(), translation.to_owned()));
                            } else {
                                skip_indices.insert(i);

                                if self.create_ignore {
                                    ignore_entry.as_mut().unwrap().insert(original.to_owned());
                                }
                            }
                        }
                    }
                }
            }
        }

        if !self.stat {
            let mut output_content: String = match self.maps_processing_mode {
                MapsProcessingMode::Default | MapsProcessingMode::Separate => String::from_iter(
                    translation_maps
                        .into_iter()
                        .flat_map(|hashmap| hashmap.1.into_iter())
                        .enumerate()
                        .filter(|(i, _)| !skip_indices.contains(i))
                        .map(|(_, (original, translation))| format!("{original}{LINES_SEPARATOR}{translation}\n")),
                ),
                MapsProcessingMode::Preserve => {
                    for (i, (original, translation)) in translation_map_vec.iter().enumerate() {
                        if self.leave_filled && !translation.is_empty() {
                            continue;
                        }

                        // ! I have no idea, how to implement other args for preserve
                        if !original.starts_with("<!--") && !lines_set.contains(original) {
                            skip_indices.insert(i);
                        }
                    }

                    String::from_iter(
                        translation_map_vec
                            .into_iter()
                            .enumerate()
                            .filter(|(i, _)| !skip_indices.contains(i))
                            .map(|(_, (original, translation))| format!("{original}{LINES_SEPARATOR}{translation}\n")),
                    )
                }
            };

            output_content.pop();
            write(txt_file_path, output_content).unwrap_log();

            if self.logging {
                println!("{PURGED_FILE_MSG} maps.txt");
            }
        };
    }
}

/// A struct for purging unused or empty translation from non-map data files.
///
/// `OtherPurger` analyzes RPG Maker data files (like `Items`, `Actors`, `CommonEvents`, etc.)
/// and their corresponding translation files, removing translation that are no longer
/// needed or are empty. This helps keep translation files clean and focused only on
/// text that actually appears in the game.
///
/// # Fields
///
/// - `original_path` - Path to the directory containing the original data files
/// - `translation_path` - Path to the directory containing the translation files
/// - `romanize` - Whether to romanize non-Latin text
/// - `logging` - Whether to log processing information
/// - `game_type` - Optional specific game type for specialized processing
/// - `engine_type` - The RPG Maker engine type
/// - `stat` - Whether to generate statistics instead of modifying files
/// - `leave_filled` - Whether to leave filled translation even if they're unused
/// - `purge_empty` - Whether to purge empty translation
/// - `create_ignore` - Whether to create an ignore file for purged entries
/// - `trim` - Whether to trim whitespace from strings
pub struct OtherPurger<P: AsRef<Path>> {
    original_path: P,
    translation_path: P,
    romanize: bool,
    logging: bool,
    game_type: Option<GameType>,
    engine_type: EngineType,
    stat: bool,
    leave_filled: bool,
    purge_empty: bool,
    create_ignore: bool,
    trim: bool,
}

impl<P: AsRef<Path>> OtherPurger<P> {
    /// Creates a new `OtherPurger` with default values.
    ///
    /// # Parameters
    ///
    /// - `original_path` - Path to the directory containing the original data files
    /// - `translation_path` - Path to the directory containing the translation files
    /// - `engine_type` - The RPG Maker engine type
    pub fn new(original_path: P, translation_path: P, engine_type: EngineType) -> Self {
        Self::default(original_path, translation_path, engine_type)
    }

    /// Creates a new `OtherPurger` with default values.
    ///
    /// Default values are:
    /// - `maps_processing_mode`: `MapsProcessingMode::Default`
    /// - `romanize`: `false`
    /// - `logging`: `false`
    /// - `game_type`: `None`
    /// - `stat`: `false`
    /// - `leave_filled`: `false`
    /// - `create_ignore`: `false`
    /// - `purge_empty`: `false`
    /// - `trim`: `false`
    ///
    /// # Parameters
    ///
    /// - `original_path` - Path to the directory containing the original data files
    /// - `translation_path` - Path to the directory containing the translation files
    /// - `engine_type` - The RPG Maker engine type
    pub fn default(original_path: P, translation_path: P, engine_type: EngineType) -> Self {
        Self {
            original_path,
            translation_path,
            engine_type,
            romanize: false,
            logging: false,
            game_type: None,
            stat: false,
            leave_filled: false,
            create_ignore: false,
            purge_empty: false,
            trim: false,
        }
    }

    /// Sets whether to romanize text.
    ///
    /// Must be the same value, as in previous read.
    pub fn romanize(mut self, romanize: bool) -> Self {
        self.romanize = romanize;
        self
    }

    /// Sets whether to log processing information.
    ///
    /// When enabled, the purger will log information about the files being processed.
    pub fn logging(mut self, logging: bool) -> Self {
        self.logging = logging;
        self
    }

    /// Sets the game type for specialized processing.
    ///
    /// Must be the same value, as in previous read.
    pub fn game_type(mut self, game_type: Option<GameType>) -> Self {
        self.game_type = game_type;
        self
    }

    /// Sets whether to trim whitespace from strings.
    ///
    /// Must be the same value, as in previous read.
    pub fn trim(mut self, trim: bool) -> Self {
        self.trim = trim;
        self
    }

    /// Sets whether to generate statistics instead of modifying files.
    ///
    /// When enabled, the purger will generate statistics about what would be purged
    /// without actually modifying any files.
    pub fn stat(mut self, stat: bool) -> Self {
        self.stat = stat;
        self
    }

    /// Sets whether to leave filled translation even if they're unused.
    ///
    /// When enabled, translation fields that have content will not be purged even if
    /// they're no longer used in the game.
    pub fn leave_filled(mut self, leave_filled: bool) -> Self {
        self.leave_filled = leave_filled;
        self
    }

    /// Sets whether to create an ignore file for purged entries.
    ///
    /// When enabled, the purger will create a `.rvpacker-ignore` file containing
    /// entries for all purged entries.
    pub fn create_ignore(mut self, create_ignore: bool) -> Self {
        self.create_ignore = create_ignore;
        self
    }

    /// Sets whether to purge empty translation.
    ///
    /// When enabled, only translation fields that are empty will be purged.
    pub fn purge_empty(mut self, purge_empty: bool) -> Self {
        self.purge_empty = purge_empty;
        self
    }

    /// This method analyzes the data files and their translation, removing unused
    /// or empty translation based on the configured settings.
    ///
    /// # Parameters
    ///
    /// - `ignore_map` - Optional map to store ignored entries
    /// - `stat_vec` - Optional vector to store statistics
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use std::path::Path;
    /// use rvpacker_txt_rs_lib::purge::OtherPurger;
    /// use rvpacker_txt_rs_lib::types::EngineType;
    ///
    /// let purger = OtherPurger::new(
    ///     "data",
    ///     "translation",
    ///     EngineType::New
    /// )
    /// .logging(true)
    /// .purge_empty(true);
    ///
    /// purger.purge(None, None);
    /// ```
    #[inline(always)]
    pub fn purge(self, mut ignore_map: Option<&mut IgnoreMap>, mut stat_vec: Option<&mut Vec<(String, String)>>) {
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

        let mut skip_indices: HashSet<usize, GxBuildHasher> = HashSet::default();

        for (filename, path) in read_dir(self.original_path)
            .unwrap_log()
            .filter_map(|entry| filter_other(entry, self.engine_type, self.game_type))
        {
            let basename: String = filename.rsplit_once('.').unwrap_log().0.to_owned().to_lowercase();
            let txt_filename: String = basename.clone() + ".txt";
            let txt_output_path: &Path = &self.translation_path.as_ref().join(txt_filename.clone());

            let mut lines: IndexSetGx = IndexSet::default();
            let lines_mut_ref: &mut IndexSetGx = unsafe { &mut *(&mut lines as *mut IndexSetGx) };

            if self.stat {
                stat_vec
                    .as_mut()
                    .unwrap()
                    .push((format!("<!-- {basename} -->"), String::new()));
            }

            let mut ignore_entry: Option<&mut IgnoreEntry> = if self.create_ignore {
                Some(
                    ignore_map
                        .as_mut()
                        .unwrap()
                        .entry(format!("<!-- File: {basename} -->"))
                        .or_default(),
                )
            } else {
                None
            };

            let translation_map: IndexMapGx = IndexMap::from_iter(parse_translation(
                &read_to_string(txt_output_path).unwrap_log(),
                &txt_filename,
                false,
                true,
            ));

            if self.purge_empty {
                for (i, (original, translation)) in translation_map.iter().enumerate() {
                    if !original.starts_with("<!--") && translation.is_empty() {
                        if self.stat {
                            stat_vec
                                .as_mut()
                                .unwrap()
                                .push((original.to_owned(), translation.to_owned()));
                        } else {
                            skip_indices.insert(i);

                            if self.create_ignore {
                                ignore_entry.as_mut().unwrap().insert(original.to_owned());
                            }
                        }
                    }
                }
            } else {
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

                                let variable_string: String = {
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
                                    variable_string,
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
                                    let mut replaced: String = String::from_iter(parsed.split('\n').map(
                                        |x: &str| if self.trim { x.trim_replace() } else { x.to_owned() } + NEW_LINE,
                                    ));

                                    replaced.drain(replaced.len() - 2..);
                                    lines_mut_ref.insert(replaced);
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
                                (code_label, parameters_label),
                                lines_mut_ref,
                                None,
                                None,
                                self.trim,
                            );
                        }
                    }
                }

                for (i, (original, translation)) in translation_map.iter().enumerate() {
                    if self.leave_filled && !translation.is_empty() {
                        continue;
                    }

                    if !original.starts_with("<!--") && !lines.contains(original) {
                        if self.stat {
                            stat_vec
                                .as_mut()
                                .unwrap()
                                .push((original.to_owned(), translation.to_owned()));
                        } else {
                            skip_indices.insert(i);

                            if self.create_ignore {
                                ignore_entry.as_mut().unwrap().insert(original.to_owned());
                            }
                        }
                    }
                }
            }

            if !self.stat {
                let mut output_content: String = String::from_iter(
                    translation_map
                        .into_iter()
                        .enumerate()
                        .filter(|(i, _)| !skip_indices.contains(i))
                        .map(|(_, (original, translated))| format!("{original}{LINES_SEPARATOR}{translated}\n")),
                );

                output_content.pop();

                write(txt_output_path, output_content).unwrap_log();

                if self.logging {
                    println!("{PURGED_FILE_MSG} {basename}.txt",);
                }
            }

            skip_indices.clear();
        }
    }
}

/// A struct for purging unused or empty translation from the `System` file.
///
/// `SystemPurger` analyzes the RPG Maker `System` file and its corresponding translation file,
/// removing translation that are no longer needed or are empty. This helps keep
/// translation files clean and focused only on text that actually appears in the game.
///
/// # Fields
///
/// - `system_file_path` - Path to the original `System` file
/// - `translation_path` - Path to the directory containing the translation files
/// - `romanize` - Whether to romanize non-Latin text
/// - `logging` - Whether to log processing information
/// - `engine_type` - The RPG Maker engine type
/// - `stat` - Whether to generate statistics instead of modifying files
/// - `leave_filled` - Whether to leave filled translation even if they're unused
/// - `purge_empty` - Whether to purge empty translation
/// - `create_ignore` - Whether to create an ignore file for purged entries
/// - `trim` - Whether to trim whitespace from strings
pub struct SystemPurger<P: AsRef<Path>> {
    system_file_path: P,
    translation_path: P,
    romanize: bool,
    logging: bool,
    engine_type: EngineType,
    stat: bool,
    leave_filled: bool,
    purge_empty: bool,
    create_ignore: bool,
    trim: bool,
}

impl<P: AsRef<Path>> SystemPurger<P> {
    /// Creates a new `SystemPurger` with default values.
    ///
    /// # Parameters
    ///
    /// - `system_file_path` - Path to the original System file
    /// - `translation_path` - Path to the directory containing the translation files
    /// - `engine_type` - The RPG Maker engine type
    pub fn new(system_file_path: P, translation_path: P, engine_type: EngineType) -> Self {
        Self::default(system_file_path, translation_path, engine_type)
    }

    /// Creates a new `SystemPurger` with default values.
    ///
    /// Default values are:
    /// - `romanize`: `false`
    /// - `logging`: `false`
    /// - `stat`: `false`
    /// - `leave_filled`: `false`
    /// - `create_ignore`: `false`
    /// - `purge_empty`: `false`
    /// - `trim`: `false`
    ///
    /// # Parameters
    ///
    /// - `system_file_path` - Path to the original System file
    /// - `translation_path` - Path to the directory containing the translation files
    /// - `engine_type` - The RPG Maker engine type
    pub fn default(system_file_path: P, translation_path: P, engine_type: EngineType) -> Self {
        Self {
            system_file_path,
            translation_path,
            engine_type,
            romanize: false,
            logging: false,
            stat: false,
            leave_filled: false,
            create_ignore: false,
            purge_empty: false,
            trim: false,
        }
    }

    /// Sets whether to romanize text.
    ///
    /// Must be the same value, as in previous read.
    pub fn romanize(mut self, romanize: bool) -> Self {
        self.romanize = romanize;
        self
    }

    /// Sets whether to log processing information.
    ///
    /// When enabled, the purger will log information about the files being processed.
    pub fn logging(mut self, logging: bool) -> Self {
        self.logging = logging;
        self
    }

    /// Sets whether to trim whitespace from strings.
    ///
    /// Must be the same value, as in previous read.
    pub fn trim(mut self, trim: bool) -> Self {
        self.trim = trim;
        self
    }

    /// Sets whether to generate statistics instead of modifying files.
    ///
    /// When enabled, the purger will generate statistics about what would be purged
    /// without actually modifying any files.
    pub fn stat(mut self, stat: bool) -> Self {
        self.stat = stat;
        self
    }

    /// Sets whether to leave filled translation even if they're unused.
    ///
    /// When enabled, translation fields that have content will not be purged even if
    /// they're no longer used in the game.
    pub fn leave_filled(mut self, leave_filled: bool) -> Self {
        self.leave_filled = leave_filled;
        self
    }

    /// Sets whether to create an ignore file for purged entries.
    ///
    /// When enabled, the purger will create a `.rvpacker-ignore` file containing
    /// entries for all purged entries.
    pub fn create_ignore(mut self, create_ignore: bool) -> Self {
        self.create_ignore = create_ignore;
        self
    }

    /// Sets whether to purge empty translation.
    ///
    /// When enabled, only translation fields that are empty will be purged.
    pub fn purge_empty(mut self, purge_empty: bool) -> Self {
        self.purge_empty = purge_empty;
        self
    }

    /// This method analyzes the `System` file and its translation, removing unused
    /// or empty translation based on the configured settings.
    ///
    /// # Parameters
    ///
    /// - `ignore_map` - Optional map to store ignored entries
    /// - `stat_vec` - Optional vector to store statistics
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use std::path::Path;
    /// use rvpacker_txt_rs_lib::purge::SystemPurger;
    /// use rvpacker_txt_rs_lib::types::EngineType;
    ///
    /// let purger = SystemPurger::new(
    ///     "data/System.json",
    ///     "translation",
    ///     EngineType::New
    /// )
    /// .logging(true)
    /// .purge_empty(true);
    ///
    /// purger.purge(None, None);
    /// ```
    #[inline(always)]
    pub fn purge(self, mut ignore_map: Option<&mut IgnoreMap>, mut stat_vec: Option<&mut Vec<(String, String)>>) {
        let txt_output_path: &Path = &self.translation_path.as_ref().join("system.txt");

        let lines: UnsafeCell<IndexSetGx> = UnsafeCell::new(IndexSet::default());
        let lines_mut_ref: &mut IndexSetGx = unsafe { &mut *lines.get() };

        if self.stat {
            stat_vec
                .as_mut()
                .unwrap()
                .push((String::from("<!-- System -->"), String::new()));
        }

        let mut ignore_entry: Option<&mut IgnoreEntry> = if self.create_ignore {
            Some(
                ignore_map
                    .as_mut()
                    .unwrap()
                    .entry(String::from("<!-- File: system -->"))
                    .or_default(),
            )
        } else {
            None
        };

        let translation_map: IndexMapGx = IndexMap::from_iter(parse_translation(
            &read_to_string(txt_output_path).unwrap_log(),
            "system.txt",
            false,
            true,
        ));

        let mut skip_indices: HashSet<usize, GxBuildHasher> = HashSet::default();

        if self.purge_empty {
            for (i, (original, translation)) in translation_map.iter().take(translation_map.len() - 1).enumerate() {
                if !original.starts_with("<!--") && translation.is_empty() {
                    if self.stat {
                        stat_vec
                            .as_mut()
                            .unwrap()
                            .push((original.to_owned(), translation.to_owned()));
                    } else {
                        skip_indices.insert(i);

                        if self.create_ignore {
                            ignore_entry.as_mut().unwrap().insert(original.to_owned());
                        }
                    }
                }
            }
        } else {
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

                    let trimmed = str.trim();

                    if trimmed.is_empty() {
                        return;
                    }

                    if self.trim { trimmed } else { str }.to_owned()
                };

                if self.romanize {
                    string = romanize_string(string)
                }

                lines_mut_ref.insert(string);
            };

            let (
                armor_types_label,
                elements_label,
                skill_types_label,
                terms_label,
                weapon_types_label,
                game_title_label,
            ) = get_system_labels(self.engine_type);

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

                    let game_title =
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
            }

            for (i, (original, translation)) in translation_map.iter().enumerate() {
                if self.leave_filled && !translation.is_empty() {
                    continue;
                }

                if !original.starts_with("<!--") && !lines_mut_ref.contains(original) {
                    if self.stat {
                        stat_vec
                            .as_mut()
                            .unwrap()
                            .push((original.to_owned(), translation.to_owned()));
                    } else {
                        skip_indices.insert(i);

                        if self.create_ignore {
                            ignore_entry.as_mut().unwrap().insert(original.to_owned());
                        }
                    }
                }
            }
        }

        if !self.stat {
            let mut output_content: String = String::from_iter(
                translation_map
                    .into_iter()
                    .enumerate()
                    .filter(|(i, _)| !skip_indices.contains(i))
                    .map(|(_, (original, translated))| format!("{original}{LINES_SEPARATOR}{translated}\n")),
            );

            output_content.pop();

            write(txt_output_path, output_content).unwrap_log();

            if self.logging {
                println!("{PURGED_FILE_MSG} system.txt");
            }
        }
    }
}

/// A struct for purging unused or empty translation from the `plugins.js` file.
///
/// `PluginPurger` analyzes the RPG Maker MV/MZ `plugins.js` file and its corresponding translation file,
/// removing translation that are no longer needed or are empty. This helps keep
/// translation files clean and focused only on text that actually appears in the game.
///
/// # Fields
///
/// - `plugins_file_path` - Path to the original `plugins.js` file
/// - `translation_path` - Path to the directory containing the translation files
/// - `romanize` - Whether to romanize non-Latin text
/// - `logging` - Whether to log processing information
/// - `stat` - Whether to generate statistics instead of modifying files
/// - `leave_filled` - Whether to leave filled translation even if they're unused
/// - `purge_empty` - Whether to purge empty translation
/// - `create_ignore` - Whether to create an ignore file for purged entries
pub struct PluginPurger<P: AsRef<Path>> {
    plugins_file_path: P,
    translation_path: P,
    romanize: bool,
    logging: bool,
    stat: bool,
    leave_filled: bool,
    purge_empty: bool,
    create_ignore: bool,
}

impl<P: AsRef<Path>> PluginPurger<P> {
    /// Creates a new `PluginPurger` with default values.
    ///
    /// # Parameters
    ///
    /// - `plugins_file_path` - Path to the original `plugins.js` file
    /// - `translation_path` - Path to the directory containing the translation files
    pub fn new(plugins_file_path: P, translation_path: P) -> Self {
        Self::default(plugins_file_path, translation_path)
    }

    /// Creates a new `PluginPurger` with default values.
    ///
    /// Default values are:
    /// - `romanize`: `false`
    /// - `logging`: `false`
    /// - `stat`: `false`
    /// - `leave_filled`: `false`
    /// - `create_ignore`: `false`
    /// - `purge_empty`: `false`
    ///
    /// # Parameters
    ///
    /// - `plugins_file_path` - Path to the original `plugins.js` file
    /// - `translation_path` - Path to the directory containing the translation files
    pub fn default(plugins_file_path: P, translation_path: P) -> Self {
        Self {
            plugins_file_path,
            translation_path,
            romanize: false,
            logging: false,
            stat: false,
            leave_filled: false,
            create_ignore: false,
            purge_empty: false,
        }
    }

    /// Sets whether to romanize text.
    ///
    /// Must be the same value, as in previous read.
    pub fn romanize(mut self, romanize: bool) -> Self {
        self.romanize = romanize;
        self
    }

    /// Sets whether to log processing information.
    ///
    /// When enabled, the purger will log information about the files being processed.
    pub fn logging(mut self, logging: bool) -> Self {
        self.logging = logging;
        self
    }

    /// Sets whether to generate statistics instead of modifying files.
    ///
    /// When enabled, the purger will generate statistics about what would be purged
    /// without actually modifying any files.
    pub fn stat(mut self, stat: bool) -> Self {
        self.stat = stat;
        self
    }

    /// Sets whether to leave filled translation even if they're unused.
    ///
    /// When enabled, translation fields that have content will not be purged even if
    /// they're no longer used in the game.
    pub fn leave_filled(mut self, leave_filled: bool) -> Self {
        self.leave_filled = leave_filled;
        self
    }

    /// Sets whether to create an ignore file for purged entries.
    ///
    /// When enabled, the purger will create a `.rvpacker-ignore` file containing
    /// entries for all purged entries.
    pub fn create_ignore(mut self, create_ignore: bool) -> Self {
        self.create_ignore = create_ignore;
        self
    }

    /// Sets whether to purge empty translation.
    ///
    /// When enabled, only translation fields that are empty will be purged.
    pub fn purge_empty(mut self, purge_empty: bool) -> Self {
        self.purge_empty = purge_empty;
        self
    }

    /// This method analyzes the `plugins.js` file and its translation, removing unused
    /// or empty translation based on the configured settings.
    ///
    /// # Parameters
    ///
    /// - `ignore_map` - Optional map to store ignored entries
    /// - `stat_vec` - Optional vector to store statistics
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use std::path::Path;
    /// use rvpacker_txt_rs_lib::purge::PluginPurger;
    ///
    /// let purger = PluginPurger::new(
    ///     "js/plugins.js",
    ///     "translation"
    /// )
    /// .logging(true)
    /// .purge_empty(true);
    ///
    /// purger.purge(None, None);
    /// ```
    #[inline(always)]
    pub fn purge(self, mut ignore_map: Option<&mut IgnoreMap>, mut stat_vec: Option<&mut Vec<(String, String)>>) {
        let txt_output_path: &Path = &self.translation_path.as_ref().join("plugins.txt");

        let mut ignore_entry: Option<&mut IgnoreEntry> = if self.create_ignore {
            Some(
                ignore_map
                    .as_mut()
                    .unwrap()
                    .entry(String::from("<!-- File: plugins -->"))
                    .or_default(),
            )
        } else {
            None
        };

        let mut translation_map: IndexMapGx = IndexMapGx::from_iter(parse_translation(
            &read_to_string(txt_output_path).unwrap_log(),
            "plugins.txt",
            false,
            false,
        ));

        let mut skip_indices: HashSet<usize, GxBuildHasher> = HashSet::default();

        if self.stat {
            stat_vec
                .as_mut()
                .unwrap()
                .push((String::from("<!-- Plugins -->"), String::new()));
        }

        if self.purge_empty {
            for (i, (original, translation)) in translation_map.iter().enumerate() {
                if !original.starts_with("<!--") && translation.is_empty() {
                    if self.stat {
                        stat_vec
                            .as_mut()
                            .unwrap()
                            .push((original.to_owned(), translation.to_owned()));
                    } else {
                        skip_indices.insert(i);

                        if self.create_ignore {
                            ignore_entry.as_mut().unwrap().insert(original.to_owned());
                        }
                    }
                }
            }
        } else {
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
                ProcessingMode::Default,
                None,
            );

            for (i, (original, translation)) in translation_map.iter().enumerate() {
                if self.leave_filled && !translation.is_empty() {
                    continue;
                }

                if !original.starts_with("<!--") && !lines_vec.contains(original) {
                    if self.stat {
                        stat_vec
                            .as_mut()
                            .unwrap()
                            .push((original.to_owned(), translation.to_owned()));
                    } else {
                        skip_indices.insert(i);

                        if self.create_ignore {
                            ignore_entry.as_mut().unwrap().insert(original.to_owned());
                        }
                    }
                }
            }
        }

        if !self.stat {
            let mut output_content: String = String::from_iter(
                translation_map
                    .into_iter()
                    .enumerate()
                    .filter(|(i, _)| !skip_indices.contains(i))
                    .map(|(_, (original, translated))| format!("{original}{LINES_SEPARATOR}{translated}\n")),
            );

            output_content.pop();

            write(txt_output_path, output_content).unwrap_log();

            if self.logging {
                println!("{PURGED_FILE_MSG} plugins.txt");
            }
        }
    }
}

/// A struct for purging unused or empty translation from the `Scripts` file.
///
/// `ScriptPurger` analyzes the RPG Maker `Scripts` file, which contains Ruby code
/// and its corresponding translation file, removing translation
/// that are no longer needed or are empty. This helps keep translation files clean
/// and focused only on text that actually appears in the game.
///
/// # Fields
///
/// - `scripts_file_path` - Path to the original `Scripts` file
/// - `translation_path` - Path to the directory containing the translation files
/// - `romanize` - Whether to romanize non-Latin text
/// - `logging` - Whether to log processing information
/// - `stat` - Whether to generate statistics instead of modifying files
/// - `leave_filled` - Whether to leave filled translation even if they're unused
/// - `purge_empty` - Whether to purge empty translation
/// - `create_ignore` - Whether to create an ignore file for purged entries
pub struct ScriptPurger<P: AsRef<Path>> {
    scripts_file_path: P,
    translation_path: P,
    romanize: bool,
    logging: bool,
    stat: bool,
    leave_filled: bool,
    purge_empty: bool,
    create_ignore: bool,
}

impl<P: AsRef<Path>> ScriptPurger<P> {
    /// Creates a new `ScriptPurger` with default values.
    ///
    /// # Parameters
    ///
    /// - `scripts_file_path` - Path to the original `Scripts` file
    /// - `translation_path` - Path to the directory containing the translation files
    pub fn new(scripts_file_path: P, translation_path: P) -> Self {
        Self::default(scripts_file_path, translation_path)
    }

    /// Creates a new `ScriptPurger` with default values.
    ///
    /// Default values are:
    /// - `romanize`: `false`
    /// - `logging`: `false`
    /// - `stat`: `false`
    /// - `leave_filled`: `false`
    /// - `create_ignore`: `false`
    /// - `purge_empty`: `false`
    ///
    /// # Parameters
    ///
    /// - `scripts_file_path` - Path to the original `Scripts` file
    /// - `translation_path` - Path to the directory containing the translation files
    pub fn default(scripts_file_path: P, translation_path: P) -> Self {
        Self {
            scripts_file_path,
            translation_path,
            romanize: false,
            logging: false,
            stat: false,
            leave_filled: false,
            create_ignore: false,
            purge_empty: false,
        }
    }

    /// Sets whether to romanize text.
    ///
    /// Must be the same value, as in previous read.
    pub fn romanize(mut self, romanize: bool) -> Self {
        self.romanize = romanize;
        self
    }

    /// Sets whether to log processing information.
    ///
    /// When enabled, the purger will log information about the files being processed.
    pub fn logging(mut self, logging: bool) -> Self {
        self.logging = logging;
        self
    }

    /// Sets whether to generate statistics instead of modifying files.
    ///
    /// When enabled, the purger will generate statistics about what would be purged
    /// without actually modifying any files.
    pub fn stat(mut self, stat: bool) -> Self {
        self.stat = stat;
        self
    }

    /// Sets whether to leave filled translation fields even if they're unused.
    ///
    /// When enabled, translation fields that have content will not be purged even if
    /// they're no longer used in the game.
    pub fn leave_filled(mut self, leave_filled: bool) -> Self {
        self.leave_filled = leave_filled;
        self
    }

    /// Sets whether to create an ignore file for purged entries.
    ///
    /// When enabled, the purger will create a `.rvpacker-ignore` file containing
    /// entries for all purged entries.
    pub fn create_ignore(mut self, create_ignore: bool) -> Self {
        self.create_ignore = create_ignore;
        self
    }

    /// Sets whether to purge empty translation.
    ///
    /// When enabled, only translation fields that are empty will be purged.
    pub fn purge_empty(mut self, purge_empty: bool) -> Self {
        self.purge_empty = purge_empty;
        self
    }

    /// This method analyzes the `Scripts` file and its translation, removing unused
    /// or empty translation based on the configured settings.
    ///
    /// # Parameters
    ///
    /// - `ignore_map` - Optional map to store ignored entries
    /// - `stat_vec` - Optional vector to store statistics
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use std::path::Path;
    /// use rvpacker_txt_rs_lib::purge::ScriptPurger;
    ///
    /// let purger = ScriptPurger::new(
    ///     "data/Scripts.rxdata",
    ///     "translation"
    /// )
    /// .logging(true)
    /// .purge_empty(true);
    ///
    /// purger.purge(None, None);
    /// ```
    #[inline(always)]
    pub fn purge(self, mut ignore_map: Option<&mut IgnoreMap>, mut stat_vec: Option<&mut Vec<(String, String)>>) {
        let txt_output_path: &Path = &self.translation_path.as_ref().join("scripts.txt");

        let mut lines_vec: Vec<String> = Vec::new();
        let translation_map: Vec<(String, String)> = Vec::from_iter(parse_translation(
            &read_to_string(txt_output_path).unwrap_log(),
            "scripts.txt",
            false,
            false,
        ));

        if self.stat {
            stat_vec
                .as_mut()
                .unwrap()
                .push((String::from("<!-- Scripts -->"), String::new()));
        }

        let mut ignore_entry: Option<&mut IgnoreEntry> = if self.create_ignore {
            Some(
                ignore_map
                    .as_mut()
                    .unwrap()
                    .entry(String::from("<!-- File: Scripts -->"))
                    .or_default(),
            )
        } else {
            None
        };

        let mut skip_indices: HashSet<usize, GxBuildHasher> = HashSet::default();

        if self.purge_empty {
            for (i, (original, translation)) in translation_map.iter().enumerate() {
                if !original.starts_with("<!--") && translation.is_empty() {
                    if self.stat {
                        stat_vec
                            .as_mut()
                            .unwrap()
                            .push((original.to_owned(), translation.to_owned()));
                    } else {
                        skip_indices.insert(i);

                        if self.create_ignore {
                            ignore_entry.as_mut().unwrap().insert(original.to_owned());
                        }
                    }
                }
            }
        } else {
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

                lines_vec.push(extracted);
            }

            for (i, (original, translation)) in translation_map.iter().enumerate() {
                if self.leave_filled && !translation.is_empty() {
                    continue;
                }

                if !original.starts_with("<!--") && !lines_vec.contains(original) {
                    if self.stat {
                        stat_vec
                            .as_mut()
                            .unwrap()
                            .push((original.to_owned(), translation.to_owned()));
                    } else {
                        skip_indices.insert(i);

                        if self.create_ignore {
                            ignore_entry.as_mut().unwrap().insert(original.to_owned());
                        }
                    }
                }
            }
        }

        if !self.stat {
            let mut output_content: String = String::from_iter(
                translation_map
                    .into_iter()
                    .enumerate()
                    .filter(|(i, _)| !skip_indices.contains(i))
                    .map(|(_, (original, translated))| format!("{original}{LINES_SEPARATOR}{translated}\n")),
            );

            output_content.pop();

            write(txt_output_path, output_content).unwrap_log();

            if self.logging {
                println!("{PURGED_FILE_MSG} scripts.txt");
            }
        }
    }
}
