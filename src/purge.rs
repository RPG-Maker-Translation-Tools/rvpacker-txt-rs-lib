#![allow(clippy::too_many_arguments)]
#[cfg(feature = "log")]
use crate::println;
use crate::{
    functions::{
        ends_with_if_index, extract_strings, filter_maps, filter_other, find_lisa_prefix_index, get_maps_labels,
        get_object_data, get_other_labels, get_system_labels, is_allowed_code, parse_map_number, romanize_string,
        string_is_only_symbols, traverse_json,
    },
    read_to_string_without_bom,
    statics::{
        localization::{AT_POSITION_MSG, COULD_NOT_SPLIT_LINE_MSG, PURGED_FILE_MSG},
        regexes::{INVALID_MULTILINE_VARIABLE_RE, INVALID_VARIABLE_RE},
        ENCODINGS, HASHER, LINES_SEPARATOR, NEW_LINE,
    },
    types::{
        Code, EngineType, GameType, MapsProcessingMode, OptionExt, ProcessingMode, ResultExt, TrimReplace, Variable,
    },
};
use flate2::read::ZlibDecoder;
use indexmap::{IndexMap, IndexSet};
use marshal_rs::{load, StringMode};
use regex::Regex;
use sonic_rs::{from_str, from_value, prelude::*, Array, Value};
use std::{
    cell::UnsafeCell,
    collections::VecDeque,
    fs::{read, read_dir, read_to_string, write},
    io::Read,
    mem::take,
    mem::transmute,
    path::Path,
    str::Chars,
};
use xxhash_rust::xxh3::Xxh3DefaultBuilder;

type IndexSetXxh3 = IndexSet<String, Xxh3DefaultBuilder>;
type IndexMapXxh3 = IndexMap<String, String, Xxh3DefaultBuilder>;

#[inline]
fn parse_translation<'a>(translation: &'a str) -> Box<dyn Iterator<Item = (String, String)> + 'a> {
    Box::new(translation.split('\n').enumerate().filter_map(move |(i, line)| {
        let mut split = line.split(LINES_SEPARATOR);

        if let Some((original, translated)) = split.next().zip(split.last()) {
            Some((original.to_owned(), translated.to_owned()))
        } else {
            eprintln!("{COULD_NOT_SPLIT_LINE_MSG} {line}\n{AT_POSITION_MSG} {i}");
            None
        }
    }))
}

#[allow(clippy::single_match, clippy::match_single_binding, unused_mut)]
#[inline]
fn parse_parameter(
    code: Code,
    mut parameter: &str,
    game_type: Option<GameType>,
    engine_type: EngineType,
    romanize: bool,
) -> Option<String> {
    if string_is_only_symbols(parameter) {
        return None;
    }

    if let Some(game_type) = game_type {
        match game_type {
            GameType::Termina => {
                if parameter
                    .chars()
                    .all(|char: char| char.is_ascii_lowercase() || char.is_ascii_punctuation())
                {
                    return None;
                }

                match code {
                    Code::System => {
                        if !parameter.starts_with("Gab")
                            && (!parameter.starts_with("choice_text") || parameter.ends_with("????"))
                        {
                            return None;
                        }
                    }
                    _ => {}
                }
            }
            GameType::LisaRPG => {
                match code {
                    Code::Dialogue | Code::DialogueStart => {
                        if let Some(i) = find_lisa_prefix_index(parameter) {
                            if string_is_only_symbols(&parameter[i..]) {
                                return None;
                            }

                            if !parameter.starts_with(r"\et") {
                                parameter = &parameter[i..];
                            }
                        }
                    }
                    _ => {}
                } // custom processing for other games
            }
        }
    }

    if engine_type != EngineType::New {
        if let Some(i) = ends_with_if_index(parameter) {
            parameter = &parameter[..i]
        }

        match code {
            Code::Shop => {
                if !parameter.contains("shop_talk") {
                    return None;
                }

                let (_, mut actual_string) = unsafe { parameter.split_once('=').unwrap_unchecked() };

                actual_string = actual_string.trim();

                // removing the quotes
                parameter = &actual_string[1..actual_string.len() - 1];

                if parameter.is_empty() || string_is_only_symbols(parameter) {
                    return None;
                }
            }
            _ => {}
        }
    }

    let mut result: String = parameter.to_owned();

    if romanize {
        result = romanize_string(result);
    }

    Some(result)
}

#[allow(clippy::single_match, clippy::match_single_binding, unused_mut)]
#[inline]
fn parse_variable(
    mut variable_text: String,
    variable_type: &Variable,
    filename: &str,
    game_type: Option<GameType>,
    engine_type: EngineType,
    romanize: bool,
) -> Option<(String, bool)> {
    if string_is_only_symbols(&variable_text) {
        return None;
    }

    if engine_type != EngineType::New {
        if variable_text
            .split('\n')
            .all(|line: &str| line.is_empty() || INVALID_MULTILINE_VARIABLE_RE.is_match(line))
            || INVALID_VARIABLE_RE.is_match(&variable_text)
        {
            return None;
        };

        variable_text = variable_text.replace("\r\n", "\n");
    }

    let mut is_continuation_of_description: bool = false;

    #[allow(clippy::collapsible_match)]
    if let Some(game_type) = game_type {
        match game_type {
            GameType::Termina => {
                if variable_text.contains("---") || variable_text.starts_with("///") {
                    return None;
                }

                match variable_type {
                    Variable::Name | Variable::Nickname => {
                        if filename.starts_with("Ac") {
                            if ![
                                "Levi",
                                "Marina",
                                "Daan",
                                "Abella",
                                "O'saa",
                                "Blood golem",
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
                            .contains(&variable_text.as_str())
                            {
                                return None;
                            }
                        } else if filename.starts_with("Ar") {
                            if variable_text.starts_with("test_armor") {
                                return None;
                            }
                        } else if filename.starts_with("Cl") {
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
                            .contains(&variable_text.as_str())
                            {
                                return None;
                            }
                        } else if filename.starts_with("En") {
                            if ["Spank Tank", "giant", "test"].contains(&variable_text.as_str()) {
                                return None;
                            }
                        } else if filename.starts_with("It") {
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
                            .contains(&variable_text.as_str())
                                || variable_text.starts_with("The Fellowship")
                                || variable_text.starts_with("Studies of")
                                || variable_text.starts_with("Blueish")
                                || variable_text.starts_with("Skeletal")
                                || variable_text.ends_with("soul")
                                || variable_text.ends_with("schematics")
                            {
                                return None;
                            }
                        } else if filename.starts_with("We") && variable_text == "makeshift2" {
                            return None;
                        }
                    }
                    Variable::Message1 | Variable::Message2 | Variable::Message3 | Variable::Message4 => {
                        return None;
                    }
                    Variable::Note => {
                        if filename.starts_with("Ac") {
                            return None;
                        }

                        if !filename.starts_with("Cl") {
                            let mut variable_text_chars: Chars = variable_text.chars();

                            if !variable_text.starts_with("flesh puppetry") {
                                if let Some(first_char) = variable_text_chars.next() {
                                    if let Some(second_char) = variable_text_chars.next() {
                                        if ((first_char == '\n' && second_char != '\n')
                                            || (first_char.is_ascii_alphabetic()
                                                || first_char == '"'
                                                || variable_text.starts_with("4 sticks")))
                                            && !matches!(first_char, '.' | '!' | '/' | '?')
                                        {
                                            is_continuation_of_description = true;
                                        }
                                    }
                                }
                            }

                            if is_continuation_of_description {
                                if let Some((mut left, _)) = variable_text.trim_start().split_once('\n') {
                                    left = left.trim();

                                    if !left.ends_with(['.', '%', '!', '"']) {
                                        return None;
                                    }

                                    variable_text = NEW_LINE.to_owned() + left;
                                } else {
                                    if !variable_text.ends_with(['.', '%', '!', '"']) {
                                        return None;
                                    }

                                    variable_text = NEW_LINE.to_owned() + &variable_text
                                }
                            } else {
                                return None;
                            }
                        }
                    }
                    _ => {}
                }
            }
            _ => {} // custom processing for other games
        }
    }

    if romanize {
        variable_text = romanize_string(variable_text);
    }

    Some((variable_text, is_continuation_of_description))
}

#[inline]
fn parse_list(
    list: &Array,
    romanize: bool,
    game_type: Option<GameType>,
    engine_type: EngineType,
    (code_label, parameters_label): (&str, &str),
    set_mut_ref: &mut IndexSetXxh3,
    mut translation_map_vec: Option<&mut Vec<(String, String)>>,
    maps_processing_mode: Option<MapsProcessingMode>,
) {
    let mut in_sequence: bool = false;

    let mut lines: Vec<&str> = Vec::with_capacity(4);
    let buf: UnsafeCell<Vec<Vec<u8>>> = UnsafeCell::new(Vec::with_capacity(4));

    let mut process_parameter = |code: Code, parameter: &str| {
        if let Some(parsed) = parse_parameter(code, parameter, game_type, engine_type, romanize) {
            if maps_processing_mode.is_some_and(|mode| mode == MapsProcessingMode::Preserve) {
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

            if code == Code::DialogueStart && engine_type != EngineType::XP {
                Code::Bad
            } else {
                code
            }
        } else {
            Code::Bad
        };

        if in_sequence
            && (!matches!(code, Code::Dialogue | Code::DialogueStart | Code::Credit)
                || (engine_type == EngineType::XP && code == Code::DialogueStart && !lines.is_empty()))
        {
            if !lines.is_empty() {
                let joined: String = lines.join(NEW_LINE);

                process_parameter(Code::Dialogue, &joined);

                lines.clear();
                unsafe { (*buf.get()).clear() };
            }

            in_sequence = false;
        }

        if code == Code::Bad {
            continue;
        }

        let parameters: &Array = item[parameters_label].as_array().unwrap_log();

        let value_i: usize = match code {
            Code::Misc1 | Code::Misc2 => 1,
            _ => 0,
        };
        let value: &Value = &parameters[value_i];

        if code == Code::ChoiceArray {
            for i in 0..value.as_array().unwrap_log().len() {
                let mut buf: Vec<u8> = Vec::new();

                let subparameter_string: &str = value[i]
                    .as_str()
                    .unwrap_or_else(|| match value[i].as_object() {
                        Some(obj) => {
                            buf = get_object_data(obj);
                            unsafe { std::str::from_utf8_unchecked(&buf) }
                        }
                        None => unreachable!(),
                    })
                    .trim();

                if subparameter_string.is_empty() {
                    continue;
                }

                process_parameter(code, subparameter_string);
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

            if code != Code::Credit && parameter_string.is_empty() {
                continue;
            }

            if matches!(code, Code::Dialogue | Code::DialogueStart | Code::Credit) {
                lines.push(parameter_string);
                in_sequence = true;
            } else {
                process_parameter(code, parameter_string);
            }
        }
    }
}

/// Reads all Map files of original_path and parses them into .txt files in `output_path`.
/// # Parameters
/// * `original_path` - path to directory that contains game files
/// * `output_path` - path to output directory
/// * `maps_processing_mode` - how to deal with lines duplicates in maps
/// * `romanize` - whether to romanize text
/// * `logging` - whether to log
/// * `game_type` - game type for custom parsing
/// * `engine_type` - which engine's files are we processing, essential for the right processing
/// * `processing_mode` - whether to read in default mode, force rewrite or append new text to existing files
/// * `generate_json` - whether to generate json representations of older engines' files
#[inline(always)]
pub fn purge_map<P: AsRef<Path>>(
    original_path: P,
    output_path: P,
    maps_processing_mode: MapsProcessingMode,
    romanize: bool,
    logging: bool,
    game_type: Option<GameType>,
    engine_type: EngineType,
) {
    let translation_path: &Path = &output_path.as_ref().join("maps.txt");

    // Allocated when maps processing mode is DEFAULT or SEPARATE.
    let mut lines_set: IndexSetXxh3 = IndexSet::with_hasher(HASHER);

    // Allocated when maps processing mode is DEFAULT or SEPARATE.
    // Reads the translation from existing .txt file and then appends new lines.
    let mut translation_map: &mut IndexMapXxh3 = &mut IndexMap::with_hasher(HASHER);

    // Allocated when maps processing mode is SEPARATE.
    let mut translation_maps: IndexMap<u16, IndexMapXxh3> = IndexMap::new();

    // Allocated when maps processing mode is PRESERVE or SEPARATE.
    let mut translation_map_vec: Vec<(String, String)> = Vec::new(); // This map is implemented via Vec<Tuple> because required to preserve duplicates.

    let mut new_translation_map_vec: Vec<(String, String)> = Vec::new();

    let translation: String = read_to_string(translation_path).unwrap_log();
    let parsed_translation: Box<dyn Iterator<Item = (String, String)>> = parse_translation(&translation);

    match maps_processing_mode {
        MapsProcessingMode::Default | MapsProcessingMode::Separate => {
            translation_maps.reserve(512);

            let mut map: IndexMapXxh3 = IndexMap::with_hasher(HASHER);
            let mut map_number: u16 = u16::MAX;
            let mut prev_map: String = String::new();

            for (original, translation) in parsed_translation {
                if original == "<!-- Map -->" {
                    if map.is_empty() {
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

    let (_, events_label, pages_label, list_label, code_label, parameters_label) = get_maps_labels(engine_type);

    let obj_vec_iter = read_dir(original_path)
        .unwrap_log()
        .filter_map(|entry| filter_maps(entry, engine_type));

    for (filename, obj) in obj_vec_iter {
        let map_number: u16 = parse_map_number(&filename);

        if maps_processing_mode != MapsProcessingMode::Preserve {
            translation_map = translation_maps.get_mut(&map_number).unwrap_log();
        }

        let events_arr: Box<dyn Iterator<Item = &Value>> = if engine_type == EngineType::New {
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
                    romanize,
                    game_type,
                    engine_type,
                    (code_label, parameters_label),
                    unsafe { &mut *(&mut lines_set as *mut IndexSetXxh3) },
                    Some(&mut new_translation_map_vec),
                    Some(maps_processing_mode),
                );
            }
        }

        if maps_processing_mode != MapsProcessingMode::Preserve {
            for (original, _) in unsafe { &mut *(translation_map as *mut IndexMapXxh3) } {
                if !original.starts_with("<!--") && !lines_set.contains(original) {
                    translation_map.shift_remove(original);
                }
            }
        }
    }

    let mut output_content: String = match maps_processing_mode {
        MapsProcessingMode::Default | MapsProcessingMode::Separate => String::from_iter(
            translation_maps
                .into_iter()
                .flat_map(|hashmap| hashmap.1.into_iter())
                .map(|(original, translation)| format!("{original}{LINES_SEPARATOR}{translation}\n")),
        ),
        MapsProcessingMode::Preserve => {
            translation_map_vec
                .retain(|x| x.0.starts_with("<!--") || new_translation_map_vec.iter().any(|y| x.0 == y.0));

            String::from_iter(
                translation_map_vec
                    .into_iter()
                    .map(|(original, translation)| format!("{original}{LINES_SEPARATOR}{translation}\n")),
            )
        }
    };

    output_content.pop();
    write(translation_path, output_content).unwrap_log();

    if logging {
        println!("{PURGED_FILE_MSG} maps.txt");
    }
}

/// Reads all other files of original_path and parses them into .txt files in `output_path`.
/// # Parameters
/// * `original_path` - path to directory that contains game files
/// * `output_path` - path to output directory
/// * `romanize` - whether to romanize text
/// * `logging` - whether to log
/// * `game_type` - game type for custom parsing
/// * `processing_mode` - whether to read in default mode, force rewrite or append new text to existing files
/// * `engine_type` - which engine's files are we processing, essential for the right processing
/// * `generate_json` - whether to generate json representations of older engines' files
#[inline(always)]
pub fn purge_other<P: AsRef<Path>>(
    original_path: P,
    output_path: P,
    romanize: bool,
    logging: bool,
    game_type: Option<GameType>,
    engine_type: EngineType,
) {
    let obj_arr_iter = read_dir(original_path)
        .unwrap_log()
        .filter_map(|entry| filter_other(entry, engine_type, game_type));

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
    ) = get_other_labels(engine_type);

    for (filename, obj_arr) in obj_arr_iter {
        let basename: String = filename.rsplit_once('.').unwrap_log().0.to_owned().to_lowercase();
        let txt_output_path: &Path = &output_path.as_ref().join(basename.clone() + ".txt");

        let mut lines: IndexSetXxh3 = IndexSet::with_hasher(HASHER);
        let lines_mut_ref: &mut IndexSetXxh3 = unsafe { &mut *(&mut lines as *mut IndexSetXxh3) };

        let mut translation_map: IndexMapXxh3 = IndexMap::with_hasher(HASHER);
        let translation: String = read_to_string(txt_output_path).unwrap_log();
        translation_map.extend(parse_translation(&translation));

        // Other files except CommonEvents and Troops have the structure that consists
        // of name, nickname, description and note
        if !filename.starts_with("Co") && !filename.starts_with("Tr") {
            if game_type.is_some_and(|game_type: GameType| game_type == GameType::Termina) && filename.starts_with("It")
            {
                lines_mut_ref.extend([
                    String::from("<Menu Category: Items>"),
                    String::from("<Menu Category: Food>"),
                    String::from("<Menu Category: Healing>"),
                    String::from("<Menu Category: Body bag>"),
                ]);
            }

            if let Some(obj_real_arr) = obj_arr.as_array() {
                'obj: for obj in obj_real_arr {
                    let mut prev_variable_type: Option<Variable> = None;

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

                            let string: &str = value.as_str().unwrap_or_else(|| match value.as_object() {
                                Some(obj) => {
                                    buf = get_object_data(obj);
                                    unsafe { std::str::from_utf8_unchecked(&buf) }
                                }
                                None => "",
                            });

                            let trimmed: &str = string.trim();

                            if trimmed.is_empty() {
                                continue;
                            }

                            if variable_type != Variable::Note {
                                trimmed
                            } else {
                                string
                            }
                            .to_owned()
                        };

                        let parsed: Option<(String, bool)> =
                            parse_variable(string, &variable_type, &filename, game_type, engine_type, romanize);

                        if let Some((parsed, is_continuation_of_description)) = parsed {
                            if is_continuation_of_description {
                                if prev_variable_type != Some(Variable::Description) {
                                    continue;
                                }

                                if let Some(last) = lines_mut_ref.pop() {
                                    lines_mut_ref.insert(last.trim_replace() + &parsed);
                                }
                                continue;
                            }

                            prev_variable_type = Some(variable_type);

                            let mut replaced: String =
                                String::from_iter(parsed.split('\n').map(|x: &str| x.trim_replace() + NEW_LINE));

                            replaced.drain(replaced.len() - 2..);
                            replaced = replaced.trim_replace();

                            lines_mut_ref.insert(replaced);
                        } else if variable_type == Variable::Name {
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
                        romanize,
                        game_type,
                        engine_type,
                        (code_label, parameters_label),
                        lines_mut_ref,
                        None,
                        None,
                    );
                }
            }
        }

        for (original, _) in unsafe { &mut *(&mut translation_map as *mut IndexMapXxh3) } {
            if !original.starts_with("<!--") && !lines.contains(original) {
                translation_map.shift_remove(original);
            }
        }

        let mut output_content: String = String::from_iter(
            translation_map
                .into_iter()
                .map(|(original, translated)| format!("{original}{LINES_SEPARATOR}{translated}\n")),
        );

        output_content.pop();

        write(txt_output_path, output_content).unwrap_log();

        if logging {
            println!("{PURGED_FILE_MSG} {}", basename + ".txt");
        }
    }
}

/// Reads System file of system_file_path and parses it into .txt file of `output_path`.
/// # Parameters
/// * `system_file_path` - path to the system file
/// * `output_path` - path to output directory
/// * `romanize` - whether to romanize text
/// * `logging` - whether to log
/// * `processing_mode` - whether to read in default mode, force rewrite or append new text to existing files
/// * `engine_type` - which engine's files are we processing, essential for the right processing
/// * `generate_json` - whether to generate json representations of older engines' files
#[inline(always)]
pub fn purge_system<P: AsRef<Path>>(
    system_file_path: P,
    output_path: P,
    romanize: bool,
    logging: bool,
    engine_type: EngineType,
) {
    let txt_output_path: &Path = &output_path.as_ref().join("system.txt");

    let lines: UnsafeCell<IndexSetXxh3> = UnsafeCell::new(IndexSet::with_hasher(HASHER));
    let lines_mut_ref: &mut IndexSetXxh3 = unsafe { &mut *lines.get() };

    let mut translation_map: IndexMapXxh3 = IndexMap::with_hasher(HASHER);

    let translation: String = read_to_string(txt_output_path).unwrap_log();
    translation_map.extend(parse_translation(&translation));

    let mut parse_str = |value: &Value| {
        let mut string: String = {
            let mut buf: Vec<u8> = Vec::new();

            let str: &str = value
                .as_str()
                .unwrap_or_else(|| match value.as_object() {
                    Some(obj) => {
                        buf = get_object_data(obj);
                        unsafe { std::str::from_utf8_unchecked(&buf) }
                    }
                    None => "",
                })
                .trim();

            if str.is_empty() {
                return;
            }

            str.to_owned()
        };

        if romanize {
            string = romanize_string(string)
        }

        lines_mut_ref.insert(string);
    };

    let (armor_types_label, elements_label, skill_types_label, terms_label, weapon_types_label, game_title_label) =
        get_system_labels(engine_type);

    let obj: Value = if engine_type == EngineType::New {
        from_str(&read_to_string_without_bom(&system_file_path).unwrap_log()).unwrap_log()
    } else {
        load(&read(&system_file_path).unwrap_log(), Some(StringMode::UTF8), Some("")).unwrap_log()
    };

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
        if engine_type != EngineType::New && !key.starts_with("__symbol__") {
            continue;
        }

        if key != "messages" {
            if let Some(arr) = value.as_array() {
                for value in arr {
                    parse_str(value);
                }
            } else if (value.is_object() && value["__type"].as_str().is_some_and(|x| x == "bytes")) || value.is_str() {
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

    if engine_type != EngineType::New {
        parse_str(&obj["__symbol__currency_unit"]);
    }

    // Game title - Translators may add something like "ELFISH TRANSLATION v1.0.0" to the title
    {
        let mut game_title_string: String = {
            let mut buf: Vec<u8> = Vec::new();

            obj[game_title_label]
                .as_str()
                .unwrap_or_else(|| match obj[game_title_label].as_object() {
                    Some(obj) => {
                        buf = get_object_data(obj);
                        unsafe { std::str::from_utf8_unchecked(&buf) }
                    }
                    None => "",
                })
                .trim_replace()
        };

        // We aren't checking if game_title_string is empty because VX and XP don't include game title in System file, and we still need it last

        if romanize {
            game_title_string = romanize_string(game_title_string)
        }

        lines_mut_ref.insert(game_title_string);
    }

    let mut output_content: String = String::from_iter(
        translation_map
            .into_iter()
            .map(|(original, translated)| format!("{original}{LINES_SEPARATOR}{translated}\n")),
    );

    output_content.pop();

    write(txt_output_path, output_content).unwrap_log();

    if logging {
        println!("{PURGED_FILE_MSG} system.txt");
    }
}

/// Reads Scripts file of scripts_file_path and parses it into .txt file of `output_path`.
/// # Parameters
/// * `scripts_file_path` - path to the scripts file
/// * `output_path` - path to output directory
/// * `romanize` - whether to romanize text
/// * `logging` - whether to log
/// * `processing_mode` - whether to read in default mode, force rewrite or append new text to existing files
/// * `generate_json` - whether to generate json representations of older engines' files
#[inline(always)]
pub fn purge_scripts<P: AsRef<Path>>(scripts_file_path: P, output_path: P, romanize: bool, logging: bool) {
    let txt_output_path: &Path = &output_path.as_ref().join("scripts.txt");

    let mut lines: Vec<String> = Vec::new();
    let mut translation_map: Vec<(String, String)> = Vec::new();

    let translation: String = read_to_string(txt_output_path).unwrap_log();
    translation_map.extend(parse_translation(&translation));

    let scripts_entries: Value = load(
        &read(scripts_file_path.as_ref()).unwrap_log(),
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
    let extracted_strings: IndexSetXxh3 = extract_strings(&codes_text, false).0;

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

    lines.reserve_exact(extracted_strings.len());

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

        if romanize {
            extracted = romanize_string(extracted);
        }

        lines.push(extracted);
    }

    let mut output_content: String = String::from_iter(
        translation_map
            .into_iter()
            .map(|(original, translated)| format!("{original}{LINES_SEPARATOR}{translated}\n")),
    );

    output_content.pop();

    write(txt_output_path, output_content).unwrap_log();

    if logging {
        println!("{PURGED_FILE_MSG} scripts.txt");
    }
}

/// * `plugins_file_path` - path to the plugins.js file
/// * `output_path` - path to output directory
/// * `romanize` - whether to romanize text
/// * `logging` - whether to log
/// * `processing_mode` - whether to read in default mode, force rewrite or append new text to existing files
#[inline(always)]
pub fn purge_plugins<P: AsRef<Path>>(
    plugins_file_path: P,
    output_path: P,
    romanize: bool,
    logging: bool,
    processing_mode: ProcessingMode,
) {
    let txt_output_path: &Path = &output_path.as_ref().join("plugins.txt");

    let mut translation_map: VecDeque<(String, String)> = VecDeque::new();

    let translation: String = read_to_string(txt_output_path).unwrap_log();
    translation_map.extend(parse_translation(&translation));

    let plugins_content: String = read_to_string(plugins_file_path.as_ref()).unwrap_log();

    let plugins_object: &str = plugins_content
        .split_once('=')
        .unwrap_log()
        .1
        .trim_end_matches([';', '\n']);

    let mut plugins_json: Value = from_str(plugins_object).unwrap_log();
    let mut lines: Vec<String> = Vec::new();

    traverse_json(
        None,
        &mut plugins_json,
        &mut Some(&mut lines),
        &mut Some(&mut translation_map),
        &None,
        false,
        romanize,
        processing_mode,
    );

    let mut output_content: String = String::from_iter(
        translation_map
            .into_iter()
            .map(|(original, translated)| format!("{original}{LINES_SEPARATOR}{translated}\n")),
    );

    output_content.pop();

    write(txt_output_path, output_content).unwrap_log();

    if logging {
        println!("{PURGED_FILE_MSG} plugins.json");
    }
}
