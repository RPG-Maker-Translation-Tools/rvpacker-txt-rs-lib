#![allow(clippy::too_many_arguments)]
#[cfg(feature = "log")]
use crate::println;
use crate::{
    determine_extension,
    functions::{
        extract_strings, filter_maps, filter_other, get_maps_labels, get_object_data, get_other_labels,
        get_system_labels, is_allowed_code, parse_ignore, parse_map_number, parse_translation, process_parameter,
        process_variable, romanize_string, string_is_only_symbols, traverse_json,
    },
    read_to_string_without_bom,
    statics::{
        localization::{FILES_ARE_NOT_PARSED_MSG, FILE_ALREADY_EXISTS_MSG, PARSED_FILE_MSG},
        ENCODINGS, HASHER, LINES_SEPARATOR, NEW_LINE,
    },
    types::{
        Code, EngineType, GameType, IgnoreMap, IndexMapXxh3, IndexSetXxh3, MapsProcessingMode, OptionExt,
        ProcessingMode, ResultExt, TrimReplace, Variable,
    },
};
use flate2::read::ZlibDecoder;
use indexmap::{IndexMap, IndexSet};
use marshal_rs::{load, StringMode};
use regex::Regex;
use sonic_rs::{from_str, from_value, prelude::*, Array, Value};
use std::{
    cell::UnsafeCell,
    collections::{HashSet, VecDeque},
    fs::{read, read_dir, read_to_string, write},
    io::Read,
    mem::{take, transmute},
    path::{Path, PathBuf},
};
use xxhash_rust::xxh3::Xxh3DefaultBuilder;

#[inline(always)]
fn parse_list<'a>(
    list: &Array,
    romanize: bool,
    game_type: Option<GameType>,
    engine_type: EngineType,
    processing_mode: ProcessingMode,
    (code_label, parameters_label): (&str, &str),
    map: &'a mut IndexMapXxh3,
    set: &'a mut IndexSetXxh3,
    mut translation_map_vec: Option<&mut Vec<(String, String)>>,
    lines_pos: &mut usize,
    maps_processing_mode: Option<MapsProcessingMode>,
    ignore_entry: Option<&HashSet<String, Xxh3DefaultBuilder>>,
) {
    let mut in_sequence: bool = false;

    let mut lines_vec: Vec<&str> = Vec::with_capacity(4);
    let buf: UnsafeCell<Vec<Vec<u8>>> = UnsafeCell::new(Vec::with_capacity(4));

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

                if processing_mode == ProcessingMode::Append {
                    if vec.get(pos).is_some_and(|(o, _)| *o != parsed) {
                        vec.insert(pos, (parsed, String::new()));
                    }
                } else {
                    vec.push((parsed, String::new()))
                }

                *lines_pos += 1;
            } else {
                let absent: bool = set.insert(parsed.clone());
                let pos: usize = if maps_processing_mode == Some(MapsProcessingMode::Default) {
                    if absent {
                        *lines_pos += 1;
                    }

                    *lines_pos
                } else {
                    set.len() - 1
                };

                if processing_mode == ProcessingMode::Append {
                    if !map.contains_key(&parsed)
                        && (matches!(maps_processing_mode, None | Some(MapsProcessingMode::Separate))
                            || !set.contains(&parsed))
                    {
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
                || (engine_type == EngineType::XP && code == Code::DialogueStart && !lines_vec.is_empty()))
        {
            if !lines_vec.is_empty() {
                let joined: String = lines_vec.join(NEW_LINE);

                process_parameter(Code::Dialogue, &joined);

                lines_vec.clear();
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
                        std::str::from_utf8_unchecked(&(*buf.get())[lines_vec.len()])
                    },
                    None => "",
                })
                .trim();

            if code != Code::Credit && parameter_string.is_empty() {
                continue;
            }

            if matches!(code, Code::Dialogue | Code::DialogueStart | Code::Credit) {
                lines_vec.push(parameter_string);
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
#[inline(always)]
pub fn read_map<P: AsRef<Path>>(
    original_path: P,
    output_path: P,
    maps_processing_mode: MapsProcessingMode,
    romanize: bool,
    logging: bool,
    game_type: Option<GameType>,
    engine_type: EngineType,
    processing_mode: ProcessingMode,
    ignore: bool,
) {
    let txt_output_path: &Path = &output_path.as_ref().join("maps.txt");

    if processing_mode == ProcessingMode::Default && txt_output_path.exists() {
        println!("maps.txt {FILE_ALREADY_EXISTS_MSG}");
        return;
    }

    // Allocated when maps processing mode is DEFAULT or SEPARATE.
    let mut lines_set: IndexSetXxh3 = IndexSet::with_hasher(HASHER);

    // Allocated when maps processing mode is DEFAULT or SEPARATE.
    // Reads the translation from existing .txt file and then appends new lines.
    let translation_map: &mut IndexMapXxh3 = &mut IndexMap::with_hasher(HASHER);

    // Allocated when maps processing mode is SEPARATE.
    let mut translation_maps: IndexMap<u16, IndexMapXxh3> = IndexMap::new();

    // Allocated when maps processing mode is PRESERVE or SEPARATE.
    let mut translation_map_vec: Vec<(String, String)> = Vec::new(); // This map is implemented via Vec<Tuple> because required to preserve duplicates.

    // Used when maps processing mode is PRESERVE.
    let mut lines_pos: usize = 0;

    let mut ignore_map: IndexMap<String, HashSet<String, Xxh3DefaultBuilder>, Xxh3DefaultBuilder> =
        IndexMap::with_hasher(HASHER);

    if processing_mode == ProcessingMode::Append {
        if txt_output_path.exists() {
            if ignore {
                ignore_map = parse_ignore(output_path.as_ref().join(".rvpacker-ignore"));
            }

            let translation: String = read_to_string(txt_output_path).unwrap_log();
            let parsed_translation: Box<dyn Iterator<Item = (String, String)>> =
                parse_translation(&translation, "maps.txt", false);
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
        } else {
            println!("{FILES_ARE_NOT_PARSED_MSG}");
            return;
        }
    };

    let (display_name_label, events_label, pages_label, list_label, code_label, parameters_label) =
        get_maps_labels(engine_type);

    let mapinfos_path: PathBuf = original_path
        .as_ref()
        .join("MapInfos".to_owned() + determine_extension(engine_type));
    let mapinfos: Value = if engine_type == EngineType::New {
        from_str(&read_to_string(mapinfos_path).unwrap_log()).unwrap_log()
    } else {
        load(&read(mapinfos_path).unwrap_log(), Some(StringMode::UTF8), None).unwrap_log()
    };

    let obj_vec_iter = read_dir(original_path)
        .unwrap_log()
        .filter_map(|entry| filter_maps(entry, engine_type));

    for (filename, obj) in obj_vec_iter {
        let map_number: u16 = parse_map_number(&filename);
        let map_number_string: String = map_number.to_string();

        let ignore_entry = ignore_map.get(&format!("<!-- File: map{map_number}"));

        let map_number_comment: String = String::from("<!-- Map -->");
        let mut map_name_comment: String = String::new();

        let order_number: String = {
            let entry: String = format!("__integer__{map_number}");

            let order_number: String = if engine_type == EngineType::New {
                &mapinfos[map_number as usize]["order"]
            } else {
                &mapinfos[&entry]["__symbol__@order"]
            }
            .as_u64()
            .unwrap_log()
            .to_string();

            let map_name: String = if engine_type == EngineType::New {
                &mapinfos[map_number as usize]["name"]
            } else {
                &mapinfos[&entry]["__symbol__@name"]
            }
            .as_str()
            .unwrap_log()
            .to_string();

            if !map_name.is_empty() {
                map_name_comment = format!("<!-- Map Name: {map_name} -->");
            }

            order_number
        };

        let mut map_display_name_comment: String = String::new();

        if let Some(display_name) = obj[display_name_label].as_str() {
            if !display_name.is_empty() {
                let mut display_name_string: String = display_name.to_owned();

                if romanize {
                    display_name_string = romanize_string(display_name_string);
                }

                map_display_name_comment = format!("<!-- In-game Displayed Name: {display_name_string} -->");
            }
        }
        let order: String = String::from("<!-- Order -->");

        match (processing_mode, maps_processing_mode) {
            (
                ProcessingMode::Default | ProcessingMode::Force,
                MapsProcessingMode::Default | MapsProcessingMode::Separate,
            ) => {
                translation_map.insert(map_number_comment.clone(), map_number_string.clone());
                translation_map.insert(map_name_comment.clone(), String::new());

                if !map_display_name_comment.is_empty() {
                    translation_map.insert(map_display_name_comment.clone(), String::new());
                }

                translation_map.insert(order.clone(), order_number.clone());
                translation_maps.insert(map_number, take(translation_map));

                if maps_processing_mode == MapsProcessingMode::Separate {
                    lines_set.clear();
                }
            }
            (ProcessingMode::Default | ProcessingMode::Force, MapsProcessingMode::Preserve) => {
                translation_map_vec.push((map_number_comment.clone(), map_number_string.clone()));
                translation_map_vec.push((map_name_comment.clone(), String::new()));

                if !map_display_name_comment.is_empty() {
                    translation_map_vec.push((map_display_name_comment.clone(), String::new()));
                    lines_pos += 1;
                }

                translation_map_vec.push((order.clone(), order_number.clone()));
                lines_pos += 3;
            }
            (ProcessingMode::Append, MapsProcessingMode::Default | MapsProcessingMode::Separate) => {
                let translation_maps_mut: &mut IndexMap<u16, IndexMapXxh3> =
                    unsafe { &mut *(&mut translation_maps as *mut IndexMap<u16, IndexMapXxh3>) };

                let translation_map: &mut IndexMap<String, String, Xxh3DefaultBuilder> =
                    translation_maps_mut.entry(map_number).or_insert_with(|| {
                        let mut new_map = IndexMap::with_hasher(HASHER);
                        new_map.insert(map_number_comment.clone(), map_number_string.clone());
                        new_map
                    });

                let mut map_number_comment_index: usize = translation_map
                    .iter()
                    .take(4)
                    .position(|(k, _)| k == "<!-- Map -->")
                    .unwrap();

                if !map_name_comment.is_empty() && !translation_map.contains_key(&map_name_comment) {
                    if let Some(index) = translation_map
                        .iter()
                        .take(4)
                        .position(|(k, _)| k.starts_with("<!-- Map Name"))
                    {
                        translation_map.shift_remove_index(index);
                    }

                    translation_map.shift_insert(map_number_comment_index + 1, map_name_comment, String::new());
                    map_number_comment_index += 1;
                }

                if !map_display_name_comment.is_empty() && !translation_map.contains_key(&map_display_name_comment) {
                    let mut translation: String = String::new();

                    if let Some(index) = translation_map
                        .iter()
                        .take(4)
                        .position(|(k, _)| k.starts_with("<!-- In-game"))
                    {
                        if let Some((_, t)) = translation_map.shift_remove_index(index) {
                            translation = t
                        }
                    }

                    translation_map.shift_insert(
                        map_number_comment_index + 1,
                        map_display_name_comment.clone(),
                        translation,
                    );
                    map_number_comment_index += 1;
                }

                if let Some(x) = translation_map.get(&order) {
                    if *x != order_number {
                        translation_map.insert(order.clone(), order_number.clone());
                    }
                } else {
                    translation_map.shift_insert(map_number_comment_index + 1, order.clone(), order_number.clone());
                }

                match maps_processing_mode {
                    MapsProcessingMode::Separate => lines_set.clear(),
                    _ => lines_pos = 0,
                }
            }
            (ProcessingMode::Append, MapsProcessingMode::Preserve) => {
                let mut map_number_comment_index: usize = translation_map_vec
                    .iter()
                    .skip(lines_pos)
                    .take(4)
                    .position(|(k, _)| k == "<!-- Map -->")
                    .unwrap();

                if !map_name_comment.is_empty() {
                    if let Some(index) = translation_map_vec
                        .iter()
                        .take(4)
                        .position(|(k, _)| k.starts_with("<!-- Map Name"))
                    {
                        translation_map_vec.remove(index);
                    }

                    translation_map_vec.insert(map_number_comment_index + 1, (map_name_comment, String::new()));
                    map_number_comment_index += 1;
                }

                if !map_display_name_comment.is_empty() {
                    let mut translation: String = String::new();

                    if let Some(index) = translation_map_vec
                        .iter()
                        .take(4)
                        .position(|(k, _)| k.starts_with("<!-- In-game"))
                    {
                        if translation_map_vec.get(index).is_some() {
                            let (_, t) = translation_map_vec.remove(index);
                            translation = t;
                        }
                    }

                    translation_map_vec.insert(
                        map_number_comment_index + 1,
                        (map_display_name_comment.clone(), translation),
                    );
                    map_number_comment_index += 1;
                }

                translation_map_vec.insert(map_number_comment_index + 1, (order.clone(), order_number.clone()));
                lines_pos += 3;
            }
        }

        if maps_processing_mode != MapsProcessingMode::Preserve && !map_display_name_comment.is_empty() {
            lines_set.insert(map_display_name_comment);
            lines_pos += 1;
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
                    processing_mode,
                    (code_label, parameters_label),
                    unsafe { &mut *(translation_map as *mut IndexMapXxh3) },
                    unsafe { &mut *(&mut lines_set as *mut IndexSetXxh3) },
                    Some(&mut translation_map_vec),
                    &mut lines_pos,
                    Some(maps_processing_mode),
                    ignore_entry,
                );
            }
        }

        if logging {
            println!("{PARSED_FILE_MSG} {filename}");
        }
    }

    if maps_processing_mode != MapsProcessingMode::Preserve {
        translation_maps.last_mut().unwrap().1.extend(translation_map.drain(..));
    }

    let mut output_content: String = match maps_processing_mode {
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

/// Reads all other files of original_path and parses them into .txt files in `output_path`.
/// # Parameters
/// * `original_path` - path to directory that contains game files
/// * `output_path` - path to output directory
/// * `romanize` - whether to romanize text
/// * `logging` - whether to log
/// * `game_type` - game type for custom parsing
/// * `processing_mode` - whether to read in default mode, force rewrite or append new text to existing files
/// * `engine_type` - which engine's files are we processing, essential for the right processing
#[inline(always)]
pub fn read_other<P: AsRef<Path>>(
    original_path: P,
    output_path: P,
    romanize: bool,
    logging: bool,
    game_type: Option<GameType>,
    processing_mode: ProcessingMode,
    engine_type: EngineType,
    ignore: bool,
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

    let mut ignore_map: IgnoreMap = IndexMap::default();

    if ignore {
        ignore_map = parse_ignore(output_path.as_ref().join(".rvpacker-ignore"));
    }

    for (filename, obj_arr) in obj_arr_iter {
        let basename: String = filename.rsplit_once('.').unwrap_log().0.to_owned().to_lowercase();
        let txt_filename: String = basename.clone() + ".txt";
        let txt_output_path: &Path = &output_path.as_ref().join(txt_filename.clone());

        if processing_mode == ProcessingMode::Default && txt_output_path.exists() {
            println!("{:?} {FILE_ALREADY_EXISTS_MSG}", unsafe {
                txt_output_path.file_name().unwrap_unchecked()
            });
            continue;
        }

        let ignore_entry = ignore_map.get(&format!("<!-- File: {basename} -->"));

        let mut lines_set: IndexSetXxh3 = IndexSet::with_hasher(HASHER);
        let lines_mut_ref: &mut IndexSetXxh3 = unsafe { &mut *(&mut lines_set as *mut IndexSetXxh3) };
        let lines_ref: &IndexSetXxh3 = unsafe { &*(&mut lines_set as *mut IndexSetXxh3) };

        let mut translation_map: IndexMapXxh3 = IndexMap::with_hasher(HASHER);

        if processing_mode == ProcessingMode::Append {
            if txt_output_path.exists() {
                let translation: String = read_to_string(txt_output_path).unwrap_log();
                translation_map.extend(parse_translation(&translation, &txt_filename, false));
            } else {
                println!("{FILES_ARE_NOT_PARSED_MSG}");
                continue;
            }
        }

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

                        let note_text: Option<&str> =
                            if game_type == Some(GameType::Termina) && variable_type == Variable::Description {
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
                            game_type,
                            engine_type,
                            romanize,
                            None,
                            false,
                        );

                        if let Some(parsed) = parsed {
                            let mut replaced: String =
                                String::from_iter(parsed.split('\n').map(|x: &str| x.trim_replace() + NEW_LINE));

                            replaced.drain(replaced.len() - 2..);
                            replaced = replaced.trim_replace();

                            lines_mut_ref.insert(replaced);
                            let string_ref: &str = unsafe { lines_ref.last().unwrap_unchecked() }.as_str();

                            if processing_mode == ProcessingMode::Append && !translation_map.contains_key(string_ref) {
                                translation_map.shift_insert(lines_ref.len() - 1, string_ref.to_owned(), String::new());
                            }
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

                let commonevent_name: &str = obj[if engine_type == EngineType::New {
                    "name"
                } else {
                    "__symbol__name"
                }]
                .as_str()
                .unwrap_log();

                if !commonevent_name.is_empty() {
                    let event_name_comment: String = format!("<!-- Event Name: {commonevent_name} -->");
                    lines_set.insert(event_name_comment.clone());

                    if processing_mode == ProcessingMode::Append && !translation_map.contains_key(commonevent_name) {
                        translation_map.shift_insert(lines_set.len() - 1, event_name_comment, String::new());
                    } else {
                        translation_map.insert(event_name_comment, String::new());
                    }
                }

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
                        processing_mode,
                        (code_label, parameters_label),
                        unsafe { &mut *(&mut translation_map as *mut IndexMapXxh3) },
                        lines_mut_ref,
                        None,
                        &mut 0,
                        None,
                        ignore_entry,
                    );
                }
            }
        }

        let mut output_content: String = if processing_mode == ProcessingMode::Append {
            String::from_iter(
                translation_map
                    .into_iter()
                    .map(|(original, translated)| format!("{original}{LINES_SEPARATOR}{translated}\n")),
            )
        } else {
            String::from_iter(lines_set.into_iter().map(|line: String| line + LINES_SEPARATOR + "\n"))
        };

        output_content.pop();

        write(txt_output_path, output_content).unwrap_log();

        if logging {
            println!("{PARSED_FILE_MSG} {filename}");
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
#[inline(always)]
pub fn read_system<P: AsRef<Path>>(
    system_file_path: P,
    output_path: P,
    romanize: bool,
    logging: bool,
    processing_mode: ProcessingMode,
    engine_type: EngineType,
    ignore: bool,
) {
    let txt_output_path: &Path = &output_path.as_ref().join("system.txt");

    if processing_mode == ProcessingMode::Default && txt_output_path.exists() {
        println!("system.txt {FILE_ALREADY_EXISTS_MSG}");
        return;
    }

    let mut ignore_map: IgnoreMap = IndexMap::default();

    let mut lines_set: IndexSetXxh3 = IndexSet::with_hasher(HASHER);
    let lines_mut_ref: &mut IndexSetXxh3 = unsafe { &mut *(&mut lines_set as *mut IndexSetXxh3) };
    let lines_ref: &IndexSetXxh3 = unsafe { &*(&lines_set as *const IndexSetXxh3) };

    let mut translation_map: IndexMapXxh3 = IndexMap::with_hasher(HASHER);

    if processing_mode == ProcessingMode::Append {
        if txt_output_path.exists() {
            if ignore {
                ignore_map = parse_ignore(output_path.as_ref().join(".rvpacker-ignore"));
            }

            let translation: String = read_to_string(txt_output_path).unwrap_log();

            translation_map.extend(parse_translation(&translation, "system.txt", false));
        } else {
            println!("{FILES_ARE_NOT_PARSED_MSG}");
            return;
        }
    }

    let ignore_entry = ignore_map.get("<!-- File: system -->");

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

        if let Some(entry) = ignore_entry {
            if entry.contains(&string) {
                return;
            }
        }

        lines_mut_ref.insert(string);
        let string_ref: &str = unsafe { lines_ref.last().unwrap_unchecked() }.as_str();

        if processing_mode == ProcessingMode::Append && !translation_map.contains_key(string_ref) {
            translation_map.shift_insert(lines_ref.len() - 1, string_ref.to_owned(), String::new());
        }
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
        let string_ref: &str = unsafe { lines_ref.last().unwrap_unchecked() }.as_str();

        if processing_mode == ProcessingMode::Append && !translation_map.contains_key(string_ref) {
            translation_map.shift_insert(lines_ref.len() - 1, string_ref.to_owned(), String::new());
        }
    }

    let mut output_content: String = if processing_mode == ProcessingMode::Append {
        String::from_iter(
            translation_map
                .into_iter()
                .map(|(original, translated)| format!("{original}{LINES_SEPARATOR}{translated}\n")),
        )
    } else {
        String::from_iter(lines_set.into_iter().map(|line: String| line + LINES_SEPARATOR + "\n"))
    };

    output_content.pop();

    write(txt_output_path, output_content).unwrap_log();

    if logging {
        println!("{PARSED_FILE_MSG} {:?}", unsafe {
            system_file_path.as_ref().file_name().unwrap_unchecked()
        });
    }
}

/// Reads Scripts file of scripts_file_path and parses it into .txt file of `output_path`.
/// # Parameters
/// * `scripts_file_path` - path to the scripts file
/// * `output_path` - path to output directory
/// * `romanize` - whether to romanize text
/// * `logging` - whether to log
/// * `processing_mode` - whether to read in default mode, force rewrite or append new text to existing files
#[inline(always)]
pub fn read_scripts<P: AsRef<Path>>(
    scripts_file_path: P,
    output_path: P,
    romanize: bool,
    logging: bool,
    processing_mode: ProcessingMode,
    ignore: bool,
) {
    let txt_output_path: &Path = &output_path.as_ref().join("scripts.txt");

    if processing_mode == ProcessingMode::Default && txt_output_path.exists() {
        println!("scripts.txt {FILE_ALREADY_EXISTS_MSG}");
        return;
    }

    let mut ignore_map: IgnoreMap = IndexMap::default();

    let mut lines_vec: Vec<String> = Vec::new();
    let mut translation_map: Vec<(String, String)> = Vec::new();

    if processing_mode == ProcessingMode::Append {
        if txt_output_path.exists() {
            if ignore {
                ignore_map = parse_ignore(output_path.as_ref().join(".rvpacker-ignore"));
            }

            let translation: String = read_to_string(txt_output_path).unwrap_log();
            translation_map.extend(parse_translation(&translation, "scripts.txt", false));
        } else {
            println!("{FILES_ARE_NOT_PARSED_MSG}");
            return;
        }
    }

    let ignore_entry = ignore_map.get("<!-- File: scripts -->");

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

    lines_vec.reserve_exact(extracted_strings.len());

    if processing_mode == ProcessingMode::Append {
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

        if romanize {
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

        if processing_mode == ProcessingMode::Append && translation_map.get(pos).is_some_and(|x| *last != x.0) {
            translation_map.insert(pos, (last.to_owned(), String::new()));
        }
    }

    let mut output_content: String = if processing_mode == ProcessingMode::Append {
        String::from_iter(
            translation_map
                .into_iter()
                .map(|(original, translated)| format!("{original}{LINES_SEPARATOR}{translated}\n")),
        )
    } else {
        String::from_iter(lines_vec.into_iter().map(|line: String| line + LINES_SEPARATOR + "\n"))
    };

    output_content.pop();

    write(txt_output_path, output_content).unwrap_log();

    if logging {
        println!("{PARSED_FILE_MSG} {:?}", unsafe {
            scripts_file_path.as_ref().file_name().unwrap_unchecked()
        });
    }
}

/// * `plugins_file_path` - path to the plugins.js file
/// * `output_path` - path to output directory
/// * `romanize` - whether to romanize text
/// * `logging` - whether to log
/// * `processing_mode` - whether to read in default mode, force rewrite or append new text to existing files
#[inline(always)]
pub fn read_plugins<P: AsRef<Path>>(
    plugins_file_path: P,
    output_path: P,
    romanize: bool,
    logging: bool,
    processing_mode: ProcessingMode,
    ignore: bool,
) {
    let txt_output_path: &Path = &output_path.as_ref().join("plugins.txt");

    if processing_mode == ProcessingMode::Default && txt_output_path.exists() {
        println!("scripts.txt {FILE_ALREADY_EXISTS_MSG}");
        return;
    }

    let mut ignore_map: IgnoreMap = IndexMap::default();

    let mut translation_map: VecDeque<(String, String)> = VecDeque::new();
    let translation: String;

    if processing_mode == ProcessingMode::Append {
        if ignore {
            ignore_map = parse_ignore(output_path.as_ref().join(".rvpacker-ignore"));
        }

        if txt_output_path.exists() {
            translation = read_to_string(txt_output_path).unwrap_log();
            translation_map.extend(parse_translation(&translation, "plugins.txt", false));
        } else {
            println!("{FILES_ARE_NOT_PARSED_MSG}");
            return;
        }
    }

    let ignore_entry = ignore_map.get("<!-- File: plugins -->");

    let plugins_content: String = read_to_string(plugins_file_path.as_ref()).unwrap_log();

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
        &None,
        false,
        romanize,
        processing_mode,
        ignore_entry,
    );

    let mut output_content: String = if processing_mode == ProcessingMode::Append {
        String::from_iter(
            translation_map
                .into_iter()
                .map(|(original, translated)| format!("{original}{LINES_SEPARATOR}{translated}\n")),
        )
    } else {
        String::from_iter(lines_vec.into_iter().map(|line: String| line + LINES_SEPARATOR + "\n"))
    };

    output_content.pop();

    write(txt_output_path, output_content).unwrap_log();

    if logging {
        println!("{PARSED_FILE_MSG} {:?}", unsafe {
            plugins_file_path.as_ref().file_name().unwrap_unchecked()
        });
    }
}
