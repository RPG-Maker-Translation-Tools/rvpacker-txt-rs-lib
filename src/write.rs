#![allow(clippy::too_many_arguments)]
#[cfg(feature = "log")]
use crate::{eprintln, println};
use crate::{
    functions::{
        determine_extension, extract_strings, filter_maps, filter_other, get_maps_labels, get_object_data,
        get_other_labels, get_system_labels, read_to_string_without_bom, romanize_string,
    },
    statics::{
        localization::{AT_POSITION_MSG, COULD_NOT_SPLIT_LINE_MSG, IN_FILE_MSG, WROTE_FILE_MSG},
        regexes::{ENDS_WITH_IF_RE, LISA_PREFIX_RE, STRING_IS_ONLY_SYMBOLS_RE},
        ALLOWED_CODES, ENCODINGS, HASHER, LINES_SEPARATOR, NEW_LINE,
    },
    types::{Code, EngineType, GameType, MapsProcessingMode, OptionExt, ResultExt, Variable},
};
use flate2::{read::ZlibDecoder, write::ZlibEncoder, Compression};
use marshal_rs::{dump, load, StringMode};
use rayon::prelude::*;
use sonic_rs::{from_str, from_value, json, prelude::*, to_string, to_vec, Array, Object, Value};
use std::{
    collections::{HashMap, HashSet, VecDeque},
    fs::{read, read_dir, read_to_string, write},
    io::{Read, Write},
    mem::{take, transmute},
    path::Path,
    str::Chars,
    sync::{Arc, Mutex},
};
use xxhash_rust::xxh3::{Xxh3Builder, Xxh3DefaultBuilder};

type StringHashMap = HashMap<String, String, Xxh3DefaultBuilder>;

fn parse_translation(translation: String, file: &str) -> StringHashMap {
    HashMap::from_iter(translation.split('\n').enumerate().filter_map(|(i, line)| {
        if line.starts_with("<!--") {
            None
        } else if let Some((original, translated)) = line.split_once(LINES_SEPARATOR) {
            #[cfg(not(debug_assertions))]
            if translated.is_empty() {
                return None;
            }

            Some((
                original.replace(r"\#", "\n").trim().to_owned(),
                translated.replace(r"\#", "\n").trim().to_owned(),
            ))
        } else {
            eprintln!(
                "{COULD_NOT_SPLIT_LINE_MSG} ({line})\n{AT_POSITION_MSG} {i}\n{IN_FILE_MSG} {file}",
                i = i + 1
            );
            None
        }
    }))
}

fn process_parameter(
    code: Code,
    mut parameter: String,
    map: Option<&StringHashMap>,
    deque: Option<Arc<Mutex<VecDeque<String>>>>,
    game_type: Option<GameType>,
    engine_type: EngineType,
    romanize: bool,
    value: &mut Value,
) {
    if romanize {
        parameter = romanize_string(parameter);
    }

    let translated: Option<String> = get_translated_parameter(code, &parameter, map, deque, game_type, engine_type);

    if let Some(mut translated) = translated {
        if code == Code::Shop {
            let left: &str = unsafe { parameter.split_once('=').unwrap_unchecked().0 };
            translated = left.to_owned() + &translated;
        }

        *value = if engine_type == EngineType::New {
            Value::from(&translated)
        } else {
            json!({"__type": "bytes", "data": Array::from(translated.as_bytes())})
        };
    }
}

#[allow(clippy::single_match, clippy::match_single_binding, unused_mut)]
fn get_translated_parameter(
    code: Code,
    mut parameter: &str,
    hashmap: Option<&StringHashMap>,
    deque: Option<Arc<Mutex<VecDeque<String>>>>,
    game_type: Option<GameType>,
    engine_type: EngineType,
) -> Option<String> {
    let mut remaining_strings: Vec<String> = Vec::with_capacity(4);

    // bool indicates insert whether at start or at end
    // true inserts at end
    // false inserts at start
    let mut insert_positions: Vec<bool> = Vec::with_capacity(4);

    #[allow(unreachable_patterns)]
    if let Some(game_type) = game_type {
        match game_type {
            GameType::Termina => match code {
                Code::System => {
                    if !parameter.starts_with("Gab")
                        && (!parameter.starts_with("choice_text") || parameter.ends_with("????"))
                    {
                        return None;
                    }
                }
                _ => {}
            },
            GameType::LisaRPG => match code {
                Code::DialogueMain | Code::DialogueAdditional => {
                    if let Some(re_match) = LISA_PREFIX_RE.find(parameter) {
                        parameter = &parameter[re_match.end()..];
                        remaining_strings.push(re_match.as_str().to_owned());
                        insert_positions.push(false);
                    }
                }
                _ => {}
            },
            _ => {} // custom processing for other games
        }
    }

    if engine_type != EngineType::New {
        if let Some(re_match) = ENDS_WITH_IF_RE.find(parameter) {
            parameter = &parameter[re_match.start()..];
            remaining_strings.push(re_match.as_str().to_owned());
            insert_positions.push(true);
        }

        match code {
            Code::Shop => {
                if !parameter.contains("shop_talk") {
                    return None;
                }

                let actual_string: &str = unsafe { parameter.split_once('=').unwrap_unchecked().1 }.trim();
                let without_quotes: &str = &actual_string[1..actual_string.len() - 1];

                if STRING_IS_ONLY_SYMBOLS_RE.is_match(without_quotes) {
                    return None;
                }

                parameter = without_quotes;
            }
            _ => {}
        }
    }

    let translated: Option<String> = if let Some(mut deque) = deque {
        let deque: &mut VecDeque<String> = &mut deque.lock().unwrap();

        if code == Code::ChoiceArray {
            deque.front().map(String::to_owned)
        } else {
            deque.pop_front()
        }
    } else {
        unsafe { hashmap.unwrap_unchecked() }
            .get(parameter)
            .map(|translated: &String| {
                let mut result: String = translated.to_owned();
                result
            })
    };

    if let Some(mut translated) = translated {
        for (string, position) in remaining_strings.into_iter().zip(insert_positions) {
            match position {
                false => translated = string + &translated,
                true => translated += &string,
            }
        }

        Some(translated)
    } else {
        translated
    }
}

#[allow(clippy::single_match, clippy::match_single_binding, unused_mut)]
fn get_translated_variable(
    mut variable_text: String,
    note_text: Option<&str>, // note_text is some only when getting description
    variable_type: Variable,
    filename: &str,
    hashmap: &StringHashMap,
    game_type: Option<GameType>,
    engine_type: EngineType,
) -> Option<String> {
    let mut remaining_strings: Vec<String> = Vec::with_capacity(4);

    // bool indicates insert whether at start or at end
    // true inserts at end
    // false inserts at start
    let mut insert_positions: Vec<bool> = Vec::with_capacity(4);

    if engine_type != EngineType::New {
        variable_text = variable_text.replace("\r\n", "\n");
    }

    #[allow(clippy::collapsible_match)]
    if let Some(game_type) = game_type {
        match game_type {
            GameType::Termina => match variable_type {
                Variable::Description => match note_text {
                    Some(mut note) => {
                        let mut note_string: String = String::from(note);

                        let mut note_chars: Chars = note.chars();
                        let mut is_continuation_of_description: bool = false;

                        if !note.starts_with("flesh puppetry") {
                            if let Some(first_char) = note_chars.next() {
                                if let Some(second_char) = note_chars.next() {
                                    if ((first_char == '\n' && second_char != '\n')
                                        || (first_char.is_ascii_alphabetic()
                                            || first_char == '"'
                                            || note.starts_with("4 sticks")))
                                        && !['.', '!', '/', '?'].contains(&first_char)
                                    {
                                        is_continuation_of_description = true;
                                    }
                                }
                            }
                        }

                        if is_continuation_of_description {
                            if let Some((mut left, _)) = note.trim_start().split_once('\n') {
                                left = left.trim();

                                if left.ends_with(['.', '%', '!', '"']) {
                                    note_string = String::from("\n") + left;
                                }
                            } else if note.ends_with(['.', '%', '!', '"']) {
                                note_string = note.to_owned();
                            }

                            if !note_string.is_empty() {
                                variable_text = variable_text + &note_string;
                            }
                        }
                    }
                    None => {}
                },
                Variable::Message1 | Variable::Message2 | Variable::Message3 | Variable::Message4 => {
                    return None;
                }
                Variable::Note => {
                    if filename.starts_with("It") {
                        for string in [
                            "<Menu Category: Items>",
                            "<Menu Category: Food>",
                            "<Menu Category: Healing>",
                            "<Menu Category: Body bag>",
                        ] {
                            if variable_text.contains(string) {
                                variable_text = variable_text.replace(string, &hashmap[string]);
                            }
                        }
                    }

                    if !filename.starts_with("Cl") {
                        let mut variable_text_chars: Chars = variable_text.chars();
                        let mut is_continuation_of_description: bool = false;

                        if let Some(first_char) = variable_text_chars.next() {
                            if let Some(second_char) = variable_text_chars.next() {
                                if ((first_char == '\n' && second_char != '\n')
                                    || (first_char.is_ascii_alphabetic()
                                        || first_char == '"'
                                        || variable_text.starts_with("4 sticks")))
                                    && !['.', '!', '/', '?'].contains(&first_char)
                                {
                                    is_continuation_of_description = true;
                                }
                            }
                        }

                        return if is_continuation_of_description {
                            if let Some((_, right)) = variable_text.trim_start().split_once('\n') {
                                Some(right.to_owned())
                            } else {
                                Some(String::new())
                            }
                        } else {
                            Some(variable_text)
                        };
                    }
                }
                _ => {}
            },
            _ => {} // custom processing for other games
        }
    }

    let translated: Option<String> = hashmap.get(&variable_text).map(|translated: &String| {
        let mut result: String = translated.to_owned();

        for (string, position) in remaining_strings.into_iter().zip(insert_positions) {
            match position {
                true => result.push_str(&string),
                false => result = string + &result,
            }
        }

        if matches!(
            variable_type,
            Variable::Message1 | Variable::Message2 | Variable::Message3 | Variable::Message4
        ) {
            result = String::from(" ") + &result;
        }

        #[allow(clippy::collapsible_if, clippy::collapsible_match)]
        if let Some(game_type) = game_type {
            match game_type {
                GameType::Termina => match variable_type {
                    Variable::Note => {
                        if let Some(first_char) = result.chars().next() {
                            if first_char != '\n' {
                                result = String::from("\n") + &result
                            }
                        }
                    }
                    _ => {}
                },
                _ => {}
            }
        }

        result
    });

    translated
}

fn write_list(
    list: &mut Array,
    romanize: bool,
    game_type: Option<GameType>,
    engine_type: EngineType,
    map: &StringHashMap,
    (code_label, parameters_label): (&str, &str),
) {
    let list_length: usize = list.len();

    let mut in_sequence: bool = false;
    let mut lines: Vec<String> = Vec::with_capacity(4);
    let mut item_indices: Vec<usize> = Vec::with_capacity(4);

    for it in 0..list_length {
        let code: u16 = list[it][code_label].as_u64().unwrap_log() as u16;

        let code: Code = if !ALLOWED_CODES.contains(&code) {
            Code::Bad
        } else {
            unsafe { transmute::<u16, Code>(code) }
        };

        let write_string_literally: bool = if engine_type == EngineType::New {
            true
        } else {
            !match code {
                Code::ChoiceArray => list[it][parameters_label][0][0].is_object(),
                Code::Misc1 | Code::Misc2 | Code::Choice => list[it][parameters_label][1].is_object(),
                _ => list[it][parameters_label][0].is_object(),
            }
        };

        if in_sequence && ![Code::DialogueMain, Code::DialogueAdditional, Code::Credit].contains(&code) {
            if !lines.is_empty() {
                let mut joined: String = lines.join("\n");

                if romanize {
                    joined = romanize_string(joined)
                }

                let translated: Option<String> =
                    get_translated_parameter(Code::DialogueMain, &joined, Some(map), None, game_type, engine_type);

                if let Some(translated) = translated {
                    let split_vec: Vec<&str> = translated.split('\n').collect();
                    let split_length: usize = split_vec.len();
                    let line_length: usize = lines.len();

                    for (i, &index) in item_indices.iter().enumerate() {
                        list[index][parameters_label][0] = if i < split_length {
                            if write_string_literally {
                                Value::from(split_vec[i])
                            } else {
                                json!({
                                    "__type": "bytes",
                                    "data": Array::from(split_vec[i].as_bytes())
                                })
                            }
                        } else {
                            Value::from_static_str(" ")
                        }
                    }

                    if split_length > line_length {
                        let remaining: String = split_vec[line_length - 1..].join("\n");
                        list[*unsafe { item_indices.last().unwrap_unchecked() }][parameters_label][0] =
                            Value::from(&remaining);
                    }
                }

                lines.clear();
                item_indices.clear();
            }

            in_sequence = false
        }

        if code == Code::Bad {
            continue;
        }

        let value_i: usize = match code {
            Code::Misc1 | Code::Misc2 | Code::Choice => 1,
            _ => 0,
        };
        let value: &mut Value = &mut list[it][parameters_label][value_i];

        match code {
            Code::ChoiceArray => {
                for i in 0..value.as_array().unwrap_log().len() {
                    let subparameter_string: String = {
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

                        subparameter_string.to_owned()
                    };

                    process_parameter(
                        code,
                        subparameter_string,
                        Some(map),
                        None,
                        game_type,
                        engine_type,
                        romanize,
                        &mut value[i],
                    );
                }
            }
            _ => {
                let parameter_string: String = {
                    let mut buf: Vec<u8> = Vec::new();

                    let parameter_string: &str = value
                        .as_str()
                        .unwrap_or_else(|| match value.as_object() {
                            Some(obj) => {
                                buf = get_object_data(obj);
                                unsafe { std::str::from_utf8_unchecked(&buf) }
                            }
                            None => "",
                        })
                        .trim();

                    if code != Code::Credit && parameter_string.is_empty() {
                        continue;
                    }

                    parameter_string.to_owned()
                };

                match code {
                    Code::DialogueMain | Code::DialogueAdditional | Code::Credit => {
                        lines.push(parameter_string);
                        item_indices.push(it);
                        in_sequence = true;
                    }
                    _ => {
                        process_parameter(
                            code,
                            parameter_string,
                            Some(map),
                            None,
                            game_type,
                            engine_type,
                            romanize,
                            value,
                        );
                    }
                }
            }
        }
    }
}

/// Writes .txt files from maps folder back to their initial form.
/// # Parameters
/// * `maps_path` - path to the maps directory
/// * `original_path` - path to the original directory
/// * `output_path` - path to the output directory
/// * `maps_processing_mode` - how to deal with lines duplicates in maps
/// * `romanize` - if files were read with romanize, this option will romanize original game text to compare with parsed
/// * `logging` - whether to log or not
/// * `game_type` - game type for custom parsing
/// * `engine_type` - engine type for right files processing
pub fn write_maps<P: AsRef<Path> + std::marker::Sync>(
    maps_path: P,
    original_path: P,
    output_path: P,
    maps_processing_mode: MapsProcessingMode,
    romanize: bool,
    logging: bool,
    game_type: Option<GameType>,
    engine_type: EngineType,
) {
    let translation: String = read_to_string(maps_path.as_ref().join("maps.txt")).unwrap_log();

    let (names_map, translation_deque, translation_maps) = {
        // Allocated when maps processing mode is PRESERVE.
        let mut translation_deque: VecDeque<String> = VecDeque::new();
        // Default map for translation from the .txt file.
        let mut translation_map: StringHashMap = HashMap::with_hasher(HASHER);
        // A vec that holds translation maps. If maps processing mode is
        // DEFAULT, only ever holds one hashmap with all the translation lines.
        // If maps processing mode is SEPARATE, holds multiple hashmap, each
        // respective to a single map file.
        let mut translation_maps: Vec<StringHashMap> = Vec::new();
        // Always allocated.
        let mut names_map: StringHashMap = HashMap::with_hasher(HASHER);

        for (i, line) in translation.split('\n').enumerate() {
            if line.starts_with("<!-- Map") {
                if let Some((original, translated)) = line.split_once(LINES_SEPARATOR) {
                    if original.len() > 20 {
                        let map_name: &str = &original[17..original.len() - 4];
                        names_map.insert(map_name.trim().to_owned(), translated.trim().to_owned());
                    }

                    if maps_processing_mode == MapsProcessingMode::Separate && i > 0 {
                        translation_maps.push(take(&mut translation_map));
                    }
                } else {
                    eprintln!("{COULD_NOT_SPLIT_LINE_MSG} ({line})\n{AT_POSITION_MSG} {i}", i = i + 1);
                }
            } else if !line.starts_with("<!--") {
                if let Some((original, translated)) = line.split_once(LINES_SEPARATOR) {
                    #[cfg(not(debug_assertions))]
                    if translated.is_empty() {
                        continue;
                    }

                    if maps_processing_mode == MapsProcessingMode::Preserve {
                        translation_deque.push_back(translated.replace(NEW_LINE, "\n").trim().to_owned());
                    } else {
                        translation_map.insert(
                            original.replace(NEW_LINE, "\n").trim().to_owned(),
                            translated.replace(NEW_LINE, "\n").trim().to_owned(),
                        );
                    }
                } else {
                    eprintln!(
                        "{COULD_NOT_SPLIT_LINE_MSG} ({line})\n{AT_POSITION_MSG} {i}\n{IN_FILE_MSG} maps.txt",
                        i = i + 1
                    );
                }
            }
        }

        if maps_processing_mode == MapsProcessingMode::Default {
            translation_maps.push(translation_map);
        }

        (names_map, translation_deque, translation_maps)
    };

    let translation_deque_mutex: Arc<Mutex<VecDeque<String>>> = Arc::new(Mutex::new(translation_deque));

    let (display_name_label, events_label, pages_label, list_label, code_label, parameters_label) =
        get_maps_labels(engine_type);

    let maps_obj_iter = read_dir(original_path)
        .unwrap_log()
        .par_bridge()
        .filter_map(|entry| filter_maps(entry, engine_type));

    maps_obj_iter.for_each(|(filename, mut obj)| {
        if let Some(display_name) = obj[display_name_label].as_str() {
            let mut display_name: String = display_name.to_owned();

            if romanize {
                display_name = romanize_string(display_name)
            }

            if let Some(location_name) = names_map.get(&display_name) {
                obj[display_name_label] = Value::from(location_name);
            }
        }

        let hashmap: &StringHashMap = if maps_processing_mode == MapsProcessingMode::Preserve {
            &StringHashMap::with_hasher(HASHER)
        } else {
            let hashmap: &StringHashMap = if maps_processing_mode == MapsProcessingMode::Separate {
                unsafe {
                    let filename: &str = filename.split_once('.').unwrap_unchecked().0;
                    let map_number: &str = &filename[filename.len() - 3..];
                    translation_maps.get_unchecked(map_number.parse::<usize>().unwrap_unchecked())
                }
            } else {
                unsafe { translation_maps.get_unchecked(0) }
            };

            if hashmap.is_empty() {
                return;
            }

            hashmap
        };

        let mut events_arr: Vec<&mut Value> = if engine_type == EngineType::New {
            // Skipping first element in array as it is null
            obj[events_label]
                .as_array_mut()
                .unwrap_log()
                .par_iter_mut()
                .skip(1)
                .collect()
        } else {
            obj[events_label]
                .as_object_mut()
                .unwrap_log()
                .iter_mut()
                .par_bridge()
                .map(|(_, value)| value)
                .collect()
        };

        let lines_deque_mutex = translation_deque_mutex.clone();
        events_arr.par_iter_mut().for_each(|event: &mut &mut Value| {
            if event.is_null() {
                return;
            }

            let lines_deque_mutex = lines_deque_mutex.clone();
            event[pages_label]
                .as_array_mut()
                .unwrap_log()
                .par_iter_mut()
                .for_each(move |page: &mut Value| {
                    if maps_processing_mode == MapsProcessingMode::Preserve {
                        let lines_deque_mutex: Arc<Mutex<VecDeque<String>>> = lines_deque_mutex.clone();

                        let list: &mut Array = page[list_label].as_array_mut().unwrap_log();
                        let list_length: usize = list.len();

                        let mut in_sequence: bool = false;
                        let mut line: Vec<String> = Vec::with_capacity(4);
                        let mut item_indices: Vec<usize> = Vec::with_capacity(4);

                        for it in 0..list_length {
                            let code: u16 = list[it][code_label].as_u64().unwrap_log() as u16;

                            let code: Code = if !ALLOWED_CODES.contains(&code) {
                                Code::Bad
                            } else {
                                unsafe { transmute::<u16, Code>(code) }
                            };

                            let write_string_literally: bool = if engine_type == EngineType::New {
                                true
                            } else {
                                !match code {
                                    Code::ChoiceArray => list[it][parameters_label][0][0].is_object(),
                                    Code::Misc1 | Code::Misc2 | Code::Choice => {
                                        list[it][parameters_label][1].is_object()
                                    }
                                    _ => list[it][parameters_label][0].is_object(),
                                }
                            };

                            if in_sequence && ![Code::DialogueMain, Code::DialogueAdditional].contains(&code) {
                                if !line.is_empty() {
                                    let mut joined: String = line.join("\n");

                                    if romanize {
                                        joined = romanize_string(joined)
                                    }

                                    let translated: Option<String> = get_translated_parameter(
                                        Code::DialogueMain,
                                        &joined,
                                        None,
                                        Some(lines_deque_mutex.clone()),
                                        game_type,
                                        engine_type,
                                    );

                                    if let Some(translated) = translated {
                                        let split_vec: Vec<&str> = translated.split('\n').collect();
                                        let split_length: usize = split_vec.len();
                                        let line_length: usize = line.len();

                                        for (i, &index) in item_indices.iter().enumerate() {
                                            list[index][parameters_label][0] = if i < split_length {
                                                if write_string_literally {
                                                    Value::from(split_vec[i])
                                                } else {
                                                    json!({
                                                        "__type": "bytes",
                                                        "data": Array::from(split_vec[i].as_bytes())
                                                    })
                                                }
                                            } else {
                                                Value::from_static_str(" ")
                                            }
                                        }

                                        if split_length > line_length {
                                            let remaining: String = split_vec[line_length - 1..].join("\n");
                                            list[*unsafe { item_indices.last().unwrap_unchecked() }]
                                                [parameters_label][0] = Value::from(&remaining);
                                        }
                                    }

                                    line.clear();
                                    item_indices.clear();
                                }

                                in_sequence = false
                            }

                            if code == Code::Bad {
                                continue;
                            }

                            let value_i: usize = match code {
                                Code::Misc1 | Code::Misc2 | Code::Choice => 1,
                                _ => 0,
                            };

                            let value: &mut Value = &mut list[it][parameters_label][value_i];

                            match code {
                                Code::ChoiceArray => {
                                    for i in 0..value.as_array().unwrap_log().len() {
                                        let subparameter_string: String = {
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

                                            subparameter_string.to_owned()
                                        };

                                        process_parameter(
                                            code,
                                            subparameter_string,
                                            None,
                                            Some(lines_deque_mutex.clone()),
                                            game_type,
                                            engine_type,
                                            romanize,
                                            &mut value[i],
                                        );
                                    }
                                }
                                _ => {
                                    let parameter_string: String = {
                                        let mut buf: Vec<u8> = Vec::new();

                                        let parameter_string: &str = value
                                            .as_str()
                                            .unwrap_or_else(|| match value.as_object() {
                                                Some(obj) => {
                                                    buf = get_object_data(obj);
                                                    unsafe { std::str::from_utf8_unchecked(&buf) }
                                                }
                                                None => "",
                                            })
                                            .trim();

                                        if parameter_string.is_empty() {
                                            continue;
                                        }

                                        parameter_string.to_owned()
                                    };

                                    match code {
                                        Code::DialogueMain | Code::DialogueAdditional => {
                                            line.push(parameter_string);
                                            item_indices.push(it);
                                            in_sequence = true;
                                        }
                                        _ => {
                                            process_parameter(
                                                code,
                                                parameter_string,
                                                None,
                                                Some(lines_deque_mutex.clone()),
                                                game_type,
                                                engine_type,
                                                romanize,
                                                value,
                                            );
                                        }
                                    }
                                }
                            }
                        }
                    } else {
                        write_list(
                            page[list_label].as_array_mut().unwrap_log(),
                            romanize,
                            game_type,
                            engine_type,
                            hashmap,
                            (code_label, parameters_label),
                        );
                    }
                });
        });

        let output_data: Vec<u8> = if engine_type == EngineType::New {
            unsafe { to_vec(&obj).unwrap_unchecked() }
        } else {
            dump(obj, Some(""))
        };

        write(output_path.as_ref().join(&filename), output_data).unwrap_log();

        if logging {
            println!("{WROTE_FILE_MSG} {filename}");
        }
    });
}

/// Writes .txt files from other folder back to their initial form.
/// # Parameters
/// * `other_path` - path to the other directory
/// * `original_path` - path to the original directory
/// * `output_path` - path to the output directory
/// * `romanize` - if files were read with romanize, this option will romanize original game text to compare with parsed
/// * `logging` - whether to log or not
/// * `game_type` - game type for custom parsing
/// * `engine_type` - engine type for right files processing
pub fn write_other<P: AsRef<Path> + std::marker::Sync>(
    other_path: P,
    original_path: P,
    output_path: P,
    romanize: bool,
    logging: bool,
    game_type: Option<GameType>,
    engine_type: EngineType,
) {
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

    let variable_tuples: Arc<[(&str, Variable); 8]> = Arc::new([
        (name_label, Variable::Name),
        (nickname_label, Variable::Nickname),
        (description_label, Variable::Description),
        (message1_label, Variable::Message1),
        (message2_label, Variable::Message2),
        (message3_label, Variable::Message3),
        (message4_label, Variable::Message4),
        (note_label, Variable::Note),
    ]);

    let other_obj_arr_iter = read_dir(original_path)
        .unwrap_log()
        .par_bridge()
        .filter_map(|entry| filter_other(entry, engine_type, game_type));

    other_obj_arr_iter.for_each(|(filename, mut obj_arr)| {
        let txt_filename: &str =
            &(unsafe { filename.rsplit_once('.').unwrap_unchecked() }.0.to_owned() + ".txt").to_lowercase();

        let translation: String = read_to_string(other_path.as_ref().join(txt_filename)).unwrap_log();
        let translation_map: StringHashMap = parse_translation(translation, txt_filename);

        if translation_map.is_empty() {
            return;
        }

        // Other files except CommonEvents and Troops have the structure that consists
        // of name, nickname, description and note
        if !filename.starts_with("Co") && !filename.starts_with("Tr") {
            obj_arr
                .as_array_mut()
                .unwrap_log()
                .par_iter_mut()
                .skip(1) // Skipping first element in array as it is null
                .for_each(|obj: &mut Value| {
                    for (variable_label, variable_type) in variable_tuples.into_iter() {
                        if let Some(mut variable_str) = obj[variable_label].as_str() {
                            let mut variable_string: String = if variable_type == Variable::Note {
                                // TODO: that's to remove
                                if let Some(game_type) = game_type {
                                    if game_type == GameType::LisaRPG {
                                        variable_str = variable_str.trim()
                                    }
                                }

                                variable_str
                            } else {
                                variable_str.trim()
                            }
                            .to_owned();

                            if !variable_string.is_empty() {
                                if romanize {
                                    variable_string = romanize_string(variable_string)
                                }

                                variable_string = variable_string
                                    .split('\n')
                                    .map(str::trim)
                                    .collect::<Vec<_>>()
                                    .join("\n");

                                let note_text: Option<&str> = if game_type
                                    .is_some_and(|game_type: GameType| game_type != GameType::Termina)
                                    && variable_type != Variable::Description
                                {
                                    None
                                } else {
                                    match obj.get(unsafe { variable_tuples.last().unwrap_unchecked() }.0) {
                                        Some(value) => value.as_str(),
                                        None => None,
                                    }
                                };

                                let translated: Option<String> = get_translated_variable(
                                    variable_string,
                                    note_text,
                                    variable_type,
                                    &filename,
                                    &translation_map,
                                    game_type,
                                    engine_type,
                                );

                                if let Some(translated) = translated {
                                    obj[variable_label] = Value::from(&translated);
                                }
                            }
                        }
                    }
                });
        } else {
            // Other files have the structure somewhat similar to Maps files
            obj_arr
                .as_array_mut()
                .unwrap_log()
                .par_iter_mut()
                .skip(1) // Skipping first element in array as it is null
                .for_each(|obj: &mut Value| {
                    // CommonEvents doesn't have pages, so we can just check if it's Troops
                    let pages_length: usize = if filename.starts_with("Tr") {
                        obj[pages_label].as_array().unwrap_log().len()
                    } else {
                        1
                    };

                    for i in 0..pages_length {
                        // If element has pages, then we'll iterate over them
                        // Otherwise we'll just iterate over the list
                        let list: &mut Value = if pages_length != 1 {
                            &mut obj[pages_label][i][list_label]
                        } else {
                            &mut obj[list_label]
                        };

                        if let Some(list) = list.as_array_mut() {
                            write_list(
                                list,
                                romanize,
                                game_type,
                                engine_type,
                                &translation_map,
                                (code_label, parameters_label),
                            );
                        }
                    }
                });
        }

        let output_data: Vec<u8> = if engine_type == EngineType::New {
            unsafe { to_vec(&obj_arr).unwrap_unchecked() }
        } else {
            dump(obj_arr, Some(""))
        };

        write(output_path.as_ref().join(&filename), output_data).unwrap_log();

        if logging {
            println!("{WROTE_FILE_MSG} {filename}");
        }
    });
}

/// Writes system.txt file back to its initial form.
///
/// For inner code documentation, check `read_system` function.
/// # Parameters
/// * `system_file_path` - path to the original system file
/// * `other_path` - path to the other directory
/// * `output_path` - path to the output directory
/// * `romanize` - if files were read with romanize, this option will romanize original game text to compare with parsed
/// * `logging` - whether to log or not
/// * `engine_type` - engine type for right files processing
pub fn write_system<P: AsRef<Path>>(
    system_file_path: P,
    other_path: P,
    output_path: P,
    romanize: bool,
    logging: bool,
    engine_type: EngineType,
) {
    let translation: String = read_to_string(other_path.as_ref().join("system.txt")).unwrap_log();

    let game_title: String = translation.rsplit_once(LINES_SEPARATOR).unwrap_log().1.to_owned();
    let translation_map: StringHashMap = parse_translation(translation, "system.txt");

    if translation_map.is_empty() {
        return;
    }

    let replace_value = |value: &mut Value| {
        let mut buf: Vec<u8> = Vec::new();
        let str: &str = value.as_str().unwrap_or_else(|| {
            if let Some(obj) = value.as_object() {
                buf = get_object_data(obj);
                unsafe { std::str::from_utf8_unchecked(&buf) }
            } else {
                ""
            }
        });

        let mut string: String = str.trim().to_owned();

        if !string.is_empty() {
            if romanize {
                string = romanize_string(string);
            }

            if let Some(translated) = translation_map.get(&string) {
                *value = if engine_type == EngineType::New {
                    Value::from(translated)
                } else {
                    json!({"__type": "bytes", "data": Array::from(translated.as_bytes())})
                };
            }
        }
    };

    let (armor_types_label, elements_label, skill_types_label, terms_label, weapon_types_label, game_title_label) =
        get_system_labels(engine_type);

    let mut obj: Value = if engine_type == EngineType::New {
        from_str(&read_to_string_without_bom(&system_file_path).unwrap_log()).unwrap_log()
    } else {
        load(&read(&system_file_path).unwrap_log(), Some(StringMode::UTF8), Some("")).unwrap_log()
    };

    for label in [
        armor_types_label,
        elements_label,
        skill_types_label,
        weapon_types_label,
        "equipTypes",
    ] {
        if let Some(arr) = obj[label].as_array_mut() {
            arr.iter_mut().for_each(replace_value);
        }
    }

    obj[terms_label]
        .as_object_mut()
        .unwrap_log()
        .iter_mut()
        .for_each(|(key, value): (&str, &mut Value)| {
            if engine_type != EngineType::New && !key.starts_with("__symbol__") {
                return;
            }

            if key != "messages" {
                if let Some(arr) = value.as_array_mut() {
                    arr.par_iter_mut().for_each(replace_value);
                } else if (value.is_object() && value["__type"].as_str().is_some_and(|x| x == "bytes"))
                    || value.is_str()
                {
                    replace_value(value)
                }
            } else {
                if !value.is_object() {
                    return;
                }

                value
                    .as_object_mut()
                    .unwrap_log()
                    .iter_mut()
                    .for_each(|(_, value)| replace_value(value));
            }
        });

    if engine_type != EngineType::New {
        replace_value(&mut obj["__symbol__currency_unit"]);
    }

    if !game_title.is_empty() {
        obj[game_title_label] = Value::from(&game_title);
    }

    let output_data: Vec<u8> = if engine_type == EngineType::New {
        unsafe { to_vec(&obj).unwrap_unchecked() }
    } else {
        dump(obj, Some(""))
    };

    let filename: &str = unsafe {
        system_file_path
            .as_ref()
            .file_name()
            .unwrap_unchecked()
            .to_str()
            .unwrap_unchecked()
    };

    write(output_path.as_ref().join(filename), output_data).unwrap_log();

    if logging {
        println!("{WROTE_FILE_MSG} {filename}");
    }
}

/// Writes plugins.txt file back to its initial form. Currently works only if `game_type` is `GameType::Termina`.
/// # Parameters
/// * `plugins_file_path` - path to the original plugins file
/// * `plugins_path` - path to the plugins directory
/// * `output_path` - path to the output directory
/// * `logging` - whether to log or not
pub fn write_plugins<P: AsRef<Path>>(pluigns_file_path: P, plugins_path: P, output_path: P, logging: bool) {
    let translation: String = read_to_string(plugins_path.as_ref().join("plugins.txt")).unwrap_log();
    let translation_map: StringHashMap = parse_translation(translation, "plugins.txt");

    let mut obj_arr: Vec<Object> = from_str(&read_to_string_without_bom(pluigns_file_path).unwrap_log()).unwrap_log();

    obj_arr.par_iter_mut().for_each(|obj: &mut Object| {
        // For now, plugins writing only implemented for Fear & Hunger: Termina, so you should manually translate the plugins.js file if it's not Termina

        // Plugins with needed text
        let plugin_names: HashSet<&str, Xxh3Builder> = HashSet::from_iter([
            "YEP_BattleEngineCore",
            "YEP_OptionsCore",
            "SRD_NameInputUpgrade",
            "YEP_KeyboardConfig",
            "YEP_ItemCore",
            "YEP_X_ItemDiscard",
            "YEP_EquipCore",
            "YEP_ItemSynthesis",
            "ARP_CommandIcons",
            "YEP_X_ItemCategories",
            "Olivia_OctoBattle",
        ]);

        let name: &str = obj["name"].as_str().unwrap_log();

        // It it's a plugin with the needed text, proceed
        if plugin_names.contains(name) {
            // YEP_OptionsCore should be processed differently, as its parameters is a mess, that can't even be parsed to json
            if name == "YEP_OptionsCore" {
                obj["parameters"]
                    .as_object_mut()
                    .unwrap_log()
                    .iter_mut()
                    .par_bridge()
                    .for_each(|(key, value): (&str, &mut Value)| {
                        let mut string: String = value.as_str().unwrap_log().to_owned();

                        if key == "OptionsCategories" {
                            for (text, translated) in translation_map.keys().zip(translation_map.values()) {
                                string = string.replacen(text, translated, 1);
                            }

                            *value = Value::from(string.as_str());
                        } else if let Some(translated) = translation_map.get(&string) {
                            *value = Value::from(translated);
                        }
                    });
            }
            // Everything else is an easy walk
            else {
                obj["parameters"]
                    .as_object_mut()
                    .unwrap_log()
                    .iter_mut()
                    .par_bridge()
                    .for_each(|(_, value)| {
                        if let Some(str) = value.as_str() {
                            if let Some(translated) = translation_map.get(str) {
                                *value = Value::from(translated);
                            }
                        }
                    });
            }
        }
    });

    write(
        output_path.as_ref().join("plugins.js"),
        String::from("var $plugins =\n") + unsafe { &to_string(&obj_arr).unwrap_unchecked() },
    )
    .unwrap_log();

    if logging {
        println!("{WROTE_FILE_MSG} plugins.js");
    }
}

/// Writes scripts.txt file back to its initial form.
///
/// For inner code documentation, check read_system function.
/// # Parameters
/// * `scripts_file_path` - path to the original system file
/// * `other_path` - path to the other directory
/// * `output_path` - path to the output directory
/// * `romanize` - if files were read with romanize, this option will romanize original game text to compare with parsed
/// * `logging` - whether to log or not
/// * `engine_type` - engine type for right files processing
pub fn write_scripts<P: AsRef<Path>>(
    scripts_file_path: P,
    other_path: P,
    output_path: P,
    romanize: bool,
    logging: bool,
    engine_type: EngineType,
) {
    let translation: String = read_to_string(other_path.as_ref().join("scripts.txt")).unwrap_log();
    let translation_map: StringHashMap = parse_translation(translation, "scripts.txt");

    if translation_map.is_empty() {
        return;
    }

    let mut script_entries: Value =
        load(&read(&scripts_file_path).unwrap_log(), Some(StringMode::Binary), None).unwrap_log();

    script_entries
        .as_array_mut()
        .unwrap_log()
        .par_iter_mut()
        .for_each(|script: &mut Value| {
            let data: Vec<u8> = from_value(&script.as_array().unwrap_log()[2]["data"]).unwrap_log();

            let mut inflated: Vec<u8> = Vec::new();
            ZlibDecoder::new(&*data).read_to_end(&mut inflated).unwrap_log();

            let mut code: String = String::new();

            for encoding in ENCODINGS {
                let (cow, _, had_errors) = encoding.decode(&inflated);

                if !had_errors {
                    code = cow.into_owned();
                    break;
                }
            }

            let (strings_array, indices_array) = extract_strings(&code, true);

            for (mut string, range) in strings_array.into_iter().zip(indices_array).rev() {
                if !translation_map.contains_key(&string) {
                    continue;
                }

                if romanize {
                    string = romanize_string(string);
                }

                let translated: Option<&String> = translation_map.get(&string);

                if let Some(translated) = translated {
                    code.replace_range(range, translated);
                }
            }

            let mut buf: Vec<u8> = Vec::new();

            ZlibEncoder::new(&mut buf, Compression::new(6))
                .write_all(code.as_bytes())
                .unwrap_log();

            if let Some(obj) = script[2].as_object_mut() {
                obj["data"] = Array::from(buf).into()
            };
        });

    write(
        output_path
            .as_ref()
            .join(String::from("Scripts") + determine_extension(engine_type)),
        dump(script_entries, None),
    )
    .unwrap_log();

    if logging {
        println!("{WROTE_FILE_MSG} {}", unsafe {
            scripts_file_path
                .as_ref()
                .file_name()
                .unwrap_unchecked()
                .to_str()
                .unwrap_unchecked()
        });
    }
}
