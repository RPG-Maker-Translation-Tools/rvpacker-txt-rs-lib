use crate::{
    statics::NEW_LINE,
    types::{EachLine, EngineType, GameType, OptionExt, ResultExt},
};
use indexmap::IndexSet;
use marshal_rs::load;
use sonic_rs::{from_str, prelude::*, Object, Value};
use std::{
    ffi::OsString,
    fs::{read, read_to_string, DirEntry},
    hash::BuildHasherDefault,
    str::from_utf8_unchecked,
};
use xxhash_rust::xxh3::Xxh3;

pub fn romanize_string(string: String) -> String {
    let mut result: String = String::with_capacity(string.capacity());

    for char in string.chars() {
        let replacement: &str = match char {
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

pub fn get_object_data(object: &Object) -> Vec<u8> {
    let mut vec: Vec<u8> = Vec::new();

    if let Some(object_type) = object.get(&"__type") {
        if object_type.as_str().is_some_and(|_type: &str| _type == "bytes") {
            vec = unsafe { sonic_rs::from_value::<Vec<u8>>(&object["data"]).unwrap_unchecked() };
        }
    }

    vec
}

pub fn extract_strings(
    ruby_code: &str,
    write: bool,
) -> (IndexSet<String, BuildHasherDefault<Xxh3>>, Vec<std::ops::Range<usize>>) {
    fn is_escaped(index: usize, string: &str) -> bool {
        let mut backslash_count: u8 = 0;

        for char in string[..index].chars().rev() {
            if char == '\\' {
                backslash_count += 1;
            } else {
                break;
            }
        }

        backslash_count % 2 == 1
    }

    let mut strings: IndexSet<String, BuildHasherDefault<Xxh3>> = IndexSet::default();
    let mut ranges: Vec<std::ops::Range<usize>> = Vec::new();
    let mut inside_string: bool = false;
    let mut inside_multiline_comment: bool = false;
    let mut string_start_index: usize = 0;
    let mut current_quote_type: char = '\0';
    let mut global_index: usize = 0;

    for line in ruby_code.each_line() {
        let trimmed: &str = line.trim();

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

        let char_indices: std::str::CharIndices = line.char_indices();

        for (i, char) in char_indices {
            if !inside_string && char == '#' {
                break;
            }

            if !inside_string && (char == '"' || char == '\'') {
                inside_string = true;
                string_start_index = global_index + i;
                current_quote_type = char;
            } else if inside_string && char == current_quote_type && !is_escaped(i, &line) {
                let range: std::ops::Range<usize> = string_start_index + 1..global_index + i;

                let extracted_string: String = ruby_code[range.clone()]
                    .replace("\r\n", NEW_LINE)
                    .replace('\n', NEW_LINE);

                if !strings.contains(&extracted_string) {
                    strings.insert(extracted_string);

                    if write {
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

pub fn determine_extension(engine_type: EngineType) -> &'static str {
    match engine_type {
        EngineType::New => ".json",
        EngineType::VXAce => ".rvdata2",
        EngineType::VX => ".rvdata",
        EngineType::XP => ".rxdata",
    }
}

pub fn filter_maps(entry: Result<DirEntry, std::io::Error>, engine_type: EngineType) -> Option<(String, Value)> {
    if let Ok(entry) = entry {
        let filename: OsString = entry.file_name();
        let filename_str: &str = unsafe { from_utf8_unchecked(filename.as_encoded_bytes()) };

        if filename_str.starts_with("Map")
            && unsafe { (*filename_str.as_bytes().get_unchecked(3) as char).is_ascii_digit() }
            && filename_str.ends_with(determine_extension(engine_type))
        {
            let json: Value = if engine_type == EngineType::New {
                from_str(&read_to_string(entry.path()).unwrap_log()).unwrap_log()
            } else {
                load(&read(entry.path()).unwrap_log(), None, Some("")).unwrap_log()
            };

            Some((filename_str.to_owned(), json))
        } else {
            None
        }
    } else {
        None
    }
}

pub fn filter_other(
    entry: Result<DirEntry, std::io::Error>,
    engine_type: EngineType,
    game_type: Option<GameType>,
) -> Option<(String, Value)> {
    if let Ok(entry) = entry {
        let filename_os_string: OsString = entry.file_name();
        let filename: &str = unsafe { from_utf8_unchecked(filename_os_string.as_encoded_bytes()) };
        let (name, _) = filename.split_once('.').unwrap_log();

        if !name.starts_with("Map")
            && !matches!(name, "Tilesets" | "Animations" | "System" | "Scripts")
            && filename.ends_with(determine_extension(engine_type))
        {
            if game_type.is_some_and(|game_type: GameType| game_type == GameType::Termina) && name == "States" {
                return None;
            }

            let json: Value = if engine_type == EngineType::New {
                from_str(&read_to_string(entry.path()).unwrap_log()).unwrap_log()
            } else {
                load(&read(entry.path()).unwrap_log(), None, Some("")).unwrap_log()
            };

            Some((filename.to_owned(), json))
        } else {
            None
        }
    } else {
        None
    }
}

pub fn get_system_labels(
    engine_type: EngineType,
) -> (
    &'static str,
    &'static str,
    &'static str,
    &'static str,
    &'static str,
    &'static str,
) {
    if engine_type == EngineType::New {
        (
            "armorTypes",
            "elements",
            "skillTypes",
            "terms",
            "weaponTypes",
            "gameTitle",
        )
    } else {
        (
            "__symbol__armor_types",
            "__symbol__elements",
            "__symbol__skill_types",
            if engine_type == EngineType::XP {
                "__symbol__words"
            } else {
                "__symbol__terms"
            },
            "__symbol__weapon_types",
            "__symbol__game_title",
        )
    }
}

pub fn get_maps_labels(
    engine_type: EngineType,
) -> (
    &'static str,
    &'static str,
    &'static str,
    &'static str,
    &'static str,
    &'static str,
) {
    if engine_type == EngineType::New {
        ("displayName", "events", "pages", "list", "code", "parameters")
    } else {
        (
            "__symbol__display_name",
            "__symbol__events",
            "__symbol__pages",
            "__symbol__list",
            "__symbol__code",
            "__symbol__parameters",
        )
    }
}

#[allow(clippy::type_complexity)]
pub fn get_other_labels(
    engine_type: EngineType,
) -> (
    &'static str,
    &'static str,
    &'static str,
    &'static str,
    &'static str,
    &'static str,
    &'static str,
    &'static str,
    &'static str,
    &'static str,
    &'static str,
    &'static str,
) {
    if engine_type == EngineType::New {
        (
            "name",
            "nickname",
            "description",
            "message1",
            "message2",
            "message3",
            "message4",
            "note",
            "pages",
            "list",
            "code",
            "parameters",
        )
    } else {
        (
            "__symbol__name",
            "__symbol__nickname",
            "__symbol__description",
            "__symbol__message1",
            "__symbol__message2",
            "__symbol__message3",
            "__symbol__message4",
            "__symbol__note",
            "__symbol__pages",
            "__symbol__list",
            "__symbol__code",
            "__symbol__parameters",
        )
    }
}
