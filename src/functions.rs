use crate::{
    statics::{regexes::STRING_IS_ONLY_SYMBOLS_RE, HASHER, NEW_LINE},
    types::{EachLine, EngineType, GameType, ResultExt},
};
use indexmap::IndexSet;
use marshal_rs::{load, StringMode};
use sonic_rs::{from_str, prelude::*, Object, Value};
use std::{
    ffi::OsString,
    fs::{read, DirEntry, File},
    io::{self, BufReader, Read},
    path::Path,
    str::from_utf8_unchecked,
};
use xxhash_rust::xxh3::Xxh3DefaultBuilder;

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
) -> (IndexSet<String, Xxh3DefaultBuilder>, Vec<std::ops::Range<usize>>) {
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

    let mut strings: IndexSet<String, Xxh3DefaultBuilder> = IndexSet::with_hasher(HASHER);
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

                if !extracted_string.is_empty() && !strings.contains(&extracted_string) {
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

        if !entry.file_type().unwrap_log().is_file() {
            return None;
        };

        if filename_str.starts_with("Map")
            && unsafe { (*filename_str.as_bytes().get_unchecked(3) as char).is_ascii_digit() }
            && filename_str.ends_with(determine_extension(engine_type))
        {
            let json: Value = if engine_type == EngineType::New {
                from_str(&read_to_string_without_bom(entry.path()).unwrap_log()).unwrap_log()
            } else {
                load(&read(entry.path()).unwrap_log(), Some(StringMode::UTF8), Some("")).unwrap_log()
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
        let filename: OsString = entry.file_name();
        let filename_str: &str = unsafe { from_utf8_unchecked(filename.as_encoded_bytes()) };

        if !entry.file_type().unwrap_log().is_file() {
            return None;
        };

        let (name, _) = unsafe { filename_str.split_once('.').unwrap_unchecked() };

        if !name.starts_with("Map")
            && !matches!(name, "Areas" | "Tilesets" | "Animations" | "System" | "Scripts")
            && filename_str.ends_with(determine_extension(engine_type))
        {
            if game_type.is_some_and(|game_type: GameType| game_type == GameType::Termina) && name == "States" {
                return None;
            }

            let json: Value = if engine_type == EngineType::New {
                from_str(&read_to_string_without_bom(entry.path()).unwrap_log()).unwrap_log()
            } else {
                load(&read(entry.path()).unwrap_log(), Some(StringMode::UTF8), Some("")).unwrap_log()
            };

            Some((filename_str.to_owned(), json))
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

pub fn read_to_string_without_bom<P: AsRef<Path>>(file_path: P) -> io::Result<String> {
    const BOM: [u8; 3] = [0xEF, 0xBB, 0xBF];

    let file: File = File::open(file_path.as_ref())?;
    let mut reader: BufReader<File> = BufReader::new(file);

    let mut buffer: [u8; 3] = [0u8; 3];
    let mut content: String = String::new();

    let read_bytes: usize = reader.read(&mut buffer)?;

    if read_bytes == 3 && buffer == BOM {
        reader.read_to_string(&mut content)?;
    } else {
        reader.seek_relative(-3)?;
        reader.read_to_string(&mut content)?;
    }

    Ok(content)
}

pub fn traverse_json(
    key: Option<&str>,
    value: &mut Value,
    map: Option<&std::collections::HashMap<String, String, Xxh3DefaultBuilder>>,
    strings: &mut Option<&mut Vec<String>>,
    write: bool,
    romanize: bool,
) {
    match value.get_type() {
        sonic_rs::JsonType::String => {
            // TODO: Remake this shit in regexps
            if key.is_some_and(|str| {
                [
                    "name",
                    "description",
                    "Window Width",
                    "Window Height",
                    "ATTENTION!!!",
                    "Shown Elements",
                    "Width",
                    "Outline Color",
                    "Command Alignment",
                    "Command Position",
                    "Command Rows",
                    "Chinese Font",
                    "Korean Font",
                    "Default Font",
                    "Text Align",
                    "Scenes To Draw",
                    "displacementImage",
                    "Turn Alignment",
                    "Buff Formula",
                    "Counter Alignment",
                    "Buff Formula",
                    "Counter Alignment",
                    "Default Width",
                    "Face Indent",
                    "Fast Forward Key",
                    "Font Name",
                    "Font Name CH",
                    "Font Name KR",
                    "Name Box Padding",
                    "Name Box Added Text",
                    "Critical Rate Formula",
                    "Critical Multplier Formula",
                    "Flat Critical Formula",
                    "Default SE",
                    "---List---",
                    "Button Events List",
                    "Kill Switch",
                    "Ex Turn Image",
                    "Ex Turn Name Color",
                    "Non Ex Turn Name Color",
                    "Option menu entry",
                    "Add to options",
                    "Default Ambient Light",
                    "Reset Lights",
                    "Gab Font Name",
                    "Escape Ratio",
                    "Translated Format",
                    "Default Sound",
                    "Action Speed",
                    "Default System",
                    "Untranslated Format",
                    "Default Format",
                    "Victory Screen Level Sound",
                    "Warning Side Battle UI",
                    "Weapon Swap Text Hit",
                    "Weapon Swap Text Critical",
                    "Weapon Swap Command",
                    "Weapon Swap Text Evasion",
                    "alwaysDash",
                    "renderingMode",
                    "Attributes Command",
                    "Attributes Column 1",
                    "Attributes Column 2",
                    "Attributes Column 3",
                    "Warning OTB",
                    "</span> Minimum Damage</span></td>",
                    "Present Settings",
                ]
                .contains(&str)
                    || str.starts_with("Boost Point") && !str.ends_with("Command")
                    || str.contains("Icon")
                    || str.starts_with("Folder") && str.ends_with(|c: char| c.is_alphanumeric())
                    || str.ends_with(['X', 'Y'])
                    || str.contains("BGM")
                    || str.contains("Label")
                    || str.starts_with("Custom ") && str.chars().nth(7).is_some_and(|c: char| c.is_alphanumeric())
                    || str.starts_with("outlineColor")
                    || ((str.starts_with("Menu")
                        || str.starts_with("Item")
                        || str.starts_with("Skill")
                        || str.starts_with("Equip")
                        || str.starts_with("Status")
                        || str.starts_with("Save")
                        || str.starts_with("Options")
                        || str.starts_with("End"))
                        && (str.ends_with("Background") || str.ends_with("Motion")))
                    || str.starts_with("Menu ") && str.chars().nth(5).is_some_and(|c: char| c.is_alphanumeric())
                    || ((str.starts_with("MHP")
                        || str.starts_with("MMP")
                        || str.starts_with("ATK")
                        || str.starts_with("DEF")
                        || str.starts_with("MAT")
                        || str.starts_with("MDF")
                        || str.starts_with("AGI")
                        || str.starts_with("LUK"))
                        && ((str.ends_with("Formula")
                            || str.ends_with("Maximum")
                            || str.ends_with("Minimum")
                            || str.ends_with("Effect"))
                            || str.ends_with("Color")))
                    || str.starts_with("Damage") && str.ends_with(|c: char| c.is_alphanumeric())
            }) {
                return;
            }

            let str: &str = unsafe { value.as_str().unwrap_unchecked() }.trim();

            if !(str.is_empty()
                || STRING_IS_ONLY_SYMBOLS_RE.is_match(str)
                || ["true", "false", "none", "time", "off"].contains(&str)
                || str.starts_with("this.")
                    && str.chars().nth(5).is_some_and(|c: char| c.is_alphabetic())
                    && str.ends_with(")")
                || str.starts_with("rgba"))
            {
                let mut string: String = str.replace('\n', NEW_LINE);

                if romanize {
                    string = romanize_string(string);
                }

                if write {
                    if let Some(translated) = unsafe { map.unwrap_unchecked() }.get(&string) {
                        *value = translated.into();
                    }
                } else {
                    unsafe { strings.as_mut().unwrap_unchecked() }.push(string);
                }
            }
        }
        sonic_rs::JsonType::Object => {
            for (key, value) in value.as_object_mut().unwrap() {
                traverse_json(Some(key), value, map, strings, write, romanize);
            }
        }
        sonic_rs::JsonType::Array => {
            for value in value.as_array_mut().unwrap() {
                traverse_json(None, value, map, strings, write, romanize);
            }
        }
        _ => {}
    }
}
