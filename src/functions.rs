use crate::{
    statics::{
        localization::{AT_POSITION_MSG, COULD_NOT_SPLIT_LINE_MSG, IN_FILE_MSG},
        regexes::{INVALID_MULTILINE_VARIABLE_RE, INVALID_VARIABLE_RE, IS_ONLY_SYMBOLS_RE, PLUGINS_REGEXPS},
        HASHER, LINES_SEPARATOR, NEW_LINE, SYMBOLS,
    },
    types::{
        Code, EachLine, EngineType, GameType, OptionExt, ProcessingMode, ResultExt, StringHashMap, TrimReplace,
        Variable,
    },
};
use indexmap::{IndexMap, IndexSet};
use marshal_rs::{load, StringMode};
use smallvec::SmallVec;
use sonic_rs::{from_str, prelude::*, Object, Value};
use std::{
    collections::{HashMap, HashSet, VecDeque},
    ffi::OsString,
    fs::{read, read_to_string, DirEntry, File},
    io::{self, BufReader, Read},
    path::{Path, PathBuf},
    str::{from_utf8_unchecked, Chars},
    sync::{Arc, Mutex},
};
use xxhash_rust::xxh3::Xxh3DefaultBuilder;

#[inline]
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

#[inline(always)]
pub fn get_object_data(object: &Object) -> Vec<u8> {
    let mut vec: Vec<u8> = Vec::new();

    if let Some(object_type) = object.get(&"__type") {
        if object_type.as_str().is_some_and(|_type: &str| _type == "bytes") {
            vec = unsafe { sonic_rs::from_value::<Vec<u8>>(&object["data"]).unwrap_unchecked() };
        }
    }

    vec
}

#[inline]
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

#[inline(always)]
pub const fn determine_extension(engine_type: EngineType) -> &'static str {
    match engine_type {
        EngineType::New => ".json",
        EngineType::VXAce => ".rvdata2",
        EngineType::VX => ".rvdata",
        EngineType::XP => ".rxdata",
    }
}

#[inline]
pub fn filter_maps(entry: Result<DirEntry, std::io::Error>, engine_type: EngineType) -> Option<(String, Value)> {
    if let Ok(entry) = entry {
        if !entry.file_type().unwrap_log().is_file() {
            return None;
        };

        let filename: OsString = entry.file_name();
        let filename_str: &str = unsafe { from_utf8_unchecked(filename.as_encoded_bytes()) };

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

#[inline]
pub fn filter_other(
    entry: Result<DirEntry, std::io::Error>,
    engine_type: EngineType,
    game_type: Option<GameType>,
) -> Option<(String, Value)> {
    if let Ok(entry) = entry {
        if !entry.file_type().unwrap_log().is_file() {
            return None;
        };

        let filename: OsString = entry.file_name();
        let filename_str: &str = unsafe { from_utf8_unchecked(filename.as_encoded_bytes()) };

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

#[inline(always)]
pub const fn get_system_labels(
    engine_type: EngineType,
) -> (
    &'static str,
    &'static str,
    &'static str,
    &'static str,
    &'static str,
    &'static str,
) {
    match engine_type {
        EngineType::New => (
            "armorTypes",
            "elements",
            "skillTypes",
            "terms",
            "weaponTypes",
            "gameTitle",
        ),
        EngineType::XP => (
            "__symbol__armor_types",
            "__symbol__elements",
            "__symbol__skill_types",
            "__symbol__words",
            "__symbol__weapon_types",
            "__symbol__game_title",
        ),
        _ => (
            "__symbol__armor_types",
            "__symbol__elements",
            "__symbol__skill_types",
            "__symbol__terms",
            "__symbol__weapon_types",
            "__symbol__game_title",
        ),
    }
}

#[inline(always)]
pub const fn get_maps_labels(
    engine_type: EngineType,
) -> (
    &'static str,
    &'static str,
    &'static str,
    &'static str,
    &'static str,
    &'static str,
) {
    match engine_type {
        EngineType::New => ("displayName", "events", "pages", "list", "code", "parameters"),
        _ => (
            "__symbol__display_name",
            "__symbol__events",
            "__symbol__pages",
            "__symbol__list",
            "__symbol__code",
            "__symbol__parameters",
        ),
    }
}

#[allow(clippy::type_complexity)]
#[inline(always)]
pub const fn get_other_labels(
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
    match engine_type {
        EngineType::New => (
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
        ),
        _ => (
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
        ),
    }
}

#[inline]
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

#[allow(clippy::too_many_arguments)]
pub fn traverse_json(
    key: Option<&str>,
    value: &mut Value,
    lines: &mut Option<&mut Vec<String>>,
    map: &mut Option<&mut VecDeque<(String, String)>>,
    set: &Option<&HashSet<String, Xxh3DefaultBuilder>>,
    write: bool,
    romanize: bool,
    processing_mode: ProcessingMode,
    ignore_entry: Option<&HashSet<String, Xxh3DefaultBuilder>>,
) {
    let invalid_key = |key: &Option<&str>| -> bool {
        if let Some(str) = key {
            if str.starts_with("LATIN") {
                false
            } else {
                PLUGINS_REGEXPS.iter().any(|re| re.is_match(str))
            }
        } else {
            false
        }
    };

    match value.get_type() {
        sonic_rs::JsonType::String => {
            if invalid_key(&key) {
                return;
            }

            let str: &str = unsafe { value.as_str().unwrap_unchecked() }.trim();

            if !(str.is_empty()
                || IS_ONLY_SYMBOLS_RE.is_match(str)
                || ["true", "false", "none", "time", "off"].contains(&str)
                || str.starts_with("this.")
                    && str.chars().nth(5).is_some_and(|c: char| c.is_alphabetic())
                    && str.ends_with(")")
                || str.starts_with("rgba"))
                || key.is_some_and(|x| x.starts_with("LATIN"))
            {
                let mut string: String = str.replace('\n', NEW_LINE);

                if romanize {
                    string = romanize_string(string);
                }

                if write {
                    if !unsafe { set.as_ref().unwrap_unchecked() }.contains(&string) {
                        return;
                    }

                    if let Some((_, translated)) = unsafe { map.as_mut().unwrap_unchecked().pop_front() } {
                        *value = (&translated).into();
                    }
                } else {
                    if let Some(entry) = ignore_entry {
                        if entry.contains(&string) {
                            return;
                        }
                    }

                    let lines = unsafe { lines.as_mut().unwrap_unchecked() };

                    lines.push(string);
                    let last: &String = unsafe { lines.last().unwrap_unchecked() };
                    let pos: usize = lines.len() - 1;

                    if processing_mode == ProcessingMode::Append {
                        let translation_map = unsafe { map.as_mut().unwrap_unchecked() };

                        if translation_map.get(pos).is_some_and(|x| *last != x.0) {
                            translation_map.insert(pos, (last.to_owned(), String::new()));
                        }
                    }
                }
            }
        }
        sonic_rs::JsonType::Object => {
            for (key, value) in value.as_object_mut().unwrap_log() {
                traverse_json(
                    Some(key),
                    value,
                    lines,
                    map,
                    set,
                    write,
                    romanize,
                    processing_mode,
                    ignore_entry,
                );
            }
        }
        sonic_rs::JsonType::Array => {
            for value in value.as_array_mut().unwrap_log() {
                traverse_json(
                    None,
                    value,
                    lines,
                    map,
                    set,
                    write,
                    romanize,
                    processing_mode,
                    ignore_entry,
                );
            }
        }
        _ => {}
    }
}

#[inline(always)]
pub fn string_is_only_symbols(string: &str) -> bool {
    !string.chars().any(|c| !SYMBOLS.contains(&c))
}

#[inline]
pub fn ends_with_if_index(string: &str) -> Option<usize> {
    if string.ends_with(')') {
        let mut step: u8 = 0;
        let chars = string.char_indices().rev().skip(1);

        for (i, char) in chars {
            match step {
                0 => {
                    if char == '(' {
                        step = 1;
                    }
                }
                1 => {
                    if char == 'f' {
                        step = 2;
                    } else {
                        return None;
                    }
                }
                2 => {
                    if char == 'i' {
                        step = 3;
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
    }

    None
}

#[inline]
pub fn find_lisa_prefix_index(string: &str) -> Option<usize> {
    if string.starts_with(r"\et") {
        let mut index: usize = 5;

        loop {
            let char: &str = unsafe { string.get(index..index + 1).unwrap_unchecked() };

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
        Some(4)
    } else {
        None
    }
}

/// 401 - Dialogue line.
///
/// 101 - Start of the dialogue line. (**XP ENGINE ONLY!**)
///
/// 102 - Dialogue choices array.
///
/// 402 - One of the dialogue choices from the array. (**WRITE ONLY!**)
///
/// 405 - Credits lines. (**probably NEWER ENGINES ONLY!**)
///
/// 356 - System line, special text. (that one needs clarification)
///
/// 655 - Line displayed in shop - from an external script. (**OLDER ENGINES ONLY!**)
///
/// 324, 320 - Some used in-game line. (**probably NEWER ENGINES ONLY!**)
#[inline(always)]
pub const fn is_allowed_code(code: u16) -> bool {
    matches!(code, 101 | 102 | 320 | 324 | 356 | 401 | 402 | 405 | 655)
}

#[inline(always)]
pub fn parse_map_number(string: &str) -> u16 {
    unsafe {
        String::from_utf8_unchecked(
            string
                .as_bytes()
                .iter()
                .filter(|c| c.is_ascii_digit())
                .take(3)
                .copied()
                .collect(),
        )
        .parse::<u16>()
        .unwrap_unchecked()
    }
}

#[inline]
pub fn parse_translation<'a>(
    translation: &'a str,
    file: &'a str,
    write: bool,
) -> Box<dyn Iterator<Item = (String, String)> + 'a> {
    Box::new(translation.split('\n').enumerate().filter_map(move |(i, line)| {
        if write && line.starts_with("<!--") {
            return None;
        }

        let split: Vec<&str> = line.split(LINES_SEPARATOR).collect();

        if split.len() >= 2 {
            let original: &str = split.first().unwrap();
            let translation: &str = split.into_iter().skip(1).rfind(|x| !x.is_empty()).unwrap_or("");

            if write {
                #[cfg(not(debug_assertions))]
                if translation.is_empty() {
                    return None;
                }

                Some((
                    original.replace(NEW_LINE, "\n").trim_replace(),
                    translation.replace(NEW_LINE, "\n").trim_replace(),
                ))
            } else {
                Some((original.to_owned(), translation.to_owned()))
            }
        } else {
            eprintln!(
                "{COULD_NOT_SPLIT_LINE_MSG} ({line})\n{AT_POSITION_MSG} {i}\n{IN_FILE_MSG} {file}",
                i = i + 1,
            );
            None
        }
    }))
}

pub fn parse_ignore(
    ignore_file_path: PathBuf,
) -> IndexMap<String, HashSet<String, Xxh3DefaultBuilder>, Xxh3DefaultBuilder> {
    let mut map: IndexMap<String, HashSet<String, Xxh3DefaultBuilder>, Xxh3DefaultBuilder> =
        IndexMap::with_hasher(HASHER);

    if ignore_file_path.exists() {
        let ignore_file_content: String = read_to_string(ignore_file_path).unwrap_log();

        for line in ignore_file_content.split('\n') {
            if !line.is_empty() {
                if line.starts_with("<!-- File") {
                    map.insert(line.to_owned(), HashSet::default());
                } else {
                    map.last_mut().unwrap().1.insert(line.to_owned());
                }
            }
        }
    }

    map
}

#[allow(clippy::too_many_arguments, clippy::collapsible_match, clippy::single_match)]
#[inline(always)]
pub fn process_variable(
    mut variable_text: String,
    note_text: Option<&str>,
    variable_type: Variable,
    filename: &str,
    game_type: Option<GameType>,
    engine_type: EngineType,
    romanize: bool,
    hashmap: Option<&HashMap<String, String, Xxh3DefaultBuilder>>, // some only when write is true
    write: bool,
) -> Option<String> {
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
        }

        variable_text = variable_text.replace("\r\n", "\n");
    }

    let remaining_strings: SmallVec<[(String, bool); 4]> = SmallVec::with_capacity(4);

    if let Some(game_type) = game_type {
        match game_type {
            GameType::Termina => {
                if variable_text.contains("---") || variable_text.starts_with("///") {
                    return None;
                }

                match variable_type {
                    Variable::Description => {
                        if write {
                            if let Some(note) = note_text {
                                let mut note_string: String = String::from(note);
                                let mut note_chars: Chars = note.chars();
                                let mut note_is_continuation: bool = false;

                                if !note.starts_with("flesh puppetry") {
                                    if let Some((first_char, second_char)) = note_chars.next().zip(note_chars.next()) {
                                        if ((first_char == '\n' && second_char != '\n')
                                            || (first_char.is_ascii_alphabetic()
                                                || first_char == '"'
                                                || note.starts_with("4 sticks")))
                                            && !matches!(first_char, '.' | '!' | '/' | '?')
                                        {
                                            note_is_continuation = true;
                                        }
                                    }
                                }

                                if note_is_continuation {
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
                        } else if let Some(note) = note_text {
                            let mut note_chars: Chars = note.chars();
                            let mut note_is_continuation: bool = false;

                            if !note.starts_with("flesh puppetry") {
                                if let Some((first_char, second_char)) = note_chars.next().zip(note_chars.next()) {
                                    if ((first_char == '\n' && second_char != '\n')
                                        || (first_char.is_ascii_alphabetic()
                                            || first_char == '"'
                                            || note.starts_with("4 sticks")))
                                        && !matches!(first_char, '.' | '!' | '/' | '?')
                                    {
                                        note_is_continuation = true;
                                    }
                                }
                            }

                            if note_is_continuation {
                                let note_string: String;

                                if let Some((mut left, _)) = note.trim_start().split_once('\n') {
                                    left = left.trim();

                                    if !left.ends_with(['.', '%', '!', '"']) {
                                        return None;
                                    }

                                    note_string = String::from(NEW_LINE) + left;
                                } else {
                                    if !note.ends_with(['.', '%', '!', '"']) && !note.ends_with("takes place?") {
                                        return None;
                                    }

                                    note_string = note.to_owned();
                                }

                                if note_string.is_empty() {
                                    return None;
                                }

                                variable_text = variable_text + &note_string;
                            }
                        }
                    }
                    Variable::Message1 | Variable::Message2 | Variable::Message3 | Variable::Message4 => {
                        return None;
                    }
                    Variable::Note => {
                        if write && filename.starts_with("It") {
                            for string in [
                                "<Menu Category: Items>",
                                "<Menu Category: Food>",
                                "<Menu Category: Healing>",
                                "<Menu Category: Body bag>",
                            ] {
                                if variable_text.contains(string) {
                                    variable_text = variable_text.replace(string, &hashmap?[string]);
                                }
                            }
                        }

                        if !filename.starts_with("Cl") {
                            return None;
                        }
                    }
                    Variable::Name | Variable::Nickname => match &filename[..2] {
                        "Ac" => {
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
                            .contains(&variable_text.as_str())
                            {
                                return None;
                            }
                        }
                        "Ar" => {
                            if variable_text.starts_with("test_armor") {
                                return None;
                            }
                        }
                        "Cl" => {
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
                        }
                        "En" => {
                            if ["Spank Tank", "giant", "test"].contains(&variable_text.as_str()) {
                                return None;
                            }
                        }
                        "It" => {
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
                        }
                        "We" => {
                            if variable_text == "makeshift2" {
                                return None;
                            }
                        }
                        _ => {}
                    },
                }
            }
            _ => {} // custom processing for other games
        }
    }

    if romanize {
        variable_text = romanize_string(variable_text);
    }

    if write {
        let translated: Option<String> = hashmap?.get(&variable_text).map(|translated: &String| {
            let mut result: String = translated.to_owned();

            for (string, position) in remaining_strings.into_iter() {
                match position {
                    true => result += &string,
                    false => result = string + &result,
                }
            }

            if matches!(
                variable_type,
                Variable::Message1 | Variable::Message2 | Variable::Message3 | Variable::Message4
            ) && !(variable_type == Variable::Message2 && filename.starts_with("Sk"))
            {
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
    } else {
        Some(variable_text)
    }
}

#[allow(
    clippy::single_match,
    clippy::match_single_binding,
    clippy::too_many_arguments,
    unused_mut,
    unreachable_patterns
)]
#[inline(always)]
pub fn process_parameter(
    code: Code,
    mut parameter: &str,
    game_type: Option<GameType>,
    engine_type: EngineType,
    romanize: bool,
    map: Option<&StringHashMap>,                 // used only when write is true
    deque: Option<Arc<Mutex<VecDeque<String>>>>, // used only when write is true
    write: bool,
) -> Option<String> {
    if string_is_only_symbols(parameter) {
        return None;
    }

    let mut extra_strings: SmallVec<[(&str, bool); 4]> = SmallVec::with_capacity(4);

    if let Some(game_type) = game_type {
        match game_type {
            GameType::Termina => {
                if parameter
                    .chars()
                    .all(|c| c.is_ascii_lowercase() || (c.is_ascii_punctuation() && c != '"'))
                {
                    return None;
                }
                if code == Code::System
                    && !parameter.starts_with("Gab")
                    && (!parameter.starts_with("choice_text") || parameter.ends_with("????"))
                {
                    return None;
                }
            }
            GameType::LisaRPG => {
                if matches!(code, Code::Dialogue | Code::DialogueStart) {
                    if let Some(i) = find_lisa_prefix_index(parameter) {
                        if string_is_only_symbols(&parameter[i..]) {
                            return None;
                        }

                        if write {
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
    }

    if engine_type != EngineType::New {
        if let Some(i) = ends_with_if_index(parameter) {
            if write {
                extra_strings.push((&parameter[..i], true));
            }

            parameter = &parameter[..i];
        }

        if code == Code::Shop {
            if !parameter.contains("shop_talk") {
                return None;
            }

            let (_, mut actual_string) = unsafe { parameter.split_once('=').unwrap_unchecked() };
            actual_string = actual_string.trim();

            if actual_string.len() < 2 {
                return None;
            }

            let without_quotes: &str = &actual_string[1..actual_string.len() - 1];

            if without_quotes.is_empty() || string_is_only_symbols(without_quotes) {
                return None;
            }

            parameter = without_quotes;
        }
    }

    if write {
        let translated: Option<String> = if let Some(map) = map {
            map.get(parameter).map(|s| s.to_owned())
        } else {
            let deque = &mut deque.as_ref().unwrap_log().lock().unwrap_log();

            if code == Code::ChoiceArray {
                deque.front().map(String::to_owned)
            } else {
                deque.pop_front()
            }
        };

        translated.map(|mut trans| {
            for (s, append) in extra_strings {
                if append {
                    trans.push_str(s);
                } else {
                    trans = format!("{}{}", s, trans);
                }
            }
            trans
        })
    } else {
        let mut result: String = parameter.to_owned();

        if romanize {
            result = romanize_string(result);
        }

        Some(result)
    }
}
