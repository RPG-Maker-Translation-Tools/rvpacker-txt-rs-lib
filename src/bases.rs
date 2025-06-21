#![allow(clippy::mut_from_ref)]
#![allow(invalid_reference_casting)]

use crate::{
    constants::{localization::*, regexes::*, *},
    functions::*,
    types::*,
};
#[cfg(feature = "log")]
use crate::{eprintln, println};
use flate2::{read::ZlibDecoder, write::ZlibEncoder, Compression};
use marshal_rs::{dump, load_binary, load_utf8};
use regex::Regex;
use smallvec::SmallVec;
use sonic_rs::{
    from_str, from_value, json, prelude::*, to_string, to_vec, Array, Object,
    Value,
};
use std::{
    fs::{read, read_dir, read_to_string, write, DirEntry},
    io::{Read, Write},
    mem::{take, transmute},
    path::{Path, PathBuf},
    str::Chars,
};

trait AsUnsafeMut {
    fn unsafe_mut(&self) -> &mut Self;
}

impl<T> AsUnsafeMut for T {
    fn unsafe_mut(&self) -> &mut Self {
        unsafe { &mut *(self as *const T as *mut T) }
    }
}

#[inline]
fn make_string_value(string: &str, literal: bool) -> Value {
    if literal {
        Value::from(string)
    } else {
        json!({"__type": "bytes", "data": Array::from(string.as_bytes())})
    }
}

#[inline(always)]
fn romanize_string(string: String) -> String {
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
fn get_object_data(object: &Object) -> Vec<u8> {
    let type_field = object.get(&"__type");

    let Some(object_type) = type_field.as_str() else {
        return Vec::new();
    };

    if object_type != "bytes" {
        return Vec::new();
    }

    from_value(&object["data"]).unwrap_log()
}

#[inline(always)]
fn string_is_only_symbols(string: &str) -> bool {
    !string.chars().any(|c| !SYMBOLS.contains(&c))
}

#[inline(always)]
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

#[inline(always)]
fn find_lisa_prefix_index(string: &str) -> Option<usize> {
    if string.starts_with(r"\et[") {
        let mut index: usize = r"\et[".len() + 1;

        loop {
            let char: &str = string.get(index..index + 1).unwrap_log();

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
const fn is_allowed_code(code: u16) -> bool {
    matches!(code, 101 | 102 | 320 | 324 | 356 | 401 | 402 | 405 | 655)
}

#[inline(always)]
const fn is_bad_code(code: Code, engine_type: EngineType) -> bool {
    code.is_dialogue_start() && !engine_type.is_xp()
}

#[inline(always)]
fn parse_translation<'a>(
    translation: &'a str,
    file: &'a str,
    write: bool,
    trim: bool,
) -> Box<dyn Iterator<Item = (String, String)> + 'a> {
    Box::new(translation.lines().enumerate().filter_map(move |(i, line)| {
        if write && line.starts_with(COMMENT_PREFIX) {
            return None;
        }

        let split: Vec<&str> = line.split(LINES_SEPARATOR).collect();

        if split.len() < 2 {
            eprintln!("{COULD_NOT_SPLIT_LINE_MSG} ({line})\n{AT_POSITION_MSG} {i}\n{IN_FILE_MSG} {file}", i = i + 1,);
            return None;
        }

        let source: &str = split.first().unwrap_log();
        let translation: &str = split.into_iter().skip(1).rfind(|x| !x.is_empty()).unwrap_or_default();

        if write {
            #[cfg(not(debug_assertions))]
            if translation.is_empty() {
                return None;
            }

            let source_replaced: String = source.replace(NEW_LINE, "\n");
            let translation_replaced: String = translation.replace(NEW_LINE, "\n");

            if trim {
                return Some((source_replaced.trim_replace(), translation_replaced.trim_replace()));
            } else {
                return Some((source_replaced, translation_replaced));
            }
        }

        Some((source.to_owned(), translation.to_owned()))
    }))
}

pub fn extract_string(value: &Value, trim: bool, ret: bool) -> Option<String> {
    let string = value.as_str().map(str::to_owned).unwrap_or_else(|| {
        let Some(obj) = value.as_object() else {
            return String::new();
        };

        String::from_utf8(get_object_data(obj)).unwrap_log()
    });

    let trimmed: &str = string.trim();

    if ret && trimmed.is_empty() {
        return None;
    }

    Some(if trim { trimmed.to_owned() } else { string })
}

pub struct Base<'a> {
    pub engine_type: EngineType,
    pub game_type: GameType,

    pub romanize: bool,
    pub logging: bool,
    pub trim: bool,

    pub read_mode: ReadMode,
    pub ignore: bool,
    pub sort: bool,

    pub stat: bool,
    pub leave_filled: bool,
    pub purge_empty: bool,
    pub create_ignore: bool,
    pub purge_indices: PurgeIndices,

    pub ignore_map: &'a mut IgnoreMap,
    pub ignore_entry: &'static mut IgnoreEntry,
    pub stat_vec: &'a mut StatVec,

    pub translation_set: TranslationSet,
    pub translation_map: &'static mut TranslationMap,

    translation_duplicate_map: TranslationDuplicateMap,

    pub labels: Labels,

    pub mode: ProcessingMode,

    pub file_type: RPGMFileType,

    text_file_path: PathBuf,
    translation_path: &'a Path,
    output_path: &'a Path,
}

impl<'a> Base<'a> {
    #[inline]
    fn purge_translation(&mut self) {
        let mut iter: Box<dyn Iterator<Item = (&String, &String)>> =
            Box::new(self.translation_map.iter());

        if self.purge_empty && self.file_type.is_system() {
            iter = Box::new(iter.take(self.translation_map.len() - 1));
        }

        for (i, (source, translation)) in iter.enumerate() {
            if source.starts_with(COMMENT_PREFIX) {
                continue;
            }

            if self.purge_empty {
                if !translation.is_empty() {
                    continue;
                }
            } else {
                if self.leave_filled && !translation.is_empty() {
                    continue;
                }

                if self.translation_set.contains(source) {
                    continue;
                }
            }

            if self.stat {
                self.stat_vec
                    .push((source.to_owned(), translation.to_owned()));
                continue;
            }

            self.purge_indices.insert(i);

            if self.create_ignore {
                self.ignore_entry.insert(source.to_owned());
            }
        }
    }

    #[allow(
        clippy::single_match,
        clippy::match_single_binding,
        unreachable_patterns
    )]
    #[inline(always)]
    pub fn process_parameter(
        &mut self,
        code: Code,
        parameter: String,
    ) -> Option<String> {
        if string_is_only_symbols(&parameter) {
            return None;
        }

        let mut parameter: &str = &parameter;
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
                    if let Some(i) = find_lisa_prefix_index(parameter) {
                        if string_is_only_symbols(&parameter[i..]) {
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
            if let Some(i) = ends_with_if_index(parameter) {
                if self.mode.is_write() {
                    extra_strings.push((&parameter[..i], true));
                }

                parameter = &parameter[..i];
            }

            if code.is_shop() {
                if !parameter.contains("shop_talk") {
                    return None;
                }

                let (_, mut actual_string) =
                    parameter.split_once('=').unwrap_log();
                actual_string = actual_string.trim();

                if actual_string.len() < 2 {
                    return None;
                }

                let without_quotes: &str =
                    &actual_string[1..actual_string.len() - 1];

                if without_quotes.is_empty()
                    || string_is_only_symbols(without_quotes)
                {
                    return None;
                }

                parameter = without_quotes;
            }
        }

        if self.mode.is_write() {
            let translated: Option<String> =
                self.translation_map.get(parameter).map(|s| s.to_owned());

            translated.map(|mut trans| {
                for (s, append) in extra_strings {
                    if append {
                        trans.push_str(s);
                    } else {
                        trans = format!("{s}{trans}");
                    }
                }
                trans
            })
        } else {
            let mut result: String = parameter.to_owned();

            if self.romanize {
                result = romanize_string(result);
            }

            Some(result)
        }
    }

    #[inline]
    fn write_output(&mut self, json: Value, output_file: impl AsRef<Path>) {
        let output_file_path = if self.mode.is_write() {
            self.output_path.join(&output_file)
        } else {
            take(&mut self.text_file_path)
        };

        let mut output_content: Vec<u8> = if !self.mode.is_write() {
            let mut iter: Box<dyn Iterator<Item = (String, String)>>;

            if self.file_type.is_map()
                || self.file_type.is_scripts()
                || self.file_type.is_plugins()
            {
                iter = Box::new(
                    take(&mut self.translation_duplicate_map).into_iter(),
                );
            } else {
                iter = Box::new(self.translation_map.drain(..));

                if self.mode.is_purge() {
                    iter = Box::new(
                        iter.enumerate()
                            .filter(|(i, _)| !self.purge_indices.remove(i))
                            .map(|(_, kv)| kv),
                    );
                }
            }

            iter.map(|(source, translated)| {
                format!("{source}{LINES_SEPARATOR}{translated}\n")
            })
            .collect::<String>()
            .into_bytes()
        } else {
            if self.file_type.is_plugins() {
                format!("var $plugins =\n{}", to_string(&json).unwrap_log())
                    .into_bytes()
            } else {
                if self.engine_type.is_new() {
                    to_vec(&json).unwrap_log()
                } else {
                    let json_str = sonic_rs::to_string(&json).unwrap_log();

                    dump(
                        serde_json::from_str(&json_str).unwrap_log(),
                        if self.file_type.is_scripts() {
                            None
                        } else {
                            INSTANCE_VAR_PREFIX
                        },
                    )
                }
            }
        };

        if !self.mode.is_write() {
            output_content.pop();
        };

        write(&output_file_path, output_content).unwrap_log();

        if self.logging {
            let msg = if self.mode.is_read() {
                PARSED_FILE_MSG
            } else if self.mode.is_write() {
                WROTE_FILE_MSG
            } else {
                PURGED_FILE_MSG
            };

            println!(
                "{msg} {}",
                match self.file_type {
                    RPGMFileType::Actors => "actors",
                    RPGMFileType::Armors => "armors",
                    RPGMFileType::Classes => "classes",
                    RPGMFileType::Enemies => "enemies",
                    RPGMFileType::Events => "events",
                    RPGMFileType::Items => "items",
                    RPGMFileType::Map => "map",
                    RPGMFileType::Plugins => "plugins",
                    RPGMFileType::Scripts => "scripts",
                    RPGMFileType::Skills => "skills",
                    RPGMFileType::States => "states",
                    RPGMFileType::System => "system",
                    RPGMFileType::Troops => "troops",
                    RPGMFileType::Weapons => "weapons",
                    RPGMFileType::Invalid => unreachable!(),
                }
            );
        }
    }

    #[inline]
    fn process_param(
        &mut self,
        value: &mut Value,
        code: Code,
        mut parameter: String,
    ) {
        if self.mode.is_write() {
            if self.romanize {
                parameter = romanize_string(parameter);
            }

            let Some(mut translated) =
                self.process_parameter(code, parameter.clone())
            else {
                return;
            };

            if code.is_shop() {
                if let Some((left, _)) = parameter.split_once('=') {
                    translated = format!("{left}=\"{translated}\"");
                }
            }

            *value = make_string_value(&translated, self.engine_type.is_new());
            return;
        }

        let Some(parsed) = self.process_parameter(code, parameter) else {
            return;
        };

        if self.ignore_entry.contains(&parsed) {
            return;
        }

        if self.mode.is_purge() {
            return;
        }

        if self.translation_map.contains_key(&parsed) {
            if self.sort {
                self.sort_present(&parsed);
            }
        } else {
            self.insert_not_present(parsed);
        }
    }

    #[inline]
    fn insert_not_present(&mut self, string: String) {
        if self.read_mode.is_append() {
            self.translation_map.shift_insert(
                self.translation_set.len().saturating_sub(1),
                string,
                String::new(),
            );
        } else {
            self.translation_map.insert(string, String::new());
        }
    }

    #[inline]
    fn sort_present(&mut self, string: &str) {
        let map_index: usize =
            self.translation_map.get_index_of(string).unwrap_log();

        let mut set_index: usize =
            self.translation_set.get_index_of(string).unwrap_log();

        set_index += self
            .translation_map
            .iter()
            .take(4)
            .filter(|(k, _)| k.starts_with(COMMENT_PREFIX))
            .count();

        let map_len: usize = self.translation_map.len();
        if set_index >= map_len {
            set_index = map_len - 1;
        }

        self.translation_map.swap_indices(set_index, map_index);
    }

    #[inline]
    fn process_dialogue_lines(
        &mut self,
        list: &mut Array,
        dialogue_lines: &mut SmallVec<[String; 4]>,
        dialogue_line_indices: &mut SmallVec<[usize; 4]>,
        write_string_literally: bool,
    ) {
        let mut joined: String = dialogue_lines.join(if self.mode.is_write() {
            "\n"
        } else {
            NEW_LINE
        });

        if !self.mode.is_write() {
            self.process_param(&mut Value::new(), Code::Dialogue, joined);
            return;
        }

        if self.romanize {
            joined = romanize_string(joined);
        }

        let Some(translation) = self.process_parameter(Code::Dialogue, joined)
        else {
            return;
        };

        let translation_lines: Vec<&str> = translation.lines().collect();
        let split_line_count: usize = translation_lines.len();
        let dialogue_line_count: usize = dialogue_lines.len();

        for (i, &index) in dialogue_line_indices.iter().enumerate() {
            list[index][self.labels.parameters][0] = if i < split_line_count {
                make_string_value(translation_lines[i], write_string_literally)
            } else {
                // Overwrite leftover source text
                Value::from_static_str(" ")
            }
        }

        if split_line_count > dialogue_line_count {
            let remaining: String =
                translation_lines[dialogue_line_count - 1..].join("\n");

            list[*dialogue_line_indices.last().unwrap_log()]
                [self.labels.parameters][0] = Value::from(&remaining);
        }
    }

    #[inline]
    fn process_list(&mut self, list: &mut Array) {
        let mut in_sequence: bool = false;

        let mut dialogue_lines: SmallVec<[String; 4]> =
            SmallVec::with_capacity(4);
        let mut dialogue_line_indices: SmallVec<[usize; 4]> =
            SmallVec::with_capacity(4);

        let mut write_string_literally: bool = self.engine_type.is_new();

        for (item_idx, item) in
            unsafe { &mut *(list as *mut Array) }.iter_mut().enumerate()
        {
            let code: u16 = item[self.labels.code].as_u64().unwrap_log() as u16;

            let code: Code = if is_allowed_code(code) {
                let code: Code = unsafe { transmute(code) };

                if is_bad_code(code, self.engine_type) {
                    Code::Bad
                } else {
                    code
                }
            } else {
                Code::Bad
            };

            if self.mode.is_write() && !self.engine_type.is_new() {
                let parameters: &Array =
                    item[self.labels.parameters].as_array().unwrap_log();

                if !parameters.is_empty() {
                    write_string_literally = !match code {
                        Code::ChoiceArray => parameters[0][0].is_object(),
                        Code::Misc1 | Code::Misc2 | Code::Choice => {
                            parameters[1].is_object()
                        }
                        _ => parameters[0].is_object(),
                    }
                }
            }

            if in_sequence
                && (!self.engine_type.is_xp() && !code.is_any_dialogue())
                || (code.is_dialogue_start() && !dialogue_lines.is_empty())
            {
                if !dialogue_lines.is_empty() {
                    self.process_dialogue_lines(
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

            let parameters: &mut Array =
                item[self.labels.parameters].as_array_mut().unwrap_log();

            if parameters.is_empty() {
                continue;
            }

            let value_index: usize = if code.is_any_misc() || code.is_choice() {
                1
            } else {
                0
            };

            let value: &mut Value = &mut parameters[value_index];

            if code.is_choice_array() {
                for value in value.as_array_mut().unwrap_log() {
                    let Some(string) = extract_string(value, self.trim, true)
                    else {
                        continue;
                    };

                    self.process_param(value, code, string);
                }
            } else {
                let parameter_string: String =
                    extract_string(value, self.trim, false).unwrap_log();

                if !code.is_credit() && parameter_string.is_empty() {
                    continue;
                }

                if code.is_any_dialogue() {
                    dialogue_lines.push(parameter_string);

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

    fn init_stat_and_ignore(&mut self, entry_string: &str) {
        if self.stat {
            self.stat_vec.push((entry_string.to_owned(), String::new()));
        }

        if self.ignore || self.create_ignore {
            *self.ignore_entry.unsafe_mut() =
                self.ignore_map.entry(entry_string.to_owned()).or_default();
        }
    }

    fn parse_translation(
        &mut self,
        filename: &str,
        game_title: Option<&mut String>,
    ) -> bool {
        if self.mode.is_read() && !self.read_mode.is_append() {
            return false;
        }

        if self.read_mode.is_append() && !self.text_file_path.exists() {
            println!("{FILES_ARE_NOT_PARSED_MSG}");
            return true;
        }

        let translation: String =
            read_to_string(self.translation_path.join(filename)).unwrap_log();

        if let Some(game_title) = game_title {
            *game_title =
                translation[translation.rfind(LINES_SEPARATOR).unwrap_log()
                    + LINES_SEPARATOR.len()..]
                    .to_owned();
        }

        self.translation_map.extend(parse_translation(
            &translation,
            filename,
            self.mode.is_write(),
            self.trim,
        ));

        if self.mode.is_write() && self.translation_map.is_empty() {
            return true;
        }

        false
    }

    #[inline(always)]
    #[track_caller]
    fn parse_rpgm_file(&self, path: impl AsRef<Path>) -> Value {
        match self.engine_type {
            EngineType::New => {
                from_str(&read_to_string_without_bom(path).unwrap_log())
                    .unwrap_log()
            }
            _ => {
                let content = read(path).unwrap_log();

                if self.file_type.is_scripts() {
                    let serde_value =
                        load_binary(&content, INSTANCE_VAR_PREFIX).unwrap_log();
                    from_str(&serde_json::to_string(&serde_value).unwrap_log())
                        .unwrap_log()
                } else {
                    let serde_value =
                        load_utf8(&content, INSTANCE_VAR_PREFIX).unwrap_log();
                    from_str(&serde_json::to_string(&serde_value).unwrap_log())
                        .unwrap_log()
                }
            }
        }
    }

    fn get_next_translation_map<'b>(
        &mut self,
        translation_lines: &mut std::iter::Peekable<
            impl Iterator<Item = &'b str>,
        >,
        map_start_comment_prefix: &str,
    ) -> bool {
        let next = translation_lines.next();

        if next.is_none() {
            return true;
        }

        let (source, translation) =
            next.unwrap_log().split_once(LINES_SEPARATOR).unwrap_log();

        self.translation_map
            .insert(source.to_owned(), translation.to_owned());

        while let Some(line) = translation_lines.peek() {
            if let Some((source, translation)) =
                line.split_once(LINES_SEPARATOR)
            {
                if source.starts_with(map_start_comment_prefix) {
                    break;
                }

                translation_lines.next();

                if self.mode.is_write() && translation.is_empty() {
                    continue;
                }

                self.translation_map
                    .insert(source.to_owned(), translation.to_owned());
            } else {
                translation_lines.next();
            }
        }

        if self.mode.is_write() {
            if self.file_type.is_map() {
                let comment_count: usize = self
                    .translation_map
                    .iter()
                    .take(5)
                    .filter(|(k, _)| k.starts_with(COMMENT_PREFIX))
                    .count();

                if self.translation_map.len() - comment_count == 0 {
                    self.translation_map.clear();
                    return true;
                }
            } else if self.translation_map.len() <= 1 {
                self.translation_map.clear();
                return true;
            }
        }

        false
    }
}

pub struct MapBase<'a> {
    pub base: Base<'a>,

    // Private
    source_path: &'a Path,
}

impl<'a> MapBase<'a> {
    pub fn new(
        source_path: &'a Path,
        translation_path: &'a Path,
        output_path: &'a Path,
        engine_type: EngineType,
        mode: ProcessingMode,
    ) -> Self {
        let base = Base {
            engine_type,
            romanize: false,
            logging: false,
            trim: false,
            read_mode: ReadMode::Default,
            ignore: false,
            sort: false,
            stat: false,
            leave_filled: false,
            create_ignore: false,
            purge_empty: false,
            ignore_map: Box::leak(Box::new(IgnoreMap::default())),
            ignore_entry: Box::leak(Box::new(IgnoreEntry::default())),
            stat_vec: Box::leak(Box::new(StatVec::default())),
            translation_set: TranslationSet::default(),
            translation_map: Box::leak(Box::new(TranslationMap::default())),
            translation_duplicate_map: TranslationDuplicateMap::default(),
            purge_indices: PurgeIndices::default(),
            mode,
            labels: Labels::new(engine_type),
            game_type: GameType::None,
            file_type: RPGMFileType::Map,
            output_path,
            text_file_path: PathBuf::new(),
            translation_path,
        };

        Self {
            // Base
            base,

            // Private
            source_path,
        }
    }

    #[inline]
    fn filter_maps(
        entry: Result<DirEntry, std::io::Error>,
        engine_type: EngineType,
    ) -> Option<(String, PathBuf)> {
        let Ok(entry) = entry else {
            return None;
        };

        if !entry.file_type().unwrap_log().is_file() {
            return None;
        };

        let filename = entry.file_name();
        let filename_str: &str = filename.to_str().unwrap_log();

        if filename_str.starts_with("Map")
            && (*filename_str.as_bytes().get(3).unwrap_log() as char)
                .is_ascii_digit()
            && filename_str.ends_with(get_engine_extension(engine_type))
        {
            return Some((filename_str.to_owned(), entry.path()));
        }

        None
    }

    #[inline(always)]
    fn parse_map_number(string: &str) -> u16 {
        String::from_utf8(
            string
                .as_bytes()
                .iter()
                .filter(|c| c.is_ascii_digit())
                .take(3)
                .copied()
                .collect(),
        )
        .unwrap_log()
        .parse::<u16>()
        .unwrap_log()
    }

    #[inline]
    fn get_order_number(
        &self,
        mapinfos: &Value,
        entry: &str,
        map_number: &str,
    ) -> String {
        if self.base.engine_type.is_new() {
            &mapinfos[map_number.parse::<usize>().unwrap_log()]["order"]
        } else {
            &mapinfos[entry]["__symbol__order"]
        }
        .as_u64()
        .unwrap_or(0)
        .to_string()
    }

    #[inline]
    fn get_map_name(
        &self,
        mapinfos: &Value,
        entry: &str,
        map_number: &str,
    ) -> String {
        if self.base.engine_type.is_new() {
            &mapinfos[map_number.parse::<usize>().unwrap_log()]["name"]
        } else {
            &mapinfos[&entry]["__symbol__name"]
        }
        .as_str()
        .unwrap_or_default()
        .to_string()
    }

    #[inline]
    fn process_display_name(&mut self, map_object: &mut Value) -> String {
        let mut map_display_name_comment: String = String::new();

        if let Some(display_name) =
            map_object[self.base.labels.display_name].as_str()
        {
            if !display_name.is_empty() {
                if self.base.mode.is_write() {
                    let mut display_name: String = display_name.to_owned();

                    if self.base.romanize {
                        display_name = romanize_string(display_name)
                    }

                    if let Some(location_name) =
                        self.base.translation_map.get(&format!(
                            "{}{}{}",
                            DISPLAY_NAME_COMMENT_PREFIX,
                            display_name,
                            COMMENT_SUFFIX
                        ))
                    {
                        map_object[self.base.labels.display_name] =
                            Value::from(location_name);
                    }
                } else if self.base.mode.is_read() {
                    let mut display_name: String = display_name.to_owned();

                    if self.base.romanize {
                        display_name = romanize_string(display_name);
                    }

                    map_display_name_comment = format!("{DISPLAY_NAME_COMMENT_PREFIX}{display_name}{COMMENT_SUFFIX}");
                }
            }
        }

        map_display_name_comment
    }

    #[inline]
    fn process_maps(
        &mut self,
        map_entries: impl Iterator<Item = (String, PathBuf)>,
    ) {
        let mut mapinfos: Value = Value::new();
        if self.base.mode.is_read() {
            let mapinfos_path: PathBuf = self.source_path.join(format!(
                "MapInfos.{}",
                get_engine_extension(self.base.engine_type)
            ));
            mapinfos = self.base.parse_rpgm_file(mapinfos_path.as_path());
        }

        let translation: String;
        let mut translation_lines = "".lines().peekable();

        if self.base.read_mode.is_append() || !self.base.mode.is_read() {
            translation =
                read_to_string(&self.base.text_file_path).unwrap_log();
            translation_lines = translation.lines().peekable();
        }

        for (filename, path) in map_entries {
            let map_number: String =
                Self::parse_map_number(&filename).to_string();

            self.base.init_stat_and_ignore(&format!(
                "{FILE_ENTRY_PREFIX}map{map_number}{COMMENT_SUFFIX}"
            ));

            if self.base.read_mode.is_append() || !self.base.mode.is_read() {
                let proceed = self.base.get_next_translation_map(
                    &mut translation_lines,
                    MAP_NUMBER_COMMENT,
                );

                if proceed {
                    continue;
                }
            }

            if self.base.purge_empty {
                self.base.purge_translation();

                let iter = self
                    .base
                    .translation_map
                    .drain(..)
                    .enumerate()
                    .filter(|(i, _)| !self.base.purge_indices.remove(i))
                    .map(|(_, kv)| kv);

                self.base.translation_set.clear();

                if !self.base.mode.is_write() {
                    self.base.translation_duplicate_map.extend(iter);
                }

                if self.base.logging {
                    let msg = if self.base.mode.is_read() {
                        PARSED_FILE_MSG
                    } else if self.base.mode.is_write() {
                        WROTE_FILE_MSG
                    } else {
                        PURGED_FILE_MSG
                    };

                    println!("{msg} {filename}");
                }

                continue;
            }

            // TODO: Parse event name
            let mut map_object: Value = self.base.parse_rpgm_file(path);

            if self.base.mode.is_read() {
                let entry: String = format!("__integer__{map_number}");

                let order_number: String =
                    self.get_order_number(&mapinfos, &entry, &map_number);

                let map_display_name_comment: String =
                    self.process_display_name(&mut map_object);

                let map_name: String =
                    self.get_map_name(&mapinfos, &entry, &map_number);

                let map_name_comment: String = if !map_name.is_empty() {
                    format!(
                        "{MAP_NAME_COMMENT_PREFIX}{map_name}{COMMENT_SUFFIX}"
                    )
                } else {
                    String::new()
                };

                if self.base.read_mode.is_append() {
                    let range_end = if self.base.translation_map.len() > 4 {
                        4
                    } else {
                        self.base.translation_map.len() - 1
                    };

                    let start_comments =
                        self.base.translation_map.get_range(1..range_end);

                    if let Some(comments) = start_comments {
                        let mut order_comment_index: i8 = -1;
                        let mut map_name_comment_index: i8 = -1;
                        let mut map_display_name_comment_index: i8 = -1;

                        let mut replace_map_name: bool = false;
                        let mut replace_map_display_name: bool = false;

                        for ((k, _), i) in comments.iter().zip(1i8..4) {
                            if k == ORDER_COMMENT {
                                order_comment_index = i;
                            } else if k.starts_with(MAP_NAME_COMMENT_PREFIX) {
                                replace_map_name = *k != map_name_comment;
                                map_name_comment_index = i;
                            } else if k.starts_with(DISPLAY_NAME_COMMENT_PREFIX)
                            {
                                replace_map_display_name =
                                    *k != map_display_name_comment;
                                map_display_name_comment_index = i;
                            }
                        }

                        if order_comment_index == -1 {
                            self.base.translation_map.shift_insert(
                                1,
                                ORDER_COMMENT.to_owned(),
                                order_number,
                            );
                        } else {
                            self.base
                                .translation_map
                                .swap_indices(1, order_comment_index as usize);
                        }

                        if map_name_comment_index == -1 {
                            if !map_name_comment.is_empty() {
                                self.base.translation_map.shift_insert(
                                    2,
                                    map_name_comment.clone(),
                                    String::new(),
                                );
                            }
                        } else {
                            self.base.translation_map.swap_indices(
                                2,
                                map_name_comment_index as usize,
                            );
                        }

                        if map_display_name_comment_index == -1 {
                            if !map_display_name_comment.is_empty() {
                                self.base.translation_map.shift_insert(
                                    3,
                                    map_display_name_comment.clone(),
                                    String::new(),
                                );
                            }
                        } else {
                            self.base.translation_map.swap_indices(
                                3,
                                map_display_name_comment_index as usize,
                            );
                        }

                        if replace_map_name {
                            self.base.translation_map.insert(
                                map_name_comment.clone(),
                                String::new(),
                            );
                        }

                        if replace_map_display_name {
                            self.base.translation_map.insert(
                                map_display_name_comment.clone(),
                                String::new(),
                            );
                        }
                    }
                } else {
                    self.base
                        .translation_map
                        .insert(MAP_NUMBER_COMMENT.to_owned(), map_number);

                    self.base
                        .translation_map
                        .insert(ORDER_COMMENT.to_owned(), order_number);

                    if !map_name_comment.is_empty() {
                        self.base
                            .translation_map
                            .insert(map_name_comment.clone(), String::new());
                    }

                    if !map_display_name_comment.is_empty() {
                        self.base.translation_map.insert(
                            map_display_name_comment.clone(),
                            String::new(),
                        );
                    }
                }

                self.base
                    .translation_set
                    .insert(MAP_NUMBER_COMMENT.to_owned());

                self.base.translation_set.insert(ORDER_COMMENT.to_owned());

                if !map_name_comment.is_empty() {
                    self.base.translation_set.insert(map_name_comment);
                }

                if !map_display_name_comment.is_empty() {
                    self.base
                        .translation_set
                        .insert(map_display_name_comment.clone());
                }
            }

            let events: Box<dyn Iterator<Item = &mut Value>> =
                if self.base.engine_type.is_new() {
                    Box::new(
                        map_object[self.base.labels.events]
                            .as_array_mut()
                            .unwrap_log()
                            .iter_mut()
                            .skip(1),
                    )
                } else {
                    Box::new(
                        map_object[self.base.labels.events]
                            .as_object_mut()
                            .unwrap_log()
                            .iter_mut()
                            .map(|(_, value)| value),
                    )
                };

            for event in events {
                let Some(pages) = event[self.base.labels.pages].as_array_mut()
                else {
                    continue;
                };

                for page in pages {
                    let list =
                        page[self.base.labels.list].as_array_mut().unwrap_log();

                    self.base.process_list(list);
                }
            }

            if self.base.mode.is_write() {
                self.base.write_output(map_object, &filename);
            }

            if self.base.mode.is_purge() {
                self.base.purge_translation();
            }

            let mut iter: Box<dyn Iterator<Item = (String, String)>> =
                Box::new(self.base.translation_map.drain(..));

            if self.base.mode.is_purge() {
                iter = Box::new(
                    iter.enumerate()
                        .filter(|(i, _)| !self.base.purge_indices.remove(i))
                        .map(|(_, kv)| kv),
                )
            }

            self.base.translation_set.clear();

            if !self.base.mode.is_write() {
                self.base.translation_duplicate_map.extend(iter);
            }

            if self.base.logging {
                let msg = if self.base.mode.is_read() {
                    PARSED_FILE_MSG
                } else if self.base.mode.is_write() {
                    WROTE_FILE_MSG
                } else {
                    PURGED_FILE_MSG
                };

                println!("{msg} {filename}");
            }
        }

        self.base
            .translation_duplicate_map
            .extend(self.base.translation_map.drain(..));
    }

    #[inline]
    pub fn process(mut self) {
        self.base.text_file_path = self.base.translation_path.join("maps.txt");

        if self.base.mode.is_read()
            && self.base.read_mode.is_default()
            && self.base.text_file_path.exists()
        {
            println!("maps.txt {FILE_ALREADY_EXISTS_MSG}");
            return;
        }

        if self.base.read_mode.is_append() && !self.base.text_file_path.exists()
        {
            return;
        }

        let map_entries =
            read_dir(self.source_path)
                .unwrap_log()
                .filter_map(move |entry| {
                    Self::filter_maps(entry, self.base.engine_type)
                });

        self.process_maps(map_entries);

        if self.base.mode.is_write() || self.base.stat {
            return;
        }

        self.base.write_output(Value::new(), "");
    }
}

pub struct OtherBase<'a> {
    pub base: Base<'a>,

    // Private
    source_path: &'a Path,
    entry_json: Value,
}

impl<'a> OtherBase<'a> {
    pub fn new(
        source_path: &'a Path,
        translation_path: &'a Path,
        output_path: &'a Path,
        engine_type: EngineType,
        mode: ProcessingMode,
    ) -> Self {
        let base = Base {
            engine_type,
            romanize: false,
            logging: false,
            trim: false,
            read_mode: ReadMode::Default,
            ignore: false,
            sort: false,
            stat: false,
            leave_filled: false,
            purge_empty: false,
            create_ignore: false,
            ignore_map: Box::leak(Box::new(IgnoreMap::default())),
            ignore_entry: Box::leak(Box::new(IgnoreEntry::default())),
            stat_vec: Box::leak(Box::new(StatVec::default())),
            translation_set: TranslationSet::default(),
            translation_map: Box::leak(Box::new(TranslationMap::default())),
            translation_duplicate_map: TranslationDuplicateMap::default(),
            purge_indices: PurgeIndices::default(),
            mode,
            labels: Labels::new(engine_type),
            game_type: GameType::None,
            file_type: RPGMFileType::Invalid,
            output_path,
            text_file_path: PathBuf::new(),
            translation_path,
        };

        Self {
            // Base
            base,

            // Private
            source_path,
            entry_json: Value::new(),
        }
    }

    #[inline]
    pub fn process_variable_termina(
        &mut self,
        variable_type: Variable,
        mut variable_text: String,
        note_text: Option<&str>,
    ) -> Option<String> {
        if variable_text.starts_with("///") || variable_text.contains("---") {
            return None;
        }

        match variable_type {
            Variable::Description => {
                if let Some(note) = note_text {
                    let mut note_is_continuation: bool = false;

                    if note.starts_with("flesh puppetry") {
                        let mut note_chars: Chars = note.chars();

                        let first_char = note_chars.next();
                        let second_char = note_chars.next();

                        if let Some((note_first_char, note_second_char)) =
                            first_char.zip(second_char)
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
                        let mut note_string: String = String::from(note);

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
                            note_string = note.to_owned();
                        } else if !self.base.mode.is_write() {
                            return None;
                        }

                        if note_string.is_empty() {
                            if !self.base.mode.is_write() {
                                return None;
                            }
                        } else {
                            variable_text = variable_text + &note_string;
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
                        if variable_text.contains(string) {
                            return Some(variable_text.replace(
                                string,
                                &self.base.translation_map[string],
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
                    .contains(&variable_text.as_str())
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
                    .contains(&variable_text.as_str())
                    {
                        return None;
                    }
                }
                RPGMFileType::Enemies => {
                    if ["Spank Tank", "giant", "test"]
                        .contains(&variable_text.as_str())
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
                RPGMFileType::Weapons => {
                    if variable_text == "makeshift2" {
                        return None;
                    }
                }
                _ => {}
            },
        }

        Some(variable_text)
    }

    #[allow(clippy::collapsible_match, clippy::single_match)]
    #[inline(always)]
    pub fn process_variable(
        &mut self,
        mut variable_text: String,
        note_text: Option<&str>,
        variable_type: Variable,
    ) -> Option<String> {
        if string_is_only_symbols(&variable_text) {
            return None;
        }

        if !self.base.engine_type.is_new() {
            if variable_text.lines().all(|line: &str| {
                line.is_empty()
                    || IS_INVALID_MULTILINE_VARIABLE_RE.is_match(line)
            }) || IS_INVALID_VARIABLE_RE.is_match(&variable_text)
            {
                return None;
            }

            variable_text = variable_text.replace("\r\n", "\n");
        }

        let remaining_strings: SmallVec<[(String, bool); 4]> =
            SmallVec::with_capacity(4);

        match self.base.game_type {
            GameType::Termina => {
                if let Some(text) = self.process_variable_termina(
                    variable_type,
                    variable_text,
                    note_text,
                ) {
                    variable_text = text
                } else {
                    return None;
                }
            }
            _ => {} // custom processing for other games
        }

        if self.base.romanize {
            variable_text = romanize_string(variable_text);
        }

        if !self.base.mode.is_write() {
            return Some(variable_text);
        }

        let translated: Option<String> = self
            .base
            .translation_map
            .get(&variable_text)
            .map(|translated| {
                let mut result: String = translated.to_owned();

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

                #[allow(clippy::collapsible_if, clippy::collapsible_match)]
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

    fn process_entry_object(&mut self, entry_object: &mut Value) {
        if self.base.mode.is_read() {
            let commonevent_name: &str =
                entry_object[&self.base.labels.name].as_str().unwrap_log();

            if !commonevent_name.is_empty() {
                let event_name_comment: String = format!("{EVENT_NAME_COMMENT_PREFIX}{commonevent_name}{COMMENT_SUFFIX}");
                self.base.translation_set.insert(event_name_comment.clone());

                if !self.base.translation_map.contains_key(&event_name_comment)
                {
                    self.base.insert_not_present(event_name_comment);
                }
            }
        }

        let pages_count: usize = if self.base.file_type.is_troops() {
            entry_object[self.base.labels.pages]
                .as_array()
                .unwrap_log()
                .len()
        } else {
            1
        };

        for i in 0..pages_count {
            let list = if pages_count != 1 {
                &mut entry_object[self.base.labels.pages][i]
                    [self.base.labels.list]
            } else {
                &mut entry_object[self.base.labels.list]
            };

            let Some(list_array) = list.as_array_mut() else {
                continue;
            };

            self.base.process_list(list_array);
        }
    }

    fn process_entry_array(&mut self, entry_object: &mut Value) -> bool {
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
            let value: &Value = &entry_object[variable_label];

            let Some(mut string) = extract_string(value, self.base.trim, true)
            else {
                continue;
            };

            if self.base.mode.is_write() && self.base.romanize {
                string = romanize_string(string);
            }

            if !self.base.mode.is_read() {
                string = string
                    .lines()
                    .map(str::trim)
                    .collect::<Vec<_>>()
                    .join("\n");
            }

            let note_text: Option<&str> = if self.base.game_type.is_termina()
                && variable_type.is_description()
            {
                entry_object[self.base.labels.note].as_str()
            } else {
                None
            };

            let Some(parsed) =
                self.process_variable(string, note_text, variable_type)
            else {
                if variable_type.is_name() {
                    return true;
                }

                continue;
            };

            if self.base.mode.is_write() {
                entry_object[variable_label] = Value::from(&parsed);
                continue;
            }

            if !self.base.mode.is_write() {
                let replaced: String = parsed
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
                    .unwrap_log()
                    .to_owned();

                if self.base.mode.is_read()
                    && self.base.ignore_entry.contains(&replaced)
                {
                    continue;
                }

                self.base.translation_set.insert(replaced);
            }

            let last: &str = unsafe { &mut *(self as *mut Self) }
                .base
                .translation_set
                .last()
                .unwrap_log()
                .as_str();

            if self.base.translation_map.contains_key(last) {
                if self.base.sort {
                    self.base.sort_present(last);
                }
            } else {
                self.base.insert_not_present(last.to_owned());
            }
        }

        false
    }

    #[inline]
    pub fn filter_other(
        entry: Result<DirEntry, std::io::Error>,
        engine_type: EngineType,
        game_type: GameType,
    ) -> Option<(String, PathBuf)> {
        let Ok(entry) = entry else {
            return None;
        };

        if !entry.file_type().unwrap_log().is_file() {
            return None;
        };

        let filename = entry.file_name();
        let filename_str: &str = filename.to_str().unwrap_log();

        let (basename, _) = filename_str.split_once('.').unwrap_log();
        let file_type = RPGMFileType::from_filename(basename);

        if filename_str.ends_with(get_engine_extension(engine_type))
            && file_type.is_other()
        {
            if game_type.is_termina() && file_type.is_states() {
                return None;
            }

            return Some((filename_str.to_owned(), entry.path()));
        }

        None
    }

    pub fn process(mut self) {
        let entries =
            read_dir(self.source_path)
                .unwrap_log()
                .filter_map(move |entry| {
                    Self::filter_other(
                        entry,
                        self.base.engine_type,
                        self.base.game_type,
                    )
                });

        for (filename, path) in entries {
            let basename =
                filename.rsplit_once('.').unwrap_log().0.to_lowercase();
            let text_file_name =
                PathBuf::from(filename.to_lowercase()).with_extension("txt");

            self.base.file_type = RPGMFileType::from_filename(&basename);
            self.base.text_file_path =
                self.base.translation_path.join(&text_file_name);

            if self.base.mode.is_read()
                && self.base.read_mode.is_default()
                && self.base.text_file_path.exists()
            {
                println!(
                    "{} {FILE_ALREADY_EXISTS_MSG}",
                    text_file_name.display()
                );
                continue;
            }

            let ret = self
                .base
                .parse_translation(text_file_name.to_str().unwrap_log(), None);

            if ret {
                continue;
            }

            if self.base.purge_empty {
                self.base.purge_translation();
                self.base.write_output(Value::new(), &filename);
                continue;
            }

            self.base.init_stat_and_ignore(&format!(
                "{FILE_ENTRY_PREFIX}{basename}{COMMENT_SUFFIX}"
            ));

            if !self.base.mode.is_write()
                && self.base.game_type.is_termina()
                && self.base.file_type.is_items()
            {
                self.base.translation_set.extend([
                    String::from("<Menu Category: Items>"),
                    String::from("<Menu Category: Food>"),
                    String::from("<Menu Category: Healing>"),
                    String::from("<Menu Category: Body bag>"),
                ]);
            }

            self.entry_json = self.base.parse_rpgm_file(&path);

            for entry_object in unsafe { &mut *(&mut self as *mut Self) }
                .entry_json
                .as_array_mut()
                .unwrap_log()
                .iter_mut()
                .skip(1)
            {
                if self.base.file_type.is_events()
                    || self.base.file_type.is_troops()
                {
                    self.process_entry_object(entry_object);
                } else {
                    let proceed = self.process_entry_array(entry_object);

                    if proceed {
                        continue;
                    }
                }
            }

            if self.base.mode.is_purge() {
                self.base.purge_translation();
            }

            if self.base.stat {
                continue;
            }

            self.base.write_output(self.entry_json.take(), filename);
            self.base.translation_map.clear();
            self.base.translation_set.clear();
        }
    }
}

pub struct SystemBase<'a> {
    // Base
    pub base: Base<'a>,

    // Private
    system_file_path: &'a Path,

    game_title: String,
    system_json: Value,
}

impl<'a> SystemBase<'a> {
    pub fn new(
        system_file_path: &'a Path,
        translation_path: &'a Path,
        output_path: &'a Path,
        engine_type: EngineType,
        mode: ProcessingMode,
    ) -> Self {
        let base = Base {
            // Base
            engine_type,

            romanize: false,
            logging: false,
            trim: false,

            read_mode: ReadMode::Default,
            ignore: false,
            sort: false,

            stat: false,
            leave_filled: false,
            purge_empty: false,
            create_ignore: false,

            ignore_map: Box::leak(Box::new(IgnoreMap::default())),
            ignore_entry: Box::leak(Box::new(IgnoreEntry::default())),
            stat_vec: Box::leak(Box::new(StatVec::default())),

            translation_set: TranslationSet::default(),
            translation_map: Box::leak(Box::new(TranslationMap::default())),
            translation_duplicate_map: TranslationDuplicateMap::default(),

            purge_indices: PurgeIndices::default(),

            mode,
            labels: Labels::new(engine_type),

            game_type: GameType::None,

            file_type: RPGMFileType::System,

            output_path,

            text_file_path: PathBuf::new(),
            translation_path,
        };

        Self {
            base,

            // Private
            system_file_path,

            game_title: String::new(),
            system_json: Value::new(),
        }
    }

    fn process_types(&mut self) {
        for label in [
            self.base.labels.armor_types,
            self.base.labels.elements,
            self.base.labels.skill_types,
            self.base.labels.weapon_types,
            self.base.labels.equip_types,
        ] {
            let Some(array) = unsafe { &mut *(self as *mut Self) }.system_json
                [label]
                .as_array_mut()
            else {
                continue;
            };

            for value in array {
                self.process_value(value);
            }
        }
    }

    fn process_terms(&mut self) {
        let Some(terms) = unsafe { &mut *(self as *mut Self) }.system_json
            [self.base.labels.terms]
            .as_object_mut()
        else {
            return;
        };

        for (key, value) in terms {
            if !self.base.engine_type.is_new() && !key.starts_with("__symbol__")
            {
                continue;
            }

            if key == "messages" {
                if let Some(messages) = value.as_object_mut() {
                    for (_, value) in messages {
                        self.process_value(value);
                    }
                }
            } else {
                if let Some(array) = value.as_array_mut() {
                    for value in array {
                        self.process_value(value);
                    }
                } else if (value.is_object()
                    && value["__type"].as_str() == Some("bytes"))
                    || value.is_str()
                {
                    self.process_value(value);
                }
            }
        }
    }

    fn process_value(&mut self, value: &mut Value) {
        let Some(mut extracted) = extract_string(value, self.base.trim, true)
        else {
            return;
        };

        if self.base.romanize {
            extracted = romanize_string(extracted);
        }

        if self.base.mode.is_read() {
            if self.base.ignore_entry.contains(&extracted) {
                return;
            }

            self.base.translation_set.insert(extracted);

            let extracted: &str = unsafe { &mut *(self as *mut Self) }
                .base
                .translation_set
                .last()
                .unwrap_log();

            if self.base.translation_map.contains_key(extracted) {
                if self.base.sort {
                    self.base.sort_present(extracted);
                }
            } else {
                self.base.insert_not_present(extracted.to_owned());
            }

            return;
        }

        if self.base.mode.is_write() {
            if let Some(translated) = self.base.translation_map.get(&extracted)
            {
                *value = make_string_value(
                    translated,
                    self.base.engine_type.is_new(),
                );
            }
            return;
        }

        if self.base.mode.is_purge() {
            self.base.translation_map.insert(extracted, String::new());
        }
    }

    fn process_currency_unit(&mut self) {
        if self.base.engine_type.is_new() {
            return;
        }

        unsafe { &mut *(self as *mut Self) }.process_value(
            &mut self.system_json[self.base.labels.currency_unit],
        );
    }

    fn process_game_title(&mut self) {
        if self.base.mode.is_write() {
            if !self.game_title.is_empty() {
                self.system_json[self.base.labels.game_title] =
                    Value::from(&self.game_title);
            }

            return;
        }

        let Some(mut game_title) = extract_string(
            &self.system_json[self.base.labels.game_title],
            self.base.trim,
            true,
        ) else {
            return;
        };

        if self.base.romanize {
            game_title = romanize_string(game_title);
        }

        if self.base.mode.is_purge() {
            self.base
                .translation_map
                .insert(game_title.clone(), String::new());
        }

        self.base.translation_set.insert(game_title);

        let game_title: &str = unsafe { &mut *(self as *mut Self) }
            .base
            .translation_set
            .last()
            .unwrap_log();

        if self.base.translation_map.contains_key(game_title) {
            if self.base.sort {
                self.base.sort_present(game_title);
            }
        } else {
            self.base.insert_not_present(game_title.to_owned());
        }
    }

    pub fn process(mut self) {
        self.base.text_file_path =
            self.base.translation_path.join("system.txt");

        if self.base.mode.is_read()
            && self.base.read_mode.is_default()
            && self.base.text_file_path.exists()
        {
            println!("system.txt {FILE_ALREADY_EXISTS_MSG}",);
            return;
        }

        let ret = self
            .base
            .parse_translation("system.txt", Some(&mut self.game_title));

        if ret {
            return;
        }

        self.base.init_stat_and_ignore(SYSTEM_ENTRY_STRING);

        if self.base.purge_empty {
            self.base.purge_translation();
            self.base.write_output(
                self.system_json.take(),
                self.system_file_path
                    .file_name()
                    .unwrap_log()
                    .to_str()
                    .unwrap_log(),
            );
            return;
        }

        self.system_json = self.base.parse_rpgm_file(self.system_file_path);

        self.process_types();
        self.process_terms();
        self.process_currency_unit();
        self.process_game_title();

        if self.base.mode.is_purge() {
            self.base.purge_translation();
        }

        if self.base.stat {
            return;
        }

        self.base.write_output(
            self.system_json.take(),
            self.system_file_path.file_name().unwrap_log(),
        );
    }
}

pub struct ScriptBase<'a> {
    pub base: Base<'a>,

    // Private
    scripts_file_path: &'a Path,

    scripts_array: Array,
    script_names: Vec<String>,
    scripts_content: Vec<String>,
}

impl<'a> ScriptBase<'a> {
    pub fn new(
        scripts_file_path: &'a Path,
        translation_path: &'a Path,
        output_path: &'a Path,
        mode: ProcessingMode,
    ) -> Self {
        let base = Base {
            engine_type: EngineType::VXAce,
            romanize: false,
            logging: false,
            trim: false,
            read_mode: ReadMode::Default,
            ignore: false,
            sort: false,
            stat: false,
            leave_filled: false,
            create_ignore: false,
            purge_empty: false,
            ignore_map: Box::leak(Box::new(IgnoreMap::default())),
            ignore_entry: Box::leak(Box::new(IgnoreEntry::default())),
            stat_vec: Box::leak(Box::new(StatVec::default())),
            translation_set: TranslationSet::default(),
            translation_map: Box::leak(Box::new(TranslationMap::default())),
            translation_duplicate_map: TranslationDuplicateMap::default(),
            purge_indices: PurgeIndices::default(),
            mode,
            labels: Labels::new(EngineType::VXAce),
            game_type: GameType::None,
            file_type: RPGMFileType::Scripts,
            output_path,
            text_file_path: PathBuf::new(),
            translation_path,
        };

        Self {
            // Base
            base,

            // Private
            scripts_file_path,
            scripts_array: Array::new(),
            script_names: Vec::new(),
            scripts_content: Vec::new(),
        }
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

    #[inline]
    pub fn extract_strings(
        ruby_code: &str,
        write: bool,
    ) -> (TranslationSet, Vec<std::ops::Range<usize>>) {
        let mut strings: TranslationSet = TranslationSet::default();
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
                } else if inside_string
                    && char == current_quote_type
                    && !Self::is_escaped(i, &line)
                {
                    let range: std::ops::Range<usize> =
                        string_start_index + 1..global_index + i;

                    let extracted_string: String = ruby_code[range.clone()]
                        .replace("\r\n", NEW_LINE)
                        .replace('\n', NEW_LINE);

                    if !extracted_string.is_empty()
                        && !strings.contains(&extracted_string)
                    {
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

    fn decode_scripts(&mut self) {
        self.scripts_content.reserve_exact(self.scripts_array.len());
        self.script_names.reserve_exact(self.scripts_array.len());

        for script in self.scripts_array.iter() {
            let script_name_data: Vec<u8> =
                from_value(&script[1]["data"]).unwrap_log();
            let script_data: Vec<u8> =
                from_value(&script[2]["data"]).unwrap_log();

            let mut script_decoded: Vec<u8> = Vec::new();

            ZlibDecoder::new(script_data.as_slice())
                .read_to_end(&mut script_decoded)
                .unwrap_log();

            for encoding in ENCODINGS {
                let (script_cow, _, had_errors) =
                    encoding.decode(&script_decoded);
                let (script_name_cow, _, _) =
                    encoding.decode(&script_name_data);

                if !had_errors {
                    self.scripts_content.push(script_cow.into_owned());
                    self.script_names.push(script_name_cow.into_owned());
                    break;
                }
            }
        }
    }

    pub fn process(mut self) {
        self.base.text_file_path =
            self.base.translation_path.join("scripts.txt");

        if self.base.mode.is_read()
            && self.base.read_mode.is_default()
            && self.base.text_file_path.exists()
        {
            println!("scripts.txt {FILE_ALREADY_EXISTS_MSG}");
            return;
        }

        self.scripts_array = self
            .base
            .parse_rpgm_file(self.scripts_file_path)
            .into_array()
            .unwrap_log();

        self.decode_scripts();

        let regexes: [Regex; 5] = [Regex::new(r"(Graphics|Data|Audio|Movies|System)\/.*\/?").unwrap_log(), Regex::new(r"r[xv]data2?$").unwrap_log(), Regex::new(r".*\(").unwrap_log(), Regex::new(r"^([d\d\&'a Path{&'a Path}+-]*|[d\&'a Path{&'a Path}+-]&*)$").unwrap_log(), Regex::new(r"^(Actor<id>|ExtraDropItem|EquipLearnSkill|GameOver|Iconset|Window|true|false|MActor%d|[wr]b|\\f|\\n|\[[A-Z]*\])$").unwrap_log()];

        let translation: String;
        let mut translation_lines = "".lines().peekable();

        if !self.base.mode.is_read() || self.base.read_mode.is_append() {
            translation =
                read_to_string(&self.base.text_file_path).unwrap_log();
            translation_lines = translation.lines().peekable();
        }

        let mut changed: bool = false;

        for ((script, script_name), mut code) in
            unsafe { &mut *(&mut self as *mut Self) }
                .scripts_array
                .iter_mut()
                .zip(take(&mut self.script_names))
                .zip(take(&mut self.scripts_content))
        {
            if self.base.read_mode.is_append() || !self.base.mode.is_read() {
                let proceed = self.base.get_next_translation_map(
                    &mut translation_lines,
                    SCRIPT_COMMENT_PREFIX,
                );

                if proceed {
                    continue;
                }
            }

            changed = true;

            if self.base.purge_empty {
                self.base.purge_translation();

                let translation_map_entries: Box<
                    dyn Iterator<Item = (String, String)>,
                > = Box::new(
                    self.base
                        .translation_map
                        .drain(..)
                        .enumerate()
                        .filter(|(i, _)| !self.base.purge_indices.remove(i))
                        .map(|(_, kv)| kv),
                );

                self.base
                    .translation_duplicate_map
                    .extend(translation_map_entries);
                self.base.translation_set.clear();
                continue;
            }

            let script_name_comment = format!(
                "{}{}{}",
                SCRIPT_COMMENT_PREFIX, script_name, COMMENT_SUFFIX
            );

            self.base.init_stat_and_ignore(
                &script_name_comment.replace(COMMENT_PREFIX, FILE_ENTRY_PREFIX),
            );

            self.base
                .translation_set
                .insert(script_name_comment.clone());
            self.base
                .translation_map
                .insert(script_name_comment, String::new());

            let (extracted_strings, ranges) =
                Self::extract_strings(&code, self.base.mode.is_write());

            if self.base.mode.is_write() {
                let mut buf: Vec<u8> = Vec::new();

                ZlibEncoder::new(&mut buf, Compression::new(6))
                    .write_all(code.as_bytes())
                    .unwrap_log();

                if let Some(obj) = script[2].as_object_mut() {
                    obj["data"] = Array::from(buf).into()
                }
            } else {
                self.base
                    .translation_map
                    .reserve_exact(extracted_strings.len());

                if self.base.mode.is_read() && self.base.read_mode.is_append() {
                    self.base
                        .translation_set
                        .reserve_exact(extracted_strings.len());
                }
            }

            if self.base.mode.is_write() {
                for (mut extracted, range) in extracted_strings
                    .into_iter()
                    .zip(ranges)
                    .filter(|(s, _)| !s.trim().is_empty())
                    .rev()
                {
                    if self.base.romanize {
                        extracted = romanize_string(extracted);
                    }

                    if let Some(translated) =
                        self.base.translation_map.get(&extracted)
                    {
                        code.replace_range(range, translated);
                    }
                }
            } else {
                for mut extracted in extracted_strings
                    .into_iter()
                    .filter(|s| !s.trim().is_empty())
                {
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

                    if self.base.romanize {
                        extracted = romanize_string(extracted);
                    }

                    if self.base.ignore
                        && self.base.ignore_entry.contains(&extracted)
                    {
                        continue;
                    }

                    self.base.translation_set.insert(extracted);

                    if self.base.mode.is_purge() {
                        continue;
                    }

                    let extracted = unsafe { &mut *(&mut self as *mut Self) }
                        .base
                        .translation_set
                        .last()
                        .unwrap_log();

                    if self.base.translation_map.contains_key(extracted) {
                        if self.base.sort {
                            self.base.sort_present(extracted);
                        }
                    } else {
                        self.base.insert_not_present(extracted.to_owned());
                    }
                }

                if self.base.mode.is_purge() {
                    self.base.purge_translation();
                }

                let mut translation_map_entries: Box<
                    dyn Iterator<Item = (String, String)>,
                > = Box::new(self.base.translation_map.drain(..));

                if self.base.mode.is_purge() {
                    translation_map_entries = Box::new(
                        translation_map_entries
                            .enumerate()
                            .filter(|(i, _)| !self.base.purge_indices.remove(i))
                            .map(|(_, kv)| kv),
                    );
                }

                self.base
                    .translation_duplicate_map
                    .extend(translation_map_entries);
                self.base.translation_set.clear();
            }
        }

        if !changed || self.base.stat {
            return;
        }

        self.base.write_output(
            self.scripts_array.into_value().take(),
            self.scripts_file_path.file_name().unwrap_log(),
        );
    }
}

pub struct PluginBase<'a> {
    pub base: Base<'a>,

    // Private
    plugins_file_path: &'a Path,
    plugins_array: Array,
}

impl<'a> PluginBase<'a> {
    pub fn new(
        plugins_file_path: &'a Path,
        translation_path: &'a Path,
        output_path: &'a Path,
        mode: ProcessingMode,
    ) -> Self {
        let base = Base {
            engine_type: EngineType::New,
            romanize: false,
            logging: false,
            trim: false,
            read_mode: ReadMode::Default,
            ignore: false,
            sort: false,
            stat: false,
            leave_filled: false,
            create_ignore: false,
            purge_empty: false,
            ignore_map: Box::leak(Box::new(IgnoreMap::default())),
            ignore_entry: Box::leak(Box::new(IgnoreEntry::default())),
            stat_vec: Box::leak(Box::new(StatVec::default())),
            translation_set: TranslationSet::default(),
            translation_map: Box::leak(Box::new(TranslationMap::default())),
            translation_duplicate_map: TranslationDuplicateMap::default(),
            purge_indices: PurgeIndices::default(),
            mode,
            labels: Labels::new(EngineType::New),
            game_type: GameType::None,
            file_type: RPGMFileType::Plugins,
            output_path,
            text_file_path: PathBuf::new(),
            translation_path,
        };

        Self {
            base,

            // Private
            plugins_file_path,
            plugins_array: Array::new(),
        }
    }

    pub fn parse_plugin(&mut self, key: Option<&str>, value: &mut Value) {
        let is_invalid_key = |key: &Option<&str>| {
            let Some(key_string) = key else {
                return false;
            };

            if key_string.starts_with("LATIN") {
                false
            } else {
                PLUGINS_REGEXPS.iter().any(|re| re.is_match(key_string))
            }
        };

        match value.get_type() {
            sonic_rs::JsonType::String => {
                if is_invalid_key(&key) {
                    return;
                }

                let value_string: &str = value.as_str().unwrap_log();

                if !(value_string.trim().is_empty()
                    || IS_ONLY_SYMBOLS_RE.is_match(value_string)
                    || ["true", "false", "none", "time", "off"]
                        .contains(&value_string)
                    || value_string.starts_with("this.")
                        && value_string
                            .chars()
                            .nth(5)
                            .is_some_and(|c: char| c.is_alphabetic())
                        && value_string.ends_with(')')
                    || value_string.starts_with("rgba"))
                    || key.is_some_and(|x| x.starts_with("LATIN"))
                {
                    let mut string: String =
                        value_string.replace('\n', NEW_LINE);

                    if self.base.romanize {
                        string = romanize_string(string);
                    }

                    if self.base.mode.is_write() {
                        if !self.base.translation_map.contains_key(&string) {
                            return;
                        }

                        if let Some(translated) =
                            self.base.translation_map.get(&string)
                        {
                            *value = translated.into();
                        }
                    } else {
                        if self.base.ignore
                            && self.base.ignore_entry.contains(&string)
                        {
                            return;
                        };

                        self.base.translation_set.insert(string);

                        let string: &str = unsafe { &mut *(self as *mut Self) }
                            .base
                            .translation_set
                            .last()
                            .unwrap_log();

                        if self.base.translation_map.contains_key(string) {
                            if self.base.sort {
                                self.base.sort_present(string);
                            }
                        } else {
                            self.base.insert_not_present(string.to_owned());
                        }
                    }
                }
            }
            sonic_rs::JsonType::Object => {
                for (key, value) in value.as_object_mut().unwrap_log() {
                    self.parse_plugin(Some(key), value);
                }
            }
            sonic_rs::JsonType::Array => {
                for value in value.as_array_mut().unwrap_log() {
                    self.parse_plugin(None, value);
                }
            }
            _ => {}
        }
    }

    pub fn process(mut self) {
        self.base.text_file_path =
            self.base.translation_path.join("plugins.txt");

        if self.base.mode.is_read()
            && self.base.read_mode.is_default()
            && self.base.text_file_path.exists()
        {
            println!("plugins.txt {FILE_ALREADY_EXISTS_MSG}");
            return;
        }

        let plugins_content: String =
            read_to_string(self.plugins_file_path).unwrap_log();

        let plugins_array: &str = plugins_content
            .split_once('=')
            .unwrap_log()
            .1
            .trim_end_matches([';', '\r', '\n']);

        self.plugins_array = from_str(plugins_array).unwrap_log();

        let translation;
        let mut translation_lines = "".lines().peekable();

        if self.base.mode.is_write() || self.base.read_mode.is_append() {
            translation =
                read_to_string(&self.base.text_file_path).unwrap_log();
            translation_lines = translation.lines().peekable();
        }

        for plugin_object in unsafe { &mut *(&mut self as *mut Self) }
            .plugins_array
            .iter_mut()
        {
            if self.base.mode.is_write() || self.base.read_mode.is_append() {
                let proceed = self.base.get_next_translation_map(
                    &mut translation_lines,
                    PLUGIN_COMMENT_PREFIX,
                );

                if proceed {
                    continue;
                }
            }

            let plugin_name = plugin_object["name"].as_str().unwrap_log();
            let plugin_name_comment = format!(
                "{}{}{}",
                PLUGIN_COMMENT_PREFIX, plugin_name, COMMENT_SUFFIX
            );

            self.base.init_stat_and_ignore(
                &plugin_name_comment.replace(COMMENT_PREFIX, FILE_ENTRY_PREFIX),
            );

            if self.base.purge_empty {
                self.base.purge_translation();

                let translation_map_entries = self
                    .base
                    .translation_map
                    .drain(..)
                    .enumerate()
                    .filter(|(i, _)| !self.base.purge_indices.remove(i))
                    .map(|(_, kv)| kv);

                self.base
                    .translation_duplicate_map
                    .extend(translation_map_entries);
                self.base.translation_set.clear();
                continue;
            }

            self.base
                .translation_set
                .insert(plugin_name_comment.clone());
            self.base
                .translation_map
                .insert(plugin_name_comment, String::new());

            self.parse_plugin(None, plugin_object);

            if self.base.mode.is_purge() {
                self.base.purge_translation();
            }

            let mut translation_map_entries: Box<
                dyn Iterator<Item = (String, String)>,
            > = Box::new(self.base.translation_map.drain(..));

            if self.base.mode.is_purge() {
                translation_map_entries = Box::new(
                    translation_map_entries
                        .enumerate()
                        .filter(|(i, _)| !self.base.purge_indices.remove(i))
                        .map(|(_, kv)| kv),
                );
            }

            self.base
                .translation_duplicate_map
                .extend(translation_map_entries);
            self.base.translation_set.clear();
        }

        if self.base.stat {
            return;
        }

        self.base.write_output(
            self.plugins_array.into_value().take(),
            self.plugins_file_path.file_name().unwrap_log(),
        );
    }
}
