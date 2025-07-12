use crate::{
    constants::{localization::*, *},
    functions::*,
    types::*,
};
#[cfg(feature = "log")]
use crate::{eprintln, println};
use flate2::{Compression, read::ZlibDecoder, write::ZlibEncoder};
use gxhash::GxBuildHasher;
use marshal_rs::{Value, ValueType, dump, load_binary, load_utf8};
use regex::Regex;
use serde_json::{from_str, to_string, to_vec};
use smallvec::SmallVec;
use std::{
    cell::LazyCell,
    fs::{DirEntry, read, read_dir, read_to_string, write},
    io::{Read, Write},
    mem::take,
    ops::Range,
    path::{Path, PathBuf},
};

macro_rules! mutable {
    ($var:expr, $t:ty) => {{
        #[allow(invalid_reference_casting)]
        unsafe {
            &mut *($var as *const $t as *mut $t)
        }
    }};
}

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
}

pub struct Base<'a> {
    pub engine_type: EngineType,
    pub game_type: GameType,
    mode: ProcessingMode,
    file_type: RPGMFileType,

    pub romanize: bool,
    pub logging: bool,
    pub trim: bool,

    pub read_mode: ReadMode,
    // TODO: Seems like I fucked up and duplicate modes do the opposite what they should do
    pub duplicate_mode: DuplicateMode,
    pub ignore: bool,

    pub create_ignore: bool,
    pub ignore_map: &'a mut IgnoreMap,
    ignore_entry: &'static mut IgnoreEntry,

    lines: Lines,
    translation_map: &'static mut TranslationMap,
    translation_maps: IndexMapGx<String, TranslationMap>,
    translation_duplicate_map: TranslationDuplicateMap,
    lines_lengths: Vec<usize>,

    text_file_path: PathBuf,
    translation_path: &'a Path,
    output_path: &'a Path,

    labels: Labels,
}

impl<'a> Base<'a> {
    pub fn new(
        translation_path: &'a Path,
        output_path: &'a Path,
        engine_type: EngineType,
        file_type: RPGMFileType,
        mode: ProcessingMode,
    ) -> Self {
        Self {
            engine_type,
            game_type: GameType::None,
            mode,
            file_type,

            romanize: false,
            logging: false,
            trim: false,

            read_mode: ReadMode::Default,
            duplicate_mode: DuplicateMode::Allow,
            ignore: false,

            create_ignore: false,
            ignore_map: Box::leak(Box::default()),
            ignore_entry: Box::leak(Box::default()),

            lines: Lines::with_capacity_and_hasher(
                1024,
                GxBuildHasher::default(),
            ),
            translation_map: Box::leak(Box::new(
                TranslationMap::with_capacity_and_hasher(
                    1024,
                    GxBuildHasher::default(),
                ),
            )),
            translation_maps: IndexMapGx::default(),
            translation_duplicate_map: TranslationDuplicateMap::with_capacity(
                999,
            ),
            lines_lengths: Vec::new(),

            text_file_path: PathBuf::new(),
            translation_path,
            output_path,

            labels: Labels::new(engine_type),
        }
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

                let (_, mut actual_string) =
                    parameter.split_once('=').unwrap_log();
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
            self.get_key(parameter).cloned().map(|translation| {
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

    fn write_output(&mut self, json: Value, output_file: impl AsRef<Path>) {
        if self.mode.is_read() && self.duplicate_mode.is_remove() {
            if self.read_mode.is_append() {
                self.sync_map();
            } else {
                let mut lines_iter = take(&mut self.lines).into_iter();

                for (i, map) in
                    take(&mut self.translation_maps).into_values().enumerate()
                {
                    self.translation_duplicate_map.extend(map);

                    for _ in 0..self.lines_lengths[i] {
                        self.translation_duplicate_map.push((
                            lines_iter.next().unwrap_log(),
                            TranslationEntry::default(),
                        ));
                    }
                }
            }
        }

        let output_file_path = if self.mode.is_write() {
            self.output_path.join(&output_file)
        } else {
            take(&mut self.text_file_path)
        };

        let output_content = if self.mode.is_write() {
            if self.file_type.is_plugins() {
                format!(
                    "var $plugins =\n{}",
                    to_string(&serde_json::Value::from(json)).unwrap_log()
                )
                .into_bytes()
            } else {
                if self.engine_type.is_new() {
                    to_vec(&serde_json::Value::from(json)).unwrap_log()
                } else {
                    dump(
                        json,
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

        write(&output_file_path, output_content).unwrap_log();

        if self.logging {
            println!(
                "{msg} {file}",
                msg = if self.mode.is_read() {
                    PARSED_FILE_MSG
                } else if self.mode.is_write() {
                    WROTE_FILE_MSG
                } else {
                    PURGED_FILE_MSG
                },
                file = output_file.as_ref().display()
            );
        }

        self.lines.clear();
        self.translation_duplicate_map.clear();
    }

    fn process_param(
        &mut self,
        value: &mut Value,
        code: Code,
        parameter: &str,
    ) {
        let mut parameter = parameter.to_string();

        if self.romanize {
            parameter = Self::romanize_string(&parameter);
        }

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

                list[*dialogue_line_indices.last().unwrap_log()]
                    [self.labels.parameters][0] = Value::string(remaining);
            }
        } else {
            self.process_param(&mut Value::default(), Code::Dialogue, &joined);
        }
    }

    fn process_list(&mut self, list: &mut Vec<Value>) {
        let mut in_sequence = false;

        let mut dialogue_lines: SmallVec<[String; 4]> =
            SmallVec::with_capacity(4);
        let mut dialogue_line_indices: SmallVec<[usize; 4]> =
            SmallVec::with_capacity(4);

        let mut write_string_literally = self.engine_type.is_new();

        for (item_idx, item) in
            mutable!(list, Vec<Value>).iter_mut().enumerate()
        {
            let code: Code =
                Code::from(item[self.labels.code].as_int().unwrap_log() as u16);

            let code: Code =
                if code.is_dialogue_start() && !self.engine_type.is_xp() {
                    Code::Bad
                } else {
                    code
                };

            if self.mode.is_write() && !self.engine_type.is_new() {
                let parameters =
                    item[self.labels.parameters].as_array().unwrap_log();

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

            let parameters =
                item[self.labels.parameters].as_array_mut().unwrap_log();

            if parameters.is_empty() {
                continue;
            }

            let value_index: usize = if code.is_any_misc() || code.is_choice() {
                1
            } else {
                0
            };

            let value = &mut parameters[value_index];

            if code.is_choice_array() {
                for value in value.as_array_mut().unwrap_log() {
                    let Some(string) = mutable!(self, Self)
                        .extract_string(mutable!(value, Value), true)
                    else {
                        continue;
                    };

                    self.process_param(value, code, string);
                }
            } else {
                let parameter_string = mutable!(self, Self)
                    .extract_string(mutable!(value, Value), false)
                    .unwrap_log();

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

    fn get_ignore_entry(&mut self, entry_name: &str) {
        if self.ignore || self.create_ignore {
            let mut entry_name: &str =
                &format!("{file}: {entry_name}", file = self.file_type);

            if self.ignore && self.duplicate_mode.is_remove() {
                entry_name = &entry_name[..entry_name.find(':').unwrap_log()];
            }

            self.ignore_entry = mutable!(
                self.ignore_map
                    .entry(format!(
                        "{IGNORE_ENTRY_COMMENT}{SEPARATOR}{entry_name}"
                    ))
                    .or_default(),
                IgnoreEntry
            );
        }
    }

    fn parse_rpgm_file(&self, path: impl AsRef<Path>) -> Value {
        if self.engine_type.is_new() {
            Value::from(
                from_str::<serde_json::Value>(
                    &read_to_string_without_bom(path).unwrap_log(),
                )
                .unwrap_log(),
            )
        } else {
            let content = read(path).unwrap_log();
            if self.file_type.is_scripts() {
                load_binary(&content, INSTANCE_VAR_PREFIX).unwrap_log()
            } else {
                load_utf8(&content, INSTANCE_VAR_PREFIX).unwrap_log()
            }
        }
    }

    fn get_translation_maps(&mut self) {
        if !self.mode.is_read() || self.read_mode.is_append() {
            self.translation_maps.clear();

            let translation = read_to_string(&self.text_file_path).unwrap_log();
            let mut translation_lines =
                translation.lines().enumerate().peekable();

            let map_start_comment_prefix = if self.file_type.is_map() {
                MAP_ID_COMMENT
            } else if self.file_type.is_other() {
                if self.game_type.is_termina() && self.file_type.is_items() {
                    for _ in 0..4 {
                        let (_, item_category_line) =
                            translation_lines.next().unwrap_log();

                        if item_category_line.starts_with("<Menu Category") {
                            let (source, translation) = item_category_line
                                .split_once(SEPARATOR)
                                .unwrap_log();

                            self.translation_map
                                .insert(source.into(), translation.into());
                        } else {
                            panic!(
                                "items.txt in Fear & Hunger 2: Termina should start with 4 `Menu Category` entries."
                            )
                        }
                    }

                    self.translation_maps.insert(
                        ADDITIONAL_HASHMAP_LABEL.into(),
                        take(self.translation_map),
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

            let mut comments = Vec::with_capacity(4);

            loop {
                let Some((_, next)) = translation_lines.next() else {
                    return;
                };

                // Push the first line in iterator to comments
                // If it's not a comment, then something is wrong.
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
                        println!(
                            "{COULD_NOT_SPLIT_LINE_MSG}\n{AT_POSITION_MSG}: {i}\n{IN_FILE_MSG}: {file}",
                            file = self.file_type.to_string().to_lowercase()
                        );
                        comments.clear();
                        continue;
                    }

                    let mut source: String =
                        unsafe { split.first().unwrap_unchecked() }.to_string();
                    let mut translation: String = split
                        .into_iter()
                        .skip(1)
                        .rfind(|x| !x.is_empty())
                        .unwrap_or_default()
                        .into();

                    if self.trim {
                        source = source.trim_replace();
                        translation = translation.trim_replace();
                    }

                    if self.mode.is_write() {
                        if translation.is_empty() {
                            continue;
                        }

                        source = source.replace(NEW_LINE, "\n");
                        translation = translation.replace(NEW_LINE, "\n");
                    }

                    self.translation_map.insert(
                        source,
                        TranslationEntry::new(translation, take(&mut comments)),
                    );
                }

                if self.translation_map.is_empty() {
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
                    comments[0]
                        .rsplit_once(SEPARATOR)
                        .unwrap_log()
                        .1
                        .to_string(),
                    take(self.translation_map),
                );
            }
        }
    }

    fn get_translation_map(&mut self, entry_name: &str) {
        self.translation_map = mutable!(self, Self)
            .translation_maps
            .entry(entry_name.into())
            .or_default();
    }

    fn make_string_value(string: &str, literal: bool) -> Value {
        if literal {
            Value::string(string)
        } else {
            Value::bytes(string.as_bytes())
        }
    }

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
        } else {
            None
        }
    }

    fn extract_string(
        &'a self,
        value: &'a Value,
        ret: bool,
    ) -> Option<&'a str> {
        let string = value.as_str().unwrap_or_else(|| {
            std::str::from_utf8(value.as_byte_vec().unwrap_or_default())
                .unwrap_log()
        });

        let trimmed = string.trim();

        if trimmed.is_empty() && ret {
            return None;
        }

        Some(if self.trim { trimmed } else { string })
    }

    fn contains_key(&self, key: &str) -> bool {
        let contains = self.translation_map.contains_key(key);

        if contains {
            return contains;
        }

        if self.duplicate_mode.is_remove() {
            for translation_map in self.translation_maps.values() {
                let option = translation_map.contains_key(key);

                if option {
                    return option;
                }
            }
        }

        false
    }

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

    fn sync_map(&mut self) {
        self.translation_duplicate_map
            .reserve(self.lines.capacity());

        let push_comments =
            |map: &TranslationMap, out: &mut TranslationDuplicateMap| {
                if map.first().is_some_and(|x| x.0.is_empty()) {
                    out.extend(
                        map.first()
                            .unwrap_log()
                            .1
                            .parts()
                            .1
                            .to_vec()
                            .into_iter()
                            .map(|k| (String::new(), k.into())),
                    );
                } else {
                    for (k, v) in map
                        .iter()
                        .take_while(|(k, _)| k.starts_with(COMMENT_PREFIX))
                    {
                        out.push((k.clone(), v.clone()));
                    }
                }
            };

        if self.duplicate_mode.is_allow() {
            push_comments(
                self.translation_map,
                &mut self.translation_duplicate_map,
            );

            for key in take(&mut self.lines) {
                let val =
                    self.translation_map.swap_remove(&key).unwrap_or_default();
                self.translation_duplicate_map.push((key, val));
            }
        } else {
            let mut map_index: isize = -1;

            for key in take(&mut self.lines) {
                let mut val = TranslationEntry::default();

                for (i, map) in self.translation_maps.values_mut().enumerate() {
                    if let Some(entry) = map.swap_remove(&key) {
                        while i as isize > map_index {
                            map_index += 1;
                            push_comments(
                                &self.translation_maps[map_index as usize],
                                &mut self.translation_duplicate_map,
                            );
                        }
                        val = entry;
                        break;
                    }
                }

                self.translation_duplicate_map.push((key, val));
            }

            if (map_index as usize) < self.translation_maps.len() - 1 {
                if let Some((_name, last_map)) = self.translation_maps.last() {
                    push_comments(
                        last_map,
                        &mut self.translation_duplicate_map,
                    );
                }
            }
        }
    }

    fn purge_empty(&mut self) {
        let len_limit = if self.file_type.is_system() {
            self.translation_map.len() - 1
        } else {
            self.translation_map.len()
        };

        self.translation_duplicate_map.extend(
            self.translation_map.drain(..).enumerate().map(
                |(i, (mut k, v))| {
                    if i < len_limit
                        && !k.starts_with(COMMENT_PREFIX)
                        && v.is_empty()
                    {
                        let moved = take(&mut k);
                        if !moved.is_empty() {
                            self.ignore_entry.insert(moved);
                        }
                    }

                    (k, v)
                },
            ),
        );
    }

    fn extend_translation_map(&mut self) {
        if self.duplicate_mode.is_remove() {
            return;
        }

        if self.read_mode.is_append() {
            self.sync_map();
        } else {
            self.translation_duplicate_map.extend(
                self.translation_map.drain(..).chain(
                    take(&mut self.lines)
                        .into_iter()
                        .map(|k| (k, TranslationEntry::default())),
                ),
            );
        }
    }

    fn should_abort_read(&self) -> bool {
        let should_abort = self.mode.is_read()
            && self.read_mode.is_default()
            && self.text_file_path.exists();

        if should_abort {
            println!(
                "{filename}.txt {FILE_ALREADY_EXISTS_MSG}",
                filename = self.file_type.to_string().to_lowercase()
            );
        }

        should_abort
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

pub struct MapBase<'a> {
    pub base: Base<'a>,
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
        Self {
            base: Base::new(
                translation_path,
                output_path,
                engine_type,
                RPGMFileType::Map,
                mode,
            ),
            source_path,
        }
    }

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
        let filename_str = filename.to_str().unwrap_log();

        if filename_str.starts_with("Map")
            && (*filename_str.as_bytes().get(3).unwrap_log() as char)
                .is_ascii_digit()
            && filename_str.ends_with(get_engine_extension(engine_type))
        {
            return Some((filename_str.into(), entry.path()));
        }

        None
    }

    fn parse_map_id(string: &str) -> i32 {
        string[3..=5].parse::<i32>().unwrap_log()
    }

    fn get_map_order(&self, mapinfos: &Value, map_id: i32) -> String {
        if self.base.engine_type.is_new() {
            &mapinfos[map_id as usize]["order"]
        } else {
            &mapinfos[Value::int(map_id)]["order"]
        }
        .as_int()
        .unwrap_or_default()
        .to_string()
    }

    fn get_map_name(&self, mapinfos: &'a Value, map_id: i32) -> &'a str {
        if self.base.engine_type.is_new() {
            &mapinfos[map_id as usize]["name"]
        } else {
            &mapinfos[Value::int(map_id)]["name"]
        }
        .as_str()
        .unwrap_or_default()
    }

    fn get_display_name(&self, map_object: &Value) -> String {
        if let Some(display_name) =
            map_object.get(self.base.labels.display_name)
        {
            display_name
                .as_str()
                .map(|s| {
                    if self.base.romanize {
                        Base::romanize_string(s)
                    } else {
                        s.to_string()
                    }
                })
                .unwrap_or_default()
        } else {
            String::new()
        }
    }

    fn process_maps(&mut self) {
        let engine_type = self.base.engine_type;
        let map_entries = read_dir(self.source_path)
            .unwrap_log()
            .filter_map(|entry| Self::filter_maps(entry, engine_type));
        let mapinfos = self.base.parse_rpgm_file(self.source_path.join(
            format!("MapInfos.{}", get_engine_extension(self.base.engine_type)),
        ));
        self.base.get_translation_maps();

        if self.base.duplicate_mode.is_remove()
            && !self.base.read_mode.is_append()
        {
            self.base.lines_lengths.reserve_exact(999);
        }

        for (filename, path) in map_entries {
            let map_id = Self::parse_map_id(&filename);
            self.base.get_translation_map(&map_id.to_string());
            self.base.get_ignore_entry(&map_id.to_string());

            if self.base.mode.is_purge() {
                self.base.purge_empty();
            } else {
                let mut map_object = self.base.parse_rpgm_file(path);
                let display_name = self.get_display_name(&map_object);
                let display_name_comment = format!(
                    "{MAP_DISPLAY_NAME_COMMENT_PREFIX}{display_name}{COMMENT_SUFFIX}"
                );

                if self.base.mode.is_read() {
                    let mut comments_inserted = true;

                    let order_number = self.get_map_order(&mapinfos, map_id);
                    let map_name = self.get_map_name(&mapinfos, map_id);

                    if self.base.read_mode.is_append() {
                        if self.base.translation_map.first_mut().is_some() {
                            let start_comments =
                                self.base.translation_map.comments_mut();
                            let mut slots = vec![String::new(); 4];

                            for comment in start_comments.iter() {
                                if comment.starts_with(MAP_ID_COMMENT) {
                                    slots[0] = comment.clone();
                                } else if comment.starts_with(MAP_ORDER_COMMENT)
                                {
                                    slots[1] = comment.clone();
                                } else if comment.starts_with(MAP_NAME_COMMENT)
                                {
                                    slots[2] = comment.clone();
                                } else if comment.starts_with(
                                    MAP_DISPLAY_NAME_COMMENT_PREFIX,
                                ) {
                                    slots[3] = comment.clone();
                                }
                            }

                            *start_comments = slots;

                            let map_name_start_comment = &mut start_comments[2];
                            *map_name_start_comment = format!(
                                "{MAP_NAME_COMMENT}{SEPARATOR}{map_name}"
                            );

                            let display_name_start_comment =
                                &mut start_comments[3];

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
                            )
                        } else {
                            comments_inserted = false;
                        }
                    } else {
                        comments_inserted = false;
                    }

                    if !comments_inserted {
                        self.base.translation_map.extend([
                            (MAP_ID_COMMENT.into(), map_id.to_string().into()),
                            (MAP_ORDER_COMMENT.into(), order_number.into()),
                            (MAP_NAME_COMMENT.into(), map_name.into()),
                            (display_name_comment, TranslationEntry::default()),
                        ]);
                    }
                } else if !display_name.is_empty() {
                    let display_name_comment_line =
                        &self.base.translation_map.comments()[3];
                    let (_, translation) = display_name_comment_line
                        .split_once(SEPARATOR)
                        .unwrap_log();
                    map_object[self.base.labels.display_name] =
                        Value::string(translation);
                }

                let events = if self.base.engine_type.is_new() {
                    EventIterator::New(
                        map_object[self.base.labels.events]
                            .as_array_mut()
                            .unwrap_log()
                            .iter_mut()
                            .skip(1),
                    )
                } else {
                    EventIterator::Old(
                        map_object[self.base.labels.events]
                            .as_hashmap_mut()
                            .unwrap_log()
                            .values_mut(),
                    )
                };

                for event in events {
                    // TODO: Parse event name
                    if event.is_null() {
                        continue;
                    }

                    let Some(pages) =
                        event[self.base.labels.pages].as_array_mut()
                    else {
                        continue;
                    };

                    for page in pages {
                        self.base.process_list(
                            page[self.base.labels.list]
                                .as_array_mut()
                                .unwrap_log(),
                        );
                    }
                }

                if self.base.mode.is_write() {
                    self.base.write_output(map_object, &filename);
                } else {
                    if self.base.duplicate_mode.is_remove()
                        && !self.base.read_mode.is_append()
                    {
                        self.base.lines_lengths.push(
                            self.base.lines.len()
                                - self.base.lines_lengths.iter().sum::<usize>(),
                        );
                    }

                    self.base.extend_translation_map();
                    if self.base.logging {
                        println!("{PARSED_FILE_MSG} {filename}");
                    }
                }
            }
        }
    }

    pub fn process(mut self) {
        self.base.text_file_path = self.base.translation_path.join("maps.txt");

        if self.base.should_abort_read() {
            return;
        }

        self.process_maps();

        if !self.base.mode.is_write() {
            self.base.write_output(Value::default(), "");
        }
    }
}

pub struct OtherBase<'a> {
    pub base: Base<'a>,
    source_path: &'a Path,
}

impl<'a> OtherBase<'a> {
    pub fn new(
        source_path: &'a Path,
        translation_path: &'a Path,
        output_path: &'a Path,
        engine_type: EngineType,
        mode: ProcessingMode,
    ) -> Self {
        Self {
            base: Base::new(
                translation_path,
                output_path,
                engine_type,
                RPGMFileType::Invalid,
                mode,
            ),
            source_path,
        }
    }

    fn process_variable_termina(
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
    fn process_variable(
        &mut self,
        variable_text: &str,
        note_text: Option<&str>,
        variable_type: Variable,
    ) -> Option<String> {
        if Base::string_is_only_symbols(variable_text) {
            return None;
        }

        let mut variable_text = variable_text.to_string();

        if !self.base.engine_type.is_new() {
            if variable_text.lines().all(|line| {
                line.is_empty()
                    || IS_INVALID_MULTILINE_VARIABLE_RE
                        .with(|r| r.is_match(line))
            }) || IS_INVALID_VARIABLE_RE.with(|r| r.is_match(&variable_text))
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
                    if self.base.mode.is_write()
                        && self.base.file_type.is_items()
                        && variable_type.is_note()
                    {
                        return Some(text);
                    } else {
                        variable_text = text;
                    }
                } else {
                    return None;
                }
            }
            _ => {} // custom processing for other games
        }

        if self.base.romanize {
            variable_text = Base::romanize_string(&variable_text);
        }

        if !self.base.mode.is_write() {
            return Some(variable_text);
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
            for page in
                object[self.base.labels.pages].as_array_mut().unwrap_log()
            {
                if let Some(list_array) =
                    page[self.base.labels.list].as_array_mut()
                {
                    self.base.process_list(list_array);
                }
            }
        } else {
            self.base.process_list(
                object[self.base.labels.list].as_array_mut().unwrap_log(),
            );
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

            let Some(mut string) = self.base.extract_string(value, true) else {
                continue;
            };

            let romanized: String;

            if self.base.mode.is_write() && self.base.romanize {
                romanized = Base::romanize_string(string);
                string = &romanized;
            }

            let trimmed: String;

            if !self.base.mode.is_read() {
                trimmed = string
                    .lines()
                    .map(str::trim)
                    .collect::<Vec<_>>()
                    .join("\n");
                string = &trimmed;
            }

            let note_text = if self.base.game_type.is_termina()
                && variable_type.is_description()
            {
                array[self.base.labels.note].as_str()
            } else {
                None
            };

            let this = mutable!(self, Self);
            let Some(parsed) =
                this.process_variable(string, note_text, variable_type)
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
                let replaced = parsed
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
                    .into();

                self.base.insert_string(replaced);
            }
        }
    }

    fn filter_other(
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
        let filename_str = filename.to_str().unwrap_log();

        let (basename, _) = filename_str.split_once('.').unwrap_log();
        let file_type = RPGMFileType::from_filename(basename);

        if filename_str.ends_with(get_engine_extension(engine_type))
            && file_type.is_other()
        {
            if game_type.is_termina() && file_type.is_states() {
                return None;
            }

            return Some((filename_str.into(), entry.path()));
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

        if self.base.duplicate_mode.is_remove()
            && !self.base.read_mode.is_append()
        {
            self.base.lines_lengths.reserve_exact(999);
        }

        for (filename, path) in entries {
            let basename =
                filename.rsplit_once('.').unwrap_log().0.to_lowercase();
            let text_file_name =
                PathBuf::from(filename.to_lowercase()).with_extension("txt");

            self.base.file_type = RPGMFileType::from_filename(&basename);
            self.base.text_file_path =
                self.base.translation_path.join(&text_file_name);

            if self.base.should_abort_read() {
                continue;
            }

            if self.base.read_mode.is_append() || !self.base.mode.is_read() {
                // In previous iteration, translation_map was unsafely assigned to the map
                // located in translation_maps, and then translation_maps was cleared.
                // If we don't reassign back to valid reference, it'll cause heap corruption.
                self.base.translation_map = Box::leak(Box::default());
            }

            self.base.get_translation_maps();
            self.base.append_additional_data();

            let mut entry_json = self.base.parse_rpgm_file(&path);
            self.base.lines_lengths.clear();

            for object in
                entry_json.as_array_mut().unwrap_log().iter_mut().skip(1)
            {
                let event_name =
                    object[self.base.labels.name].as_str().unwrap_log();
                let event_id = object["id"].as_int().unwrap_log().to_string();

                self.base.get_translation_map(&event_id);
                self.base.get_ignore_entry(&event_id);

                if self.base.mode.is_purge() {
                    self.base.purge_empty();
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

                    self.base.extend_translation_map();
                }
            }

            self.base.write_output(entry_json, filename);
        }
    }
}

pub struct SystemBase<'a> {
    pub base: Base<'a>,
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
        Self {
            base: Base::new(
                translation_path,
                output_path,
                engine_type,
                RPGMFileType::System,
                mode,
            ),
            system_file_path,
            game_title: String::new(),
            system_json: Value::default(),
        }
    }

    fn process_terms(&mut self) {
        let Some(terms) = mutable!(self, Self).system_json
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
        let Some(mut extracted) = self.base.extract_string(value, true) else {
            return;
        };

        let romanized: String;

        if self.base.romanize {
            romanized = Base::romanize_string(extracted);
            extracted = &romanized;
        }

        if self.base.mode.is_read() {
            self.base.insert_string(extracted.into());
        } else if self.base.mode.is_write() {
            if let Some(translated) = self.base.get_key(extracted) {
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
                &mut self.system_json[self.base.labels.currency_unit],
            );
        }
    }

    fn process_game_title(&mut self) {
        if self.base.mode.is_write() {
            if !self.game_title.is_empty() {
                self.system_json[self.base.labels.game_title] =
                    Value::string(self.game_title.as_str());
            }
        } else if let Some(game_title_value) =
            self.system_json.get(self.base.labels.game_title)
        {
            let Some(mut game_title) =
                self.base.extract_string(game_title_value, true)
            else {
                return;
            };

            let romanized: String;

            if self.base.romanize {
                romanized = Base::romanize_string(game_title);
                game_title = &romanized;
            }

            self.base.insert_string(game_title.into());
        }
    }

    pub fn process(mut self) {
        self.base.text_file_path =
            self.base.translation_path.join("system.txt");

        if self.base.should_abort_read() {
            return;
        }

        self.base.get_translation_maps();
        self.system_json = self.base.parse_rpgm_file(self.system_file_path);

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
            self.base.get_translation_map(entry);
            self.base.get_ignore_entry(entry);

            if self.base.mode.is_purge() {
                self.base.purge_empty();
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

                    let Some(array) =
                        mutable!(&self, Self).system_json[label].as_array_mut()
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

                self.base.extend_translation_map();
            }
        }

        self.base.write_output(
            self.system_json.take(),
            self.system_file_path.file_name().unwrap_log(),
        );
    }
}

pub struct ScriptBase<'a> {
    pub base: Base<'a>,
    scripts_file_path: &'a Path,
    scripts_array: Vec<Value>,
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
        Self {
            base: Base::new(
                translation_path,
                output_path,
                EngineType::VXAce,
                RPGMFileType::Scripts,
                mode,
            ),
            scripts_file_path,
            scripts_array: Vec::new(),
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

    fn extract_strings(&self, ruby_code: &str) -> (Lines, Vec<Range<usize>>) {
        let mut strings: Lines = Lines::default();
        let mut ranges: Vec<Range<usize>> = Vec::new();
        let mut inside_string: bool = false;
        let mut inside_multiline_comment: bool = false;
        let mut string_start_index: usize = 0;
        let mut current_quote_type: char = '\0';
        let mut global_index: usize = 0;

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
                    let range: Range<usize> =
                        string_start_index + 1..global_index + i;

                    let extracted_string = ruby_code[range.clone()]
                        .replace("\r\n", NEW_LINE)
                        .replace('\n', NEW_LINE);

                    if !extracted_string.is_empty()
                        && !strings.contains(&extracted_string)
                    {
                        strings.insert(extracted_string);

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
            let script_name_data = script[1].as_byte_vec().unwrap_log();
            let script_data = script[2].as_byte_vec().unwrap_log();

            let mut decoded_script = Vec::new();

            ZlibDecoder::new(script_data)
                .read_to_end(&mut decoded_script)
                .unwrap_log();

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

    pub fn process(mut self) {
        self.base.text_file_path =
            self.base.translation_path.join("scripts.txt");

        if self.base.should_abort_read() {
            return;
        }

        self.scripts_array = self
            .base
            .parse_rpgm_file(self.scripts_file_path)
            .into_array()
            .unwrap_log();

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

        self.base.get_translation_maps();

        let mut changed = false;

        for (((script_id, script), script_name), mut code) in
            mutable!(&self, Self)
                .scripts_array
                .iter_mut()
                .enumerate()
                .zip(take(&mut self.script_names))
                .zip(take(&mut self.scripts_content))
        {
            changed = true;

            let script_id = script_id.to_string();
            self.base.get_translation_map(&script_id);
            self.base.get_ignore_entry(&script_id);

            if self.base.mode.is_purge() {
                self.base.purge_empty();
            } else {
                self.base.process_comments(&script_id, &script_name);
                let (extracted_strings, ranges) = self.extract_strings(&code);

                if self.base.mode.is_write() {
                    let mut buf = Vec::new();

                    ZlibEncoder::new(&mut buf, Compression::default())
                        .write_all(code.as_bytes())
                        .unwrap_log();

                    script[2] = Value::bytes(buf.as_slice());

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

                    self.base.extend_translation_map();
                }
            }
        }

        if !changed {
            return;
        }

        self.base.write_output(
            Value::array(self.scripts_array),
            self.scripts_file_path.file_name().unwrap_log(),
        );
    }
}

pub struct PluginBase<'a> {
    pub base: Base<'a>,
    plugins_file_path: &'a Path,
    plugins_array: Vec<Value>,
}

impl<'a> PluginBase<'a> {
    pub fn new(
        plugins_file_path: &'a Path,
        translation_path: &'a Path,
        output_path: &'a Path,
        mode: ProcessingMode,
    ) -> Self {
        Self {
            base: Base::new(
                translation_path,
                output_path,
                EngineType::New,
                RPGMFileType::Plugins,
                mode,
            ),
            plugins_file_path,
            plugins_array: Vec::new(),
        }
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
                    let mut string = value_string.replace('\n', NEW_LINE);

                    if self.base.romanize {
                        string = Base::romanize_string(&string);
                    }

                    if self.base.mode.is_write() {
                        if !self.base.contains_key(&string) {
                            return;
                        }

                        if let Some(translated) = self.base.get_key(&string) {
                            *value = Value::string(translated.to_string());
                        }
                    } else {
                        self.base.insert_string(string);
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

    pub fn process(mut self) {
        self.base.text_file_path =
            self.base.translation_path.join("plugins.txt");

        if self.base.should_abort_read() {
            return;
        }

        let plugins_content =
            read_to_string(self.plugins_file_path).unwrap_log();

        let plugins_array = plugins_content
            .split_once('=')
            .unwrap_log()
            .1
            .trim_end_matches([';', '\r', '\n']);

        self.plugins_array = Value::from(
            from_str::<serde_json::Value>(plugins_array).unwrap_log(),
        )
        .into_array()
        .unwrap_log();

        self.base.get_translation_maps();

        for (plugin_id, plugin_object) in
            mutable!(&self, Self).plugins_array.iter_mut().enumerate()
        {
            let plugin_name = plugin_object["name"].as_str().unwrap_log();
            let plugin_id = plugin_id.to_string();
            self.base.get_translation_map(&plugin_id);
            self.base.get_ignore_entry(&plugin_id);

            if self.base.mode.is_purge() {
                self.base.purge_empty();
            } else {
                self.base.process_comments(&plugin_id, plugin_name);
                self.parse_plugin(None, plugin_object);
                self.base.extend_translation_map();
            }
        }

        self.base.write_output(
            Value::array(self.plugins_array),
            self.plugins_file_path.file_name().unwrap_log(),
        );
    }
}
