use super::*;
use crate::{
    CommentPos, ProcessedData,
    types::{Error, Lines, RPGMFileType, Scripts},
};
use flate2::{Compression, read::ZlibDecoder, write::ZlibEncoder};
use marshal_rs::Value;
use regex::Regex;
use std::{
    borrow::Cow,
    io::{Read, Write},
    mem::take,
    ops::Range,
};

impl Base {
    /// Processes the RPG Maker scripts file content.
    ///
    /// # Parameters
    ///
    /// - `content` - Content of the file that's being processed.
    /// - `translation` - Contents of the translation file corresponding to the file. Isn't used with [`Mode::Read`]. Requires to be set with any other [`Mode`].
    ///
    /// # Returns
    ///
    /// - Nothing if `mode` is [`Mode::Write`] and no translation exists.
    /// - [`ProcessedData`], which contains RPG Maker data if `mode` is [`Mode::Write`] and translation data otherwise.
    /// - [`Error`], if unable to parse the content.
    ///
    /// # Errors
    ///
    /// - [`Error::MarshalLoad`] - if unable to load the Marshal data.
    /// - [`Error::JsonParse`] - if unable to parse the JSON data.
    /// - [`Error::NoTranslation`] - if mode is not [`Mode::Read`], and no translation was passed.
    ///
    /// # Panics
    ///
    /// May panic if passed content is not `Scripts`.
    ///
    /// # Example
    ///
    /// ```no_run
    /// use rvpacker_txt_rs_lib::{core::Base, Mode, EngineType, Error};
    /// use std::fs::read;
    ///
    /// fn main() -> Result<(), Box<dyn std::error::Error>> {
    ///     let mut base = Base::new(Mode::read(), EngineType::VXAce);
    ///
    ///     let script_file_content = read("C:/Game/Data/Scripts.rvdata2")?;
    ///     base.process_scripts(&script_file_content, None)?;
    ///     Ok(())
    /// }
    /// ```
    pub fn process_scripts(
        &mut self,
        content: &[u8],
        translation: Option<&str>,
    ) -> Result<Option<ProcessedData>, Error> {
        self.reset();
        self.file_type = RPGMFileType::Scripts;
        self.initialize_translation(translation)?;

        // SAFETY: Scripts are always array.
        let mut scripts_array = unsafe {
            parse_rpgm_file(content, self.engine_type, self.file_type)?
                .into_array()
                .unwrap_unchecked()
        };
        let mut scripts = Self::decode_scripts(&scripts_array);

        // SAFETY: These regexes are valid, 100% no shit.
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

        let mut processed = false;

        for (((script_id, script), script_name), mut code) in scripts_array
            .iter_mut()
            .enumerate()
            .zip(take(&mut scripts.names))
            .zip(take(&mut scripts.contents))
        {
            let id = script_id as u16 + 1;

            if self.get_translation_map(id).is_break() {
                if self.mode.is_purge() {
                    processed = true;
                }

                continue;
            }

            processed = true;

            self.update_metadata(
                id,
                Vec::from([(CommentPos::Name, script_name.as_str())]),
            );
            let (extracted_strings, ranges) = self.extract_strings(&code);

            if self.mode.is_write() {
                let mut code_changed = false;

                for (extracted, range) in extracted_strings
                    .into_iter()
                    .zip(ranges)
                    .filter(|(s, _)| !s.trim().is_empty())
                    .rev()
                {
                    if let Some(translated) = self.get_key(&extracted) {
                        code.replace_range(range, translated);
                        code_changed = true;
                    }
                }

                if code_changed {
                    let mut buf = Vec::with_capacity(code.len());

                    ZlibEncoder::new(&mut buf, Compression::default())
                        .write_all(code.as_bytes())
                        .unwrap();

                    script[2] = Value::bytes(&buf);
                }
            } else {
                for extracted in extracted_strings
                    .into_iter()
                    .filter(|s| !s.trim().is_empty())
                {
                    if string_is_only_symbols(&extracted)
                        || extracted.contains("@window")
                        || extracted.contains(r"\$game")
                        || extracted.starts_with(r"\\e")
                        || extracted.contains("ALPHAC")
                        || extracted.contains('_')
                        || regexes.iter().any(|re| re.is_match(&extracted))
                    {
                        continue;
                    }

                    self.insert_string(Cow::Owned(extracted));
                }

                self.flush_translation(id);
            }
        }

        if !processed {
            return Ok(None);
        }

        Ok(Some(self.finish(Value::array(scripts_array))))
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
        let mut strings = Lines::default();
        let mut ranges = Vec::new();
        let mut inside_string = false;
        let mut inside_multiline_comment = false;
        let mut string_start_index = 0;
        let mut current_quote_type = '\0';
        let mut global_index = 0;

        for line in ruby_code.split_inclusive('\n') {
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
                    let range = string_start_index + 1..global_index + i;

                    let extracted_string = ruby_code[range.clone()].normalize();

                    if !extracted_string.is_empty()
                        && !strings.contains(extracted_string.as_ref())
                    {
                        strings.insert(extracted_string.into_owned());

                        if self.mode.is_write() {
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

    /// Decodes an array of script entries into [`Scripts`] struct that holds `numbers`, `scripts` and `names` fields.
    ///
    /// # Parameters
    ///
    /// - `scripts_array`: Slice of script entries.
    ///
    /// # Returns
    ///
    /// A [`Scripts`] struct that holds `numbers`, `scripts` and `names` fields.
    ///
    /// # Panics
    ///
    /// May panic if decoder gets interrupted.
    ///
    #[must_use]
    pub fn decode_scripts(scripts_array: &[Value]) -> Scripts {
        let mut numbers = Vec::with_capacity(scripts_array.len());
        let mut contents = Vec::with_capacity(scripts_array.len());
        let mut names = Vec::with_capacity(scripts_array.len());

        for script in scripts_array {
            // SAFETY: Scripts always have a layout like this. `0` is magic number, `1` is name and `2` is actual script data.
            let script_number = if script[0].is_bytes() {
                unsafe {
                    str::from_utf8_unchecked(
                        script[0].as_byte_vec().unwrap_unchecked(),
                    )
                    .parse::<i32>()
                    .unwrap_unchecked()
                }
            } else if script[0].is_string() {
                unsafe {
                    script[0]
                        .as_str()
                        .unwrap_unchecked()
                        .parse::<i32>()
                        .unwrap_unchecked()
                }
            } else {
                unsafe { script[0].as_int().unwrap_unchecked() }
            };
            let script_name_data =
                unsafe { script[1].as_byte_vec().unwrap_unchecked() };
            let script_data =
                unsafe { script[2].as_byte_vec().unwrap_unchecked() };

            let mut decoded_script = Vec::with_capacity(script_data.len());
            ZlibDecoder::new(script_data)
                .read_to_end(&mut decoded_script)
                .unwrap();

            for encoding in [
                encoding_rs::UTF_8,
                encoding_rs::WINDOWS_1252,
                encoding_rs::WINDOWS_1251,
                encoding_rs::SHIFT_JIS,
                encoding_rs::GB18030,
            ] {
                let (content_cow, _, had_errors) =
                    encoding.decode(&decoded_script);
                let (name_cow, _, _) = encoding.decode(script_name_data);

                if !had_errors {
                    numbers.push(script_number);
                    contents.push(content_cow.into());
                    names.push(name_cow.into());
                    break;
                }
            }
        }

        Scripts::new(numbers, contents, names)
    }

    /// Encodes decoded [`Scripts`] struct back to [`Vec<Value>`].
    ///
    /// # Parameters
    ///
    /// - [`Scripts`] struct to encode.
    ///
    /// # Returns
    ///
    /// - [`Vec<Value>`] of encoded script entries.
    ///
    /// # Panics
    ///
    /// May panic if encoder gets interrupted.
    ///
    #[must_use]
    pub fn encode_scripts(scripts: &Scripts) -> Vec<Value> {
        let mut scripts_array = Vec::with_capacity(scripts.contents.len());

        for ((content, name), number) in scripts
            .contents
            .iter()
            .zip(scripts.names.iter())
            .zip(scripts.numbers.iter())
        {
            let mut encoder =
                ZlibEncoder::new(Vec::new(), Compression::default());
            encoder.write_all(content.as_bytes()).unwrap();
            let compressed_content = encoder.finish().unwrap();

            scripts_array.push(Value::array(vec![
                Value::int(*number),
                Value::string(name),
                Value::bytes(&compressed_content),
            ]));
        }

        scripts_array
    }
}
