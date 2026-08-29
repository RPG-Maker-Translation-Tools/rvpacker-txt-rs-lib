use super::*;
use crate::{
    CommentPos, ProcessedData,
    marshal_compat::RpgmData,
    types::{Error, Lines, RPGMFileType, Scripts},
};
use flate2::{Compression, read::ZlibDecoder, write::ZlibEncoder};
use marshal_rs::{
    arena::{Arena, ValueId},
    value::ValueRef,
};
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

        // Scripts.* never carries an encoding ivar's UTF-8 validity check
        // meaningfully - its zlib-compressed bodies are tagged UTF-8 by Ruby
        // regardless, so it's loaded via raw `Arena`/`ValueRef` here rather
        // than through the shared `Value` cursor, which would otherwise try
        // (and fail) to classify that binary content as text.
        let mut arena = marshal_rs::load(content)?.into_owned();
        // SAFETY: Scripts are always array.
        let root = arena.root();

        let mut scripts = Self::decode_scripts(&arena, root, self.read_encoding);

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

        let script_ids: Vec<ValueId> = ValueRef::new(&arena, root).array().map(|v| v.id()).collect();

        for (((script_id, script_element_id), script_name), mut code) in script_ids
            .into_iter()
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

            self.update_metadata(id, Vec::from([(CommentPos::Name, script_name.as_str())]));
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
                    let encoded_code = self.encode_with_fallback(&code);
                    let mut buf = Vec::with_capacity(encoded_code.len());

                    ZlibEncoder::new(&mut buf, Compression::default())
                        .write_all(&encoded_code)
                        .unwrap();

                    arena.set_array_bytes(script_element_id, 2, buf);
                }
            } else {
                for extracted in extracted_strings.into_iter().filter(|s| !s.trim().is_empty()) {
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

        Ok(Some(self.finish(RpgmData::Marshal(arena))))
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
                } else if inside_string && char == current_quote_type && !Self::is_escaped(i, &line) {
                    let range = string_start_index + 1..global_index + i;

                    let extracted_string = ruby_code[range.clone()].normalize();

                    if !extracted_string.is_empty() && !strings.contains(extracted_string.as_ref()) {
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
    /// - `arena` - The loaded `Scripts.*` arena.
    /// - `root` - The scripts array's id within `arena`.
    /// - `encoding` - Codepage to decode each script's name and source with,
    ///   or [`None`] to guess it per script - see [`Base::decode_with_fallback`].
    ///   [`Base::process_scripts`] passes [`Base::set_read_encoding`]'s override
    ///   through here; a caller with no [`Base`] in hand (e.g. [`crate::json`])
    ///   passes [`None`] to keep guessing.
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
    pub fn decode_scripts(
        arena: &Arena<'static>,
        root: ValueId,
        encoding: Option<&'static encoding_rs::Encoding>,
    ) -> Scripts {
        let scripts_len = ValueRef::new(arena, root).len();
        let mut numbers = Vec::with_capacity(scripts_len);
        let mut contents = Vec::with_capacity(scripts_len);
        let mut names = Vec::with_capacity(scripts_len);

        for script in ValueRef::new(arena, root).array() {
            // SAFETY: Scripts always have a layout like this. `0` is magic number, `1` is name and `2` is actual script data.
            let entry0 = unsafe { script.at(0).unwrap_unchecked() };

            // The magic number is a plain integer in modern data, but can be
            // stored as a numeric string in older/hand-edited files.
            let script_number = if let Some(n) = entry0.as_i64() {
                n as i32
            } else {
                unsafe {
                    std::str::from_utf8_unchecked(entry0.as_bytes().unwrap_unchecked())
                        .parse::<i32>()
                        .unwrap_unchecked()
                }
            };

            let script_name_data = unsafe { script.at(1).unwrap_unchecked().as_bytes().unwrap_unchecked() };
            let script_data = unsafe { script.at(2).unwrap_unchecked().as_bytes().unwrap_unchecked() };

            let mut decoded_script = Vec::with_capacity(script_data.len());
            ZlibDecoder::new(script_data).read_to_end(&mut decoded_script).unwrap();

            numbers.push(script_number);
            contents.push(Base::decode_bytes_with(&decoded_script, encoding));
            names.push(Base::decode_bytes_with(script_name_data, encoding));
        }

        Scripts::new(numbers, contents, names)
    }

    /// Encodes decoded [`Scripts`] struct back to a fresh `Arena`, rooted at
    /// the scripts array.
    ///
    /// # Parameters
    ///
    /// - [`Scripts`] struct to encode.
    ///
    /// # Returns
    ///
    /// - [`Arena`] holding the encoded scripts array, ready for [`marshal_rs::dump`].
    ///
    /// # Panics
    ///
    /// May panic if encoder gets interrupted.
    ///
    #[must_use]
    pub fn encode_scripts(scripts: &Scripts) -> Arena<'static> {
        let mut arena = Arena::builder();
        let mut entries = Vec::with_capacity(scripts.contents.len());

        for ((content, name), number) in scripts
            .contents
            .iter()
            .zip(scripts.names.iter())
            .zip(scripts.numbers.iter())
        {
            let mut encoder = ZlibEncoder::new(Vec::new(), Compression::default());
            encoder.write_all(content.as_bytes()).unwrap();
            let compressed_content = encoder.finish().unwrap();

            let number_id = arena.push_fixnum(*number);
            let name_id = arena.push_string(name.clone());
            let content_id = arena.push_bytes(compressed_content);

            entries.push(arena.push_array(&[number_id, name_id, content_id]));
        }

        let root = arena.push_array(&entries);
        arena.set_root(root);
        arena
    }
}
