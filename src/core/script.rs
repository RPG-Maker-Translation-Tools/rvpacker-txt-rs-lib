use super::{Base, CustomReplace, string_is_only_symbols};
use crate::{
    CommentPos, ProcessedData,
    marshal_compat::RpgmData,
    types::{EngineType, Error, Lines, RPGMFileType, Scripts},
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
    sync::LazyLock,
};

// Compiled once, on first use, and never on write, which doesn't filter.
static SCRIPT_REGEXPS: LazyLock<[Regex; 5]> = LazyLock::new(|| unsafe {
    [
        Regex::new(r"(Graphics|Data|Audio|Movies|System)\/.*\/?").unwrap_unchecked(),
        Regex::new(r"r[xv]data2?$").unwrap_unchecked(),
        Regex::new(r".*\(").unwrap_unchecked(),
        Regex::new(r"^([\d\p{P}+-]*|[\p{P}+-]&*)$").unwrap_unchecked(),
        Regex::new(r"^(Actor<id>|ExtraDropItem|EquipLearnSkill|GameOver|Iconset|Window|true|false|MActor%d|[wr]b|\\f|\\n|\[[A-Z]*\])$")
            .unwrap_unchecked(),
    ]
});

/// `text` fit for a string literal quoted with `quote`: every `quote` in it that is not escaped already gets a
/// backslash, or it would end the literal early.
fn escape_quote(text: &str, quote: char) -> Cow<'_, str> {
    if !text.contains(quote) {
        return Cow::Borrowed(text);
    }

    let mut escaped = String::with_capacity(text.len() + 1);
    let mut backslashes = 0_usize;

    for char in text.chars() {
        if char == quote && backslashes.is_multiple_of(2) {
            escaped.push('\\');
        }

        backslashes = if char == '\\' { backslashes + 1 } else { 0 };
        escaped.push(char);
    }

    Cow::Owned(escaped)
}

/// The most bytes one script may inflate to.
const MAX_SCRIPT_BYTES: u64 = 256 << 20;

impl Base {
    /// Processes the RPG Maker scripts file content.
    ///
    /// # Parameters
    ///
    /// - `content` - Content of the file that's being processed.
    /// - `translation` - Translation file contents. Ignored with [`crate::Mode::Read`], required otherwise.
    ///
    /// # Returns
    ///
    /// - Nothing if `mode` is [`crate::Mode::Write`] and no translation exists.
    /// - [`ProcessedData`], which contains RPG Maker data if `mode` is [`crate::Mode::Write`] and translation data otherwise.
    /// - [`Error`], if unable to parse the content.
    ///
    /// # Errors
    ///
    /// - [`Error::MarshalLoad`] - if unable to load the Marshal data.
    /// - [`Error::JsonParse`] - if unable to parse the JSON data.
    /// - [`Error::NoTranslation`] - if mode is not [`crate::Mode::Read`], and no translation was passed.
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

        let mut scripts = Self::decode_scripts(&arena, root, self.read_encoding)?;

        let mut processed = false;

        let script_ids: Vec<ValueId> = ValueRef::new(&arena, root).array().map(|v| v.id()).collect();

        // A script has no id of its own, so it takes the id of the section with its name: a script inserted into the
        // game must not shift every translation after it.
        let ids = self.ids_by_name(scripts.names.iter().map(String::as_str));

        for (((script_element_id, script_name), mut code), id) in script_ids
            .into_iter()
            .zip(take(&mut scripts.names))
            .zip(take(&mut scripts.contents))
            .zip(ids)
        {
            if self.get_translation_map(id).is_break() {
                if self.mode.is_purge() {
                    processed = true;
                }

                continue;
            }

            processed = true;

            self.update_metadata(id, &[(CommentPos::Name, script_name.as_str())]);
            let (extracted_strings, ranges) = self.extract_strings(&code);

            if self.mode.is_write() {
                let mut code_changed = false;

                for (extracted, range) in ranges.into_iter().filter(|(s, _)| !s.trim().is_empty()).rev() {
                    // Keys are stored denormalized, so a literal spanning lines must be looked up that way too.
                    if let Some(translated) = self.get_key(&extracted.denormalize()) {
                        // `range` sits between the quotes, so the one before it is the literal's own.
                        let quote = char::from(code.as_bytes()[range.start - 1]);

                        code.replace_range(range, &escape_quote(translated, quote));
                        code_changed = true;
                    }
                }

                if code_changed {
                    let encoded_code = self.encode_text(&code);
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
                        || SCRIPT_REGEXPS.iter().any(|re| re.is_match(&extracted))
                    {
                        continue;
                    }

                    self.insert_string(Cow::Owned(extracted));
                }

                self.flush_translation(id);
            }
        }

        // Code that cannot be written in the write encoding fails the file before anything of it is written.
        Self::take_decode_error()?;

        if !processed {
            return Ok(None);
        }

        Ok(Some(self.finish(RpgmData::Marshal(arena))))
    }

    fn is_escaped(index: usize, string: &str) -> bool {
        let mut escaped = false;

        for char in string[..index].chars().rev() {
            if char != '\\' {
                break;
            }

            escaped = !escaped;
        }

        escaped
    }

    /// Read: the distinct strings. Write: every occurrence with its range, so repeats get translated too.
    // shortcut: a line scanner that knows only `'`/`"` strings and `#` comments - heredocs, `%q` and regex literals
    // holding a quote can misalign it. Write only replaces spans that equal a translated line, so the damage is noise
    // lines on read; upgrade if real scripts show it.
    fn extract_strings(&self, ruby_code: &str) -> (Lines, Vec<(String, Range<usize>)>) {
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
                } else if inside_string && char == current_quote_type && !Self::is_escaped(i, line) {
                    let range = string_start_index + 1..global_index + i;

                    let extracted_string = ruby_code[range.clone()].normalize();

                    if !extracted_string.is_empty() {
                        if self.mode.is_write() {
                            ranges.push((extracted_string.into_owned(), range));
                        } else if !strings.contains(extracted_string.as_ref()) {
                            strings.insert(extracted_string.into_owned());
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
    ///   or [`None`] for UTF-8. [`Base::process_scripts`] passes
    ///   [`Base::set_read_encoding`]'s codepage through here.
    ///
    /// # Returns
    ///
    /// A [`Scripts`] struct that holds `numbers`, `scripts` and `names` fields.
    ///
    /// # Errors
    ///
    /// - [`Error::InvalidScripts`] - if an entry is missing a part or its source is not valid zlib data.
    /// - [`Error::InvalidText`] - if a name or source is not valid in `encoding`.
    ///
    pub fn decode_scripts(
        arena: &Arena<'static>,
        root: ValueId,
        encoding: Option<&'static encoding_rs::Encoding>,
    ) -> Result<Scripts, Error> {
        let scripts_len = ValueRef::new(arena, root).len();
        let mut numbers = Vec::with_capacity(scripts_len);
        let mut contents = Vec::with_capacity(scripts_len);
        let mut names = Vec::with_capacity(scripts_len);

        for script in ValueRef::new(arena, root).array() {
            // A script is `[magic number, name, zlib-compressed source]`.
            let entry0 = script
                .at(0)
                .ok_or(Error::InvalidScripts("an entry has no magic number"))?;

            // The magic number is a plain integer in modern data, but can be
            // stored as a numeric string in older/hand-edited files.
            let script_number = if let Some(n) = entry0.as_i64() {
                n as i32
            } else {
                entry0
                    .as_bytes()
                    .and_then(|bytes| std::str::from_utf8(bytes).ok())
                    .and_then(|text| text.parse::<i32>().ok())
                    .ok_or(Error::InvalidScripts("a magic number is not an integer"))?
            };

            let script_name_data = script
                .at(1)
                .and_then(|name| name.as_bytes())
                .ok_or(Error::InvalidScripts("an entry has no name"))?;
            let script_data = script
                .at(2)
                .and_then(|data| data.as_bytes())
                .ok_or(Error::InvalidScripts("an entry has no source"))?;

            // Capped: a few KiB of zlib can inflate to gigabytes. Real scripts stay far below this.
            let mut decoded_script = Vec::with_capacity(script_data.len());
            ZlibDecoder::new(script_data)
                .take(MAX_SCRIPT_BYTES + 1)
                .read_to_end(&mut decoded_script)
                .map_err(|_| Error::InvalidScripts("a source is not valid zlib data"))?;

            if decoded_script.len() as u64 > MAX_SCRIPT_BYTES {
                return Err(Error::InvalidScripts("a source is larger than 256 MiB"));
            }

            numbers.push(script_number);
            contents.push(match encoding {
                // Taking a UTF-8 buffer as is skips copying the whole script; anything else fails the same way the
                // decoder reports it.
                None => String::from_utf8(decoded_script)
                    .or_else(|error| Self::decode_bytes_with(error.as_bytes(), None))?,
                Some(_) => Self::decode_bytes_with(&decoded_script, encoding)?,
            });
            names.push(Self::decode_bytes_with(script_name_data, encoding)?);
        }

        Ok(Scripts::new(numbers, contents, names))
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
    /// - [`Arena`] holding the encoded scripts array, ready for [`marshal_rs::dump()`].
    ///
    /// # Panics
    ///
    /// May panic if encoder gets interrupted.
    ///
    #[must_use]
    pub fn encode_scripts(scripts: &Scripts) -> Arena<'static> {
        // UTF-8 encodes any text, so this cannot fail.
        Self::encode_scripts_for(scripts, EngineType::VXAce, None).expect("UTF-8 can encode anything")
    }

    /// [`Base::encode_scripts`] for `engine_type`: XP and VX (Ruby 1.8) keep script names as raw bytes, the others
    /// as strings tagged with their encoding. Sources, and the names of XP and VX, are encoded in `write_encoding`,
    /// UTF-8 if [`None`].
    ///
    /// # Errors
    ///
    /// - [`Error::UnsupportedWriteEncoding`]/[`Error::UnmappableText`] - if a source or name can't be encoded.
    pub(crate) fn encode_scripts_for(
        scripts: &Scripts,
        engine_type: EngineType,
        write_encoding: Option<&'static encoding_rs::Encoding>,
    ) -> Result<Arena<'static>, Error> {
        let mut arena = Arena::builder();
        let mut entries = Vec::with_capacity(scripts.contents.len());

        for ((content, name), number) in scripts
            .contents
            .iter()
            .zip(scripts.names.iter())
            .zip(scripts.numbers.iter())
        {
            let mut encoder = ZlibEncoder::new(Vec::new(), Compression::default());
            encoder
                .write_all(&Self::encode_bytes_with(content, write_encoding)?)
                .unwrap();
            let compressed_content = encoder.finish().unwrap();

            let number_id = arena.push_fixnum(*number);
            let name_id = if engine_type.is_vx_ace() {
                arena.push_string(name.clone())
            } else {
                arena.push_bytes(Self::encode_bytes_with(name, write_encoding)?)
            };
            let content_id = arena.push_bytes(compressed_content);

            entries.push(arena.push_array(&[number_id, name_id, content_id]).unwrap());
        }

        let root = arena.push_array(&entries).unwrap();
        arena.set_root(root);
        Ok(arena)
    }
}
