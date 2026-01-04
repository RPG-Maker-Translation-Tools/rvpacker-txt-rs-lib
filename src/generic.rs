use crate::{
    BaseFlags, Comments, Error, IgnoreEntry, IgnoreMap, Lines, Mode,
    ProcessedData, ReadMode, SEPARATOR, TranslationEntry, TranslationMap,
    constants::{
        AT_POSITION_MSG, COMMENT_PREFIX, COULD_NOT_SPLIT_LINE_MSG, IN_FILE_MSG,
    },
    core::{CustomReplace, push_entries, romanize_string},
};
use log::warn;
use marshal_rs::{Value, ValueType, load_utf8};
use serde_json::{Value as SerdeValue, from_str, to_vec};
use smallvec::smallvec;
use std::{borrow::Cow, mem::take};

pub struct GenericBase {
    pub mode: Mode,
    pub flags: BaseFlags,

    pub ignore_map: IgnoreMap,
    ignore_entry: &'static mut IgnoreEntry,

    lines: Lines,
    translation_map: TranslationMap,
}

impl Default for GenericBase {
    fn default() -> Self {
        Self {
            mode: Mode::Read(ReadMode::Default(false)),
            flags: BaseFlags::empty(),

            ignore_map: IgnoreMap::default(),
            ignore_entry: unsafe { &mut *(16 as *mut IgnoreEntry) },

            lines: Lines::default(),
            translation_map: TranslationMap::default(),
        }
    }
}

impl GenericBase {
    #[must_use]
    pub fn new(mode: Mode) -> Self {
        Self {
            mode,
            ..Default::default()
        }
    }

    fn process_string(&mut self, str: &mut String) {
        match self.mode {
            Mode::Read(_) => {
                let str =
                    Cow::Borrowed(if self.flags.contains(BaseFlags::Trim) {
                        str.trim()
                    } else {
                        str
                    });

                let str = if self.flags.contains(BaseFlags::Romanize) {
                    romanize_string(str.as_ref())
                } else {
                    str
                };

                if self.flags.contains(BaseFlags::Ignore)
                    && self.ignore_entry.contains(str.as_ref())
                {
                    return;
                }

                self.lines.insert(str.into_owned());
            }
            Mode::Write => {
                if let Some(translation) = self.translation_map.swap_remove(str)
                {
                    *str = translation.translation;
                }
            }
            _ => unreachable!(),
        }
    }

    fn traverse_json(&mut self, value: &mut SerdeValue) {
        match value {
            SerdeValue::Array(arr) => {
                for v in arr {
                    self.traverse_json(v);
                }
            }
            SerdeValue::Object(obj) => {
                for (_, v) in obj {
                    self.traverse_json(v);
                }
            }
            SerdeValue::String(str) => {
                self.process_string(str);
            }
            _ => {}
        }
    }

    fn traverse_marshal(&mut self, value: &mut Value) {
        match &mut **value {
            ValueType::Array(arr) => {
                for v in arr {
                    self.traverse_marshal(v);
                }
            }
            ValueType::HashMap(hash) => {
                for (_, v) in hash.iter_mut() {
                    self.traverse_marshal(v);
                }
            }
            ValueType::Object(obj) => {
                for (_, v) in obj.iter_mut() {
                    self.traverse_marshal(v);
                }
            }
            ValueType::Struct(struc) => {
                for (_, v) in struc.iter_mut() {
                    self.traverse_marshal(v);
                }
            }
            ValueType::String(str) => {
                self.process_string(str);
            }
            _ => {}
        }
    }

    fn purge(&mut self) -> ProcessedData {
        let output_size = 4096 * 1024;
        let mut output = Vec::with_capacity(output_size);

        for (mut source, translation) in take(&mut self.translation_map) {
            if translation.is_empty() {
                let moved = take(&mut source);

                if self.flags.contains(BaseFlags::CreateIgnore)
                    && !moved.is_empty()
                {
                    self.ignore_entry.insert(moved);
                }
            }

            push_entries(&mut output, &source, &translation);
        }

        ProcessedData::TranslationData(output)
    }

    fn initialize_translation(
        &mut self,
        translation: Option<&str>,
        filename: &str,
    ) -> Result<(), Error> {
        if self.mode.is_default() {
            return Ok(());
        }

        let Some(translation) = translation else {
            return Err(Error::NoTranslation);
        };

        let trim = self.flags.contains(BaseFlags::Trim);

        let translation_lines = translation.lines().enumerate();
        let mut comments: Comments;

        for (i, line) in translation_lines {
            comments = smallvec![String::new(); 3];

            if line.starts_with(COMMENT_PREFIX) {
                comments.push(line.to_string());
                continue;
            }

            // This split is essentially free, since we're not cloning to String
            let split: Vec<&str> = line.split(SEPARATOR).collect();

            if split.len() < 2 {
                warn!(
                    "{COULD_NOT_SPLIT_LINE_MSG}\n{AT_POSITION_MSG}: {i}\n
                    {IN_FILE_MSG}: {filename}",
                    i = i + 1,
                );
                comments.clear();
                continue;
            }

            // SAFETY: We just checked for split length.
            let source =
                Cow::Borrowed(*unsafe { split.first().unwrap_unchecked() });

            let translation = Cow::Borrowed(
                split
                    .into_iter()
                    .skip(1)
                    .rfind(|x| !x.is_empty())
                    .unwrap_or_default(),
            );

            let (source, translation) = if trim {
                (
                    Cow::Borrowed(source.trim()),
                    Cow::Borrowed(translation.trim()),
                )
            } else {
                (source, translation)
            };

            let (source, translation) = if self.mode.is_write() {
                // Discard lines with empty translation, those are unused on write
                if translation.is_empty() {
                    continue;
                }

                (source.denormalize(), translation.denormalize())
            } else {
                (source, translation)
            };

            self.translation_map.insert(
                source.into(),
                TranslationEntry {
                    comments: comments.drain(..).collect(),
                    translation: translation.into(),
                },
            );
        }

        Ok(())
    }

    fn finish(&mut self) -> Vec<u8> {
        let output_size = 4096 * 1024;
        let mut output = Vec::with_capacity(output_size);

        for source in take(&mut self.lines) {
            let translation = self
                .translation_map
                .swap_remove(&source)
                .unwrap_or_default();

            for comment in &translation.comments {
                output.extend_from_slice(comment.as_bytes());
                output.push(b'\n');
            }

            output.extend_from_slice(source.as_bytes());
            output.extend_from_slice(SEPARATOR.as_bytes());
            output.extend_from_slice(translation.as_bytes());
            output.push(b'\n');
        }

        output
    }

    pub fn process_json(
        &mut self,
        content: &str,
        filename: &str,
        translation: Option<&str>,
    ) -> Result<ProcessedData, Error> {
        self.initialize_translation(translation, filename)?;

        if self.mode.is_purge() {
            Ok(self.purge())
        } else {
            let mut json: SerdeValue = from_str(content)?;
            self.traverse_json(&mut json);

            if self.mode.is_read() {
                Ok(ProcessedData::TranslationData(self.finish()))
            } else {
                Ok(ProcessedData::RPGMData(to_vec(&json).unwrap()))
            }
        }
    }

    pub fn process_marshal(
        &mut self,
        content: &[u8],
        filename: &str,
        translation: Option<&str>,
    ) -> Result<ProcessedData, Error> {
        self.initialize_translation(translation, filename)?;

        if self.mode.is_purge() {
            Ok(self.purge())
        } else {
            let mut value: Value = load_utf8(content, Some(""))?;
            self.traverse_marshal(&mut value);

            if self.mode.is_read() {
                Ok(ProcessedData::TranslationData(self.finish()))
            } else {
                Ok(ProcessedData::RPGMData(value.to_string().into_bytes()))
            }
        }
    }
}
