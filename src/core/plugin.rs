use super::{Base, CustomReplace};
use crate::{
    CommentPos, ProcessedData,
    constants::SYMBOLS,
    json_doc::{JsonDoc, NodeId},
    types::{Error, RPGMFileType},
};
use regex::Regex;
use std::sync::LazyLock;

// The literal patterns stay regexes, so the array keeps one type.
#[allow(clippy::trivial_regex)]
// SAFETY: every pattern is a fixed literal that compiles, so `unwrap_unchecked` never sees an `Err`.
static PLUGINS_REGEXPS: LazyLock<[Regex; 11]> = LazyLock::new(|| unsafe {
    [
        Regex::new(r"^(name|description|Window Width|Window Height|ATTENTION!!!|Shown Elements|Width|Outline Color|Command Alignment|Command Position|Command Rows|Chinese Font|Korean Font|Default Font|Text Align|Scenes To Draw|displacementImage|Turn Alignment|Buff Formula|Counter Alignment|Default Width|Face Indent|Fast Forward Key|Font Name|Font Name CH|Font Name KR|Name Box Padding|Name Box Added Text|Critical Rate Formula|Critical Multplier Formula|Flat Critical Formula|Default SE|---List---|Button Events List|Kill Switch|Ex Turn Image|Ex Turn Name Color|Non Ex Turn Name Color|Option menu entry|Add to options|Default Ambient Light|Reset Lights|Gab Font Name|Escape Ratio|Translated Format|Default Sound|Action Speed|Default System|Untranslated Format|Default Format|Victory Screen Level Sound|Warning Side Battle UI|Weapon Swap Text Hit|Weapon Swap Text Critical|Weapon Swap Command|Weapon Swap Text Evasion|alwaysDash|renderingMode|Attributes Command|Attributes Column 1|Attributes Column 2|Attributes Column 3|Warning OTB|Present Settings)$").unwrap_unchecked(),
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

const MAX_NESTING: usize = 256;

/// Like `string_is_only_symbols`, but digits and quotes count too: a plugin parameter is not game text.
fn is_only_symbols_or_digits(string: &str) -> bool {
    string
        .chars()
        .all(|c| c.is_ascii_digit() || matches!(c, '"' | '\'') || c.is_whitespace() || SYMBOLS.contains(&c))
}

impl Base {
    /// Processes the RPG Maker plugins file content.
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
    /// - [`Error::JsonParse`] - if parsing plugin JSON content fails.
    /// - [`Error::InvalidPlugins`] - if the content is not `plugins = [...]`, or a plugin has no name.
    /// - [`Error::NoTranslation`] - if mode is not [`crate::Mode::Read`], and no translation was passed.
    ///
    /// # Example
    ///
    /// ```no_run
    /// use rvpacker_txt_rs_lib::{core::Base, Mode, EngineType, Error};
    /// use std::fs::read;
    ///
    /// fn main() -> Result<(), Box<dyn std::error::Error>> {
    ///     let mut base = Base::new(Mode::read(), EngineType::MVMZ);
    ///
    ///     let plugins_file_content = read("plugins.js")?;
    ///     base.process_plugins(&plugins_file_content, None)?;
    ///     Ok(())
    /// }
    /// ```
    pub fn process_plugins(
        &mut self,
        content: &[u8],
        translation: Option<&str>,
    ) -> Result<Option<ProcessedData>, Error> {
        self.reset();
        self.file_type = RPGMFileType::Plugins;
        self.initialize_translation(translation)?;

        // Plugins content should be like `plugins = [...]`, after any comments and `var $plugins`. The search starts at
        // the word `plugins`, so a `=` in a comment above it is skipped.
        let text = std::str::from_utf8(content).map_err(|_| Error::InvalidPlugins("expected `plugins = [...]`"))?;
        let anchor = text.find("plugins").unwrap_or(0);
        let equals = text[anchor..]
            .find('=')
            .map(|at| anchor + at)
            .ok_or(Error::InvalidPlugins("expected `plugins = [...]`"))?;

        // Only the array is parsed; whatever surrounds it is written back as it was.
        let array_end = text
            .trim_end_matches(|char: char| char == ';' || char.is_whitespace())
            .len();
        let array_end = array_end.max(equals + 1);
        let mut doc = JsonDoc::parse(text.as_bytes()[equals + 1..array_end].to_vec())?;
        let root = doc.root();

        if !doc.is_array(root) {
            return Err(Error::InvalidPlugins("expected an array of plugins"));
        }

        let plugin_nodes: Vec<NodeId> = (0..doc.len(root)).filter_map(|index| doc.at(root, index)).collect();
        let mut names = Vec::with_capacity(plugin_nodes.len());

        for &plugin in &plugin_nodes {
            let name = doc
                .member(plugin, "name")
                .and_then(|name| doc.as_str(name))
                .ok_or(Error::InvalidPlugins("a plugin has no name"))?;

            names.push(name.to_owned());
        }

        // A plugin has no id of its own, so it takes the id of the section with its name: a plugin inserted into the
        // game must not shift every translation after it.
        let ids = self.ids_by_name(names.iter().map(String::as_str));
        let mut processed = false;

        for ((plugin, name), id) in plugin_nodes.into_iter().zip(&names).zip(ids) {
            if self.get_translation_map(id).is_break() {
                if self.mode.is_purge() {
                    processed = true;
                }

                continue;
            }

            processed = true;

            self.update_metadata(id, &[(CommentPos::Name, name)]);
            self.parse_plugin(&mut doc, None, plugin, 0)?;
            self.flush_translation(id);
        }

        if !processed {
            return Ok(None);
        }

        if !self.mode.is_write() {
            return Ok(Some(self.finish_translation()));
        }

        let patched = doc.into_bytes();
        let mut output = Vec::with_capacity(equals + 1 + patched.len() + text.len() - array_end);

        output.extend_from_slice(&content[..=equals]);
        output.extend_from_slice(&patched);
        output.extend_from_slice(&content[array_end..]);

        Ok(Some(ProcessedData::RPGMData(output)))
    }

    fn parse_plugin(&mut self, doc: &mut JsonDoc, key: Option<&str>, node: NodeId, depth: usize) -> Result<(), Error> {
        if depth > MAX_NESTING {
            return Err(Error::InvalidPlugins("plugin parameters are nested too deeply"));
        }

        let is_invalid_key = |key: Option<&str>| {
            let Some(key_string) = key else {
                return false;
            };

            if key_string.starts_with("LATIN") {
                false
            } else {
                PLUGINS_REGEXPS.iter().any(|re| re.is_match(key_string))
            }
        };

        if let Some(value_string) = doc.as_str(node) {
            if is_invalid_key(key) {
                return Ok(());
            }

            if !(value_string.trim().is_empty()
                || is_only_symbols_or_digits(value_string)
                || ["true", "false", "none", "time", "off"].contains(&value_string)
                || value_string.starts_with("this.")
                    && value_string.chars().nth(5).is_some_and(char::is_alphabetic)
                    && value_string.ends_with(')')
                || value_string.starts_with("rgba"))
                || key.is_some_and(|x| x.starts_with("LATIN"))
            {
                let string = value_string.normalize();

                if self.mode.is_write() {
                    // Parsed keys were denormalized when the translation
                    // file was read, so the lookup has to be denormalized
                    // too. Looking up the normalized form left any plugin
                    // string with a line break in it unwritable.
                    let translated = self
                        .get_key(&string.denormalize())
                        .map(|entry| entry.translation.to_string());

                    if let Some(translated) = translated {
                        doc.set_string(node, translated);
                    }
                } else {
                    self.insert_string(string);
                }
            }
        } else if doc.is_array(node) {
            for index in 0..doc.len(node) {
                if let Some(child) = doc.at(node, index) {
                    self.parse_plugin(doc, None, child, depth + 1)?;
                }
            }
        } else {
            let members: Vec<(String, NodeId)> = doc.members(node).map(|(key, id)| (key.to_owned(), id)).collect();

            for (key, child) in members {
                self.parse_plugin(doc, Some(&key), child, depth + 1)?;
            }
        }

        Ok(())
    }
}
