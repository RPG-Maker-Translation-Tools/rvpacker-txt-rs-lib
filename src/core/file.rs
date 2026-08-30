use super::IgnoreEntry;
use crate::{
    get_ignore_entry_comment, get_line_separator,
    types::{DuplicateMode, Error, IgnoreMap, RPGMFileType},
};
use serde_json::{Value as SerdeValue, from_str};
use smallvec::SmallVec;
use std::{fs::DirEntry, path::Path};

const BOM: &[u8] = &[0xEF, 0xBB, 0xBF];

/// Filters entries of [`std::fs::ReadDir`] and returns iterator of only `Map` entries.
///
/// # Parameters
///
/// - `entries` - Entries read with [`std::fs::read_dir`].
/// - `engine_extension` - [`&str`] corresponding to the extension of read entries.
///
/// # Returns
///
/// Filtered iterator containing only `Map` entries.
///
pub fn filter_maps<'a>(
    entries: impl Iterator<Item = &'a DirEntry>,
    engine_extension: &'a str,
) -> impl Iterator<Item = &'a DirEntry> {
    let mut result: Vec<&'a DirEntry> = entries
        .filter_map(move |entry| {
            if !entry.file_type().ok()?.is_file() {
                return None;
            }

            let filename = entry.file_name();
            let extension = Path::new(&filename).extension()?;
            let filename_str = filename.to_str()?;

            if filename_str.starts_with("Map")
                && filename_str.as_bytes().get(3)?.is_ascii_digit()
                && extension == engine_extension
            {
                return Some(entry);
            }

            None
        })
        .collect();

    result.sort_by_key(|entry| {
        let filename = entry.file_name();
        let filename_str = filename.to_str().unwrap_or("");
        let digits: String = filename_str[3..].chars().take_while(|c| c.is_ascii_digit()).collect();
        digits.parse::<u32>().unwrap_or(0)
    });

    result.into_iter()
}

/// Filters entries of [`std::fs::ReadDir`] and returns iterator of only other entries.
///
/// # Parameters
///
/// - `entries` - Entries read with [`std::fs::read_dir`].
/// - `engine_extension` - [`&str`] corresponding to the extension of read entries.
///
/// # Returns
///
/// Filtered iterator containing only other entries.
///
pub fn filter_other<'a>(
    entries: impl Iterator<Item = &'a DirEntry>,
    engine_extension: &'a str,
) -> impl Iterator<Item = &'a DirEntry> {
    let mut result: Vec<&'a DirEntry> = entries
        .filter_map(move |entry| {
            if !entry.file_type().ok()?.is_file() {
                return None;
            }
            let filename = entry.file_name();
            let filename_path = Path::new(&filename);
            let basename = filename_path.file_stem().and_then(|basename| basename.to_str())?;
            let extension = filename_path.extension()?;
            let file_type = RPGMFileType::from_filename(basename);
            if extension == engine_extension && file_type.is_other() {
                return Some(entry);
            }
            None
        })
        .collect();

    result.sort_by_key(|entry| entry.file_name());
    result.into_iter()
}

/// Filters entries of [`std::fs::ReadDir`] and returns an iterator of only `MapNNNN.lmu`
/// entries, sorted by numeric id.
///
/// Rm2k projects keep every map at the project root rather than in a `Data`
/// subdirectory, and don't share [`filter_maps`]'s extension-driven filter -
/// the extension is always `lmu`, and the numeric id is four digits rather
/// than the variable-width id MV/VX uses.
///
/// # Parameters
///
/// - `entries` - Entries read with [`std::fs::read_dir`].
///
/// # Returns
///
/// Filtered iterator containing only `MapNNNN.lmu` entries.
#[must_use]
pub fn filter_rm2k_maps<'a>(entries: impl Iterator<Item = &'a DirEntry>) -> impl Iterator<Item = &'a DirEntry> {
    let mut result: Vec<&'a DirEntry> = entries
        .filter_map(move |entry| {
            if !entry.file_type().ok()?.is_file() {
                return None;
            }

            let filename = entry.file_name();
            let filename_str = filename.to_str()?;
            let digits = filename_str.strip_prefix("Map")?.strip_suffix(".lmu")?;

            if digits.len() == 4 && digits.bytes().all(|b| b.is_ascii_digit()) {
                return Some(entry);
            }

            None
        })
        .collect();

    result.sort_by_key(|entry| {
        let filename = entry.file_name();
        let filename_str = filename.to_str().unwrap_or("");
        filename_str[3..7].parse::<u32>().unwrap_or(0)
    });

    result.into_iter()
}

/// Parses ignore file contents to [`IgnoreMap`].
///
/// # Parameters
///
/// - `ignore_file_path` - Path to the `.rvpacker-ignore` file.
/// - `duplicate_mode` - [`DuplicateMode`], which was used during read.
/// - `read` - Parse for reading or purging.
///
/// # Returns
///
/// Parsed [`IgnoreMap`].
///
#[must_use]
pub fn parse_ignore(ignore_file_content: &str, duplicate_mode: DuplicateMode, read: bool) -> IgnoreMap {
    /// Canonical key for a section header.
    ///
    /// With duplicates removed every section of a file shares one entry, so the
    /// `: id` suffix is dropped and the whole file keys on its name. This has to
    /// produce exactly what `Base::get_ignore_entry` builds, including the
    /// comment prefix - previously the first header in a file kept its prefix
    /// while every later one lost it, so only the first section ever matched.
    fn section_key(line: &str, duplicate_mode: DuplicateMode, read: bool) -> String {
        let Some(rest) = line.strip_prefix(get_ignore_entry_comment()) else {
            return line.to_owned();
        };

        // These three are single-section files; there is nothing to collapse.
        let collapse = read
            && duplicate_mode.is_remove()
            && !(rest.starts_with(&format!("{sep}System", sep = get_line_separator()))
                || rest.starts_with(&format!("{sep}Scripts", sep = get_line_separator()))
                || rest.starts_with(&format!("{sep}Plugins", sep = get_line_separator())));

        if !collapse {
            return line.to_owned();
        }

        // A hand-written file may leave the id off entirely.
        let name = rest.split_once(':').map_or(rest, |(name, _)| name);
        format!("{comment}{name}", comment = get_ignore_entry_comment())
    }

    let mut ignore_map = IgnoreMap::default();
    ignore_map.reserve_exact(256);

    let mut current: Option<usize> = None;

    for line in ignore_file_content.lines().filter(|line| !line.is_empty()) {
        if line.starts_with(get_ignore_entry_comment()) {
            let entry = ignore_map.entry(section_key(line, duplicate_mode, read));

            current = Some(entry.index());
            entry.or_insert_with(|| IgnoreEntry::with_capacity(128));
        } else if let Some(index) = current {
            // Lines before the first header belong to no section, so they are
            // dropped rather than inventing one.
            if let Some((_, entry)) = ignore_map.get_index_mut(index) {
                entry.insert_line(line);
            }
        }
    }

    ignore_map
}

/// Extracts the game title from a `Game.ini` file's content.
///
/// Doubles as a practical way to guess an XP/VX project's [`Base::set_read_encoding`](super::Base::set_read_encoding)
/// (see the "Text encoding" section of the crate documentation for the underlying
/// problem): every other `Game.ini` entry is ASCII, so the title is the one place
/// non-ASCII bytes can turn up at all, and when it does contain them, it was RPG
/// Maker's own editor - running on the original developer's machine - that wrote
/// them, in whatever codepage that machine's locale used at the time (Shift-JIS
/// for a Japanese developer, Windows-1251 for a Russian one, and so on). Feeding
/// the returned bytes to each codepage `encoding_rs` supports and keeping whichever
/// one decodes without errors (ideally with a human sanity-check of the result, or
/// falling back to a language-specific heuristic when more than one decodes
/// cleanly) recovers that same codepage - not with certainty, since a purely-ASCII
/// title gives no signal either way, but it is the same trick used in practice.
///
/// # Parameters
///
/// - `ini_file_content` - raw byte content of the INI file to parse.
///
/// # Returns
///
/// - [`Vec<u8>`] - vector of extracted title's bytes on success. Title may not be UTF-8.
/// - [`Error`] - otherwise.
///
/// # Errors
///
/// - [`Error::NoTitle`] - if no "Title" entry is found in the INI file.
///
/// # Example
///
/// ```no_run
/// use rvpacker_txt_rs_lib::{get_ini_title, Error};
/// use std::fs::read;
///
/// fn main() -> Result<(), Box<dyn std::error::Error>> {
///     let ini_content = read("C:/Game/Game.ini")?;
///     let title = get_ini_title(&ini_content)?;
///     Ok(())
/// }
/// ```
pub fn get_ini_title(ini_file_content: &[u8]) -> Result<Vec<u8>, Error> {
    find_ini_value(ini_file_content, b"title").ok_or(Error::NoTitle)
}

/// Extracts the game title from an RM2K/2003 `RPG_RT.ini` file's content
/// (the `GameTitle` key).
///
/// See [`get_ini_title`]'s docs for why the title is returned as raw bytes
/// rather than a decoded [`String`] - the same reasoning applies here.
///
/// # Parameters
///
/// - `ini_file_content` - raw byte content of the INI file to parse.
///
/// # Returns
///
/// - [`Vec<u8>`] - vector of extracted title's bytes on success. Title may not be UTF-8.
/// - [`Error`] - otherwise.
///
/// # Errors
///
/// - [`Error::NoTitle`] - if no "GameTitle" entry is found in the INI file.
///
/// # Example
///
/// ```no_run
/// use rvpacker_txt_rs_lib::{get_ini_title_rm2k, Error};
/// use std::fs::read;
///
/// fn main() -> Result<(), Box<dyn std::error::Error>> {
///     let ini_content = read("C:/Game/RPG_RT.ini")?;
///     let title = get_ini_title_rm2k(&ini_content)?;
///     Ok(())
/// }
/// ```
pub fn get_ini_title_rm2k(ini_file_content: &[u8]) -> Result<Vec<u8>, Error> {
    find_ini_value(ini_file_content, b"gametitle").ok_or(Error::NoTitle)
}

fn trim_bytes(bytes: &[u8]) -> &[u8] {
    let start = bytes.iter().position(|&b| !is_ini_space(b)).unwrap_or(0);
    let end = bytes.iter().rposition(|&b| !is_ini_space(b)).map_or(0, |i| i + 1);
    &bytes[start..end]
}

fn is_ini_space(b: u8) -> bool {
    b == b' ' || b == b'\t' || b == b'\r'
}

fn split_ini_lines(data: &[u8]) -> SmallVec<[&[u8]; 4]> {
    let mut lines = SmallVec::with_capacity(4);
    let mut start = 0;
    let mut i = 0;

    while i < data.len() {
        if data[i] == b'\n' {
            lines.push(&data[start..i]);
            i += 1;
            start = i;
        } else if data[i] == b'\r' {
            lines.push(&data[start..i]);

            if data.get(i + 1).is_some_and(|ch| *ch == b'\n') {
                i += 2;
            } else {
                i += 1;
            }

            start = i;
        } else {
            i += 1;
        }
    }

    if start < data.len() {
        lines.push(&data[start..]);
    }

    lines
}

/// Finds `key`'s value in an INI file's content. `key` must already be lowercase.
fn find_ini_value(ini_file_content: &[u8], key: &[u8]) -> Option<Vec<u8>> {
    for line in split_ini_lines(ini_file_content) {
        if line.to_ascii_lowercase().starts_with(key) {
            if let Some(pos) = line.iter().position(|&b| b == b'=') {
                let right = &line[pos + 1..];
                let trimmed = trim_bytes(right);
                return Some(trimmed.to_vec());
            }
        }
    }

    None
}

/// Extracts the game title from a `System.json` file's content.
///
/// # Parameters
///
/// - `system_file_content` - JSON string content of the system file
///
/// # Returns
///
/// - [`String`] game title extracted from the "gameTitle" field if successful.
/// - [`Error`] otherwise.
///
/// # Errors
///
/// - [`Error::JsonParse`] - if parsing `system_file_content` failed.
/// - [`Error::NoTitle`] - if the parsed JSON doesn't contain "gameTitle" key.
///
/// # Example
///
/// ```no_run
/// use rvpacker_txt_rs_lib::{get_system_title, Error};
/// use std::fs::read_to_string;
///
/// fn main() -> Result<(), Box<dyn std::error::Error>> {
///     let system_file_content = read_to_string("C:/Game/www/data/System.json")?;
///     let title = get_system_title(&system_file_content)?;
///     Ok(())
/// }
/// ```
pub fn get_system_title(mut system_file_content: &str) -> Result<String, Error> {
    // MZ includes Byte Order Mark in files.
    if system_file_content.as_bytes().starts_with(BOM) {
        system_file_content = &system_file_content[BOM.len()..];
    }

    let system_file_value: SerdeValue = from_str(system_file_content)?;

    system_file_value["gameTitle"]
        .as_str()
        .map(Into::into)
        .ok_or(Error::NoTitle)
}
