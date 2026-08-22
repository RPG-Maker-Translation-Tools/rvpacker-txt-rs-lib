use super::IgnoreEntry;
use crate::{
    constants::{IGNORE_ENTRY_COMMENT, INSTANCE_VAR_PREFIX},
    types::{DuplicateMode, EngineType, Error, IgnoreMap, RPGMFileType},
};
use marshal_rs::{Value, load_binary, load_utf8};
use serde_json::{Value as SerdeValue, from_str};
use smallvec::SmallVec;
use std::{fs::DirEntry, path::Path};

const BOM: &[u8] = &[0xEF, 0xBB, 0xBF];

/// Parses RPG Maker file from passed content.
///
/// # Parameters
///
/// - `content` - Content of file to parse.
/// - `engine_type` - Engine type of the file.
/// - `file_type` - Type of the file.
///
/// # Returns
///
/// - [`Value`] - if file was parsed successfully.
/// - [`Error`] - if unable to deserialize the file.
///
/// # Errors
///
/// - [`Error::MarshalLoad`] - if unable to load the Marshal data.
/// - [`Error::JsonParse`] - if unable to parse the JSON data.
///
pub fn parse_rpgm_file(
    mut content: &[u8],
    engine_type: EngineType,
    file_type: RPGMFileType,
) -> Result<Value, Error> {
    if engine_type.is_new() {
        // MZ includes Byte Order Mark in files.
        if content.starts_with(BOM) {
            content = &content[3..];
        }

        // SAFETY: JSON is always valid UTF-8.
        let parsed = from_str::<SerdeValue>(unsafe {
            std::str::from_utf8_unchecked(content)
        })?;

        Ok(Value::from(parsed))
    } else {
        let loaded = if file_type.is_scripts() {
            load_binary(content, INSTANCE_VAR_PREFIX)
        } else {
            load_utf8(content, INSTANCE_VAR_PREFIX)
        }?;

        Ok(loaded)
    }
}

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
        let digits: String = filename_str[3..]
            .chars()
            .take_while(|c| c.is_ascii_digit())
            .collect();
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
            let basename = filename_path
                .file_stem()
                .and_then(|basename| basename.to_str())?;
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
pub fn parse_ignore(
    ignore_file_content: &str,
    duplicate_mode: DuplicateMode,
    read: bool,
) -> IgnoreMap {
    let mut ignore_map = IgnoreMap::default();
    let mut ignore_file_lines = ignore_file_content.lines();

    let Some(mut first_entry_comment) = ignore_file_lines.next() else {
        return ignore_map;
    };

    if read
        && duplicate_mode.is_remove()
        && !(first_entry_comment.contains("<#>System")
            || first_entry_comment.contains("<#>Scripts")
            || first_entry_comment.contains("<#>Plugins"))
    {
        // If duplicates are removed, we should group all ignore entries
        // that correspond to a single file into one ignore entry.
        first_entry_comment = &first_entry_comment
            [..unsafe { first_entry_comment.find(':').unwrap_unchecked() }];
    }

    ignore_map.reserve_exact(256);
    ignore_map.insert(
        first_entry_comment.to_string(),
        IgnoreEntry::with_capacity(128),
    );

    let mut ignore_entry =
        unsafe { ignore_map.last_mut().unwrap_unchecked().1 };

    for mut line in ignore_file_lines.filter(|line| !line.is_empty()) {
        if let Some(mid) = line.strip_prefix(IGNORE_ENTRY_COMMENT) {
            // If duplicates are allowed, we should group all ignore entries
            // that correspond to a single file into one ignore entry.
            if read
                && duplicate_mode.is_remove()
                && !(mid.starts_with("<#>System")
                    || mid.starts_with("<#>Scripts")
                    || mid.starts_with("<#>Plugins"))
            {
                line = &mid[..unsafe { mid.find(':').unwrap_unchecked() }];
            }

            ignore_map
                .entry(line.into())
                .or_insert(IgnoreEntry::with_capacity(128));
            ignore_entry =
                unsafe { ignore_map.last_mut().unwrap_unchecked().1 };
        } else {
            ignore_entry.insert_line(line);
        }
    }

    ignore_map
}

/// Extracts the game title from a `Game.ini` file's content.
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
    fn trim_bytes(bytes: &[u8]) -> &[u8] {
        let start = bytes.iter().position(|&b| !is_space(b)).unwrap_or(0);
        let end = bytes
            .iter()
            .rposition(|&b| !is_space(b))
            .map_or(0, |i| i + 1);
        &bytes[start..end]
    }

    fn is_space(b: u8) -> bool {
        b == b' ' || b == b'\t' || b == b'\r'
    }

    fn split_lines(data: &[u8]) -> SmallVec<[&[u8]; 4]> {
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

    for line in split_lines(ini_file_content) {
        if line.to_ascii_lowercase().starts_with(b"title") {
            if let Some(pos) = line.iter().position(|&b| b == b'=') {
                let right = &line[pos + 1..];
                let trimmed = trim_bytes(right);
                return Ok(trimmed.to_vec());
            }
        }
    }

    Err(Error::NoTitle)
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
pub fn get_system_title(
    mut system_file_content: &str,
) -> Result<String, Error> {
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
