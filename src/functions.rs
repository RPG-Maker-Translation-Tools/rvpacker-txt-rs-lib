use crate::{constants::*, types::*};
use gxhash::HashSetExt;
use std::{
    fs::{File, read_to_string},
    io::{BufReader, Read},
    path::{Path, PathBuf},
};

/// Returns the RPG Maker file extension that corresponds to given `EngineType`.
pub const fn get_engine_extension(engine_type: EngineType) -> &'static str {
    match engine_type {
        EngineType::New => "json",
        EngineType::VXAce => "rvdata2",
        EngineType::VX => "rvdata",
        EngineType::XP => "rxdata",
    }
}

/// Parses ignore file contents to `IgnoreMap`.
///
/// # Arguments
/// - `ignore_file_path` - Path to the `.rvpacker-ignore` file.
/// - `duplicate_mode` - [`DuplicateMode`], which was used during read.
/// - `read` - Parse for reading or purging.
///
/// # Returns
/// - `Ok(IgnoreMap)` if everything went fine.
/// - `Err(Error::ReadFileFailed)` if cannot read file.
pub fn parse_ignore(
    ignore_file_path: PathBuf,
    duplicate_mode: DuplicateMode,
    read: bool,
) -> Result<IgnoreMap, Error> {
    let mut ignore_map = IgnoreMap::default();

    if !ignore_file_path.exists() {
        return Ok(ignore_map);
    }

    let ignore_file_content =
        read_to_string(&ignore_file_path).map_err(|err| {
            Error::ReadFileFailed {
                file: ignore_file_path,
                err: err.to_string(),
            }
        })?;

    let mut ignore_file_lines = ignore_file_content.lines();

    let Some(mut first_entry_comment) = ignore_file_lines.next() else {
        return Ok(ignore_map);
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

    ignore_map.reserve(256);
    ignore_map
        .insert(first_entry_comment.into(), IgnoreEntry::with_capacity(128));

    let mut ignore_entry =
        unsafe { ignore_map.last_mut().unwrap_unchecked().1 };

    for mut line in ignore_file_content.lines() {
        if line.is_empty() {
            continue;
        }

        if line.starts_with(IGNORE_ENTRY_COMMENT) {
            // If duplicates are allowed, we should group all ignore entries
            // that correspond to a single file into one ignore entry.
            if read
                && duplicate_mode.is_remove()
                && !(line.contains("<#>System")
                    || line.contains("<#>Scripts")
                    || line.contains("<#>Plugins"))
            {
                line = &line[..unsafe { line.find(':').unwrap_unchecked() }];
            }

            ignore_map
                .entry(line.into())
                .or_insert(IgnoreEntry::with_capacity(128));
            ignore_entry =
                unsafe { ignore_map.last_mut().unwrap_unchecked().1 };
        } else {
            ignore_entry.insert(line.into());
        }
    }

    Ok(ignore_map)
}

/// This function is exactly similar to `std::fs::read_to_string`, but it doesn't include Byte Order Mark, if there's any.
pub fn read_to_string_without_bom<P: AsRef<Path>>(
    file_path: P,
) -> std::io::Result<String> {
    const BOM: [u8; 3] = [0xEF, 0xBB, 0xBF];

    let file = File::open(file_path.as_ref())?;
    let mut reader = BufReader::new(file);

    let mut buffer = [0u8; 3];
    let mut content = String::new();

    let read_bytes = reader.read(&mut buffer)?;

    if read_bytes == 3 && buffer == BOM {
        reader.read_to_string(&mut content)?;
    } else {
        reader.seek_relative(-3)?;
        reader.read_to_string(&mut content)?;
    }

    Ok(content)
}
