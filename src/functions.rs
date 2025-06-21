use crate::{constants::*, types::*};
use std::{
    fs::{read_to_string, File},
    io::{BufReader, Read},
    path::{Path, PathBuf},
};

#[inline(always)]
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
pub fn parse_ignore(ignore_file_path: PathBuf) -> IgnoreMap {
    let mut map: IgnoreMap = IgnoreMap::default();

    if !ignore_file_path.exists() {
        return map;
    }

    let ignore_file_content: String =
        read_to_string(ignore_file_path).unwrap_log();

    for line in ignore_file_content.lines() {
        if line.is_empty() {
            continue;
        }

        if line.starts_with(FILE_ENTRY_PREFIX) {
            map.insert(line.to_owned(), IgnoreEntry::default());
        } else {
            map.last_mut().unwrap_log().1.insert(line.to_owned());
        }
    }

    map
}

#[inline]
/// This function is exactly similar to `std::fs::read_to_string`, but it doesn't include Byte Order Mark, if there's any.
pub fn read_to_string_without_bom<P: AsRef<Path>>(
    file_path: P,
) -> std::io::Result<String> {
    const BOM: [u8; 3] = [0xEF, 0xBB, 0xBF];

    let file: File = File::open(file_path.as_ref())?;
    let mut reader: BufReader<File> = BufReader::new(file);

    let mut buffer: [u8; 3] = [0u8; 3];
    let mut content: String = String::new();

    let read_bytes: usize = reader.read(&mut buffer)?;

    if read_bytes == 3 && buffer == BOM {
        reader.read_to_string(&mut content)?;
    } else {
        reader.seek_relative(-3)?;
        reader.read_to_string(&mut content)?;
    }

    Ok(content)
}
