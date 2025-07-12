use crate::{
    constants::INSTANCE_VAR_PREFIX,
    types::{OptionExt, ReadMode, ResultExt},
};
use marshal_rs::{Value, dump, load_utf8};
use serde_json::{from_str, to_string, to_string_pretty};
use std::{
    fs::{create_dir_all, read, read_dir, read_to_string, write},
    path::{Path, PathBuf},
};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum GenerateError {
    #[error("Append mode is not supported.")]
    AppendModeIsNotSupported,
    #[error(
        "JSON files already exist. Set `read_mode` to `Force` to proceed anyway."
    )]
    JSONAlreadyExist,
    #[error("Cannot generate JSON for RPG Maker MV/MZ, it's already JSON.")]
    CannotGenerateJSON,
}

// TODO: Documentation

pub fn generate_json<P: AsRef<Path>>(
    source_path: P,
    output_path: P,
    read_mode: ReadMode,
) -> Result<(), GenerateError> {
    if read_mode.is_append() {
        return Err(GenerateError::AppendModeIsNotSupported);
    }

    if source_path.as_ref().join("System.json").exists() {
        return Err(GenerateError::CannotGenerateJSON);
    }

    let output_path = output_path.as_ref();
    let json_output_path = output_path.join("json");

    if json_output_path.exists() && !read_mode.is_force() {
        return Err(GenerateError::JSONAlreadyExist);
    }

    for entry in read_dir(source_path).unwrap_log().flatten() {
        let path = entry.path();
        let extension = path.extension().unwrap_log();

        for ext in ["rxdata", "rvdata", "rvdata2"] {
            if extension != ext {
                continue;
            }
        }

        let json = to_string(
            &load_utf8(&read(entry.path()).unwrap_log(), INSTANCE_VAR_PREFIX)
                .unwrap_log(),
        )
        .unwrap_log();

        create_dir_all(&json_output_path).unwrap_log();

        write(
            json_output_path
                .join(PathBuf::from(entry.file_name()).with_extension("json")),
            unsafe { to_string_pretty(&json).unwrap_unchecked() },
        )
        .unwrap_log();
    }

    Ok(())
}

pub fn write_json<P: AsRef<Path>>(root_dir: P) {
    let json_dir = root_dir.as_ref().join("json");

    for entry in read_dir(json_dir).unwrap_log().flatten() {
        let json: Value =
            from_str(&read_to_string(entry.path()).unwrap_log()).unwrap_log();
        let dumped = dump(json, None);
        write(root_dir.as_ref().join("written"), dumped).unwrap_log();
    }
}
