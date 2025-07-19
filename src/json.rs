use crate::{constants::INSTANCE_VAR_PREFIX, get_engine_extension, types::*};
use marshal_rs::{Value, dump, load_utf8};
use serde_json::{from_str, to_string_pretty};
use std::{
    fs::{self, create_dir_all, read, read_dir, read_to_string},
    path::{Path, PathBuf},
};

/// Generates JSON representations of older engine files (`.rxdata`, `.rvdata`, `.rvdata2`).
///
/// # Arguments
/// - `source_path` - Path to the directory containing RPG Maker files.
/// - `output_path` - Path to the directory where `json` folder with `.json` files will be created.
/// - `read_mode` - In which [`ReadMode`] to generate.
///
/// # Returns
/// [`Vec`] of [`Result<Outcome, Error>`], where each `Result` marks the success/failure of generating a JSON representation.
///
/// # Example
/// ```
/// use rvpacker_txt_rs_lib::{json::generate, ReadMode};
/// let result = generate("C:/Game/Data", "C:/Game/json", ReadMode::Default, true);
/// ```
pub fn generate<P: AsRef<Path>>(
    source_path: P,
    output_path: P,
    read_mode: ReadMode,
    logging: bool,
) -> Results {
    if read_mode.is_append() {
        return vec![Err(Error::AppendModeIsNotSupported)].into();
    }

    if source_path.as_ref().join("System.json").exists() {
        return vec![Ok(Outcome::MVMZAlreadyJSON)].into();
    }

    let output_path = output_path.as_ref();

    let result = create_dir_all(output_path)
        .map_err(|err| Error::CreateDirFailed {
            path: output_path.to_path_buf(),
            err: err.to_string(),
        })
        .map(|_| Outcome::GeneratedJSON(PathBuf::new()));

    if result.is_err() {
        return vec![result].into();
    };

    let entries = read_dir(&source_path);

    let entries = match entries {
        Ok(entries) => entries,
        Err(err) => {
            return vec![Err(Error::ReadDirFailed {
                path: source_path.as_ref().to_path_buf(),
                err: err.to_string(),
            })]
            .into();
        }
    };

    entries
        .flatten()
        .filter_map(|entry| {
            let path = entry.path();
            let ext = path.extension()?.to_str()?;

            if ["rxdata", "rvdata", "rvdata2"].contains(&ext) {
                Some(entry)
            } else {
                None
            }
        })
        .map(|entry| {
            let output_file =
                PathBuf::from(entry.file_name()).with_extension("json");
            let output_file_path = output_path.join(&output_file);

            if output_file_path.exists() && !read_mode.is_force() {
                return Ok(Outcome::JSONAlreadyExist(output_file_path));
            }

            let path = entry.path();
            let content = read(&path).map_err(|err| Error::ReadFileFailed {
                file: path.clone(),
                err: err.to_string(),
            })?;
            let loaded =
                load_utf8(&content, INSTANCE_VAR_PREFIX).map_err(|err| {
                    Error::LoadFailed {
                        file: path.clone(),
                        err: err.to_string(),
                    }
                })?;

            // If `load` function succeeded, then returned Value is always stringifiable.
            let json_string =
                unsafe { to_string_pretty(&loaded).unwrap_unchecked() };

            let result = fs::write(output_file_path, json_string)
                .map_err(|err| Error::WriteFileFailed {
                    file: path.clone(),
                    err: err.to_string(),
                })
                .map(|_| Outcome::GeneratedJSON(path));

            if result.is_ok() && logging {
                println!("Generated JSON file: {}", output_file.display());
            }

            result
        })
        .collect::<Vec<_>>()
        .into()
}

/// Writes `.json` representations created with `generate_json` back to their initial format.
///
/// # Arguments
///
/// - `json_path` - Path to the directory containing `.json` representations.
/// - `output_path` - Path to the directory, where output files in initial format will be created.
/// - `engine_type` - Engine type, to properly write file extensions.
///
/// # Returns
/// [`Vec`] of [`Result<Outcome, Error>`], where each `Result` marks the success/failure of writing a file.
///
/// # Example
/// ```
/// use rvpacker_txt_rs_lib::{json::write, EngineType};
/// let result = write("C:/Game/json", "C:/Game/json-output", EngineType::VXAce, true);
/// ```
pub fn write<P: AsRef<Path>>(
    json_path: P,
    output_path: P,
    engine_type: EngineType,
    logging: bool,
) -> Results {
    let entries = read_dir(&json_path);

    let entries = match entries {
        Ok(entries) => entries,
        Err(err) => {
            return vec![Err(Error::ReadDirFailed {
                path: json_path.as_ref().to_path_buf(),
                err: err.to_string(),
            })]
            .into();
        }
    };

    let output_path = output_path.as_ref();
    let result =
        create_dir_all(output_path).map_err(|err| Error::CreateDirFailed {
            path: output_path.to_path_buf(),
            err: err.to_string(),
        });

    if let Err(err) = result {
        return vec![Err(err)].into();
    }

    entries
        .flatten()
        .map(|entry| {
            let path = entry.path();

            let content =
                read_to_string(&path).map_err(|err| Error::ReadFileFailed {
                    file: path.clone(),
                    err: err.to_string(),
                })?;

            let json = from_str::<Value>(&content).map_err(|err| {
                Error::JSONParseFailed {
                    file: path.clone(),
                    err: err.to_string(),
                }
            })?;

            let dumped = dump(json, None);

            let output_file = PathBuf::from(entry.file_name())
                .with_extension(get_engine_extension(engine_type));
            let output_file_path = output_path.join(&output_file);
            let result = fs::write(&output_file_path, dumped)
                .map_err(|err| Error::WriteFileFailed {
                    file: output_file_path,
                    err: err.to_string(),
                })
                .map(|_| Outcome::WrittenJSON(output_file.clone()));

            if result.is_ok() && logging {
                println!("Written file: {}", output_file.display())
            }

            result
        })
        .collect::<Vec<_>>()
        .into()
}
