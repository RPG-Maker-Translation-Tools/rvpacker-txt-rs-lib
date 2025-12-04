use crate::{
    constants::INSTANCE_VAR_PREFIX,
    core::{ScriptBase, get_engine_extension},
    types::{EngineType, Error, Scripts},
};
use marshal_rs::{Value, dump, load_binary, load_utf8};
use serde_json::{from_str, to_string_pretty};
use std::{
    fmt::Write,
    fs::{self, create_dir_all, read, read_dir, read_to_string},
    path::Path,
};

/// Generates JSON representation of RPG Maker data file (`rxdata`/`rvdata`/`rvdata2`), and returns the result, that can be converted back later with [`write_file`].
///
/// # Returns
///
/// - [`String`] JSON representation of `file_content` RPG Maker file if successful.
/// - [`Error`] otherwise.
///
/// # Errors
///
/// - [`Error::MarshalLoad`] - if parsing `file_content` Marshal data fails.
///
pub fn generate_file(file_content: &[u8]) -> Result<String, Error> {
    let loaded = load_utf8(file_content, INSTANCE_VAR_PREFIX)?;
    Ok(unsafe { to_string_pretty(&loaded).unwrap_unchecked() })
}

/// Converts JSON representation of RPG Maker data file (`rxdata`/`rvdata`/`rvdata2`) created with [`generate_file`] back to initial form.
///
/// # Returns
///
/// - [`Vec<u8>`]  Marshal data of `file_content` JSON content.
/// - [`Error`] otherwise.
///
/// # Errors
///
/// - [`Error::JsonParse`] - if parsing `file_content` JSON fails.
///
pub fn write_file(file_content: &str) -> Result<Vec<u8>, Error> {
    let json = from_str::<Value>(file_content)?;
    Ok(dump(json, None))
}

/// Generates JSON representations of older engine files (`.rxdata`, `.rvdata`, `.rvdata2`).
///
/// This function uses [`generate_file`] under the hood, and manages all system calls for you.
///
/// If `force` argument is not set, skips processing already existing files.
///
/// # Parameters
/// - `source_path` - Path to the directory containing RPG Maker files.
/// - `output_path` - Path to the directory where `json` folder with `.json` files will be created.
/// - `force` - Whether to overwrite existing JSON representations.
///
/// # Returns
///
/// - Nothing if successful.
/// - [`Error`] otherwise.
///
/// # Errors
///
/// - [`Error::Io`], if any I/O operation fails.
/// - [`Error::MarshalLoad`], if deserializing RPG Maker file fails.
///
/// # Example
/// ```no_run
/// use rvpacker_txt_rs_lib::{json::generate, Error};
///
/// fn main() -> Result<(), Error> {
///     let result = generate("C:/Game/Data", "C:/Game/json", false)?;
///     Ok(())
/// }
/// ```
pub fn generate<P: AsRef<Path>>(
    source_path: P,
    output_path: P,
    force: bool,
) -> Result<(), Error> {
    create_dir_all(&output_path)
        .map_err(|e| Error::Io(output_path.as_ref().to_path_buf(), e))?;

    for entry in read_dir(source_path.as_ref())
        .map_err(|e| Error::Io(source_path.as_ref().to_path_buf(), e))?
        .flatten()
    {
        let filename = entry.file_name();
        let mut output_file_path = output_path
            .as_ref()
            .join(Path::new(&filename).with_extension("json"));

        if !force && output_file_path.exists() {
            continue;
        }

        let path = entry.path();
        let content = read(&path).map_err(|e| Error::Io(path, e))?;

        let output_content =
            if (unsafe { output_file_path.file_stem().unwrap_unchecked() }
                == "Scripts")
            {
                let scripts_array = unsafe {
                    load_binary(&content, INSTANCE_VAR_PREFIX)?
                        .into_array()
                        .unwrap_unchecked()
                };

                let scripts = ScriptBase::decode_scripts(&scripts_array);

                output_file_path.set_extension("rb");

                scripts
                    .numbers
                    .into_iter()
                    .zip(scripts.names)
                    .zip(scripts.contents)
                    .fold(String::new(), |mut result, ((a, b), c)| {
                        let _ = write!(
                            result,
                            "{a}<##>\n{b}<##>\n{c}\n<###>\n",
                            c = c.replace("\r\n", "\n")
                        );

                        result
                    })
            } else {
                generate_file(&content)?
            };

        fs::write(&output_file_path, output_content)
            .map_err(|e| Error::Io(output_file_path, e))?;

        log::info!(
            "{}: Successfully generated JSON.",
            Path::new(&filename).display()
        );
    }

    Ok(())
}

/// Writes `.json` representations created with [`generate`] back to their initial format.
///
/// This function uses [`write_file`] under the hood, and manages all system calls for you.
///
/// # Parameters
///
/// - `json_path` - Path to the directory containing `.json` representations.
/// - `output_path` - Path to the directory, where output files in initial format will be created.
/// - `engine_type` - Engine type, to properly write file extensions.
///
/// # Returns
///
/// - Nothing if successful.
/// - [`Error`] otherwise.
///
/// # Errors
///
/// - [`Error::Io`], if any I/O operation fails.
/// - [`Error::JsonParse`] - if parsing any JSON fails.
///
/// # Example
/// ```no_run
/// use rvpacker_txt_rs_lib::{json::write, EngineType, Error};
///
/// fn main() -> Result<(), Error> {
///     let result = write("C:/Game/json", "C:/Game/json-output", EngineType::VXAce);
///     Ok(())
/// }
/// ```
pub fn write<P: AsRef<Path>>(
    json_path: P,
    output_path: P,
    engine_type: EngineType,
) -> Result<(), Error> {
    create_dir_all(&output_path)
        .map_err(|e| Error::Io(output_path.as_ref().to_path_buf(), e))?;

    for entry in read_dir(json_path.as_ref())
        .map_err(|e| Error::Io(json_path.as_ref().to_path_buf(), e))?
        .flatten()
        .filter(|x| {
            Path::new(&x.file_name())
                .extension()
                .is_some_and(|ext| ext == "json" || ext == "rb")
        })
    {
        let path = entry.path();
        let content = read_to_string(&path).map_err(|e| Error::Io(path, e))?;

        let filename = entry.file_name();
        let output_file_path = output_path.as_ref().join(
            Path::new(&filename)
                .with_extension(get_engine_extension(engine_type)),
        );

        let written = if filename == "Scripts.rb" {
            let mut scripts = Scripts::new(
                Vec::with_capacity(256),
                Vec::with_capacity(256),
                Vec::with_capacity(256),
            );

            for script in content.split("\n<###>\n") {
                let mut split = script.split("<##>\n");

                let number = unsafe {
                    split
                        .next()
                        .unwrap_unchecked()
                        .parse::<i32>()
                        .unwrap_unchecked()
                };
                let name = unsafe { split.next().unwrap_unchecked() };
                let content = unsafe { split.next().unwrap_unchecked() };

                scripts.numbers.push(number);
                scripts.contents.push(content.to_string());
                scripts.names.push(name.to_string());
            }

            dump(Value::array(ScriptBase::encode_scripts(&scripts)), None)
        } else {
            write_file(&content)?
        };

        fs::write(&output_file_path, written)
            .map_err(|e| Error::Io(output_file_path, e))?;

        log::info!("{}: Successfully written.", Path::new(&filename).display());
    }

    Ok(())
}
