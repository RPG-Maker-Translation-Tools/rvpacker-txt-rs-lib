use crate::{
    core::Base,
    get_line_separator, get_script_comment,
    types::{EngineType, Error, Scripts},
};
use marshal_rs::arena::Arena;
use rm2k::{
    engine::{Engine, SaveOpt},
    file as rm2k_file,
    rpg::{Database, Map as Rm2kMap, TreeMap},
};
use serde_json::{from_str, to_string_pretty};
use std::{
    fmt::Write,
    fs::{self, create_dir_all, read, read_dir, read_to_string},
    path::Path,
};

/// Generates JSON representation of RPG Maker data file (`rxdata`/`rvdata`/`rvdata2`), and returns the result, that can be converted back later with [`write_file`].
///
/// This function has special case for when `filename` starts with "Scripts" - it will generate a text representation of Ruby code.
///
/// # Parameters
///
/// - `file_content` - content of the RPG Maker data file.
/// - `filename` - name of the RPG Maker data file.
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
pub fn generate_file(file_content: &[u8], filename: &str) -> Result<String, Error> {
    if filename.starts_with("Scripts") {
        let arena = marshal_rs::load(file_content)?.into_owned();
        let scripts = Base::decode_scripts(&arena, arena.root(), None);

        Ok(scripts
            .numbers
            .into_iter()
            .zip(scripts.names)
            .zip(scripts.contents)
            .fold(String::new(), |mut result, ((a, b), c)| {
                let _ = write!(
                    result,
                    "{comment}{sep}{a}{sep}{b}\n{c}{end}",
                    comment = get_script_comment(),
                    sep = get_line_separator(),
                    c = c.replace("\r\n", "\n"),
                    end = if c.ends_with('\n') { "" } else { "\n" }
                );

                result
            }))
    } else {
        let arena = marshal_rs::load(file_content)?;
        Ok(unsafe { to_string_pretty(&arena).unwrap_unchecked() })
    }
}

/// Converts JSON representation of RPG Maker data file (`rxdata`/`rvdata`/`rvdata2`) created with [`generate_file`] back to initial form.
///
/// # Parameters
///
/// - `file_content` - content of the JSON file created with [`generate_file`].
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
    let arena = from_str::<Arena<'static>>(file_content)?;
    Ok(marshal_rs::dump(&arena))
}

/// Generates a JSON representation of an RPG Maker 2000/2003 `RPG_RT.ldb` database
/// file, and returns the result, that can be converted back later with
/// [`write_rm2k_database_file`].
///
/// # Parameters
///
/// - `file_content` - content of the `RPG_RT.ldb` file.
///
/// # Returns
///
/// - [`String`] JSON representation of `file_content` if successful.
/// - [`Error`] otherwise.
///
/// # Errors
///
/// - [`Error::Rm2kLoad`] - if unable to load the LCF data.
#[must_use = "the JSON representation is discarded if not used"]
pub fn generate_rm2k_database_file(file_content: &[u8]) -> Result<String, Error> {
    let loaded = rm2k_file::load_database(file_content)?;
    Ok(unsafe { to_string_pretty(&loaded.value).unwrap_unchecked() })
}

/// Converts a JSON representation created with [`generate_rm2k_database_file`] back
/// to an `RPG_RT.ldb` database file.
///
/// The build (2000 vs. 2003) is read off the parsed database's own
/// `system.ldb_id`, the same way [`crate::core::Base::set_rm2k_engine`]'s callers
/// derive it - there is no separate parameter for it here.
///
/// # Parameters
///
/// - `file_content` - content of the JSON file created with
///   [`generate_rm2k_database_file`].
///
/// # Returns
///
/// - [`Vec<u8>`] LCF data of `file_content`.
/// - [`Error`] otherwise.
///
/// # Errors
///
/// - [`Error::JsonParse`] - if parsing `file_content` JSON fails.
pub fn write_rm2k_database_file(file_content: &str) -> Result<Vec<u8>, Error> {
    let database = from_str::<Database<'static>>(file_content)?;
    let engine = Engine::from_ldb_id(database.system.ldb_id);

    Ok(rm2k_file::to_vec(|out| {
        rm2k_file::save_database(&database, out, engine, SaveOpt::default(), b"")
    }))
}

/// Generates a JSON representation of an RPG Maker 2000/2003 `RPG_RT.lmt` map tree
/// file, and returns the result, that can be converted back later with
/// [`write_rm2k_tree_map_file`].
///
/// # Parameters
///
/// - `file_content` - content of the `RPG_RT.lmt` file.
///
/// # Returns
///
/// - [`String`] JSON representation of `file_content` if successful.
/// - [`Error`] otherwise.
///
/// # Errors
///
/// - [`Error::Rm2kLoad`] - if unable to load the LCF data.
#[must_use = "the JSON representation is discarded if not used"]
pub fn generate_rm2k_tree_map_file(file_content: &[u8]) -> Result<String, Error> {
    let loaded = rm2k_file::load_tree_map(file_content)?;
    Ok(unsafe { to_string_pretty(&loaded.value).unwrap_unchecked() })
}

/// Converts a JSON representation created with [`generate_rm2k_tree_map_file`] back
/// to an `RPG_RT.lmt` map tree file.
///
/// `TreeMap` carries no build indicator of its own (unlike the database's
/// `system.ldb_id`), so the caller passes it explicitly - the same [`Engine`]
/// [`crate::core::Base::set_rm2k_engine`] was given for this project.
///
/// # Parameters
///
/// - `file_content` - content of the JSON file created with
///   [`generate_rm2k_tree_map_file`].
/// - `engine` - which RPG Maker 2000/2003 build to target.
///
/// # Returns
///
/// - [`Vec<u8>`] LCF data of `file_content`.
/// - [`Error`] otherwise.
///
/// # Errors
///
/// - [`Error::JsonParse`] - if parsing `file_content` JSON fails.
pub fn write_rm2k_tree_map_file(file_content: &str, engine: Engine) -> Result<Vec<u8>, Error> {
    let tree = from_str::<TreeMap<'static>>(file_content)?;

    Ok(rm2k_file::to_vec(|out| {
        rm2k_file::save_tree_map(&tree, out, engine, SaveOpt::default(), b"")
    }))
}

/// Generates a JSON representation of one RPG Maker 2000/2003 `MapNNNN.lmu` map
/// file, and returns the result, that can be converted back later with
/// [`write_rm2k_map_file`].
///
/// # Parameters
///
/// - `file_content` - content of the `MapNNNN.lmu` file.
///
/// # Returns
///
/// - [`String`] JSON representation of `file_content` if successful.
/// - [`Error`] otherwise.
///
/// # Errors
///
/// - [`Error::Rm2kLoad`] - if unable to load the LCF data.
#[must_use = "the JSON representation is discarded if not used"]
pub fn generate_rm2k_map_file(file_content: &[u8]) -> Result<String, Error> {
    let loaded = rm2k_file::load_map(file_content)?;
    Ok(unsafe { to_string_pretty(&loaded.value).unwrap_unchecked() })
}

/// Converts a JSON representation created with [`generate_rm2k_map_file`] back to a
/// `MapNNNN.lmu` map file.
///
/// See [`write_rm2k_tree_map_file`] for why `engine` is a separate parameter here.
///
/// # Parameters
///
/// - `file_content` - content of the JSON file created with [`generate_rm2k_map_file`].
/// - `engine` - which RPG Maker 2000/2003 build to target.
///
/// # Returns
///
/// - [`Vec<u8>`] LCF data of `file_content`.
/// - [`Error`] otherwise.
///
/// # Errors
///
/// - [`Error::JsonParse`] - if parsing `file_content` JSON fails.
pub fn write_rm2k_map_file(file_content: &str, engine: Engine) -> Result<Vec<u8>, Error> {
    let map = from_str::<Rm2kMap<'static>>(file_content)?;

    Ok(rm2k_file::to_vec(|out| {
        rm2k_file::save_map(&map, out, engine, SaveOpt::default(), b"")
    }))
}

/// Generates JSON representations of older engine files (`.rxdata`, `.rvdata`, `.rvdata2`).
///
/// This function uses [`generate_file`] under the hood, and manages all system calls for you.
///
/// If `force` argument is not set, skips processing already existing files.
///
/// # Parameters
///
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
///
/// ```no_run
/// use rvpacker_txt_rs_lib::{json::generate, Error};
///
/// fn main() -> Result<(), Error> {
///     let result = generate("C:/Game/Data", "C:/Game/json", false)?;
///     Ok(())
/// }
/// ```
pub fn generate<P: AsRef<Path>>(source_path: P, output_path: P, force: bool) -> Result<(), Error> {
    create_dir_all(&output_path).map_err(|e| Error::Io(output_path.as_ref().to_path_buf(), e))?;

    for entry in read_dir(source_path.as_ref())
        .map_err(|e| Error::Io(source_path.as_ref().to_path_buf(), e))?
        .flatten()
    {
        let filename = entry.file_name();
        let mut output_file_path = output_path.as_ref().join(Path::new(&filename).with_extension("json"));

        if !force && output_file_path.exists() {
            log::info!(
                "{}: File already exists. Use force mode to overwrite.",
                output_file_path.display()
            );
            continue;
        }

        let path = entry.path();
        let content = read(&path).map_err(|e| Error::Io(path, e))?;

        let filename_str = filename.to_string_lossy();

        if filename_str.starts_with("Scripts") {
            output_file_path.set_extension("rb");
        }

        let output_content = generate_file(&content, filename_str.as_ref())?;

        fs::write(&output_file_path, output_content).map_err(|e| Error::Io(output_file_path, e))?;

        log::info!("{}: Successfully generated JSON.", Path::new(&filename).display());
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
///
/// ```no_run
/// use rvpacker_txt_rs_lib::{json::write, EngineType, Error};
///
/// fn main() -> Result<(), Error> {
///     let result = write("C:/Game/json", "C:/Game/json-output", EngineType::VXAce);
///     Ok(())
/// }
/// ```
pub fn write<P: AsRef<Path>>(json_path: P, output_path: P, engine_type: EngineType) -> Result<(), Error> {
    create_dir_all(&output_path).map_err(|e| Error::Io(output_path.as_ref().to_path_buf(), e))?;

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
        let output_file_path = output_path
            .as_ref()
            .join(Path::new(&filename).with_extension(engine_type.extension()));

        let written = if filename == "Scripts.rb" {
            let mut scripts = Scripts::new(
                Vec::with_capacity(256),
                Vec::with_capacity(256),
                Vec::with_capacity(256),
            );

            let mut prev_content_start = 0;
            let mut read = 0;

            for script_line in content.split_inclusive('\n') {
                if script_line.starts_with(get_script_comment()) {
                    let header = unsafe {
                        script_line
                            .strip_prefix(get_script_comment())
                            .unwrap_unchecked()
                            .strip_prefix(get_line_separator())
                            .unwrap_unchecked()
                            .trim_end_matches('\n')
                    };

                    let (magic_number, name) = unsafe { header.split_once(get_line_separator()).unwrap_unchecked() };

                    scripts
                        .numbers
                        .push(unsafe { magic_number.parse::<i32>().unwrap_unchecked() });
                    scripts.names.push(name.to_string());

                    if prev_content_start != 0 {
                        scripts.contents.push(content[prev_content_start..read].to_string());
                    }

                    prev_content_start = read + script_line.len();
                }

                read += script_line.len();
            }

            if prev_content_start != 0 && prev_content_start < content.len() {
                scripts.contents.push(content[prev_content_start..].to_string());
            }

            marshal_rs::dump(&Base::encode_scripts(&scripts))
        } else {
            write_file(&content)?
        };

        fs::write(&output_file_path, written).map_err(|e| Error::Io(output_file_path, e))?;

        log::info!("{}: Successfully written.", Path::new(&filename).display());
    }

    Ok(())
}

/// Generates JSON representations of an RPG Maker 2000/2003 project's `RPG_RT.ldb`,
/// `RPG_RT.lmt` and `MapNNNN.lmu` files.
///
/// This is [`generate`]'s RM2K counterpart, using [`generate_rm2k_database_file`],
/// [`generate_rm2k_tree_map_file`] and [`generate_rm2k_map_file`] under the hood -
/// three different `rm2k-lib` structs instead of one shared Marshal format, unlike
/// the other engines. Each file's JSON keeps its original extension ahead of the
/// added `.json` (`RPG_RT.ldb.json`, not `RPG_RT.json`) - `RPG_RT.ldb` and
/// `RPG_RT.lmt` share a stem and would otherwise collide.
///
/// If `force` isn't set, skips a file whose JSON representation already exists.
///
/// # Parameters
///
/// - `source_path` - Path to the directory containing the RM2K project's files.
/// - `output_path` - Path to the directory where the `.json` files will be created.
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
/// - [`Error::Rm2kLoad`], if loading any LCF file fails.
pub fn generate_rm2k<P: AsRef<Path>>(source_path: P, output_path: P, force: bool) -> Result<(), Error> {
    let source_path = source_path.as_ref();
    let output_path = output_path.as_ref();

    create_dir_all(output_path).map_err(|e| Error::Io(output_path.to_path_buf(), e))?;

    let write_json = |filename: &str, json: String| -> Result<(), Error> {
        let output_file_path = output_path.join(format!("{filename}.json"));

        if !force && output_file_path.exists() {
            log::info!(
                "{}: File already exists. Use force mode to overwrite.",
                output_file_path.display()
            );

            return Ok(());
        }

        fs::write(&output_file_path, json).map_err(|e| Error::Io(output_file_path, e))?;
        log::info!("{filename}: Successfully generated JSON.");

        Ok(())
    };

    let ldb_path = source_path.join("RPG_RT.ldb");

    if ldb_path.exists() {
        let content = read(&ldb_path).map_err(|e| Error::Io(ldb_path, e))?;
        write_json("RPG_RT.ldb", generate_rm2k_database_file(&content)?)?;
    }

    let lmt_path = source_path.join("RPG_RT.lmt");

    if lmt_path.exists() {
        let content = read(&lmt_path).map_err(|e| Error::Io(lmt_path, e))?;
        write_json("RPG_RT.lmt", generate_rm2k_tree_map_file(&content)?)?;
    }

    for entry in read_dir(source_path)
        .map_err(|e| Error::Io(source_path.to_path_buf(), e))?
        .flatten()
    {
        let filename = entry.file_name();
        let filename_str = filename.to_string_lossy();

        if !Path::new(filename_str.as_ref())
            .extension()
            .is_some_and(|ext| ext == "lmu")
        {
            continue;
        }

        let path = entry.path();
        let content = read(&path).map_err(|e| Error::Io(path, e))?;
        write_json(&filename_str, generate_rm2k_map_file(&content)?)?;
    }

    Ok(())
}

/// Writes `.json` representations created with [`generate_rm2k`] back to their
/// original LCF format (`RPG_RT.ldb`, `RPG_RT.lmt`, `MapNNNN.lmu`).
///
/// Which file gets read first doesn't matter to the caller: `RPG_RT.ldb.json` is
/// read first if present, so `RPG_RT.lmt`/`MapNNNN.lmu` get built for the project's
/// actual [`Engine`] (2000 vs. 2003) - the same value
/// [`crate::core::Base::set_rm2k_engine`]'s callers derive it from. With no
/// database JSON to read (a selective conversion missing `RPG_RT.ldb.json`), they
/// fall back to [`Engine::R2K`], matching this crate's own default when the engine
/// hasn't been determined yet.
///
/// # Parameters
///
/// - `json_path` - Path to the directory containing the `.json` representations.
/// - `output_path` - Path to the directory where the rebuilt LCF files will be created.
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
pub fn write_rm2k<P: AsRef<Path>>(json_path: P, output_path: P) -> Result<(), Error> {
    let json_path = json_path.as_ref();
    let output_path = output_path.as_ref();

    create_dir_all(output_path).map_err(|e| Error::Io(output_path.to_path_buf(), e))?;

    let write_bytes = |filename: &str, bytes: Vec<u8>| -> Result<(), Error> {
        let output_file_path = output_path.join(filename);
        fs::write(&output_file_path, bytes).map_err(|e| Error::Io(output_file_path, e))?;
        log::info!("{filename}: Successfully written.");
        Ok(())
    };

    let mut engine = Engine::R2K;

    let ldb_json_path = json_path.join("RPG_RT.ldb.json");

    if ldb_json_path.exists() {
        let content = read_to_string(&ldb_json_path).map_err(|e| Error::Io(ldb_json_path, e))?;
        let database = from_str::<Database<'static>>(&content)?;
        engine = Engine::from_ldb_id(database.system.ldb_id);

        let bytes = rm2k_file::to_vec(|out| rm2k_file::save_database(&database, out, engine, SaveOpt::default(), b""));

        write_bytes("RPG_RT.ldb", bytes)?;
    }

    let lmt_json_path = json_path.join("RPG_RT.lmt.json");

    if lmt_json_path.exists() {
        let content = read_to_string(&lmt_json_path).map_err(|e| Error::Io(lmt_json_path, e))?;
        write_bytes("RPG_RT.lmt", write_rm2k_tree_map_file(&content, engine)?)?;
    }

    for entry in read_dir(json_path)
        .map_err(|e| Error::Io(json_path.to_path_buf(), e))?
        .flatten()
    {
        let filename = entry.file_name();
        let filename_str = filename.to_string_lossy();

        let Some(stem) = filename_str.strip_suffix(".lmu.json") else {
            continue;
        };

        let path = entry.path();
        let content = read_to_string(&path).map_err(|e| Error::Io(path, e))?;

        write_bytes(&format!("{stem}.lmu"), write_rm2k_map_file(&content, engine)?)?;
    }

    Ok(())
}
