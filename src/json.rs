use crate::{
    core::Base,
    get_line_separator, get_script_comment,
    processors::write_data_file,
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
    fs::{create_dir_all, read, read_dir, read_to_string},
    path::Path,
};

/// A script as [`generate_file`] wrote it, without the newline that ends it.
fn script_body(text: &str) -> String {
    text.strip_suffix('\n').unwrap_or(text).to_owned()
}

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
/// - [`Error::InvalidScripts`] - if `filename` starts with "Scripts", and `file_content` is not a valid scripts file.
///
pub fn generate_file(
    file_content: &[u8],
    filename: &str,
    read_encoding: Option<&'static encoding_rs::Encoding>,
) -> Result<String, Error> {
    if filename.starts_with("Scripts") {
        let arena = marshal_rs::load(file_content)?.into_owned();
        let scripts = Base::decode_scripts(&arena, arena.root(), read_encoding)?;

        Ok(scripts
            .numbers
            .into_iter()
            .zip(scripts.names)
            .zip(scripts.contents)
            .fold(String::new(), |mut result, ((a, b), c)| {
                // A newline of its own ends every script, so that [`write`] gives back exactly what it was.
                let _ = write!(
                    result,
                    "{comment}{sep}{a}{sep}{b}\n{c}\n",
                    comment = get_script_comment(),
                    sep = get_line_separator(),
                );

                result
            }))
    } else if filename.starts_with("Save") {
        // A save is a series of `Marshal.dump` streams in one file, not one.
        let arenas = marshal_rs::load_many(file_content)?;
        Ok(to_string_pretty(&arenas)?)
    } else {
        let arena = marshal_rs::load(file_content)?;
        Ok(to_string_pretty(&arena)?)
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
    // A save is a series of `Marshal.dump` streams, which `generate_file` writes as an array of arenas.
    if file_content.trim_start().starts_with('[') {
        let arenas = from_str::<Vec<Arena<'static>>>(file_content)?;
        return Ok(arenas.iter().flat_map(marshal_rs::dump).collect());
    }

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
    Ok(to_string_pretty(&loaded.value)?)
}

/// Generates a JSON representation of an RPG Maker 2000/2003 save file (`SaveNN.lsd`). Unlike the other RM2K files, it
/// is for looking at only: there is no way back.
///
/// # Errors
///
/// - [`Error::Rm2kLoad`] - if unable to load the LCF data.
#[must_use = "the JSON representation is discarded if not used"]
pub fn generate_rm2k_save_file(file_content: &[u8]) -> Result<String, Error> {
    let loaded = rm2k_file::load_save(file_content)?;
    Ok(to_string_pretty(&loaded.value)?)
}

/// Converts a JSON representation created with [`generate_rm2k_database_file`] back
/// to an `RPG_RT.ldb` database file.
///
/// The build (2000 vs. 2003) comes from the database's `system.ldb_id`.
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
    Ok(to_string_pretty(&loaded.value)?)
}

/// Converts a JSON representation created with [`generate_rm2k_tree_map_file`] back
/// to an `RPG_RT.lmt` map tree file.
///
/// `TreeMap` has no build indicator, so the caller passes `engine`.
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
    Ok(to_string_pretty(&loaded.value)?)
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
/// - `read_encoding` - Codepage to decode scripts in, UTF-8 if [`None`]. See [`generate_file`].
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
///     let result = generate("C:/Game/Data", "C:/Game/json", false, None)?;
///     Ok(())
/// }
/// ```
pub fn generate<P: AsRef<Path>, Q: AsRef<Path>>(
    source_path: P,
    output_path: Q,
    force: bool,
    read_encoding: Option<&'static encoding_rs::Encoding>,
) -> Result<(), Error> {
    create_dir_all(&output_path).map_err(|e| Error::Io(output_path.as_ref().to_path_buf(), e))?;

    for entry in read_dir(source_path.as_ref())
        .map_err(|e| Error::Io(source_path.as_ref().to_path_buf(), e))?
        .flatten()
    {
        let filename = entry.file_name();

        // Only the engines' data files: anything else in the folder (`desktop.ini`, a backup folder) isn't Marshal.
        if !entry.file_type().is_ok_and(|kind| kind.is_file())
            || !Path::new(&filename)
                .extension()
                .is_some_and(|ext| ext == "rxdata" || ext == "rvdata" || ext == "rvdata2")
        {
            log::info!("{}: Not a data file. Skipped.", Path::new(&filename).display());
            continue;
        }

        let is_scripts = filename.to_string_lossy().starts_with("Scripts");

        // Scripts become Ruby source, and it is that file the existing-output check has to look at.
        let output_file_path = output_path
            .as_ref()
            .join(Path::new(&filename).with_extension(if is_scripts { "rb" } else { "json" }));

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
        let output_content = generate_file(&content, filename_str.as_ref(), read_encoding)?;

        write_data_file(&output_file_path, output_content)?;

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
/// - `engine_type` - Engine type, to properly write file extensions. Not [`EngineType::RM2K`], which has
///   [`write_rm2k`].
/// - `write_encoding` - Codepage to encode scripts in, UTF-8 if [`None`].
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
/// - [`Error::UnsupportedEngine`] - if `engine_type` is [`EngineType::RM2K`].
/// - [`Error::UnsupportedWriteEncoding`]/[`Error::UnmappableText`] - if a script can't be encoded in `write_encoding`.
///
/// # Example
///
/// ```no_run
/// use rvpacker_txt_rs_lib::{json::write, EngineType, Error};
///
/// fn main() -> Result<(), Error> {
///     let result = write("C:/Game/json", "C:/Game/json-output", EngineType::VXAce, None);
///     Ok(())
/// }
/// ```
pub fn write<P: AsRef<Path>, Q: AsRef<Path>>(
    json_path: P,
    output_path: Q,
    engine_type: EngineType,
    write_encoding: Option<&'static encoding_rs::Encoding>,
) -> Result<(), Error> {
    if engine_type == EngineType::RM2K {
        return Err(Error::UnsupportedEngine(engine_type.to_str()));
    }

    Base::check_write_encoding(write_encoding)?;

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
                    let header = script_line[get_script_comment().len()..]
                        .strip_prefix(get_line_separator())
                        .ok_or(Error::InvalidScripts("a script header has no magic number or name"))?
                        .trim_end_matches(['\r', '\n']);

                    let (magic_number, name) = header
                        .split_once(get_line_separator())
                        .ok_or(Error::InvalidScripts("a script header has no magic number or name"))?;

                    scripts.numbers.push(
                        magic_number
                            .parse::<i32>()
                            .map_err(|_| Error::InvalidScripts("a magic number is not an integer"))?,
                    );
                    scripts.names.push(name.to_string());

                    if prev_content_start != 0 {
                        scripts.contents.push(script_body(&content[prev_content_start..read]));
                    }

                    prev_content_start = read + script_line.len();
                }

                read += script_line.len();
            }

            // Even when empty, so an empty last script keeps its entry.
            if prev_content_start != 0 {
                scripts.contents.push(script_body(&content[prev_content_start..]));
            }

            marshal_rs::dump(&Base::encode_scripts_for(&scripts, engine_type, write_encoding)?)
        } else {
            write_file(&content)?
        };

        write_data_file(&output_file_path, written)?;

        log::info!("{}: Successfully written.", Path::new(&filename).display());
    }

    Ok(())
}

/// Generates JSON representations of an RPG Maker 2000/2003 project's `RPG_RT.ldb`,
/// `RPG_RT.lmt` and `MapNNNN.lmu` files.
///
/// RM2K counterpart of [`generate`]. Each JSON keeps the original extension before `.json` (`RPG_RT.ldb.json`), since
/// `RPG_RT.ldb` and `RPG_RT.lmt` share a stem.
///
/// If `force` isn't set, skips a file whose JSON representation already exists.
///
/// # Parameters
///
/// - `source_path` - Path to the directory containing the RM2K project's files.
/// - `output_path` - Path to the directory where the `.json` files will be created.
/// - `force` - Whether to overwrite existing JSON representations.
/// - `read_encoding` - Codepage to decode scripts in, UTF-8 if [`None`]. See [`generate_file`].
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
pub fn generate_rm2k<P: AsRef<Path>, Q: AsRef<Path>>(source_path: P, output_path: Q, force: bool) -> Result<(), Error> {
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

        write_data_file(&output_file_path, json)?;
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

        if Path::new(filename_str.as_ref())
            .extension()
            .is_none_or(|ext| ext != "lmu")
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
/// `RPG_RT.ldb.json` is read first to determine the [`Engine`] (2000 vs. 2003) for the other files. Without it,
/// falls back to [`Engine::R2K`].
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
pub fn write_rm2k<P: AsRef<Path>, Q: AsRef<Path>>(json_path: P, output_path: Q) -> Result<(), Error> {
    let json_path = json_path.as_ref();
    let output_path = output_path.as_ref();

    create_dir_all(output_path).map_err(|e| Error::Io(output_path.to_path_buf(), e))?;

    let write_bytes = |filename: &str, bytes: Vec<u8>| -> Result<(), Error> {
        let output_file_path = output_path.join(filename);
        write_data_file(&output_file_path, bytes)?;
        log::info!("{filename}: Successfully written.");
        Ok(())
    };

    let mut engine = Engine::R2K;

    let ldb_json_path = json_path.join("RPG_RT.ldb.json");

    if !ldb_json_path.exists() {
        log::warn!(
            "RPG_RT.ldb.json not found: the maps and the map tree are written as RPG Maker 2000, which drops fields only 2003 has."
        );
    }

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
