use crate::{
    read_to_string_without_bom,
    statics::localization::{APPEND_MODE_IS_NOT_SUPPORTED, CANNOT_GENERATE_JSON, JSON_ALREADY_EXIST},
    types::{EngineType, OptionExt, ProcessingMode, ResultExt},
};
use marshal_rs::{dump, load, StringMode};
use sonic_rs::{from_str, to_string, Value};
use std::{
    fs::{create_dir_all, read, read_dir, read_to_string, write},
    path::Path,
};

#[inline(always)]
pub fn generate_json<P: AsRef<Path>>(
    original_path: P,
    output_path: P,
    engine_type: EngineType,
    processing_mode: ProcessingMode,
) {
    if processing_mode == ProcessingMode::Append {
        println!("{APPEND_MODE_IS_NOT_SUPPORTED}");
        return;
    }

    if output_path.as_ref().join("json").exists() && processing_mode != ProcessingMode::Force {
        println!("{JSON_ALREADY_EXIST}");
        return;
    }

    if original_path.as_ref().join("System.json").exists() {
        println!("{CANNOT_GENERATE_JSON}");
        return;
    }

    for entry in read_dir(original_path).unwrap_log().flatten() {
        let filename: String = entry.file_name().into_string().unwrap_log();

        for ext in ["rxdata", "rvdata", "rvdata2"] {
            if !filename.ends_with(ext) {
                continue;
            }
        }

        let obj: Value = if engine_type == EngineType::New {
            from_str(&read_to_string_without_bom(entry.path()).unwrap_log()).unwrap_log()
        } else {
            load(&read(entry.path()).unwrap_log(), Some(StringMode::UTF8), Some("")).unwrap_log()
        };

        create_dir_all(output_path.as_ref().join("json")).unwrap();

        write(
            output_path
                .as_ref()
                .join(format!("json/{}.json", &filename.rsplit_once('.').unwrap_log().0)),
            unsafe { to_string(&obj).unwrap_unchecked() },
        )
        .unwrap_log();
    }
}

#[inline(always)]
pub fn write_json<P: AsRef<Path>>(root_dir: P) {
    let json_dir: &Path = &root_dir.as_ref().join("json");

    for entry in read_dir(json_dir).unwrap_log().flatten() {
        let json: Value = from_str(&read_to_string(entry.path()).unwrap_log()).unwrap_log();
        let marshal_obj: Vec<u8> = dump(json, None);

        write(root_dir.as_ref().join("written"), marshal_obj).unwrap_log();
    }
}
