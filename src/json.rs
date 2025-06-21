use crate::{
    constants::{
        localization::{
            APPEND_MODE_IS_NOT_SUPPORTED, CANNOT_GENERATE_JSON,
            JSON_ALREADY_EXIST,
        },
        INSTANCE_VAR_PREFIX,
    },
    functions::read_to_string_without_bom,
    types::{EngineType, OptionExt, ReadMode, ResultExt},
};
use marshal_rs::{dump, load_utf8};
use sonic_rs::{from_str, to_string, Value};
use std::{
    fs::{create_dir_all, read, read_dir, read_to_string, write},
    path::{Path, PathBuf},
};

#[inline(always)]
pub fn generate_json<P: AsRef<Path>>(
    source_path: P,
    output_path: P,
    engine_type: EngineType,
    read_mode: ReadMode,
) {
    if read_mode.is_append() {
        println!("{APPEND_MODE_IS_NOT_SUPPORTED}");
        return;
    }

    let output_path = output_path.as_ref();
    let json_output_path = output_path.join("json");

    if json_output_path.exists() && read_mode != ReadMode::Force {
        println!("{JSON_ALREADY_EXIST}");
        return;
    }

    if source_path.as_ref().join("System.json").exists() {
        println!("{CANNOT_GENERATE_JSON}");
        return;
    }

    for entry in read_dir(source_path).unwrap_log().flatten() {
        let path = entry.path();
        let extension = path.extension().unwrap_log();

        for ext in ["rxdata", "rvdata", "rvdata2"] {
            if extension != ext {
                continue;
            }
        }

        let json: Value = if engine_type.is_new() {
            from_str(&read_to_string_without_bom(entry.path()).unwrap_log())
                .unwrap_log()
        } else {
            sonic_rs::from_str(
                &serde_json::to_string(
                    &load_utf8(
                        &read(entry.path()).unwrap_log(),
                        INSTANCE_VAR_PREFIX,
                    )
                    .unwrap_log(),
                )
                .unwrap_log(),
            )
            .unwrap_log()
        };

        create_dir_all(&json_output_path).unwrap_log();

        write(
            json_output_path
                .join(PathBuf::from(entry.file_name()).with_extension("json")),
            unsafe { to_string(&json).unwrap_unchecked() },
        )
        .unwrap_log();
    }
}

#[inline(always)]
pub fn write_json<P: AsRef<Path>>(root_dir: P) {
    let json_dir: &Path = &root_dir.as_ref().join("json");

    for entry in read_dir(json_dir).unwrap_log().flatten() {
        let json: Value =
            from_str(&read_to_string(entry.path()).unwrap_log()).unwrap_log();

        let dumped: Vec<u8> = dump(
            serde_json::from_str(&sonic_rs::to_string(&json).unwrap_log())
                .unwrap_log(),
            None,
        );

        write(root_dir.as_ref().join("written"), dumped).unwrap_log();
    }
}
