use rvpacker_txt_rs_lib::{read::PluginReader, types::ProcessingMode, write::PluginWriter};
use std::{env::var, fs::create_dir_all, path::PathBuf};

#[test]
fn plugins_mz() {
    let game_path = PathBuf::from(var("MZ_GAME_PATH").unwrap());

    let translation_path = game_path.join("translation");
    let output_path = game_path.join("output").join("data");
    let plugins_file_path = game_path.join("js/plugins.js");

    create_dir_all(&translation_path).unwrap();
    create_dir_all(&output_path).unwrap();

    // Without romanize
    PluginReader::new(&plugins_file_path, &translation_path)
        .processing_mode(ProcessingMode::Force)
        .read();
    PluginWriter::new(&plugins_file_path, &translation_path, &output_path).write();

    // With romanize
    PluginReader::new(&plugins_file_path, &translation_path)
        .processing_mode(ProcessingMode::Force)
        .romanize(true)
        .read();
    PluginWriter::new(&plugins_file_path, &translation_path, &output_path)
        .romanize(true)
        .write();
}

#[test]
fn plugins_mv() {
    let game_path = PathBuf::from(var("MV_GAME_PATH").unwrap());

    let translation_path = game_path.join("translation");
    let output_path = game_path.join("output").join("data");
    let plugins_file_path = game_path.join("js/plugins.js");

    create_dir_all(&translation_path).unwrap();
    create_dir_all(&output_path).unwrap();

    // Without romanize
    PluginReader::new(&plugins_file_path, &translation_path)
        .processing_mode(ProcessingMode::Force)
        .read();
    PluginWriter::new(&plugins_file_path, &translation_path, &output_path).write();

    // With romanize
    PluginReader::new(&plugins_file_path, &translation_path)
        .processing_mode(ProcessingMode::Force)
        .romanize(true)
        .read();
    PluginWriter::new(&plugins_file_path, &translation_path, &output_path)
        .romanize(true)
        .write();
}
