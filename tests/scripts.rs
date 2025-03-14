use rvpacker_txt_rs_lib::{read::ScriptReader, types::ProcessingMode, write::ScriptWriter};
use std::{env::var, fs::create_dir_all, path::PathBuf};

#[test]
fn scripts_vxace() {
    let game_path = PathBuf::from(var("VXACE_GAME_PATH").unwrap());
    let data_path = game_path.join("data");

    let translation_path = game_path.join("translation");
    let output_path = game_path.join("output").join("data");
    let scripts_file_path = data_path.join("Scripts.rvdata2");

    create_dir_all(&translation_path).unwrap();
    create_dir_all(&output_path).unwrap();

    // Without romanize
    ScriptReader::new(&scripts_file_path, &translation_path)
        .processing_mode(ProcessingMode::Force)
        .read();
    ScriptWriter::new(&scripts_file_path, &translation_path, &output_path).write();

    // With romanize
    ScriptReader::new(&scripts_file_path, &translation_path)
        .processing_mode(ProcessingMode::Force)
        .romanize(true)
        .read();
    ScriptWriter::new(&scripts_file_path, &translation_path, &output_path)
        .romanize(true)
        .write();
}

#[test]
fn scripts_vx() {
    let game_path = PathBuf::from(var("VX_GAME_PATH").unwrap());
    let data_path = game_path.join("data");

    let translation_path = game_path.join("translation");
    let output_path = game_path.join("output").join("data");
    let scripts_file_path = data_path.join("Scripts.rvdata");

    create_dir_all(&translation_path).unwrap();
    create_dir_all(&output_path).unwrap();

    // Without romanize
    ScriptReader::new(&scripts_file_path, &translation_path)
        .processing_mode(ProcessingMode::Force)
        .read();
    ScriptWriter::new(&scripts_file_path, &translation_path, &output_path).write();

    // With romanize
    ScriptReader::new(&scripts_file_path, &translation_path)
        .processing_mode(ProcessingMode::Force)
        .romanize(true)
        .read();
    ScriptWriter::new(&scripts_file_path, &translation_path, &output_path)
        .romanize(true)
        .write();
}

#[test]
fn scripts_xp() {
    let game_path = PathBuf::from(var("XP_GAME_PATH").unwrap());
    let data_path = game_path.join("data");

    let translation_path = game_path.join("translation");
    let output_path = game_path.join("output").join("data");
    let scripts_file_path = data_path.join("Scripts.rxdata");

    create_dir_all(&translation_path).unwrap();
    create_dir_all(&output_path).unwrap();

    // Without romanize
    ScriptReader::new(&scripts_file_path, &translation_path)
        .processing_mode(ProcessingMode::Force)
        .read();
    ScriptWriter::new(&scripts_file_path, &translation_path, &output_path).write();

    // With romanize
    ScriptReader::new(&scripts_file_path, &translation_path)
        .processing_mode(ProcessingMode::Force)
        .romanize(true)
        .read();
    ScriptWriter::new(&scripts_file_path, &translation_path, &output_path)
        .romanize(true)
        .write();
}
