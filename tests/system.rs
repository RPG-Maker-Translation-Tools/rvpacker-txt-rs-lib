use rvpacker_txt_rs_lib::{
    read::SystemReader,
    types::{EngineType, ProcessingMode},
    write::SystemWriter,
};
use std::{env::var, fs::create_dir_all, path::PathBuf};

#[test]
fn system_mz() {
    let game_path = PathBuf::from(var("MZ_GAME_PATH").unwrap());
    let data_path = game_path.join("data");

    let translation_path = game_path.join("translation");
    let output_path = game_path.join("output").join("data");
    let system_file_path = data_path.join("System.json");
    let engine_type = EngineType::New;

    create_dir_all(&translation_path).unwrap();
    create_dir_all(&output_path).unwrap();

    // Without romanize
    SystemReader::new(&system_file_path, &translation_path, engine_type)
        .processing_mode(ProcessingMode::Force)
        .read();
    SystemWriter::new(&system_file_path, &translation_path, &output_path, engine_type).write();

    // With romanize
    SystemReader::new(&system_file_path, &translation_path, engine_type)
        .processing_mode(ProcessingMode::Force)
        .romanize(true)
        .read();
    SystemWriter::new(&system_file_path, &translation_path, &output_path, engine_type)
        .romanize(true)
        .write();
}

#[test]
fn system_mv() {
    let game_path = PathBuf::from(var("MV_GAME_PATH").unwrap());
    let data_path = game_path.join("data");

    let translation_path = game_path.join("translation");
    let output_path = game_path.join("output").join("data");
    let system_file_path = data_path.join("System.json");
    let engine_type = EngineType::New;

    create_dir_all(&translation_path).unwrap();
    create_dir_all(&output_path).unwrap();

    // Without romanize
    SystemReader::new(&system_file_path, &translation_path, engine_type)
        .processing_mode(ProcessingMode::Force)
        .read();
    SystemWriter::new(&system_file_path, &translation_path, &output_path, engine_type).write();

    // With romanize
    SystemReader::new(&system_file_path, &translation_path, engine_type)
        .processing_mode(ProcessingMode::Force)
        .romanize(true)
        .read();
    SystemWriter::new(&system_file_path, &translation_path, &output_path, engine_type)
        .romanize(true)
        .write();
}

#[test]
fn system_vxace() {
    let game_path = PathBuf::from(var("VXACE_GAME_PATH").unwrap());
    let data_path = game_path.join("data");

    let translation_path = game_path.join("translation");
    let output_path = game_path.join("output").join("data");
    let system_file_path = data_path.join("System.rvdata2");
    let engine_type = EngineType::VXAce;

    create_dir_all(&translation_path).unwrap();
    create_dir_all(&output_path).unwrap();

    // Without romanize
    SystemReader::new(&system_file_path, &translation_path, engine_type)
        .processing_mode(ProcessingMode::Force)
        .read();
    SystemWriter::new(&system_file_path, &translation_path, &output_path, engine_type).write();

    // With romanize
    SystemReader::new(&system_file_path, &translation_path, engine_type)
        .processing_mode(ProcessingMode::Force)
        .romanize(true)
        .read();
    SystemWriter::new(&system_file_path, &translation_path, &output_path, engine_type)
        .romanize(true)
        .write();
}

#[test]
fn system_vx() {
    let game_path = PathBuf::from(var("VX_GAME_PATH").unwrap());
    let data_path = game_path.join("data");

    let translation_path = game_path.join("translation");
    let output_path = game_path.join("output").join("data");
    let system_file_path = data_path.join("System.rvdata");
    let engine_type = EngineType::VX;

    create_dir_all(&translation_path).unwrap();
    create_dir_all(&output_path).unwrap();

    // Without romanize
    SystemReader::new(&system_file_path, &translation_path, engine_type)
        .processing_mode(ProcessingMode::Force)
        .read();
    SystemWriter::new(&system_file_path, &translation_path, &output_path, engine_type).write();

    // With romanize
    SystemReader::new(&system_file_path, &translation_path, engine_type)
        .processing_mode(ProcessingMode::Force)
        .romanize(true)
        .read();
    SystemWriter::new(&system_file_path, &translation_path, &output_path, engine_type)
        .romanize(true)
        .write();
}

#[test]
fn system_xp() {
    let game_path = PathBuf::from(var("XP_GAME_PATH").unwrap());
    let data_path = game_path.join("data");

    let translation_path = game_path.join("translation");
    let output_path = game_path.join("output").join("data");
    let system_file_path = data_path.join("System.rxdata");
    let engine_type = EngineType::XP;

    create_dir_all(&translation_path).unwrap();
    create_dir_all(&output_path).unwrap();

    // Without romanize
    SystemReader::new(&system_file_path, &translation_path, engine_type)
        .processing_mode(ProcessingMode::Force)
        .read();
    SystemWriter::new(&system_file_path, &translation_path, &output_path, engine_type).write();

    // With romanize
    SystemReader::new(&system_file_path, &translation_path, engine_type)
        .processing_mode(ProcessingMode::Force)
        .romanize(true)
        .read();
    SystemWriter::new(&system_file_path, &translation_path, &output_path, engine_type)
        .romanize(true)
        .write();
}
