use rvpacker_txt_rs_lib::{
    read::MapReader,
    types::{EngineType, MapsProcessingMode, ProcessingMode},
    write::MapWriter,
};
use std::{env::var, fs::create_dir_all, path::PathBuf};

#[test]
fn map_mz() {
    let game_path = PathBuf::from(var("MZ_GAME_PATH").unwrap());
    let data_path = game_path.join("data");

    let translation_path = game_path.join("translation");
    let output_path = game_path.join("output").join("data");

    let engine_type = EngineType::New;

    create_dir_all(&translation_path).unwrap();
    create_dir_all(&output_path).unwrap();

    // Processing modes without args
    MapReader::new(&data_path, &translation_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Default)
        .processing_mode(ProcessingMode::Force)
        .read();
    MapWriter::new(&data_path, &translation_path, &output_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Default)
        .write();

    MapReader::new(&data_path, &translation_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Separate)
        .processing_mode(ProcessingMode::Force)
        .read();
    MapWriter::new(&data_path, &translation_path, &output_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Separate)
        .write();

    MapReader::new(&data_path, &translation_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Preserve)
        .processing_mode(ProcessingMode::Force)
        .read();
    MapWriter::new(&data_path, &translation_path, &output_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Preserve)
        .write();

    // Processing modes with romanize
    MapReader::new(&data_path, &translation_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Default)
        .romanize(true)
        .processing_mode(ProcessingMode::Force)
        .read();
    MapWriter::new(&data_path, &translation_path, &output_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Default)
        .romanize(true)
        .write();

    MapReader::new(&data_path, &translation_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Separate)
        .romanize(true)
        .processing_mode(ProcessingMode::Force)
        .read();
    MapWriter::new(&data_path, &translation_path, &output_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Separate)
        .romanize(true)
        .write();

    MapReader::new(&data_path, &translation_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Preserve)
        .romanize(true)
        .processing_mode(ProcessingMode::Force)
        .read();
    MapWriter::new(&data_path, &translation_path, &output_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Preserve)
        .romanize(true)
        .write();
}

#[test]
fn map_mv() {
    let game_path = PathBuf::from(var("MV_GAME_PATH").unwrap());
    let data_path = game_path.join("data");

    let translation_path = game_path.join("translation");
    let output_path = game_path.join("output").join("data");

    let engine_type = EngineType::New;

    create_dir_all(&translation_path).unwrap();
    create_dir_all(&output_path).unwrap();

    // Processing modes without args
    MapReader::new(&data_path, &translation_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Default)
        .processing_mode(ProcessingMode::Force)
        .read();
    MapWriter::new(&data_path, &translation_path, &output_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Default)
        .write();

    MapReader::new(&data_path, &translation_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Separate)
        .processing_mode(ProcessingMode::Force)
        .read();
    MapWriter::new(&data_path, &translation_path, &output_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Separate)
        .write();

    MapReader::new(&data_path, &translation_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Preserve)
        .processing_mode(ProcessingMode::Force)
        .read();
    MapWriter::new(&data_path, &translation_path, &output_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Preserve)
        .write();

    // Processing modes with romanize
    MapReader::new(&data_path, &translation_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Default)
        .romanize(true)
        .processing_mode(ProcessingMode::Force)
        .read();
    MapWriter::new(&data_path, &translation_path, &output_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Default)
        .romanize(true)
        .write();

    MapReader::new(&data_path, &translation_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Separate)
        .romanize(true)
        .processing_mode(ProcessingMode::Force)
        .read();
    MapWriter::new(&data_path, &translation_path, &output_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Separate)
        .romanize(true)
        .write();

    MapReader::new(&data_path, &translation_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Preserve)
        .romanize(true)
        .processing_mode(ProcessingMode::Force)
        .read();
    MapWriter::new(&data_path, &translation_path, &output_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Preserve)
        .romanize(true)
        .write();
}

#[test]
fn map_vxace() {
    let game_path = PathBuf::from(var("VXACE_GAME_PATH").unwrap());
    let data_path = game_path.join("data");

    let translation_path = game_path.join("translation");
    let output_path = game_path.join("output").join("data");

    let engine_type = EngineType::VXAce;

    create_dir_all(&translation_path).unwrap();
    create_dir_all(&output_path).unwrap();

    // Processing modes without args
    MapReader::new(&data_path, &translation_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Default)
        .processing_mode(ProcessingMode::Force)
        .read();
    MapWriter::new(&data_path, &translation_path, &output_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Default)
        .write();

    MapReader::new(&data_path, &translation_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Separate)
        .processing_mode(ProcessingMode::Force)
        .read();
    MapWriter::new(&data_path, &translation_path, &output_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Separate)
        .write();

    MapReader::new(&data_path, &translation_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Preserve)
        .processing_mode(ProcessingMode::Force)
        .read();
    MapWriter::new(&data_path, &translation_path, &output_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Preserve)
        .write();

    // Processing modes with romanize
    MapReader::new(&data_path, &translation_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Default)
        .romanize(true)
        .processing_mode(ProcessingMode::Force)
        .read();
    MapWriter::new(&data_path, &translation_path, &output_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Default)
        .romanize(true)
        .write();

    MapReader::new(&data_path, &translation_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Separate)
        .romanize(true)
        .processing_mode(ProcessingMode::Force)
        .read();
    MapWriter::new(&data_path, &translation_path, &output_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Separate)
        .romanize(true)
        .write();

    MapReader::new(&data_path, &translation_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Preserve)
        .romanize(true)
        .processing_mode(ProcessingMode::Force)
        .read();
    MapWriter::new(&data_path, &translation_path, &output_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Preserve)
        .romanize(true)
        .write();
}

#[test]
fn map_vx() {
    let game_path = PathBuf::from(var("VX_GAME_PATH").unwrap());
    let data_path = game_path.join("data");

    let translation_path = game_path.join("translation");
    let output_path = game_path.join("output").join("data");

    let engine_type = EngineType::VX;

    create_dir_all(&translation_path).unwrap();
    create_dir_all(&output_path).unwrap();

    // Processing modes without args
    MapReader::new(&data_path, &translation_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Default)
        .processing_mode(ProcessingMode::Force)
        .read();
    MapWriter::new(&data_path, &translation_path, &output_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Default)
        .write();

    MapReader::new(&data_path, &translation_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Separate)
        .processing_mode(ProcessingMode::Force)
        .read();
    MapWriter::new(&data_path, &translation_path, &output_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Separate)
        .write();

    MapReader::new(&data_path, &translation_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Preserve)
        .processing_mode(ProcessingMode::Force)
        .read();
    MapWriter::new(&data_path, &translation_path, &output_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Preserve)
        .write();

    // Processing modes with romanize
    MapReader::new(&data_path, &translation_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Default)
        .romanize(true)
        .processing_mode(ProcessingMode::Force)
        .read();
    MapWriter::new(&data_path, &translation_path, &output_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Default)
        .romanize(true)
        .write();

    MapReader::new(&data_path, &translation_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Separate)
        .romanize(true)
        .processing_mode(ProcessingMode::Force)
        .read();
    MapWriter::new(&data_path, &translation_path, &output_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Separate)
        .romanize(true)
        .write();

    MapReader::new(&data_path, &translation_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Preserve)
        .romanize(true)
        .processing_mode(ProcessingMode::Force)
        .read();
    MapWriter::new(&data_path, &translation_path, &output_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Preserve)
        .romanize(true)
        .write();
}

#[test]
fn map_xp() {
    let game_path = PathBuf::from(var("XP_GAME_PATH").unwrap());
    let data_path = game_path.join("data");

    let translation_path = game_path.join("translation");
    let output_path = game_path.join("output").join("data");

    let engine_type = EngineType::XP;

    create_dir_all(&translation_path).unwrap();
    create_dir_all(&output_path).unwrap();

    // Processing modes without args
    MapReader::new(&data_path, &translation_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Default)
        .processing_mode(ProcessingMode::Force)
        .read();
    MapWriter::new(&data_path, &translation_path, &output_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Default)
        .write();

    MapReader::new(&data_path, &translation_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Separate)
        .processing_mode(ProcessingMode::Force)
        .read();
    MapWriter::new(&data_path, &translation_path, &output_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Separate)
        .write();

    MapReader::new(&data_path, &translation_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Preserve)
        .processing_mode(ProcessingMode::Force)
        .read();
    MapWriter::new(&data_path, &translation_path, &output_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Preserve)
        .write();

    // Processing modes with romanize
    MapReader::new(&data_path, &translation_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Default)
        .romanize(true)
        .processing_mode(ProcessingMode::Force)
        .read();
    MapWriter::new(&data_path, &translation_path, &output_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Default)
        .romanize(true)
        .write();

    MapReader::new(&data_path, &translation_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Separate)
        .romanize(true)
        .processing_mode(ProcessingMode::Force)
        .read();
    MapWriter::new(&data_path, &translation_path, &output_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Separate)
        .romanize(true)
        .write();

    MapReader::new(&data_path, &translation_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Preserve)
        .romanize(true)
        .processing_mode(ProcessingMode::Force)
        .read();
    MapWriter::new(&data_path, &translation_path, &output_path, engine_type)
        .maps_processing_mode(MapsProcessingMode::Preserve)
        .romanize(true)
        .write();
}
