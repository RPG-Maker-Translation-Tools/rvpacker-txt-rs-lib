use rvpacker_txt_rs_lib::{types::*, ReaderBuilder};
use std::{env::var, path::PathBuf};

#[test]
fn mz() {
    let game_path = PathBuf::from(var("MZ_GAME_PATH").unwrap());
    let source_path = game_path.join("data");
    let translation_path = game_path.join("translation");

    let reader = ReaderBuilder::new().build();
    reader.read(&source_path, &translation_path, EngineType::New);
}

#[test]
fn mv() {
    let game_path = PathBuf::from(var("MV_GAME_PATH").unwrap());
    let source_path = game_path.join("data");
    let translation_path = game_path.join("translation");

    let reader = ReaderBuilder::new().build();
    reader.read(&source_path, &translation_path, EngineType::New);
}

#[test]
fn ace() {
    let game_path = PathBuf::from(var("VXACE_GAME_PATH").unwrap());
    let source_path = game_path.join("Data");
    let translation_path = game_path.join("translation");

    let reader = ReaderBuilder::new().build();
    reader.read(&source_path, &translation_path, EngineType::VXAce);
}

#[test]
fn vx() {
    let game_path = PathBuf::from(var("VX_GAME_PATH").unwrap());
    let source_path = game_path.join("Data");
    let translation_path = game_path.join("translation");

    let reader = ReaderBuilder::new().build();
    reader.read(&source_path, &translation_path, EngineType::VX);
}

#[test]
fn xp() {
    let game_path = PathBuf::from(var("XP_GAME_PATH").unwrap());
    let source_path = game_path.join("Data");
    let translation_path = game_path.join("translation");

    let reader = ReaderBuilder::new().build();
    reader.read(&source_path, &translation_path, EngineType::XP);
}
