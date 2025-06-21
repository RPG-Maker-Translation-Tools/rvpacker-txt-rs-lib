use rvpacker_txt_rs_lib::{types::*, PurgerBuilder};
use std::{env::var, path::PathBuf};

#[test]
fn mz() {
    let game_path = PathBuf::from(var("MZ_GAME_PATH").unwrap());
    let source_path = game_path.join("data");
    let translation_path = game_path.join("translation");

    let mut purger = PurgerBuilder::new().build();
    purger.purge(&source_path, &translation_path, EngineType::New);

    purger.set_purge_empty(true);
    purger.purge(source_path, translation_path, EngineType::New);
}

#[test]
fn mv() {
    let game_path = PathBuf::from(var("MV_GAME_PATH").unwrap());
    let source_path = game_path.join("data");
    let translation_path = game_path.join("translation");

    let mut purger = PurgerBuilder::new().build();
    purger.purge(&source_path, &translation_path, EngineType::New);

    purger.set_purge_empty(true);
    purger.purge(source_path, translation_path, EngineType::New);
}

#[test]
fn ace() {
    let game_path = PathBuf::from(var("VXACE_GAME_PATH").unwrap());
    let source_path = game_path.join("Data");
    let translation_path = game_path.join("translation");

    let mut purger = PurgerBuilder::new().build();
    purger.purge(&source_path, &translation_path, EngineType::VXAce);

    purger.set_purge_empty(true);
    purger.purge(source_path, translation_path, EngineType::VXAce);
}

#[test]
fn vx() {
    let game_path = PathBuf::from(var("VX_GAME_PATH").unwrap());
    let source_path = game_path.join("Data");
    let translation_path = game_path.join("translation");

    let mut purger = PurgerBuilder::new().build();
    purger.purge(&source_path, &translation_path, EngineType::VX);

    purger.set_purge_empty(true);
    purger.purge(source_path, translation_path, EngineType::VX);
}

#[test]
fn xp() {
    let game_path = PathBuf::from(var("XP_GAME_PATH").unwrap());
    let source_path = game_path.join("Data");
    let translation_path = game_path.join("translation");

    let mut purger = PurgerBuilder::new().build();
    purger.purge(&source_path, &translation_path, EngineType::XP);

    purger.set_purge_empty(true);
    purger.purge(source_path, translation_path, EngineType::XP);
}
