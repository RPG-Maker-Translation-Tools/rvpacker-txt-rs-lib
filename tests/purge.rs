use rvpacker_txt_rs_lib::{PurgerBuilder, types::*};
use std::{env::var, path::PathBuf};

#[test]
fn mz() -> Result<(), Box<dyn std::error::Error>> {
    let game_path = PathBuf::from(var("MZ_GAME_PATH")?);
    let source_path = game_path.join("data");
    let translation_path = game_path.join("translation");

    let mut purger = PurgerBuilder::new().build();
    purger.purge(source_path, translation_path, EngineType::New)?;

    Ok(())
}

#[test]
fn mv() -> Result<(), Box<dyn std::error::Error>> {
    let game_path = PathBuf::from(var("MV_GAME_PATH")?);
    let source_path = game_path.join("data");
    let translation_path = game_path.join("translation");

    let mut purger = PurgerBuilder::new().build();
    purger.purge(source_path, translation_path, EngineType::New)?;

    Ok(())
}

#[test]
fn ace() -> Result<(), Box<dyn std::error::Error>> {
    let game_path = PathBuf::from(var("VXACE_GAME_PATH")?);
    let source_path = game_path.join("Data");
    let translation_path = game_path.join("translation");

    let mut purger = PurgerBuilder::new().build();
    purger.purge(source_path, translation_path, EngineType::VXAce)?;

    Ok(())
}

#[test]
fn vx() -> Result<(), Box<dyn std::error::Error>> {
    let game_path = PathBuf::from(var("VX_GAME_PATH")?);
    let source_path = game_path.join("Data");
    let translation_path = game_path.join("translation");

    let mut purger = PurgerBuilder::new().build();
    purger.purge(source_path, translation_path, EngineType::VX)?;

    Ok(())
}

#[test]
fn xp() -> Result<(), Box<dyn std::error::Error>> {
    let game_path = PathBuf::from(var("XP_GAME_PATH")?);
    let source_path = game_path.join("Data");
    let translation_path = game_path.join("translation");

    let mut purger = PurgerBuilder::new().build();
    purger.purge(source_path, translation_path, EngineType::XP)?;

    Ok(())
}
