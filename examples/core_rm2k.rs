//! Driving `core::Base` directly for RPG Maker 2000/2003, instead of through
//! `Processor` (see `examples/read.rs`/`write.rs`/`purge.rs`).
//!
//! RM2K doesn't go through the JSON/Marshal `Value` cursor `examples/core.rs`
//! uses - it's parsed via `rm2k-lib`'s typed structs (`Database`, `Map`,
//! `TreeMap`) instead, so `Base` gets its own `process_rm2k_*` methods that
//! take those structs directly.

use rm2k::{
    engine::{Engine, SaveOpt},
    file,
};
use rvpacker_txt_rs_lib::{EngineType, Error, Mode, ProcessedData, core::Base};
use std::fs::read;

fn main() -> Result<(), Error> {
    let ldb_content = read("C:/Game2000/RPG_RT.ldb").map_err(|e| Error::Io("RPG_RT.ldb".into(), e))?;
    let mut database = file::load_database(&ldb_content)?;

    let mut base = Base::new(Mode::read(), EngineType::RM2K);
    // Some fields only exist on one of 2000/2003 - set from the loaded
    // database's `system.ldb_id`, the same way `Processor::process` does.
    base.set_rm2k_engine(Engine::from_ldb_id(database.value.system.ldb_id));

    map_example(&mut base)?;
    database_example(&mut base, &mut database.value)?;

    Ok(())
}

/// Unlike MV/VX, `RPG_RT.lmt` (the map tree) is loaded once up front rather
/// than lazily on first use - `process_rm2k_map` takes it as a `&TreeMap`
/// parameter directly instead of caching it on `Base` the way `mapinfos` is.
fn map_example(base: &mut Base) -> Result<(), Error> {
    let lmt_content = read("C:/Game2000/RPG_RT.lmt").map_err(|e| Error::Io("RPG_RT.lmt".into(), e))?;
    let tree = file::load_tree_map(&lmt_content)?;

    base.begin_rm2k_maps();

    for filename in ["Map0001.lmu", "Map0002.lmu"] {
        let path = format!("C:/Game2000/{filename}");
        let content = read(&path).map_err(|e| Error::Io(path.into(), e))?;

        base.process_rm2k_map(filename, &content, &tree.value, None)?;
    }

    let ProcessedData::TranslationData(maps_txt) = base.finish_rm2k_maps() else {
        unreachable!("read mode always returns TranslationData");
    };

    println!("maps.txt ({len} bytes)", len = maps_txt.len());
    Ok(())
}

/// One `RPG_RT.ldb` bundles every entity kind, so - unlike MV/VX, where each
/// kind is its own source file - each `process_rm2k_*` call here is treated as
/// if it were processing its own file: it mutates `database` in place and
/// returns its own `actors.txt`/`skills.txt`/... text, rather than returning
/// rewritten RPG Maker bytes directly. The caller re-serializes `database`
/// once, after every section has run - see the `Mode::Write` branch below.
fn database_example(base: &mut Base, database: &mut rm2k::rpg::Database<'_>) -> Result<(), Error> {
    if let Some(ProcessedData::TranslationData(actors_txt)) = base.process_rm2k_actors(&mut database.actors, None)? {
        println!("actors.txt ({len} bytes)", len = actors_txt.len());
    }

    if let Some(ProcessedData::TranslationData(skills_txt)) = base.process_rm2k_skills(&mut database.skills, None)? {
        println!("skills.txt ({len} bytes)", len = skills_txt.len());
    }

    // `troops`/`commonevents` pair a name with event-command dialogue under one
    // id, the same shape MV/VX's `Troops`/`CommonEvents` files use.
    if let Some(ProcessedData::TranslationData(troops_txt)) = base.process_rm2k_troops(&mut database.troops, None)? {
        println!("troops.txt ({len} bytes)", len = troops_txt.len());
    }

    // `terms` is RM2K's ~150-field vocabulary struct - the counterpart of
    // `System.*`'s "terms" section on MV/VX.
    if let Some(ProcessedData::TranslationData(terms_txt)) = base.process_rm2k_terms(&mut database.terms, None)? {
        println!("terms.txt ({len} bytes)", len = terms_txt.len());
    }

    // Write mode: every `process_rm2k_*` call mutates `database` and returns
    // `Ok(None)` instead of bytes - re-serialize once, after the whole pass.
    let mut write_base = Base::new(Mode::Write, EngineType::RM2K);
    write_base.set_rm2k_engine(rm2k::engine::Engine::R2K);

    let actors_translation = "Hero<#>Heros\n"; // As produced by a prior read.
    write_base.process_rm2k_actors(&mut database.actors, Some(actors_translation))?;

    let mut bytes = Vec::new();
    file::save_database(
        database,
        &mut bytes,
        Engine::R2K,
        SaveOpt { preserve_header: true },
        b"",
    )
    .expect("serializing the rebuilt database should not fail");

    println!("RPG_RT.ldb rebuilt ({len} bytes)", len = bytes.len());
    Ok(())
}
