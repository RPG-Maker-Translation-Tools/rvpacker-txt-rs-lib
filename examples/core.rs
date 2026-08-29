//! Driving `core::Base` directly, instead of through `Processor`, for the
//! JSON/Marshal-backed engines (MV/MZ, VX Ace, VX, XP).
//!
//! `Processor` (see `examples/read.rs`/`write.rs`/`purge.rs`) is the file-system
//! driver every consumer should reach for first; this example is for a caller
//! that already has file contents in hand (a GUI loading one file at a time,
//! say) and wants to call the extraction/injection logic itself.

use rvpacker_txt_rs_lib::{EngineType, Error, Mode, ProcessedData, core::Base};
use std::fs::read;

fn main() -> Result<(), Error> {
    map_example()?;
    other_example()?;
    system_example()?;
    scripts_example()?; // XP/VX/VX Ace only.
    plugins_example()?; // MV only (MZ has no `plugins.js`).
    Ok(())
}

/// Maps are the one kind processed as a run, not one shot: `MapInfos.*` is
/// parsed once and reused across every `MapNNN.*` in the run, and every map's
/// output accumulates into one `maps.txt`.
fn map_example() -> Result<(), Error> {
    let mut base = Base::new(Mode::read(), EngineType::VXAce);
    base.begin_maps();

    let mapinfos_content =
        read("C:/Game/Data/MapInfos.rvdata2").map_err(|e| Error::Io("MapInfos.rvdata2".into(), e))?;

    for filename in ["Map001.rvdata2", "Map002.rvdata2"] {
        let path = format!("C:/Game/Data/{filename}");
        let content = read(&path).map_err(|e| Error::Io(path.into(), e))?;

        // `None` on read: nothing to translate yet, so this only extracts.
        base.process_map(filename, &content, &mapinfos_content, None)?;
    }

    let ProcessedData::TranslationData(maps_txt) = base.finish_maps() else {
        unreachable!("read mode always returns TranslationData");
    };

    let maps_txt = String::from_utf8(maps_txt).unwrap();
    println!("maps.txt ({len} bytes)", len = maps_txt.len());

    // Writing back mirrors this shape: `begin_maps`, then one `process_map`
    // per file with `Some(&translation)`, writing each file's own returned
    // `ProcessedData::RPGMData` bytes out; `finish_maps` isn't called on write,
    // since there's no shared output left to flush.
    let mut write_base = Base::new(Mode::Write, EngineType::VXAce);
    write_base.begin_maps();

    if let Some(ProcessedData::RPGMData(rewritten)) = write_base.process_map(
        "Map001.rvdata2",
        &content_for("Map001.rvdata2")?,
        &mapinfos_content,
        Some(&maps_txt),
    )? {
        println!("Map001.rvdata2 rewritten ({len} bytes)", len = rewritten.len());
    }

    Ok(())
}

fn content_for(filename: &str) -> Result<Vec<u8>, Error> {
    let path = format!("C:/Game/Data/{filename}");
    read(&path).map_err(|e| Error::Io(path.into(), e))
}

/// `Actors`/`Armors`/`Classes`/`CommonEvents`/`Enemies`/`Items`/`Skills`/
/// `States`/`Troops`/`Weapons` - one call per file, no `begin`/`finish` pair.
fn other_example() -> Result<(), Error> {
    let mut base = Base::new(Mode::read(), EngineType::VXAce);

    let content = content_for("Actors.rvdata2")?;
    if let Some(ProcessedData::TranslationData(actors_txt)) = base.process_other("Actors.rvdata2", &content, None)? {
        println!("actors.txt ({len} bytes)", len = actors_txt.len());
    }

    Ok(())
}

/// `System.*` - vocabulary, currency unit, equip/armor type labels and (on
/// read, unless overridden with `set_game_title`) the game's title.
fn system_example() -> Result<(), Error> {
    let mut base = Base::new(Mode::read(), EngineType::VXAce);
    let content = content_for("System.rvdata2")?;

    if let Some(ProcessedData::TranslationData(system_txt)) = base.process_system(&content, None)? {
        println!("system.txt ({len} bytes)", len = system_txt.len());
    }

    Ok(())
}

/// `Scripts.*` decompresses to Ruby source per entry; `process_scripts` pulls
/// translatable string literals out of it (or, standalone, `decode_scripts`/
/// `encode_scripts` convert the whole file to/from `Scripts` for other uses,
/// e.g. `json::generate`'s `.rb` dump).
fn scripts_example() -> Result<(), Error> {
    let mut base = Base::new(Mode::read(), EngineType::VXAce);
    let content = content_for("Scripts.rvdata2")?;

    if let Some(ProcessedData::TranslationData(scripts_txt)) = base.process_scripts(&content, None)? {
        println!("scripts.txt ({len} bytes)", len = scripts_txt.len());
    }

    Ok(())
}

/// `js/plugins.js` - a `var $plugins = [...]` assignment. Only string values
/// under keys the built-in denylist doesn't recognise as non-translatable
/// (ids, file paths, ...) are extracted.
fn plugins_example() -> Result<(), Error> {
    let mut base = Base::new(Mode::read(), EngineType::MVMZ);
    let content = read("C:/Game/www/js/plugins.js").map_err(|e| Error::Io("plugins.js".into(), e))?;

    if let Some(ProcessedData::TranslationData(plugins_txt)) = base.process_plugins(&content, None)? {
        println!("plugins.txt ({len} bytes)", len = plugins_txt.len());
    }

    Ok(())
}
