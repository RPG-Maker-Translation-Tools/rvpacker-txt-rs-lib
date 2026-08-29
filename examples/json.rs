//! The `json` module - converts XP/VX/VX Ace's binary Marshal files to/from
//! JSON, independent of the `.txt` translation workflow the rest of these
//! examples cover. `Scripts.*` is special-cased: it round-trips through a
//! `.rb` file of decompiled Ruby source instead of JSON.

use rvpacker_txt_rs_lib::{
    EngineType, Error,
    json::{generate, generate_file, write, write_file},
};
use std::fs::read;

fn main() -> Result<(), Error> {
    whole_directory_round_trip()?;
    single_file_round_trip()?;
    Ok(())
}

/// The usual case: convert every file in a `Data` directory to JSON (`.rb` for
/// `Scripts.*`), then convert that JSON back to the original binary format.
fn whole_directory_round_trip() -> Result<(), Error> {
    // `force: false` - skip a `.json`/`.rb` file that already exists rather
    // than overwriting it.
    generate("C:/Game/Data", "C:/Game/json", false)?;

    // `engine_type` picks the output extension (`.rvdata2`/`.rvdata`/`.rxdata`)
    // - it does not need to match whatever engine `generate` read from, only
    // the one the caller wants to write back as.
    write("C:/Game/json", "C:/Game/json-output", EngineType::VXAce)?;

    Ok(())
}

/// The lower-level, no-filesystem functions `generate`/`write` use internally
/// - for a caller that already has one file's bytes in hand.
fn single_file_round_trip() -> Result<(), Error> {
    let actors_content = read("C:/Game/Data/Actors.rvdata2").map_err(|e| Error::Io("Actors.rvdata2".into(), e))?;

    // Regular data file: JSON in both directions.
    let actors_json = generate_file(&actors_content, "Actors.rvdata2")?;
    println!("Actors.rvdata2 -> JSON ({len} bytes)", len = actors_json.len());

    let actors_rewritten = write_file(&actors_json)?;
    println!("JSON -> Actors.rvdata2 ({len} bytes)", len = actors_rewritten.len());

    // `Scripts.*`: `generate_file` returns Ruby source (one script's worth of
    // comment header plus code, concatenated), not JSON, so it goes back
    // through `json::write`'s directory-level `Scripts.rb` handling rather
    // than `write_file`, which only understands JSON.
    let scripts_content = read("C:/Game/Data/Scripts.rvdata2").map_err(|e| Error::Io("Scripts.rvdata2".into(), e))?;
    let scripts_rb = generate_file(&scripts_content, "Scripts.rvdata2")?;
    println!("Scripts.rvdata2 -> Ruby source ({len} bytes)", len = scripts_rb.len());

    Ok(())
}
