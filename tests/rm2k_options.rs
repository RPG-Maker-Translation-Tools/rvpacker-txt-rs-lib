//! `Processor`'s per-entry options reach RPG Maker 2000/2003 too.

use rvpacker_txt_rs_lib::{EngineType, FileFlags, Mode, Processor, RPGMFileType};
use std::{
    fs::{create_dir_all, read_to_string, remove_dir_all},
    path::{Path, PathBuf},
};

const FORCED_READ: Mode = Mode::Read {
    append: false,
    force: true,
};

fn read_actors(tag: &str, skip: Vec<(RPGMFileType, Vec<u32>)>) -> String {
    let translation: PathBuf = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("target/rm2k_options")
        .join(tag);

    let _ = remove_dir_all(&translation);
    create_dir_all(&translation).expect("could not create the workspace");

    let mut processor = Processor {
        mode: FORCED_READ,
        file_flags: FileFlags::Database | FileFlags::Actors,
        skip_events: skip,
        read_encoding: Some(encoding_rs::WINDOWS_1252),
        ..Default::default()
    };

    processor
        .process(
            EngineType::RM2K,
            Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/RM2K"),
            &translation,
            None,
        )
        .expect("read should not fail");

    read_to_string(translation.join("actors.txt")).expect("no actors.txt")
}

#[test]
fn skipped_entries_are_left_out_of_a_read() {
    let everything = read_actors("all", Vec::new());
    let id: u32 = everything
        .lines()
        .find_map(|line| line.strip_prefix("<!>ID<#>"))
        .and_then(|id| id.trim().parse().ok())
        .expect("the fixture has no actor section");

    let header = format!("<!>ID<#>{id}");
    let skipped = read_actors("skipped", Vec::from([(RPGMFileType::Rm2kActors, Vec::from([id]))]));

    assert!(everything.lines().any(|line| line == header));
    assert!(!skipped.lines().any(|line| line == header), "{skipped}");
}
