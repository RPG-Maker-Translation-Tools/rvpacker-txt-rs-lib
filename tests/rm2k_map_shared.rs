//! An RM2K/2003 map whose only text is shared with an earlier map (with duplicates removed) must still be written with its
//! translations, as the MV/MZ and other-file cases are. Map 3 here is written from map 2's bytes, so every line of it is
//! shared with map 2's section.

use encoding_rs::WINDOWS_1252;
use rm2k::{engine::Engine, file::load_database, file::load_tree_map};
use rvpacker_txt_rs_lib::{DuplicateMode, EngineType, Mode, ProcessedData, core::Base};
use std::{fs::read, path::Path};

fn fixture(name: &str) -> Vec<u8> {
    read(Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/RM2K").join(name)).unwrap()
}

/// Appends `[T]` to every line of the translation file that has a source before its separator.
fn translate_every_line(text: &str) -> String {
    let mut output = String::with_capacity(text.len() * 2);

    for line in text.lines() {
        output.push_str(line);

        if line.strip_suffix("<#>").is_some_and(|source| !source.is_empty()) {
            output.push_str("[T]");
        }

        output.push('\n');
    }

    output
}

#[test]
fn an_rm2k_map_whose_only_text_is_shared_with_an_earlier_map_is_still_written() {
    let lmt = fixture("RPG_RT.lmt");
    let tree = load_tree_map(&lmt).expect("the tree map should load").value;
    let ldb = fixture("RPG_RT.ldb");
    let engine = Engine::from_ldb_id(
        load_database(&ldb)
            .expect("the database should load")
            .value
            .system
            .ldb_id,
    );

    // Map 1 has no text in this fixture, so map 2 is the source. Map 3 is written from the same bytes: all its lines are
    // shared with map 2's section.
    let map = fixture("Map0002.lmu");

    let mut reader = Base::new(Mode::read(), EngineType::RM2K);
    reader.duplicate_mode = DuplicateMode::Remove;
    reader.set_read_encoding(Some(WINDOWS_1252));
    reader.set_rm2k_engine(engine);
    reader.begin_rm2k_maps();
    reader
        .process_rm2k_map("Map0002.lmu", &map, &tree, None)
        .expect("read failed");

    let ProcessedData::TranslationData(text) = reader.finish_rm2k_maps() else {
        unreachable!("read mode always returns translation data");
    };
    let translated = translate_every_line(std::str::from_utf8(text.as_ref()).expect("translation is not UTF-8"));

    let mut writer = Base::new(Mode::Write, EngineType::RM2K);
    writer.duplicate_mode = DuplicateMode::Remove;
    writer.set_read_encoding(Some(WINDOWS_1252));
    writer.set_write_encoding(Some(WINDOWS_1252)).unwrap();
    writer.set_rm2k_engine(engine);

    let Some(ProcessedData::RPGMData(bytes)) = writer
        .process_rm2k_map("Map0003.lmu", &map, &tree, Some(&translated))
        .expect("write failed")
    else {
        panic!("map 3 must be written");
    };

    assert!(
        bytes.windows(3).any(|window| window == b"[T]"),
        "map 3's shared lines must be translated"
    );
}
