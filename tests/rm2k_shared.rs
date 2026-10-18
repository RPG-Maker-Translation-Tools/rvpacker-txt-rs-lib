//! With duplicates removed, an RM2K entity whose name is shared with an earlier entity has no section of its own, and
//! must still be translated through that section, as the other file kinds are.

use rm2k::{field::DbStr, file::load_database, rpg::Database};
use rvpacker_txt_rs_lib::{DuplicateMode, EngineType, Mode, core::Base};
use std::{fs::read, path::Path};

/// The fixture database reduced to two ASCII items, both with the same name and description. Item 2 has no text of its
/// own. The rest of the fixture is French in a codepage this test doesn't set, so it's left out.
fn with_shared_name(bytes: &[u8]) -> Database<'_> {
    let mut database = load_database(bytes).expect("fixture should load").value;

    assert!(database.items.0.len() >= 2, "fixture needs two items");

    database.items.0.truncate(2);

    for item in &mut database.items.0 {
        item.name = DbStr::from_bytes(b"Potion");
        item.description = DbStr::from_bytes(b"Heals.");
    }

    database
}

/// Translates the `Potion` line of every section.
fn translate_potion(text: &str) -> String {
    let mut output = String::with_capacity(text.len() * 2);

    for line in text.lines() {
        output.push_str(line);

        if line.strip_suffix("<#>") == Some("Potion") {
            output.push_str("[Potion]");
        }

        output.push('\n');
    }

    output
}

#[test]
fn an_item_whose_name_is_shared_with_an_earlier_item_is_still_translated() {
    let bytes = read(Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/RM2K/RPG_RT.ldb")).unwrap();

    let mut read_database = with_shared_name(&bytes);
    let mut read_base = Base::new(Mode::read(), EngineType::RM2K);
    read_base.duplicate_mode = DuplicateMode::Remove;

    let text = read_base
        .process_rm2k_items(&mut read_database.items, None)
        .expect("read failed")
        .expect("nothing was processed");
    let translated = translate_potion(std::str::from_utf8(text.as_ref()).expect("translation is not UTF-8"));

    let mut written_database = with_shared_name(&bytes);
    let mut write_base = Base::new(Mode::Write, EngineType::RM2K);
    write_base.duplicate_mode = DuplicateMode::Remove;

    write_base
        .process_rm2k_items(&mut written_database.items, Some(&translated))
        .expect("write failed");

    assert_eq!(written_database.items.0[0].name.as_bytes(), b"[Potion]");
    assert_eq!(
        written_database.items.0[1].name.as_bytes(),
        b"[Potion]",
        "item 2's name must be translated through item 1's section"
    );
}

/// Empty text and text that is only symbols are skipped on read, so they stay out of the translation file.
#[test]
fn empty_and_symbol_only_fields_are_not_read() {
    let bytes = read(Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/RM2K/RPG_RT.ldb")).unwrap();
    let mut database = load_database(&bytes).expect("fixture should load").value;

    database.items.0.truncate(1);
    database.items.0[0].name = DbStr::from_bytes(b"---");
    database.items.0[0].description = DbStr::from_bytes(b"");

    let mut base = Base::new(Mode::read(), EngineType::RM2K);
    base.set_read_encoding(Some(encoding_rs::WINDOWS_1252));
    base.set_rm2k_engine(rm2k::engine::Engine::from_ldb_id(database.system.ldb_id));

    let text = base
        .process_rm2k_items(&mut database.items, None)
        .expect("read failed")
        .expect("nothing was processed");
    let text = String::from_utf8(text.as_ref().to_vec()).expect("translation is not UTF-8");

    assert!(!text.contains("---"), "symbol-only text was read:\n{text}");
    assert!(
        !text.lines().any(|line| line == "<#>"),
        "an empty description was read:\n{text}"
    );
}
