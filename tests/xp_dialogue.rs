//! XP ends a message box only at the next one, so the last box of an event list has to be flushed when the list ends.
//! The default XP project's event lists each end in a box of two lines, `Multiline` and `Text`.

use rvpacker_txt_rs_lib::{EngineType, FileFlags, Mode, Processor};
use std::{fs, path::Path};

fn processor(mode: Mode) -> Processor {
    Processor {
        mode,
        file_flags: FileFlags::Map,
        ..Default::default()
    }
}

#[test]
fn the_last_message_box_of_an_event_list_is_read_and_written() {
    let manifest = Path::new(env!("CARGO_MANIFEST_DIR"));
    let source = manifest.join("tests/RMXP/Data");
    let workspace = manifest.join("target/xp-dialogue");
    let translation = workspace.join("translation");
    let output = workspace.join("output");
    let _ = fs::remove_dir_all(&workspace);

    processor(Mode::read())
        .process(EngineType::XP, &source, &translation, None)
        .expect("read failed");

    let maps = fs::read_to_string(translation.join("maps.txt")).unwrap();
    assert!(
        maps.contains(r"Multiline\#Text<#>"),
        "the last box was not read:\n{maps}"
    );

    fs::write(
        translation.join("maps.txt"),
        maps.replace(r"Multiline\#Text<#>", r"Multiline\#Text<#>Первая\#Вторая"),
    )
    .unwrap();

    processor(Mode::Write)
        .process(EngineType::XP, &source, &translation, Some(&output))
        .expect("write failed");

    let written = fs::read(output.join("Data/Map001.rxdata")).unwrap();
    let contains = |needle: &str| written.windows(needle.len()).any(|window| window == needle.as_bytes());

    assert!(contains("Первая") && contains("Вторая"), "the last box was not written");

    // XP keeps its strings as raw bytes, as the editor writes them: no string of the file is tagged with an encoding.
    let json = rvpacker_txt_rs_lib::json::generate_file(&written, "Map001.rxdata", None).unwrap();
    assert!(
        !json.contains(r#""__type": "str""#),
        "a string was written with an encoding tag"
    );
}
