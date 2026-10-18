//! A JSON file with a byte order mark, as MZ writes them, is read, and written back with it.

use rvpacker_txt_rs_lib::{EngineType, Mode, core::Base};

const MAP_BODY: &str = r#"{"displayName":"Town","events":[null,
{"id":1,"name":"EV001","x":5,"y":7,"pages":[
{"list":[{"code":101,"parameters":["",0,0,2]},
{"code":401,"parameters":["Hello there."]},
{"code":0,"parameters":[]}]}]}]}"#;

const MAPINFOS_BODY: &str = r#"[null,{"id":1,"name":"Town","order":1,"parentId":0}]"#;

#[test]
fn the_byte_order_mark_survives_a_write() {
    let map = format!("\u{feff}{MAP_BODY}");
    let mapinfos = format!("\u{feff}{MAPINFOS_BODY}");

    let mut reader = Base::new(Mode::read(), EngineType::MVMZ);
    reader.begin_maps(mapinfos.as_bytes(), None).unwrap();
    reader.process_map("Map001.json", map.as_bytes()).unwrap();
    let text = String::from_utf8(reader.finish_maps().as_ref().to_vec()).unwrap();

    assert!(text.contains("Hello there.<#>"), "{text}");

    let translated = text.replace("Hello there.<#>", "Hello there.<#>Hi there.");
    let mut writer = Base::new(Mode::Write, EngineType::MVMZ);
    writer.begin_maps(mapinfos.as_bytes(), Some(&translated)).unwrap();

    let data = writer.process_map("Map001.json", map.as_bytes()).unwrap().unwrap();
    let written = String::from_utf8(data.as_ref().to_vec()).unwrap();

    assert!(written.starts_with('\u{feff}'));
    assert_eq!(written, map.replace("Hello there.", "Hi there."));
}
