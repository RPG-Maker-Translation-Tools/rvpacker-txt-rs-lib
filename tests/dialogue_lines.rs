//! A message box with several lines is one translation entry, joined by the `\#` marker. A translation can have more or
//! fewer lines than its source, and each case has its own rule for where the lines go on write.

use rvpacker_txt_rs_lib::{EngineType, Mode, core::Base};
use serde_json::{Value as Json, from_slice};

/// One event whose message box has two lines: `Hello there.` and `Second page.`.
const MAP: &str = r#"{"displayName":"Town","events":[null,
{"id":1,"name":"EV001","x":5,"y":7,"pages":[
{"list":[{"code":101,"parameters":["",0,0,2]},
{"code":401,"parameters":["Hello there."]},
{"code":401,"parameters":["Second page."]},
{"code":0,"parameters":[]}]}]}]}"#;

const SOURCE: &str = r"Hello there.\#Second page.<#>";

const MAPINFOS: &str = r#"[null,{"id":1,"name":"Town","order":1,"parentId":0}]"#;

fn read() -> String {
    let mut base = Base::new(Mode::read(), EngineType::MVMZ);
    base.begin_maps(MAPINFOS.as_bytes(), None).expect("begin_maps failed");
    base.process_map("Map001.json", MAP.as_bytes()).expect("read failed");

    String::from_utf8(base.finish_maps().as_ref().to_vec()).expect("not UTF-8")
}

/// Writes the map with `translation` as the translation of the message box, returning the new JSON.
fn write(translation: &str) -> Json {
    let text = read();
    assert!(
        text.contains(SOURCE),
        "the message box is not in the translation file:\n{text}"
    );
    let text = text.replace(SOURCE, &format!("{SOURCE}{translation}"));

    let mut base = Base::new(Mode::Write, EngineType::MVMZ);
    base.begin_maps(MAPINFOS.as_bytes(), Some(&text))
        .expect("begin_maps failed");

    let data = base
        .process_map("Map001.json", MAP.as_bytes())
        .expect("write failed")
        .expect("the map was not written");

    from_slice(data.as_ref()).expect("not valid JSON")
}

fn lines(json: &Json) -> (&Json, &Json) {
    let list = &json["events"][1]["pages"][0]["list"];

    (&list[1]["parameters"][0], &list[2]["parameters"][0])
}

#[test]
fn a_translation_with_the_same_line_count_maps_line_by_line() {
    let json = write(r"[One]\#[Two]");

    assert_eq!(lines(&json), (&Json::from("[One]"), &Json::from("[Two]")));
}

#[test]
fn a_translation_with_fewer_lines_blanks_the_leftover_source_lines() {
    let json = write("[Only]");

    assert_eq!(lines(&json), (&Json::from("[Only]"), &Json::from(" ")));
}

#[test]
fn a_translation_with_more_lines_keeps_the_extra_lines_in_the_last_one() {
    let json = write(r"[One]\#[Two]\#[Three]");

    // Each line is written once: the last source line takes the rest of the translation, not a copy of its own line.
    assert_eq!(lines(&json), (&Json::from("[One]"), &Json::from("[Two]\n[Three]")));
}
