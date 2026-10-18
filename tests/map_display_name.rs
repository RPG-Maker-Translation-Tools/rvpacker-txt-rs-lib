//! A map's display name is written back only when it has a translation: translating a map's text must not blank the
//! name the player sees.

use rvpacker_txt_rs_lib::{EngineType, Mode, core::Base};
use serde_json::{Value as Json, from_slice};

const MAP: &str = r#"{"displayName":"Town","events":[null,
{"id":1,"name":"EV001","x":5,"y":7,"pages":[
{"list":[{"code":101,"parameters":["",0,0,2]},
{"code":401,"parameters":["Hello there."]},
{"code":0,"parameters":[]}]}]}]}"#;

const MAPINFOS: &str = r#"[null,{"id":1,"name":"Town","order":1,"parentId":0}]"#;

fn read() -> String {
    let mut base = Base::new(Mode::read(), EngineType::MVMZ);
    base.begin_maps(MAPINFOS.as_bytes(), None).expect("begin_maps failed");
    base.process_map("Map001.json", MAP.as_bytes()).expect("read failed");

    String::from_utf8(base.finish_maps().as_ref().to_vec()).expect("not UTF-8")
}

fn write(text: &str) -> Json {
    let mut base = Base::new(Mode::Write, EngineType::MVMZ);
    base.begin_maps(MAPINFOS.as_bytes(), Some(text))
        .expect("begin_maps failed");

    let data = base
        .process_map("Map001.json", MAP.as_bytes())
        .expect("write failed")
        .expect("the map was not written");

    from_slice(data.as_ref()).expect("not valid JSON")
}

#[test]
fn an_untranslated_display_name_is_left_as_it_is() {
    let text = read().replace("Hello there.<#>", "Hello there.<#>Hi there.");
    let json = write(&text);

    assert_eq!(json["displayName"], "Town", "{text}");
}

#[test]
fn a_translated_display_name_is_written() {
    let text = read();
    assert!(text.contains("Town<#>"), "{text}");

    let json = write(
        &text
            .replace("Town<#>", "Town<#>Город")
            .replace("Hello there.<#>", "Hello there.<#>Hi there."),
    );

    assert_eq!(json["displayName"], "Город");
}
