//! Event command lists with commands that are missing their parameters, have no code, or carry the wrong type. Each of
//! those must be skipped, and the rest of the list must still read and write.

use rvpacker_txt_rs_lib::{EngineType, Mode, core::Base};
use serde_json::{Value as Json, from_slice};

const MAPINFOS: &str = r#"[null,{"id":1,"name":"Town","order":1,"parentId":0}]"#;

/// Valid text around a run of commands that are malformed in one way or another.
const MAP: &str = r#"{"displayName":"Town","events":[null,
{"id":1,"name":"EV001","x":1,"y":1,"pages":[
{"list":[{"code":401,"parameters":["First."]},
{"code":401,"parameters":[""]},
{"code":401,"parameters":["Second."]},
{"code":102,"parameters":[]},
{"code":102,"parameters":[["Yes.",5,"No."],0]},
{"code":402,"parameters":[]},
{"parameters":["No code here."]},
{"code":320,"parameters":[]},
{"code":0,"parameters":[]}]}]}]}"#;

fn read() -> String {
    let mut base = Base::new(Mode::read(), EngineType::MVMZ);
    base.begin_maps(MAPINFOS.as_bytes(), None).expect("begin_maps failed");
    base.process_map("Map001.json", MAP.as_bytes()).expect("read failed");

    String::from_utf8(base.finish_maps().as_ref().to_vec()).expect("not UTF-8")
}

fn write(translation: &str) -> Json {
    let mut base = Base::new(Mode::Write, EngineType::MVMZ);
    base.begin_maps(MAPINFOS.as_bytes(), Some(translation))
        .expect("begin_maps failed");

    let data = base
        .process_map("Map001.json", MAP.as_bytes())
        .expect("write failed")
        .expect("the map was not written");

    from_slice(data.as_ref()).expect("not valid JSON")
}

#[test]
fn valid_text_around_malformed_commands_is_read() {
    let text = read();

    assert!(text.contains("First."), "{text}");
    assert!(text.contains("Second."), "{text}");
    assert!(text.contains("Yes.<#>"), "{text}");
    assert!(text.contains("No.<#>"), "{text}");
    assert!(
        !text.contains("No code here."),
        "a command without a code was read as text:\n{text}"
    );
}

#[test]
fn valid_text_around_malformed_commands_is_written() {
    let text = read()
        .replace("Yes.<#>", "Yes.<#>[Yes.]")
        .replace("No.<#>", "No.<#>[No.]");

    let json = write(&text);
    let list = json["events"][1]["pages"][0]["list"].to_string();

    assert!(list.contains(r#""[Yes.]""#), "{list}");
    assert!(list.contains(r#""[No.]""#), "{list}");
    // The command without a code is written back as it was.
    assert!(list.contains("No code here."), "{list}");
}
