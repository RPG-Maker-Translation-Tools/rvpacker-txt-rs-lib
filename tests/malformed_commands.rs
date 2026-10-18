//! Each malformed command is put between two valid dialogue lines on its own. The command must be skipped, and the two
//! lines around it must still be read and written, so one bad command can't cost its neighbours.

use rvpacker_txt_rs_lib::{EngineType, Mode, core::Base};
use serde_json::{Value as Json, from_slice};

/// Commands with a wrong shape: missing or mistyped parameters, symbols only, or a choice list that isn't one.
const MALFORMED: &[&str] = &[
    r#"{"code":401}"#,
    r#"{"code":401,"parameters":[1]}"#,
    r#"{"code":102,"parameters":"Yes"}"#,
    r#"{"code":102,"parameters":[[1,2],0]}"#,
    r#"{"code":102,"parameters":[["Yes."],"x"]}"#,
    r#"{"code":102,"parameters":[["---"],0]}"#,
    r#"{"code":102,"parameters":[[],0]}"#,
    r#"{"code":402,"parameters":"0"}"#,
    r#"{"code":101,"parameters":null}"#,
    r#"{"code":357,"parameters":["Plugin","Command"]}"#,
];

/// A map with `command` after `First.` in event 1, and `Second.` in event 2. The two lines are in different events, so
/// they can't merge into one message with each other.
fn map_with(command: &str) -> String {
    format!(
        r#"{{"displayName":"Town","events":[null,
{{"id":1,"name":"EV001","x":1,"y":1,"pages":[
{{"list":[{{"code":401,"parameters":["First."]}},
{command},
{{"code":0,"parameters":[]}}]}}]}},
{{"id":2,"name":"EV002","x":2,"y":2,"pages":[
{{"list":[{{"code":401,"parameters":["Second."]}},
{{"code":0,"parameters":[]}}]}}]}}]}}"#
    )
}

const MAPINFOS: &[u8] = br#"[null,{"id":1,"name":"Town","order":1,"parentId":0}]"#;

fn read(map: &str) -> String {
    let mut base = Base::new(Mode::read(), EngineType::MVMZ);
    base.begin_maps(MAPINFOS, None).expect("begin_maps failed");
    base.process_map("Map001.json", map.as_bytes()).expect("read failed");

    String::from_utf8(base.finish_maps().as_ref().to_vec()).expect("not UTF-8")
}

fn write(map: &str, translation: &str) -> Json {
    let mut base = Base::new(Mode::Write, EngineType::MVMZ);
    base.begin_maps(MAPINFOS, Some(translation)).expect("begin_maps failed");

    let data = base
        .process_map("Map001.json", map.as_bytes())
        .expect("write failed")
        .expect("the map was not written");

    from_slice(data.as_ref()).expect("not valid JSON")
}

#[test]
fn a_malformed_command_does_not_cost_its_neighbours() {
    for command in MALFORMED {
        let map = map_with(command);
        let text = read(&map);

        assert!(text.contains("First.<#>"), "{command}: First. was not read:\n{text}");
        assert!(text.contains("Second.<#>"), "{command}: Second. was not read:\n{text}");

        let translated = text
            .replace("First.<#>", "First.<#>[First.]")
            .replace("Second.<#>", "Second.<#>[Second.]");
        let json = write(&map, &translated);
        let first = json["events"][1]["pages"][0]["list"].to_string();
        let second = json["events"][2]["pages"][0]["list"].to_string();

        assert!(
            first.contains(r#""[First.]""#),
            "{command}: First. was not written:\n{first}"
        );
        assert!(
            second.contains(r#""[Second.]""#),
            "{command}: Second. was not written:\n{second}"
        );
    }
}
