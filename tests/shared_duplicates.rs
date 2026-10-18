//! With duplicates removed, a line shared by several entries is stored once, in the first entry's section. An entry whose
//! text is all shared has no section of its own, and must still be written with its translations, as a map is.

use rvpacker_txt_rs_lib::{DuplicateMode, EngineType, Mode, core::Base};
use serde_json::{Value as Json, from_slice};

/// Actor 2 has nothing but text that actor 1 already has.
const ACTORS: &str = r#"[null,
{"id":1,"name":"Alice","nickname":"","description":"A girl from town.","note":""},
{"id":2,"name":"Alice","nickname":"","description":"A girl from town.","note":""}]"#;

/// Reads `content` with duplicates removed and returns the translation text.
fn read_actors(content: &str) -> String {
    let mut base = Base::new(Mode::read(), EngineType::MVMZ);
    base.duplicate_mode = DuplicateMode::Remove;

    let data = base
        .process_other("Actors.json", content.as_bytes(), None)
        .expect("read failed")
        .expect("nothing was processed");

    String::from_utf8(data.as_ref().to_vec()).expect("translation is not UTF-8")
}

/// Fills each entry's translation column with `translate(source)`; `None` leaves it untranslated.
fn fill(text: &str, translate: impl Fn(&str) -> Option<String>) -> String {
    let mut output = String::with_capacity(text.len() * 2);

    for line in text.lines() {
        output.push_str(line);

        if let Some(source) = line.strip_suffix("<#>").filter(|source| !source.is_empty())
            && let Some(translation) = translate(source)
        {
            output.push_str(&translation);
        }

        output.push('\n');
    }

    output
}

/// Writes `ACTORS` with `translation` and returns the new JSON, or `None` if the file wasn't written.
fn write_actors(translation: &str) -> Option<Json> {
    let mut base = Base::new(Mode::Write, EngineType::MVMZ);
    base.duplicate_mode = DuplicateMode::Remove;

    base.process_other("Actors.json", ACTORS.as_bytes(), Some(translation))
        .expect("write failed")
        .map(|data| from_slice(data.as_ref()).expect("written file is not valid JSON"))
}

#[test]
fn an_actor_whose_only_text_is_shared_with_an_earlier_actor_is_still_translated() {
    let translated = fill(&read_actors(ACTORS), |source| match source {
        "Alice" => Some("[name]".to_owned()),
        "A girl from town." => Some("[shared]".to_owned()),
        _ => None,
    });

    let written = write_actors(&translated).expect("actors must be written");

    assert_eq!(written[1]["name"], "[name]");
    assert_eq!(written[2]["name"], "[name]", "actor 2's name must be translated");
    assert_eq!(
        written[2]["description"], "[shared]",
        "actor 2's shared description must be translated"
    );
}

#[test]
fn a_file_with_no_section_for_its_entries_and_nothing_shared_is_not_written() {
    // The translation only has a section for actor 99, which the file doesn't have.
    const OTHER: &str = r#"[null,
{"id":99,"name":"Zed","nickname":"","description":"Far away.","note":""}]"#;

    let translation = fill(&read_actors(OTHER), |source| Some(format!("[{source}]")));

    assert_eq!(write_actors(&translation), None);
}
