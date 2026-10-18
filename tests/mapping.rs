//! Translations must land on the map, event or item they were read from, by id: never by position in the file, the
//! order of MapInfos, or the order sections were written in. These tests use gapped ids (1, 4, 7), so a positional
//! mapping would put the translations of 4 and 7 into the files of 1 and 2.

use rvpacker_txt_rs_lib::{DuplicateMode, EngineType, Mode, core::Base};
use serde_json::{Value as Json, from_slice};

/// MapInfos is a sparse array indexed by map id, with `null` for the ids that aren't maps. Maps 1, 4 and 7 are in it,
/// with `order` disagreeing with id order.
const MAPINFOS: &str = r#"[null,
{"id":1,"name":"Town","order":3,"parentId":0},
null,null,
{"id":4,"name":"Cave","order":1,"parentId":0},
null,null,
{"id":7,"name":"Castle","order":2,"parentId":0}]"#;

/// A map with one event that says `dialogue`.
fn map_file(place: &str, dialogue: &str) -> String {
    format!(
        r#"{{"displayName":"{place}","events":[null,
{{"id":1,"name":"EV001","x":5,"y":7,"pages":[
{{"list":[{{"code":101,"parameters":["",0,0,2]}},
{{"code":401,"parameters":["{dialogue}"]}},
{{"code":0,"parameters":[]}}]}}]}}]}}"#
    )
}

fn three_maps() -> Vec<(&'static str, String)> {
    vec![
        ("Map001.json", map_file("Town", "Hello Town.")),
        ("Map004.json", map_file("Cave", "Hello Cave.")),
        ("Map007.json", map_file("Castle", "Hello Castle.")),
    ]
}

/// Reads `files` into a translation file.
fn read_maps(files: &[(&str, String)], duplicates: DuplicateMode, map_events: bool) -> String {
    let mut base = Base::new(Mode::read(), EngineType::MVMZ);
    base.duplicate_mode = duplicates;
    base.map_events = map_events;
    base.begin_maps(MAPINFOS.as_bytes(), None).expect("begin_maps failed");

    for (name, content) in files {
        base.process_map(name, content.as_bytes()).expect("map read failed");
    }

    String::from_utf8(base.finish_maps().as_ref().to_vec()).expect("not UTF-8")
}

/// Writes `files` with `translation`, returning each map's new JSON, or `None` for a map that wasn't written.
fn write_maps(files: &[(&str, String)], duplicates: DuplicateMode, translation: &str) -> Vec<Option<Json>> {
    let mut base = Base::new(Mode::Write, EngineType::MVMZ);
    base.duplicate_mode = duplicates;
    base.begin_maps(MAPINFOS.as_bytes(), Some(translation))
        .expect("begin_maps failed");

    files
        .iter()
        .map(|(name, content)| {
            base.process_map(name, content.as_bytes())
                .expect("map write failed")
                .map(|data| from_slice(data.as_ref()).expect("not valid JSON"))
        })
        .collect()
}

/// Fills the translation column of each entry with `translate(section id, source)`; `None` leaves that entry untranslated.
fn fill(text: &str, translate: impl Fn(u32, &str) -> Option<String>) -> String {
    const DISPLAY_NAME: &str = "<!>IN-GAME DISPLAYED NAME: ";

    let mut id = 0;
    let mut output = String::with_capacity(text.len() * 2);

    for line in text.lines() {
        if let Some(rest) = line.strip_prefix("<!>ID<#>") {
            id = rest.trim().parse().expect("section id is not a number");
        }

        output.push_str(line);

        if let Some(source) = line.strip_suffix("<#>").filter(|source| !source.is_empty()) {
            let source = source.strip_prefix(DISPLAY_NAME).unwrap_or(source);

            if let Some(translation) = translate(id, source) {
                output.push_str(&translation);
            }
        }

        output.push('\n');
    }

    output
}

/// The text of section `id`, up to the next section.
fn section(text: &str, id: u32) -> &str {
    let start = text
        .find(&format!("<!>ID<#>{id}\n"))
        .or_else(|| text.find(&format!("<!>ID<#>{id}\r\n")))
        .unwrap_or_else(|| panic!("section {id} is missing"));
    let rest = &text[start..];
    let end = rest[1..].find("<!>ID<#>").map_or(rest.len(), |next| next + 1);

    &rest[..end]
}

/// The first line of dialogue (code 401) of event `event`'s first page.
fn dialogue(map: &Json, event: usize) -> &Json {
    let list = map["events"][event]["pages"][0]["list"]
        .as_array()
        .expect("event has no command list");
    let line = list
        .iter()
        .find(|command| command["code"] == 401)
        .expect("event has no dialogue");

    &line["parameters"][0]
}

mod by_id {
    use super::*;

    #[test]
    fn gapped_ids_write_each_translation_to_its_own_map() {
        let files = three_maps();
        let text = read_maps(&files, DuplicateMode::Remove, false);
        let translated = fill(&text, |_, source| Some(format!("[{source}]")));

        let written = write_maps(&files, DuplicateMode::Remove, &translated);

        for (index, place) in [(0, "Town"), (1, "Cave"), (2, "Castle")] {
            let map = written[index].as_ref().expect("a translated map must be written");

            assert_eq!(map["displayName"], format!("[{place}]"), "map {index}");
            assert_eq!(dialogue(map, 1), &format!("[Hello {place}.]"), "map {index}");
        }
    }

    #[test]
    fn an_untranslated_map_in_the_middle_leaves_the_others_alone() {
        let files = three_maps();
        let text = read_maps(&files, DuplicateMode::Remove, false);
        // Map 4 gets no translations at all; 1 and 7 are translated.
        let translated = fill(&text, |id, source| (id != 4).then(|| format!("[{source}]")));

        let written = write_maps(&files, DuplicateMode::Remove, &translated);

        let town = written[0].as_ref().expect("map 1 must be written");
        let castle = written[2].as_ref().expect("map 7 must be written");
        assert_eq!(town["displayName"], "[Town]");
        assert_eq!(dialogue(town, 1), "[Hello Town.]");
        assert_eq!(castle["displayName"], "[Castle]");
        assert_eq!(dialogue(castle, 1), "[Hello Castle.]");

        // Map 4 may be left out or written back untranslated, but no other map's text may land in it.
        if let Some(cave) = &written[1] {
            let text = cave.to_string();
            assert!(!text.contains("[Town]") && !text.contains("[Castle]"), "{text}");
        }
    }

    #[test]
    fn allowed_duplicates_are_translated_per_map() {
        // The same line in two maps gets a different translation in each: each map's own section decides.
        let files = [
            ("Map001.json", map_file("Town", "Yes.")),
            ("Map007.json", map_file("Castle", "Yes.")),
        ];
        let text = read_maps(&files, DuplicateMode::Allow, false);
        let translated = fill(&text, |id, source| (source == "Yes.").then(|| format!("[{id}]")));

        let written = write_maps(&files, DuplicateMode::Allow, &translated);

        assert_eq!(dialogue(written[0].as_ref().unwrap(), 1), "[1]");
        assert_eq!(dialogue(written[1].as_ref().unwrap(), 1), "[7]");
    }

    #[test]
    fn removed_duplicates_are_translated_in_every_map_they_appear_in() {
        // With duplicates removed, the line is listed once, under the first map that has it. The second map has to find
        // that translation too, or it would be left untranslated.
        let files = [
            ("Map001.json", map_file("Town", "Yes.")),
            ("Map007.json", map_file("Castle", "Yes.")),
        ];
        let text = read_maps(&files, DuplicateMode::Remove, false);
        assert_eq!(
            text.matches("Yes.<#>").count(),
            1,
            "the duplicate was not removed:\n{text}"
        );

        // Display names are translated, as they are in a real project; only the shared line is the shared one.
        let translated = fill(&text, |_, source| {
            Some(if source == "Yes." {
                "[shared]".to_owned()
            } else {
                format!("[{source}]")
            })
        });
        let written = write_maps(&files, DuplicateMode::Remove, &translated);

        assert_eq!(dialogue(written[0].as_ref().unwrap(), 1), "[shared]");
        assert_eq!(dialogue(written[1].as_ref().unwrap(), 1), "[shared]");
    }

    #[test]
    fn a_map_with_nothing_translated_is_still_not_written() {
        // Map 7 has a section of its own, with an untranslated line that no other map shares: it must stay unwritten.
        let files = [
            ("Map001.json", map_file("Town", "Hello Town.")),
            ("Map007.json", map_file("", "Only here.")),
        ];
        let text = read_maps(&files, DuplicateMode::Remove, false);
        let translated = fill(&text, |id, source| (id == 1).then(|| format!("[{source}]")));

        let written = write_maps(&files, DuplicateMode::Remove, &translated);

        assert_eq!(
            dialogue(written[0].as_ref().expect("map 1 must be written"), 1),
            "[Hello Town.]"
        );
        assert!(written[1].is_none(), "a map with nothing translated was written");
    }

    /// A map whose only text is a line shared with an earlier map, and which has no display name of its own. Its section
    /// is empty once the duplicate is removed, but the line is translated in the earlier map's section, so the map must
    /// be written with it.
    #[test]
    fn a_map_whose_only_text_is_shared_with_an_earlier_map_is_still_translated() {
        let files = [
            ("Map001.json", map_file("Town", "Yes.")),
            ("Map007.json", map_file("", "Yes.")),
        ];
        let text = read_maps(&files, DuplicateMode::Remove, false);
        let translated = fill(&text, |_, source| (source == "Yes.").then(|| "[shared]".to_owned()));

        let written = write_maps(&files, DuplicateMode::Remove, &translated);

        assert_eq!(
            dialogue(written[1].as_ref().expect("map 7 must be written"), 1),
            "[shared]"
        );
    }

    #[test]
    fn events_with_gapped_ids_keep_their_own_dialogue() {
        // `events` holds nulls where ids 2, 3, 5 and 6 would be: event 4 is the fourth slot, not the second entry.
        let map = r#"{"displayName":"Town","events":[null,
{"id":1,"name":"EV001","x":1,"y":1,"pages":[{"list":[{"code":401,"parameters":["Alpha."]},{"code":0,"parameters":[]}]}]},
null,null,
{"id":4,"name":"EV004","x":2,"y":2,"pages":[{"list":[{"code":401,"parameters":["Beta."]},{"code":0,"parameters":[]}]}]},
null,null,
{"id":7,"name":"EV007","x":3,"y":3,"pages":[{"list":[{"code":401,"parameters":["Gamma."]},{"code":0,"parameters":[]}]}]}]}"#;
        let files = [("Map001.json", map.to_owned())];

        let text = read_maps(&files, DuplicateMode::Remove, true);
        let translated = fill(&text, |_, source| Some(format!("[{source}]")));
        let written = write_maps(&files, DuplicateMode::Remove, &translated);

        let written = written[0].as_ref().expect("the map must be written");
        assert_eq!(dialogue(written, 1), "[Alpha.]");
        assert_eq!(dialogue(written, 4), "[Beta.]");
        assert_eq!(dialogue(written, 7), "[Gamma.]");
    }

    #[test]
    fn items_with_gapped_ids_are_written_by_id() {
        let items = r#"[null,
{"id":1,"name":"Potion","description":"Restores HP.","note":""},
null,null,
{"id":4,"name":"Ether","description":"Restores MP.","note":""},
null,null,
{"id":7,"name":"Elixir","description":"Restores all.","note":""}]"#;

        let mut base = Base::new(Mode::read(), EngineType::MVMZ);
        let data = base
            .process_other("Items.json", items.as_bytes(), None)
            .expect("read failed")
            .expect("nothing was processed");
        let text = String::from_utf8(data.as_ref().to_vec()).expect("not UTF-8");

        // Translate items 1 and 7, leave 4 untranslated.
        let translated = fill(&text, |_, source| {
            (!matches!(source, "Ether" | "Restores MP.")).then(|| format!("[{source}]"))
        });

        let mut base = Base::new(Mode::Write, EngineType::MVMZ);
        let data = base
            .process_other("Items.json", items.as_bytes(), Some(&translated))
            .expect("write failed")
            .expect("nothing was processed");
        let json: Json = from_slice(data.as_ref()).expect("not valid JSON");

        assert_eq!(json[1]["name"], "[Potion]");
        assert_eq!(json[1]["description"], "[Restores HP.]");
        assert_eq!(json[4]["name"], "Ether");
        assert_eq!(json[4]["description"], "Restores MP.");
        assert_eq!(json[7]["name"], "[Elixir]");
        assert_eq!(json[7]["description"], "[Restores all.]");
    }

    #[test]
    fn appending_a_map_between_translated_ones_keeps_their_translations() {
        let all = three_maps();
        let first = read_maps(&[all[0].clone(), all[2].clone()], DuplicateMode::Remove, false);
        let first = fill(&first, |_, source| Some(format!("[{source}]")));

        // Map 4 is new: reading all three in append mode must add it without disturbing 1 and 7.
        let mut base = Base::new(
            Mode::Read {
                append: true,
                force: false,
            },
            EngineType::MVMZ,
        );
        base.begin_maps(MAPINFOS.as_bytes(), Some(&first))
            .expect("begin_maps failed");
        for (name, content) in &all {
            base.process_map(name, content.as_bytes()).expect("map read failed");
        }
        let appended = String::from_utf8(base.finish_maps().as_ref().to_vec()).expect("not UTF-8");

        assert!(section(&appended, 1).contains("<#>[Hello Town.]"), "{appended}");
        assert!(section(&appended, 7).contains("<#>[Hello Castle.]"), "{appended}");
        assert!(section(&appended, 4).contains("Hello Cave."), "{appended}");
        assert!(!section(&appended, 4).contains("[Hello Cave.]"), "{appended}");
    }

    #[test]
    fn a_translation_file_with_crlf_line_endings_writes_clean_text() {
        let files = three_maps();
        let text = read_maps(&files, DuplicateMode::Remove, false);
        let translated = fill(&text, |_, source| Some(format!("[{source}]"))).replace('\n', "\r\n");

        let written = write_maps(&files, DuplicateMode::Remove, &translated);

        let town = written[0].as_ref().expect("map 1 must be written");
        assert_eq!(town["displayName"], "[Town]");
        assert_eq!(dialogue(town, 1), "[Hello Town.]");
    }

    #[test]
    fn a_translation_file_with_a_byte_order_mark_still_writes_every_map() {
        // Notepad saves UTF-8 with a BOM. The first section's id line then doesn't start where the parser looks for it,
        // which silently drops that section.
        let files = three_maps();
        let text = read_maps(&files, DuplicateMode::Remove, false);
        let translated = format!("\u{feff}{}", fill(&text, |_, source| Some(format!("[{source}]"))));

        let written = write_maps(&files, DuplicateMode::Remove, &translated);

        let town = written[0]
            .as_ref()
            .expect("map 1 must be written even with a BOM in front of the file");
        assert_eq!(dialogue(town, 1), "[Hello Town.]");
    }
}
