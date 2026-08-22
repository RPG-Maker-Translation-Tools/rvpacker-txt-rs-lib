//! Read a synthetic MV/MZ file into a translation, fill the translation in, and
//! write it back - the `initialize_translation` -> `flush_translation` ->
//! `finish_translation` lifecycle that every file kind runs through.
//!
//! MV/MZ is used because its files are plain JSON, so a fixture is a string
//! literal rather than a Marshal blob. The lifecycle itself is engine-agnostic.

use rvpacker_txt_rs_lib::{EngineType, Error, Mode, core::Base};
use serde_json::{Value as Json, from_slice};

const ACTORS: &str = r#"[null,
{"id":1,"name":"Alice","nickname":"the Brave","description":"A girl from town.","note":""},
{"id":2,"name":"Bob","nickname":"","description":"Second line\nof two.","note":""}]"#;

const MAPINFOS: &str = r#"[null,{"id":1,"name":"Town","order":1,"parentId":0}]"#;

const MAP001: &str = r#"{"displayName":"Riverside","events":[null,
{"id":1,"name":"EV001","x":5,"y":7,"pages":[
{"list":[{"code":101,"parameters":["",0,0,2]},
{"code":401,"parameters":["Hello there."]},
{"code":401,"parameters":["Second page of text."]},
{"code":0,"parameters":[]}]}]}]}"#;

/// Reads a file into its translation text.
fn read_other(filename: &str, content: &str) -> String {
    let mut base = Base::new(Mode::read(), EngineType::New);
    let data = base
        .process_other(filename, content.as_bytes(), None)
        .expect("read failed")
        .expect("nothing was processed");

    String::from_utf8(data.as_ref().to_vec()).expect("translation is not UTF-8")
}

/// Writes a filled-in translation back into the file, returning the new JSON.
fn write_other(filename: &str, content: &str, translation: &str) -> Json {
    let mut base = Base::new(Mode::Write, EngineType::New);
    let data = base
        .process_other(filename, content.as_bytes(), Some(translation))
        .expect("write failed")
        .expect("nothing was processed");

    from_slice(data.as_ref()).expect("written file is not valid JSON")
}

/// Fills every entry's translation column by applying `translate` to the source.
///
/// The display name is a metadata line that carries its own translation after the
/// separator, so its marker is stripped before the source reaches `translate`.
fn translate_all(text: &str, translate: impl Fn(&str) -> String) -> String {
    const DISPLAY_NAME: &str = "<!>IN-GAME DISPLAYED NAME: ";

    let mut output = String::with_capacity(text.len() * 2);

    for line in text.lines() {
        if let Some(source) = line.strip_suffix("<#>")
            && !source.is_empty()
        {
            let source = source.strip_prefix(DISPLAY_NAME).unwrap_or(source);

            output.push_str(line);
            output.push_str(&translate(source));
        } else {
            output.push_str(line);
        }

        output.push('\n');
    }

    output
}

mod other_files {
    use super::*;

    #[test]
    fn reading_collects_every_translatable_field() {
        let text = read_other("Actors.json", ACTORS);

        // Each object opens with its id and name.
        assert!(text.contains("<!>ID<#>1"));
        assert!(text.contains("<!>NAME<#>Alice"));
        assert!(text.contains("<!>ID<#>2"));
        assert!(text.contains("<!>NAME<#>Bob"));

        // Every non-empty field becomes an untranslated entry.
        assert!(text.contains("Alice<#>\n"));
        assert!(text.contains("the Brave<#>\n"));
        assert!(text.contains("A girl from town.<#>\n"));

        // Empty fields are not entries.
        assert!(!text.contains("<#><#>"));

        // Line breaks are stored as the library's marker, not raw.
        assert!(text.contains(r"Second line\#of two.<#>"));
        assert!(!text.lines().any(|line| line.is_empty()));
    }

    #[test]
    fn writing_puts_the_translations_back() {
        let text = read_other("Actors.json", ACTORS);
        let translated = translate_all(&text, |source| format!("[{source}]"));

        let json = write_other("Actors.json", ACTORS, &translated);

        assert_eq!(json[1]["name"], "[Alice]");
        assert_eq!(json[1]["nickname"], "[the Brave]");
        assert_eq!(json[1]["description"], "[A girl from town.]");
        assert_eq!(json[2]["name"], "[Bob]");

        // The marker is turned back into a real line break, and the entry keeps
        // its shape across the round trip.
        assert_eq!(json[2]["description"], "[Second line\nof two.]");

        // Untouched fields stay untouched, ids included.
        assert_eq!(json[1]["id"], 1);
        assert_eq!(json[2]["nickname"], "");
    }

    #[test]
    fn a_wholly_untranslated_file_writes_nothing() {
        // Every entry is unused, so there is nothing to rewrite.
        let text = read_other("Actors.json", ACTORS);
        let mut base = Base::new(Mode::Write, EngineType::New);

        let data = base
            .process_other("Actors.json", ACTORS.as_bytes(), Some(&text))
            .expect("write failed");

        assert!(data.is_none());
    }

    #[test]
    fn untranslated_fields_keep_their_original_text() {
        let text = read_other("Actors.json", ACTORS);
        // Only Alice's name is filled in.
        let translated = translate_all(&text, |source| {
            if source == "Alice" {
                "Алиса".to_owned()
            } else {
                String::new()
            }
        });

        let json = write_other("Actors.json", ACTORS, &translated);

        assert_eq!(json[1]["name"], "Алиса");
        assert_eq!(json[1]["nickname"], "the Brave");
        assert_eq!(json[2]["description"], "Second line\nof two.");
    }

    #[test]
    fn writing_without_a_translation_is_an_error() {
        let mut base = Base::new(Mode::Write, EngineType::New);
        assert!(matches!(
            base.process_other("Actors.json", ACTORS.as_bytes(), None),
            Err(Error::NoTranslation)
        ));
    }
}

mod purging {
    use super::*;

    #[test]
    fn entries_without_a_translation_are_dropped() {
        let text = read_other("Actors.json", ACTORS);
        // Translate the first actor only.
        let translated = translate_all(&text, |source| {
            if source == "Alice" {
                "Алиса".to_owned()
            } else {
                String::new()
            }
        });

        let mut base = Base::new(Mode::Purge, EngineType::New);
        let data = base
            .process_other("Actors.json", ACTORS.as_bytes(), Some(&translated))
            .expect("purge failed")
            .expect("nothing was processed");
        let purged =
            String::from_utf8(data.as_ref().to_vec()).expect("not UTF-8");

        assert!(purged.contains("Alice<#>Алиса"));
        assert!(!purged.contains("the Brave"));
        assert!(!purged.contains("A girl from town."));
    }
}

mod maps {
    use super::*;

    fn read_maps() -> String {
        let mut base = Base::new(Mode::read(), EngineType::New);
        // Event ids, names and positions are only emitted on request.
        base.map_events = true;
        base.begin_maps();
        base.process_map(
            "Map001.json",
            MAP001.as_bytes(),
            MAPINFOS.as_bytes(),
            None,
        )
        .expect("map read failed");

        let data = base.finish_maps();
        String::from_utf8(data.as_ref().to_vec()).expect("not UTF-8")
    }

    #[test]
    fn reading_collects_dialogue_and_map_metadata() {
        let text = read_maps();

        assert!(text.contains("<!>ID<#>1"));
        assert!(text.contains("<!>NAME<#>Town"));
        assert!(text.contains("<!>IN-GAME DISPLAYED NAME: Riverside"));
        assert!(text.contains("<!>EVENT ID<#>1"));

        // Consecutive 401 lines are one entry, joined by the break marker.
        assert!(text.contains(r"Hello there.\#Second page of text.<#>"));
    }

    #[test]
    fn writing_puts_the_dialogue_back() {
        let text = read_maps();
        let translated = translate_all(&text, |source| format!("[{source}]"));

        let mut base = Base::new(Mode::Write, EngineType::New);
        base.begin_maps();
        let data = base
            .process_map(
                "Map001.json",
                MAP001.as_bytes(),
                MAPINFOS.as_bytes(),
                Some(&translated),
            )
            .expect("map write failed")
            .expect("nothing was processed");

        let json: Json = from_slice(data.as_ref()).expect("not valid JSON");
        let list = &json["events"][1]["pages"][0]["list"];

        assert_eq!(list[1]["parameters"][0], "[Hello there.");
        assert_eq!(list[2]["parameters"][0], "Second page of text.]");
        assert_eq!(json["displayName"], "[Riverside]");
    }

    #[test]
    fn a_map_missing_from_mapinfos_is_skipped() {
        let mut base = Base::new(Mode::read(), EngineType::New);
        base.begin_maps();

        let data = base
            .process_map(
                "Map009.json",
                MAP001.as_bytes(),
                MAPINFOS.as_bytes(),
                None,
            )
            .expect("map read failed");

        assert!(data.is_none());
    }
}

mod system_file {
    use super::*;

    const SYSTEM: &str = r#"{"gameTitle":"Some Game",
"armorTypes":["","General Armor","Magic Armor"],
"skillTypes":["","Magic","Special"],
"weaponTypes":["","Dagger","Sword"],
"equipTypes":["","Weapon","Shield"],
"elements":["","Physical","Fire"],
"currencyUnit":"G",
"terms":{"basic":["Level","Lv","HP","HP"],"commands":["Fight","Escape"],
"params":["Max HP","Attack"],"messages":{"actionFailure":"There was no effect!"}}}"#;

    fn read_system() -> String {
        let mut base = Base::new(Mode::read(), EngineType::New);
        let data = base
            .process_system(SYSTEM.as_bytes(), None)
            .expect("read failed")
            .expect("nothing was processed");

        String::from_utf8(data.as_ref().to_vec()).expect("not UTF-8")
    }

    #[test]
    fn reading_collects_every_section() {
        let text = read_system();

        for name in [
            "Armor Types",
            "Elements",
            "Skill Types",
            "Weapon Types",
            "Equip Types",
            "Terms",
            "Currency Unit",
            "Game Title",
        ] {
            assert!(
                text.contains(&format!("<!>NAME<#>{name}")),
                "{name} section missing"
            );
        }

        assert!(text.contains("Magic Armor<#>"));
        assert!(text.contains("Fight<#>"));
        assert!(text.contains("Some Game<#>"));
        // The empty first slot of each type array is not an entry.
        assert!(!text.contains("<#><#>"));
    }

    #[test]
    fn writing_puts_the_sections_back() {
        let text = read_system();
        let translated = translate_all(&text, |source| format!("[{source}]"));

        let mut base = Base::new(Mode::Write, EngineType::New);
        let data = base
            .process_system(SYSTEM.as_bytes(), Some(&translated))
            .expect("write failed")
            .expect("nothing was processed");
        let json: Json = from_slice(data.as_ref()).expect("not valid JSON");

        assert_eq!(json["armorTypes"][1], "[General Armor]");
        assert_eq!(json["terms"]["commands"][0], "[Fight]");
        assert_eq!(json["currencyUnit"], "[G]");
        // The empty slot is untouched.
        assert_eq!(json["armorTypes"][0], "");
        // The title does not come from the translation file - the caller supplies
        // it, because it also names the output directory.
        assert_eq!(json["gameTitle"], "Some Game");
    }

    #[test]
    fn the_game_title_comes_from_the_caller() {
        let text = read_system();
        let translated = translate_all(&text, |source| format!("[{source}]"));

        let mut base = Base::new(Mode::Write, EngineType::New);
        base.set_game_title("Какая-то игра");

        let data = base
            .process_system(SYSTEM.as_bytes(), Some(&translated))
            .expect("write failed")
            .expect("nothing was processed");
        let json: Json = from_slice(data.as_ref()).expect("not valid JSON");

        assert_eq!(json["gameTitle"], "Какая-то игра");
    }
}
