//! Text in VX Ace maps that carries markup outside the translatable words: the Advanced Text System's tile and actor
//! prefixes and shop talk. The markup stays out of the translation file and is put back around the translation on
//! write. A trailing `if(...)` condition is part of the text. The fixture is written by `tests/fixtures/vxace-text/generate.rb`.

use rvpacker_txt_rs_lib::{EngineType, Mode, core::Base};

const MAPINFOS: &[u8] = include_bytes!("fixtures/vxace-text/MapInfos.rvdata2");
const MAP: &[u8] = include_bytes!("fixtures/vxace-text/Map001.rvdata2");

fn read() -> String {
    let mut base = Base::new(Mode::read(), EngineType::VXAce);
    base.begin_maps(MAPINFOS, None).expect("begin_maps failed");
    base.process_map("Map001.rvdata2", MAP).expect("read failed");

    String::from_utf8(base.finish_maps().as_ref().to_vec()).expect("not UTF-8")
}

/// Writes the map with each `(source, translation)` filled in, returning the Marshal bytes of the new map.
fn write(translations: &[(&str, &str)]) -> Vec<u8> {
    let mut text = read();

    for (source, translation) in translations {
        let line = format!("{source}<#>");
        assert!(
            text.contains(&line),
            "{source:?} is not in the translation file:\n{text}"
        );
        text = text.replace(&line, &format!("{source}<#>{translation}"));
    }

    let mut base = Base::new(Mode::Write, EngineType::VXAce);
    base.begin_maps(MAPINFOS, Some(&text)).expect("begin_maps failed");

    base.process_map("Map001.rvdata2", MAP)
        .expect("write failed")
        .expect("the map was not written")
        .as_ref()
        .to_vec()
}

/// Marshal stores strings as their raw bytes, so the text is findable in the output as it is.
fn contains(bytes: &[u8], needle: &str) -> bool {
    bytes.windows(needle.len()).any(|window| window == needle.as_bytes())
}

#[test]
fn the_markup_is_kept_out_of_the_translation_file() {
    let text = read();

    assert!(text.contains("Hello there.<#>"), "{text}");
    assert!(text.contains("Second line.<#>"), "{text}");
    assert!(text.contains("Take it.if(s[1])<#>"), "{text}");
    assert!(text.contains("Welcome.<#>"), "{text}");

    // The actor prefix stays in the source line, so it can be restored if a translation drops it.
    assert!(text.contains(r"\et[1]Hi there.<#>"), "{text}");

    assert!(!text.contains(r"\nbt"), "{text}");
    assert!(!text.contains(r"\nblt"), "{text}");
    assert!(!text.contains("shop_talk"), "{text}");
}

#[test]
fn a_tile_prefix_is_put_back_before_the_translation() {
    let bytes = write(&[("Hello there.", "[Hello there.]"), ("Second line.", "[Second line.]")]);

    assert!(contains(&bytes, r"\nbt[Hello there.]"));
    assert!(contains(&bytes, r"\nblt[Second line.]"));
}

#[test]
fn an_actor_prefix_is_put_back_before_the_translation() {
    let bytes = write(&[(r"\et[1]Hi there.", "[Hi there.]")]);

    assert!(contains(&bytes, r"\et[1][Hi there.]"), "the actor prefix was lost");
}

#[test]
fn an_actor_prefix_the_translation_already_has_is_not_doubled() {
    let bytes = write(&[(r"\et[1]Hi there.", r"\et[1][Hi there.]")]);

    assert!(contains(&bytes, r"\et[1][Hi there.]"));
    assert!(!contains(&bytes, r"\et[1]\et[1]"), "the actor prefix was doubled");
}

#[test]
fn an_if_condition_is_translated_with_the_text() {
    let bytes = write(&[("Take it.if(s[1])", "[Take it.if(s[1])]")]);

    assert!(contains(&bytes, "[Take it.if(s[1])]"));
}

#[test]
fn choices_misc_system_and_credit_text_is_read_and_written_back() {
    // Each kind of line has its own write rule: choices are read from an array, misc and shop lines carry text in their
    // second parameter, and a code 101 is not a message box on VX Ace, so the line after it is read as a plain message.
    let sources = [
        "Yes.",
        "No.",
        "Old name.",
        "Other misc.",
        "System message.",
        "Credit line.",
        "After start.",
    ];

    let text = read();
    for source in sources {
        assert!(
            text.contains(&format!("{source}<#>")),
            "{source:?} is not in the translation file:\n{text}"
        );
    }

    let translations: Vec<(&str, String)> = sources.iter().map(|source| (*source, format!("[{source}]"))).collect();
    let translations: Vec<(&str, &str)> = translations
        .iter()
        .map(|(source, translation)| (*source, translation.as_str()))
        .collect();
    let bytes = write(&translations);

    for (_, translation) in &translations {
        assert!(contains(&bytes, translation), "{translation} was not written");
    }
}

#[test]
fn shop_talk_is_put_back_around_the_translation() {
    let bytes = write(&[("Welcome.", "[Welcome.]")]);

    assert!(
        contains(&bytes, r#"shop_talk_1 = "[Welcome.]""#),
        "{}",
        String::from_utf8_lossy(&bytes)
    );
}
