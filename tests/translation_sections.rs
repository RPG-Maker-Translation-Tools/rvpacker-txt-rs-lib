//! How an append treats the sections of a translation file: ids the game no longer has text for, user comments, and
//! repeated or stray sections.

use rvpacker_txt_rs_lib::{BaseFlags, EngineType, Error, Mode, ProcessedData, core::Base};

const APPEND: Mode = Mode::Read {
    append: true,
    force: false,
};

/// An `Items.json` whose one item has the given description.
fn items(description: &str) -> String {
    format!(r#"[null,{{"id":1,"name":"","description":"{description}"}}]"#)
}

fn append(game: &str, translation: &str) -> Result<String, Error> {
    let mut base = Base::new(APPEND, EngineType::MVMZ);
    let data = base
        .process_other("Items.json", game.as_bytes(), Some(translation))?
        .expect("an append always yields a translation file");

    Ok(text(&data))
}

fn text(data: &ProcessedData) -> String {
    let bytes: &[u8] = data.as_ref();

    String::from_utf8(bytes.to_vec()).expect("translation output is UTF-8")
}

#[test]
fn a_translation_the_game_no_longer_has_keeps_its_section_header() {
    let output = append(&items(""), "<!>ID<#>1\nA rusty sword<#>Ржавый меч\n").unwrap();

    assert!(output.starts_with("<!>ID<#>1\n"), "the header is missing:\n{output}");
    assert!(output.contains("A rusty sword<#>Ржавый меч"));
}

#[test]
fn comments_after_the_last_entry_survive_an_append() {
    let output = append(
        &items("A rusty sword"),
        "<!>ID<#>1\nA rusty sword<#>Ржавый меч\n<!> keep me\n",
    )
    .unwrap();

    assert!(output.ends_with("<!> keep me"), "the comment is gone:\n{output}");
}

#[test]
fn a_repeated_section_id_is_rejected() {
    let result = append(
        &items("A rusty sword"),
        "<!>ID<#>1\nA rusty sword<#>One\n<!>ID<#>1\nA rusty sword<#>Two\n",
    );

    assert!(matches!(result, Err(Error::DuplicateSection { id: 1, line: 3 })));
}

#[test]
fn a_section_the_game_no_longer_has_is_kept() {
    let output = append(
        &items("A rusty sword"),
        "<!>ID<#>1\nA rusty sword<#>Меч\n<!>ID<#>2\nGone item<#>Пропавший\n",
    )
    .unwrap();

    assert!(output.contains("<!>ID<#>2\nGone item<#>Пропавший"), "{output}");
    assert!(output.find("<!>ID<#>1").unwrap() < output.find("<!>ID<#>2").unwrap());
}

#[test]
fn skip_obsolete_drops_a_section_the_game_no_longer_has() {
    let mut base = Base::new(APPEND, EngineType::MVMZ);
    base.flags = BaseFlags::SkipObsolete;

    let data = base
        .process_other(
            "Items.json",
            items("A rusty sword").as_bytes(),
            Some("<!>ID<#>1\nA rusty sword<#>Меч\n<!>ID<#>2\nGone item<#>Пропавший\n"),
        )
        .unwrap()
        .unwrap();
    let output = text(&data);

    assert!(output.contains("A rusty sword<#>Меч"), "{output}");
    assert!(!output.contains("Gone item"), "{output}");
}
