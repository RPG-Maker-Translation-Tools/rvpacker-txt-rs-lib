//! `Change Hero Name` and `Change Hero Title` each carry one string the player sees, so they are read and written back
//! like a choice option.

use rm2k::{
    engine::Engine,
    field::{DbStr, LcfStruct, List},
    raw::{EventCommand, EventCommandList},
    rpg::{CommonEvent, enums::EventCommandCode as Code},
};
use rvpacker_txt_rs_lib::{EngineType, Mode, core::Base};

fn common_events() -> List<CommonEvent<'static>> {
    let command = |code: Code, text: &str| EventCommand {
        code: code.0,
        string: DbStr::from_vec(text.as_bytes().to_vec()),
        ..Default::default()
    };

    let mut event = CommonEvent::defaults(Engine::R2K);
    event.id = 1;
    event.name = DbStr::from_vec(b"Intro".to_vec());
    event.event_commands = EventCommandList(vec![
        command(Code::CHANGE_HERO_NAME, "Alex"),
        command(Code::CHANGE_HERO_TITLE, "Knight"),
    ]);

    List(vec![event])
}

#[test]
fn hero_names_and_titles_are_read_and_written_back() {
    let mut base = Base::new(Mode::read(), EngineType::RM2K);
    let data = base
        .process_rm2k_commonevents(&mut common_events(), None)
        .unwrap()
        .unwrap();
    let text = String::from_utf8(data.as_ref().to_vec()).unwrap();

    assert!(text.contains("Alex<#>"), "{text}");
    assert!(text.contains("Knight<#>"), "{text}");

    let translated = text
        .replace("Alex<#>", "Alex<#>Алекс")
        .replace("Knight<#>", "Knight<#>Рыцарь");
    let mut list = common_events();
    Base::new(Mode::Write, EngineType::RM2K)
        .process_rm2k_commonevents(&mut list, Some(&translated))
        .unwrap();

    let strings: Vec<&[u8]> = list[0]
        .event_commands
        .0
        .iter()
        .map(|command| command.string.as_bytes())
        .collect();
    assert_eq!(strings, ["Алекс".as_bytes(), "Рыцарь".as_bytes()]);
}
