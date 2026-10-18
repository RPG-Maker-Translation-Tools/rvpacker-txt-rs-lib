//! Choice lists go through the same command walker as dialogue. A choice's `if(...)` condition is part of its text, as
//! a dialogue line's is, and the choice text itself must still be read.

use marshal_rs::Arena;
use rvpacker_txt_rs_lib::{EngineType, Mode, core::Base};

/// `CommonEvents.rvdata2` with one event: a choice list whose first choice ends in an `if(...)` condition, then the end.
fn common_events() -> Vec<u8> {
    let mut arena = Arena::builder();

    let yes = arena.push_string("Yes if(s[1])".to_owned());
    let no = arena.push_string("No".to_owned());
    let choices = arena.push_array(&[yes, no]).expect("choices");
    let cancel = arena.push_fixnum(0);
    let choice_params = arena.push_array(&[choices, cancel]).expect("choice parameters");
    let choice_code = arena.push_fixnum(102);
    let choice_command = arena
        .push_object(
            b"RPG::EventCommand".to_vec(),
            &[
                (b"@code".to_vec(), choice_code),
                (b"@parameters".to_vec(), choice_params),
            ],
        )
        .expect("choice command");

    let end_code = arena.push_fixnum(0);
    let end_params = arena.push_array(&[]).expect("end parameters");
    let end_command = arena
        .push_object(
            b"RPG::EventCommand".to_vec(),
            &[(b"@code".to_vec(), end_code), (b"@parameters".to_vec(), end_params)],
        )
        .expect("end command");

    let list = arena.push_array(&[choice_command, end_command]).expect("command list");
    let id = arena.push_fixnum(1);
    let name = arena.push_string("EV".to_owned());
    let event = arena
        .push_object(
            b"RPG::CommonEvent".to_vec(),
            &[
                (b"@id".to_vec(), id),
                (b"@name".to_vec(), name),
                (b"@list".to_vec(), list),
            ],
        )
        .expect("event");

    let nil = arena.push_nil();
    let root = arena.push_array(&[nil, event]).expect("root");
    arena.set_root(root);

    marshal_rs::dump(&arena)
}

#[test]
fn a_choice_condition_stays_in_the_translation() {
    let data = Base::new(Mode::read(), EngineType::VXAce)
        .process_other("CommonEvents.rvdata2", &common_events(), None)
        .expect("read failed")
        .expect("nothing was processed");
    let text = String::from_utf8(data.as_ref().to_vec()).expect("translation is not UTF-8");

    assert!(text.contains("Yes if(s[1])<#>"), "{text}");
    assert!(text.contains("No<#>"), "{text}");
}
