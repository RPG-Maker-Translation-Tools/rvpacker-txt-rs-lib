//! A map's display name is untagged text like any other, so it is read in the read encoding.

use marshal_rs::arena::Arena;
use rvpacker_txt_rs_lib::{EngineType, Mode, core::Base};

fn map_with_display_name(display_name: Vec<u8>) -> Vec<u8> {
    let mut arena = Arena::builder();

    let code = arena.push_fixnum(0);
    let params = arena.push_array(&[]).unwrap();
    let end = arena
        .push_object(
            b"RPG::EventCommand".to_vec(),
            &[(b"@code".to_vec(), code), (b"@parameters".to_vec(), params)],
        )
        .unwrap();
    let list = arena.push_array(&[end]).unwrap();
    let page = arena
        .push_object(b"RPG::Event::Page".to_vec(), &[(b"@list".to_vec(), list)])
        .unwrap();
    let pages = arena.push_array(&[page]).unwrap();

    let event_id = arena.push_fixnum(1);
    let event_name = arena.push_string("EV001".to_owned());
    let event = arena
        .push_object(
            b"RPG::Event".to_vec(),
            &[
                (b"@id".to_vec(), event_id),
                (b"@name".to_vec(), event_name),
                (b"@pages".to_vec(), pages),
            ],
        )
        .unwrap();
    let key = arena.push_fixnum(1);
    let events = arena.push_hash(&[(key, event)], None).unwrap();

    let name = arena.push_bytes(display_name);
    let map = arena
        .push_object(
            b"RPG::Map".to_vec(),
            &[(b"@events".to_vec(), events), (b"@display_name".to_vec(), name)],
        )
        .unwrap();
    arena.set_root(map);

    marshal_rs::dump(&arena)
}

fn mapinfos() -> Vec<u8> {
    let mut arena = Arena::builder();

    let name = arena.push_string("Town".to_owned());
    let order = arena.push_fixnum(1);
    let info = arena
        .push_object(
            b"RPG::MapInfo".to_vec(),
            &[(b"@name".to_vec(), name), (b"@order".to_vec(), order)],
        )
        .unwrap();
    let key = arena.push_fixnum(1);
    let root = arena.push_hash(&[(key, info)], None).unwrap();
    arena.set_root(root);

    marshal_rs::dump(&arena)
}

#[test]
fn a_display_name_is_read_in_the_read_encoding() {
    let (bytes, _, _) = encoding_rs::SHIFT_JIS.encode("勇者の町");
    let content = map_with_display_name(bytes.into_owned());

    let mut base = Base::new(Mode::read(), EngineType::XP);
    base.set_read_encoding(Some(encoding_rs::SHIFT_JIS));
    base.begin_maps(&mapinfos(), None).expect("begin_maps failed");
    base.process_map("Map001.rxdata", &content).expect("read failed");

    let text = String::from_utf8(base.finish_maps().as_ref().to_vec()).expect("not UTF-8");
    assert!(text.contains("勇者の町"), "{text}");
}
