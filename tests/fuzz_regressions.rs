//! Inputs that crashed the fuzz targets, replayed through the same code as `fuzz/fuzz_targets/`. Those targets only run
//! under libFuzzer, so each body is copied here: they keep the crashes from coming back in a plain `cargo test`.

use rvpacker_txt_rs_lib::{
    DuplicateMode, EngineType, Mode,
    core::Base,
    get_ini_title, get_ini_title_rm2k, get_system_title,
    json::{generate_file, write_file},
    parse_ignore,
};

/// `fuzz/fuzz_targets/marshal.rs`.
fn replay_marshal(input: &[u8]) {
    let Some((&selector, file)) = input.split_first() else {
        return;
    };
    let name = ["Actors.rxdata", "Save01.rvdata2", "Scripts.rxdata"][selector as usize % 3];

    let Ok(json) = generate_file(file, name, None) else {
        return;
    };
    if name.starts_with("Scripts") {
        return;
    }

    let dumped = write_file(&json).expect("own JSON must convert back");
    let json_again = generate_file(&dumped, name, None).expect("own Marshal output must load");
    assert!(json == json_again, "JSON is not stable across a Marshal round trip");
}

/// `fuzz/fuzz_targets/process.rs`.
fn replay_process(input: &[u8]) {
    const ENGINES: [EngineType; 4] = [EngineType::MVMZ, EngineType::VXAce, EngineType::VX, EngineType::XP];
    const OTHER_NAMES: [&str; 4] = ["Actors", "CommonEvents", "Troops", "Items"];

    let [selector, len_lo, len_hi, rest @ ..] = input else {
        return;
    };
    let engine = ENGINES[(selector & 3) as usize];
    let kind = (selector >> 2) & 7;
    let mode = [Mode::read(), Mode::Write, Mode::Purge][((selector >> 5) % 3) as usize];

    let (data, translation) = rest.split_at(usize::from(u16::from_le_bytes([*len_lo, *len_hi])).min(rest.len()));
    let Ok(translation) = std::str::from_utf8(translation) else {
        return;
    };

    let ext = match engine {
        EngineType::MVMZ => "json",
        EngineType::VXAce => "rvdata2",
        EngineType::VX => "rvdata",
        _ => "rxdata",
    };
    let other_name = format!("{}.{ext}", OTHER_NAMES[usize::from(kind) % OTHER_NAMES.len()]);
    let map_name = format!("Map001.{ext}");

    let run = |base: &mut Base, translation: Option<&str>| {
        let _ = match kind {
            0..=3 => base.process_other(&other_name, data, translation),
            4 => base.process_system(data, translation),
            5 => base.process_scripts(data, translation),
            6 => base.process_plugins(data, translation),
            _ => base
                .begin_maps(data, translation)
                .and_then(|()| base.process_map(&map_name, data)),
        };
    };

    let mut base = Base::new(mode, engine);
    run(&mut base, (!mode.is_read()).then_some(translation));

    // Whatever reads must also write back from its own translation.
    if mode.is_read() && kind <= 5 {
        let mut reader = Base::new(Mode::read(), engine);
        let produced = match kind {
            0..=3 => reader.process_other(&other_name, data, None),
            4 => reader.process_system(data, None),
            _ => reader.process_scripts(data, None),
        };
        if let Ok(Some(produced)) = produced
            && let Ok(text) = std::str::from_utf8(produced.as_ref())
        {
            run(&mut Base::new(Mode::Write, engine), Some(text));
        }
    }
}

/// `fuzz/fuzz_targets/text.rs`.
fn replay_text(input: &[u8]) {
    let _ = get_ini_title(input);
    let _ = get_ini_title_rm2k(input);

    if let Ok(text) = std::str::from_utf8(input) {
        let _ = get_system_title(text);
        for mode in [DuplicateMode::Allow, DuplicateMode::Remove] {
            for read in [false, true] {
                let _ = parse_ignore(text, mode, read);
            }
        }
    }
}

/// A crash input of the marshal target; its body asserts that the JSON survives a Marshal round trip.
#[test]
fn marshal_input_round_trips_as_json() {
    replay_marshal(include_bytes!(
        "fixtures/fuzz/marshal-662a39bbe098f57ed59af2109e6623a4a8234491.bin"
    ));
}

/// A crash input of the process target.
#[test]
fn process_input_does_not_panic() {
    replay_process(include_bytes!(
        "fixtures/fuzz/process-0032b60ab9b9fad293d0a3c0d8a1d23a9ecd42ce.bin"
    ));
}

/// A crash input of the text target.
#[test]
fn text_input_does_not_panic() {
    replay_text(include_bytes!(
        "fixtures/fuzz/text-2e678a71c6eac8b55faf78e57b71bec10b9e18ae.bin"
    ));
}
