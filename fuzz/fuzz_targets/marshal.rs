#![no_main]
use libfuzzer_sys::fuzz_target;
use rvpacker_txt_rs_lib::json::{generate_file, write_file};

/// Marshal bytes -> JSON -> Marshal -> JSON must be stable. `Scripts` produces Ruby text, not JSON, so it has no way back.
fuzz_target!(|input: &[u8]| {
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
});
