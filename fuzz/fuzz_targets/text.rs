#![no_main]
use libfuzzer_sys::fuzz_target;
use rvpacker_txt_rs_lib::{DuplicateMode, get_ini_title, get_ini_title_rm2k, get_system_title, parse_ignore};

fuzz_target!(|input: &[u8]| {
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
});
