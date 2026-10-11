#![no_main]
use libfuzzer_sys::fuzz_target;
use rvpacker_txt_rs_lib::{EngineType, save};

fuzz_target!(|input: &[u8]| {
    let Some((&selector, file)) = input.split_first() else {
        return;
    };

    // LZString: anything that decompresses must survive compress -> decompress.
    if let Ok(text) = std::str::from_utf8(file)
        && let Some(json) = save::load(text)
    {
        assert!(
            save::load(&save::dump(&json)).as_deref() == Some(json.as_str()),
            "dump(load) lost data"
        );
    }

    let engines = [
        EngineType::MVMZ,
        EngineType::VXAce,
        EngineType::VX,
        EngineType::XP,
        EngineType::RM2K,
    ];
    let _ = save::get_save_summary(file, engines[selector as usize % engines.len()], None);
});
