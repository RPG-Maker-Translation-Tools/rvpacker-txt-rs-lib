#![no_main]
use libfuzzer_sys::fuzz_target;
use rvpacker_txt_rs_lib::{EngineType, Mode, core::Base};

const ENGINES: [EngineType; 4] = [EngineType::MVMZ, EngineType::VXAce, EngineType::VX, EngineType::XP];
const OTHER_NAMES: [&str; 4] = ["Actors", "CommonEvents", "Troops", "Items"];

/// Layout: `[selector, data length (u16 LE), data, translation text]`.
/// Selector bits: 0-1 engine, 2-4 file kind, 5-7 mode.
/// Read output is also fed back in as the translation of a write, which is the shape real runs have.
fuzz_target!(|input: &[u8]| {
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
});
