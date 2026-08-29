//! `Processor` write - rebuilds the game's data files from translated `.txt`s
//! produced by a prior read (see `examples/read.rs`).

use rvpacker_txt_rs_lib::{BaseFlags, DuplicateMode, EngineType, Error, FileFlags, Mode, Processor};
use std::path::Path;

fn main() -> Result<(), Error> {
    let mut processor = Processor {
        mode: Mode::Write,

        // `flags`/`duplicate_mode` must match what the read that produced the
        // `.txt` files used - they change how a `.txt` line maps back onto an
        // entry, not just how it was written out.
        file_flags: FileFlags::all(),
        flags: BaseFlags::Ignore | BaseFlags::SkipObsolete,
        duplicate_mode: DuplicateMode::Remove,

        // `read_encoding` matters here too if any untranslated source text
        // still needs decoding on this pass (e.g. an appended read reusing
        // this same `Processor`) - see `examples/read.rs`.
        read_encoding: Some(encoding_rs::SHIFT_JIS),

        // `write_encoding` is independent and defaults to `None` (write plain
        // UTF-8) for good reason: it is the only choice that can't silently
        // corrupt a translation into a script the source codepage can't
        // represent - translating this Shift_JIS game into Russian, for
        // instance, cannot be re-encoded as Shift_JIS at all. Leave this
        // `None` unless the target engine build has no Unicode-aware text
        // renderer (true of RM2K/2003, XP and VX - they render through the
        // OS's legacy ANSI codepage) *and* the translation's script fits in
        // that codepage - e.g. translating this same game into French, where
        // Shift_JIS's ASCII range covers it. Forcing a write encoding also
        // means whoever runs the translated game needs their system (or a
        // locale emulator) set to that same codepage - see the "Text
        // encoding" section of the README for the full explanation.
        write_encoding: None,

        // Fields with no effect on write (`game_title`, `hashes`,
        // `skip_maps`, `map_events`) are left default.
        ..Default::default()
    };

    processor.process(
        EngineType::MVMZ,
        "C:/Game/www/data",
        "C:/Game/translation",
        Some(Path::new("C:/Game/output")),
    )?;

    // RM2K: writing re-serializes `RPG_RT.ldb` once (after every database
    // section has been applied) and each translated `MapNNNN.lmu` individually.
    let mut rm2k_processor = Processor {
        mode: Mode::Write,
        file_flags: FileFlags::Map | FileFlags::Database,
        ..Default::default()
    };
    rm2k_processor.process(
        EngineType::RM2K,
        "C:/Game2000",
        "C:/Game2000/translation",
        Some(Path::new("C:/Game2000/output")),
    )?;

    Ok(())
}
