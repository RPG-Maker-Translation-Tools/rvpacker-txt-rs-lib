//! `Processor` purge - drops `.txt` entries that have no translation, and
//! optionally records what got dropped into `.rvpacker-ignore` so a later
//! read can skip them again (see `BaseFlags::Ignore` in `examples/read.rs`).

use rvpacker_txt_rs_lib::{BaseFlags, DuplicateMode, EngineType, Error, FileFlags, Mode, Processor};

fn main() -> Result<(), Error> {
    let mut processor = Processor {
        mode: Mode::Purge,
        file_flags: FileFlags::all(),

        // Must match the flags/duplicate mode used on the read that produced
        // these `.txt` files, same as write.
        flags: BaseFlags::CreateIgnore,
        duplicate_mode: DuplicateMode::Remove,

        ..Default::default()
    };

    processor.process(EngineType::MVMZ, "C:/Game/www/data", "C:/Game/translation", None)?;

    // RM2K purge works the same way, over `database`/`maps` translation files.
    let mut rm2k_processor = Processor {
        mode: Mode::Purge,
        file_flags: FileFlags::Map | FileFlags::Database,
        flags: BaseFlags::CreateIgnore,
        ..Default::default()
    };
    rm2k_processor.process(EngineType::RM2K, "C:/Game2000", "C:/Game2000/translation", None)?;

    Ok(())
}
