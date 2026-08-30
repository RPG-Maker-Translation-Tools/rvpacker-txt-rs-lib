//! `Processor` read, exercising every option it exposes.

use gxhash::HashMap;
use rvpacker_txt_rs_lib::{BaseFlags, DuplicateMode, EngineType, Error, FileFlags, Mode, Processor, RPGMFileType};

fn main() -> Result<(), Error> {
    let mut processor = Processor {
        // `Mode::read()` is `Mode::Read { append: false, force: false }` - the
        // "default" read: skip any `.txt` that already exists rather than
        // touching it. `append: true` merges newly-found source lines into an
        // existing file instead of skipping it; `force: true` overwrites it.
        mode: Mode::read(),

        // Every file kind. `FileFlags::other()` alone is every "other" entity
        // kind (`Actors`, `Armors`, ...) without `Map`/`System`/`Scripts`.
        file_flags: FileFlags::all(),

        // `Ignore` applies `.rvpacker-ignore` entries (see `examples/.rvpacker-ignore`)
        // to skip previously-purged entries on read. `SkipObsolete` additionally
        // skips entries no longer present in the game files. `CreateIgnore` only
        // has an effect on purge - see `examples/purge.rs`.
        flags: BaseFlags::Ignore | BaseFlags::SkipObsolete,

        // `Remove` drops a source line that repeats across multiple sections
        // (e.g. the same dialogue in `map002` and `map003`), keeping only its
        // first appearance in the `.txt` file. Doesn't apply to system,
        // scripts, plugins or RM2K terms.
        duplicate_mode: DuplicateMode::Remove,

        // XP/VX/VX Ace keep their title only in `Game.ini`, not necessarily as
        // UTF-8 - decode it yourself (see `get_ini_title`) and set it here so
        // it ends up in `system.txt` instead of being left blank. MV/MZ read
        // their title straight from `System.json` and ignore this field.
        game_title: String::new(),

        // Feed back hashes persisted from a previous read to let this read
        // skip any file whose content hasn't changed since. Empty here since
        // this is the first read of the run.
        hashes: HashMap::default(),

        // Map ids to leave untouched - useful for maps a translator already
        // hand-finished and doesn't want re-scanned.
        skip_maps: Vec::new(),

        // Per-file entry ids to skip, keyed by `RPGMFileType`. Has no effect on
        // `RPGMFileType::Map` - use `skip_maps` for that.
        skip_events: Vec::from([(RPGMFileType::Enemies, Vec::from([1u16, 2, 3]))]),

        // Record each event's id, name and coordinates immediately before its
        // dialogue, so a translator working straight in the `.txt` can tell
        // events apart without cross-referencing the game.
        map_events: true,

        // XP/VX (pre-1.9 Ruby) text carries no reliable in-file encoding
        // indicator; leaving this `None` tries a fixed list of common
        // codepages per string and keeps the first clean decode. Set this once
        // the actual codepage is known (e.g. from `RPG_RT.ini`). This is the
        // *read* side only - see `examples/write.rs` for why the write side
        // is a separate, independent setting.
        read_encoding: Some(encoding_rs::SHIFT_JIS),

        // Left at `None`: has no effect on a read.
        write_encoding: None,
    };

    // MV/MZ - `Data`/`www/data`/`data` directory of `.json` files.
    processor.process(EngineType::MVMZ, "C:/Game/www/data", "C:/Game/translation", None)?;

    // Persist hashes for next run, so an unchanged file is skipped instead of
    // being re-read from scratch.
    let _hashes_to_persist = processor.hashes;

    // RPG Maker 2000/2003 project root (holds `RPG_RT.ldb`/`.lmt`/`MapNNNN.lmu`
    // directly, not a `Data` subdirectory). `FileFlags::Database` replaces
    // `other()` + `System` here - RM2K bundles every entity kind plus
    // terms into one `RPG_RT.ldb`.
    let mut rm2k_processor = Processor {
        mode: Mode::read(),
        file_flags: FileFlags::Map | FileFlags::Database,
        ..Default::default()
    };
    rm2k_processor.process(EngineType::RM2K, "C:/Game2000", "C:/Game2000/translation", None)?;

    Ok(())
}
