//! Appending to a translation file the same game produced changes nothing: no section appears for an id the game has
//! no text for, such as the System sections an engine does not have.

use rvpacker_txt_rs_lib::{EngineType, FileFlags, Mode, Processor};
use std::{fs, path::Path};

fn append_changes_nothing(dir: &str, data: &str, engine: EngineType, file_flags: FileFlags) {
    let manifest = Path::new(env!("CARGO_MANIFEST_DIR"));
    let source = manifest.join("tests").join(dir).join(data);
    let translation = manifest.join("target/append-stable").join(dir);
    let _ = fs::remove_dir_all(&translation);

    let run = |mode| {
        Processor {
            file_flags,
            ..Processor::new(mode)
        }
        .process(engine, &source, &translation, None)
        .expect("run failed");
    };

    run(Mode::Read {
        append: false,
        force: true,
    });
    let read: Vec<_> = fs::read_dir(&translation)
        .unwrap()
        .map(|entry| {
            let path = entry.unwrap().path();
            let text = fs::read_to_string(&path).unwrap();
            (path, text)
        })
        .collect();

    run(Mode::Read {
        append: true,
        force: true,
    });

    for (path, before) in read {
        let after = fs::read_to_string(&path).unwrap();
        assert_eq!(before, after, "{} changed on append", path.display());
    }
}

#[test]
fn xp() {
    append_changes_nothing("RMXP", "Data", EngineType::XP, FileFlags::all());
}

#[test]
fn vx() {
    append_changes_nothing("RMVX", "Data", EngineType::VX, FileFlags::all());
}

#[test]
fn vx_ace() {
    append_changes_nothing("RMVXACE", "Data", EngineType::VXAce, FileFlags::all());
}

#[test]
fn mv() {
    append_changes_nothing("RMMV", "data", EngineType::MVMZ, FileFlags::all());
}

#[test]
fn mz() {
    append_changes_nothing("RMMZ", "data", EngineType::MVMZ, FileFlags::all() & !FileFlags::Scripts);
}
