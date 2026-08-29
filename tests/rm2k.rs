//! End-to-end lifecycle test for RM2K support, against `tests/RM2K` - a real
//! RPG Maker 2000 project's `RPG_RT.ldb`, `RPG_RT.lmt` and `Map*.lmu` files
//! (the game/graphics/audio assets `process_rm2k` never touches are left out),
//! the same shape [`tests/lifecycle.rs`] uses for the other engines.

use rvpacker_txt_rs_lib::{BaseFlags, DuplicateMode, EngineType, FileFlags, Mode, Processor};
use std::{
    fs::{create_dir_all, read_dir, read_to_string, remove_dir_all},
    path::{Path, PathBuf},
};

fn source_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/RM2K")
}

fn workspace(tag: &str) -> PathBuf {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("target/rm2k_smoke").join(tag);

    let _ = remove_dir_all(&path);
    create_dir_all(&path).expect("could not create the workspace");
    path
}

fn txt_files(dir: &Path) -> Vec<PathBuf> {
    read_dir(dir)
        .expect("no translation directory")
        .flatten()
        .map(|entry| entry.path())
        .filter(|path| path.extension().is_some_and(|ext| ext == "txt"))
        .collect()
}

#[test]
fn rm2k_read_write_round_trip() {
    let translation_dir = workspace("read_write").join("translation");
    let output_dir = workspace("read_write").join("output");

    let mut processor = Processor {
        mode: Mode::Read {
            append: false,
            force: true,
        },
        file_flags: FileFlags::all(),
        flags: BaseFlags::empty(),
        duplicate_mode: DuplicateMode::Allow,
        ..Default::default()
    };

    processor
        .process(EngineType::RM2K, source_dir(), &translation_dir, None)
        .expect("read should not fail");

    let files = txt_files(&translation_dir);
    assert!(!files.is_empty(), "read produced no translation files");
    assert!(translation_dir.join("maps.txt").exists());
    assert!(translation_dir.join("actors.txt").exists());

    for path in &files {
        let text = read_to_string(path).unwrap_or_else(|e| panic!("{}: {e}", path.display()));
        assert!(!text.is_empty(), "{} is empty", path.display());

        for (number, line) in text.lines().enumerate() {
            assert!(!line.is_empty(), "{}:{number} is blank", path.display());
            assert!(
                line.starts_with("<!>") || line.contains("<#>"),
                "{}:{number} is neither metadata nor an entry: {line}",
                path.display()
            );
        }
    }

    // Translate every source line by tagging it, then write the project back
    // out and make sure the rebuilt files parse.
    let translated_dir = workspace("translated");

    for path in &files {
        let text = read_to_string(path).unwrap();
        let name = path.file_name().unwrap();

        let translated: String = text
            .lines()
            .map(|line| {
                if let Some((source, translation)) = line.split_once("<#>") {
                    if translation.is_empty() {
                        format!("{source}<#>[TR] {source}\n")
                    } else {
                        format!("{line}\n")
                    }
                } else {
                    format!("{line}\n")
                }
            })
            .collect();

        std::fs::write(translated_dir.join(name), translated).unwrap();
    }

    processor.mode = Mode::Write;

    processor
        .process(EngineType::RM2K, source_dir(), &translated_dir, Some(&output_dir))
        .expect("write should not fail");

    let rebuilt_ldb = output_dir.join("RPG_RT.ldb");
    assert!(rebuilt_ldb.exists());

    let ldb_content = std::fs::read(&rebuilt_ldb).unwrap();
    let database = rm2k::file::load_database(&ldb_content).expect("rebuilt database should still parse");
    assert!(!database.value.actors.0.is_empty(), "rebuilt database lost its actors");

    let rebuilt_maps: Vec<PathBuf> = read_dir(&output_dir)
        .unwrap()
        .flatten()
        .map(|entry| entry.path())
        .filter(|path| path.extension().is_some_and(|ext| ext == "lmu"))
        .collect();
    assert!(!rebuilt_maps.is_empty(), "write produced no rebuilt maps");

    for path in &rebuilt_maps {
        let bytes = std::fs::read(path).unwrap();
        rm2k::file::load_map(&bytes).unwrap_or_else(|e| panic!("{}: rebuilt map does not parse: {e}", path.display()));
    }
}
