//! End-to-end lifecycle test for RM2K support, against `tests/RM2K` - a real
//! RPG Maker 2000 project's `RPG_RT.ldb`, `RPG_RT.lmt` and `Map*.lmu` files
//! (the game/graphics/audio assets `process_rm2k` never touches are left out),
//! the same shape [`tests/lifecycle.rs`] uses for the other engines.

use rvpacker_txt_rs_lib::{
    BaseFlags, DuplicateMode, EngineType, FileFlags, Mode, Processor,
    json::{
        generate_rm2k, generate_rm2k_database_file, generate_rm2k_map_file, generate_rm2k_tree_map_file, write_rm2k,
        write_rm2k_database_file, write_rm2k_map_file, write_rm2k_tree_map_file,
    },
};
use std::{
    fs::{create_dir_all, read, read_dir, read_to_string, remove_dir_all},
    path::{Path, PathBuf},
};

fn source_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/RM2K")
}

fn workspace(tag: &str) -> PathBuf {
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("target/rm2k_smoke")
        .join(tag);

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

/// A title set through `Processor::game_title` should show up in `terms.txt` on
/// read as its own `<!>ID`/`<!>Name` section - the same shape `system.rs` gives
/// every other engine's game title, not an untagged extra line inside `Terms`.
/// This is the only place RM2K can surface a translated title at all, since
/// `RPG_RT.ldb` has no field for one.
#[test]
fn rm2k_game_title_is_its_own_terms_section() {
    let translation_dir = workspace("game_title").join("translation");

    let mut processor = Processor {
        mode: Mode::read(),
        file_flags: FileFlags::Database,
        flags: BaseFlags::empty(),
        duplicate_mode: DuplicateMode::Allow,
        game_title: "My Game".to_owned(),
        ..Default::default()
    };

    processor
        .process(EngineType::RM2K, source_dir(), &translation_dir, None)
        .expect("read should not fail");

    let terms_text = read_to_string(translation_dir.join("terms.txt")).expect("terms.txt should exist");
    let lines: Vec<&str> = terms_text.lines().collect();
    let id_line = lines.len().checked_sub(3).map(|i| lines[i]);
    let name_line = lines.len().checked_sub(2).map(|i| lines[i]);
    let last_line = lines.last().copied();

    assert_eq!(
        id_line,
        Some("<!>ID<#>2"),
        "game title should get its own <!>ID section"
    );
    assert_eq!(
        name_line,
        Some("<!>NAME<#>Game Title"),
        "game title's section should be named, like every other engine's"
    );
    assert_eq!(
        last_line,
        Some("My Game<#>"),
        "game title should be the last line of terms.txt"
    );
}

/// LCF -> JSON -> LCF must not change a byte, mirroring `rm2k-lib`'s own
/// `serde_tier` guarantee, against the real fixture files this crate ships.
#[test]
fn rm2k_json_round_trips() {
    let ldb = read(source_dir().join("RPG_RT.ldb")).unwrap();
    let json = generate_rm2k_database_file(&ldb).expect("database should generate JSON");
    let rebuilt = write_rm2k_database_file(&json).expect("database JSON should write back");
    assert_eq!(ldb, rebuilt, "database LCF -> JSON -> LCF should be byte-identical");

    let lmt = read(source_dir().join("RPG_RT.lmt")).unwrap();
    let json = generate_rm2k_tree_map_file(&lmt).expect("tree map should generate JSON");
    let engine = rm2k::engine::Engine::from_ldb_id(
        rm2k::file::load_database(&ldb)
            .expect("database should still load")
            .value
            .system
            .ldb_id,
    );
    let rebuilt = write_rm2k_tree_map_file(&json, engine).expect("tree map JSON should write back");
    assert_eq!(lmt, rebuilt, "tree map LCF -> JSON -> LCF should be byte-identical");

    let lmu_path = source_dir().join("Map0001.lmu");
    let lmu = read(&lmu_path).unwrap();
    let json = generate_rm2k_map_file(&lmu).expect("map should generate JSON");
    let rebuilt = write_rm2k_map_file(&json, engine).expect("map JSON should write back");
    assert_eq!(lmu, rebuilt, "map LCF -> JSON -> LCF should be byte-identical");
}

/// The directory-batch counterpart of `rm2k_json_round_trips`: every `.ldb`/`.lmt`/
/// `.lmu` file in a real project directory should survive `generate_rm2k` and
/// `write_rm2k` byte-identically, including the engine (2000 vs. 2003) getting
/// threaded from `RPG_RT.ldb.json` into every map/tree file.
#[test]
fn rm2k_batch_json_round_trips() {
    let json_dir = workspace("batch_json").join("json");
    let rebuilt_dir = workspace("batch_json").join("rebuilt");

    generate_rm2k(source_dir(), json_dir.clone(), false).expect("batch generate should not fail");

    assert!(json_dir.join("RPG_RT.ldb.json").exists());
    assert!(json_dir.join("RPG_RT.lmt.json").exists());
    assert!(json_dir.join("Map0001.lmu.json").exists());

    write_rm2k(json_dir, rebuilt_dir.clone()).expect("batch write should not fail");

    for name in ["RPG_RT.ldb", "RPG_RT.lmt"] {
        let original = read(source_dir().join(name)).unwrap();
        let rebuilt = read(rebuilt_dir.join(name)).unwrap_or_else(|e| panic!("{name} was not rebuilt: {e}"));
        assert_eq!(original, rebuilt, "{name} should round-trip byte-identically");
    }

    for entry in read_dir(source_dir()).unwrap().flatten() {
        let path = entry.path();

        if path.extension().is_none_or(|ext| ext != "lmu") {
            continue;
        }

        let name = path.file_name().unwrap();
        let original = read(&path).unwrap();
        let rebuilt = read(rebuilt_dir.join(name)).unwrap_or_else(|e| panic!("{name:?} was not rebuilt: {e}"));
        assert_eq!(original, rebuilt, "{name:?} should round-trip byte-identically");
    }
}
