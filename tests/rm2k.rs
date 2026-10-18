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
        read_encoding: Some(encoding_rs::WINDOWS_1252),
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
        read_encoding: Some(encoding_rs::WINDOWS_1252),
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

#[test]
fn rm2k_entity_names_come_from_the_database() {
    use rvpacker_txt_rs_lib::core::{Rm2kEntity, get_rm2k_entity_name};

    let content = read(source_dir().join("RPG_RT.ldb")).expect("no RPG_RT.ldb");
    let database = rm2k::file::load_database(&content).expect("RPG_RT.ldb does not parse");

    let actor = database.actors.0.first().expect("the project has no actors");
    let variable = database.variables.0.first().expect("the project has no variables");

    // The codepage of the project is unknown here; what matters is that the right entry is found and decoded.
    let decode = |bytes: &[u8]| encoding_rs::SHIFT_JIS.decode(bytes).0.into_owned();

    assert_eq!(
        get_rm2k_entity_name(&content, Rm2kEntity::Actors, actor.id, Some(encoding_rs::SHIFT_JIS)).unwrap(),
        decode(actor.name.as_bytes())
    );
    assert_eq!(
        get_rm2k_entity_name(
            &content,
            Rm2kEntity::Variables,
            variable.id,
            Some(encoding_rs::SHIFT_JIS)
        )
        .unwrap(),
        decode(variable.name.as_bytes())
    );
    assert!(get_rm2k_entity_name(&content, Rm2kEntity::Actors, i32::MAX, None).is_err());
}

#[test]
fn rm2k_system_graphic_name_comes_from_the_database() {
    use rvpacker_txt_rs_lib::core::get_rm2k_system_graphic_name;

    let content = read(source_dir().join("RPG_RT.ldb")).expect("no RPG_RT.ldb");
    let database = rm2k::file::load_database(&content).expect("RPG_RT.ldb does not parse");

    assert_eq!(
        get_rm2k_system_graphic_name(&content, Some(encoding_rs::SHIFT_JIS)).unwrap(),
        encoding_rs::SHIFT_JIS.decode(database.system.system_name.as_bytes()).0
    );
}

fn read_rm2k(file_flags: FileFlags, mode: Mode, map_events: bool, translation_dir: &Path) -> Processor {
    let mut processor = Processor {
        mode,
        file_flags,
        map_events,
        read_encoding: Some(encoding_rs::WINDOWS_1252),
        ..Default::default()
    };

    processor
        .process(EngineType::RM2K, source_dir(), translation_dir, None)
        .expect("read should not fail");

    processor
}

const FORCED_READ: Mode = Mode::Read {
    append: false,
    force: true,
};

/// An append whose hashes say no map changed must still carry every map's
/// translation into `maps.txt`, instead of rewriting it without them.
#[test]
fn rm2k_unchanged_maps_survive_append() {
    let translation_dir = workspace("unchanged_maps").join("translation");
    let mut first = read_rm2k(FileFlags::Map, FORCED_READ, false, &translation_dir);
    let before = read_to_string(translation_dir.join("maps.txt")).unwrap();

    let mut second = Processor {
        mode: Mode::Read {
            append: true,
            force: false,
        },
        file_flags: FileFlags::Map,
        hashes: std::mem::take(&mut first.hashes),
        read_encoding: Some(encoding_rs::WINDOWS_1252),
        ..Default::default()
    };

    second
        .process(EngineType::RM2K, source_dir(), &translation_dir, None)
        .expect("append should not fail");

    let after = read_to_string(translation_dir.join("maps.txt")).unwrap();

    for line in before.lines() {
        assert!(after.lines().any(|kept| kept == line), "append dropped: {line}");
    }
}

/// Per-event headers must not suppress the map's own `<!>ID` line.
#[test]
fn rm2k_map_events_keep_map_ids() {
    let count_ids = |text: &str| text.lines().filter(|line| line.starts_with("<!>ID<#>")).count();

    let plain_dir = workspace("map_ids_plain").join("translation");
    read_rm2k(FileFlags::Map, FORCED_READ, false, &plain_dir);

    let events_dir = workspace("map_ids_events").join("translation");
    read_rm2k(FileFlags::Map, FORCED_READ, true, &events_dir);

    let plain = count_ids(&read_to_string(plain_dir.join("maps.txt")).unwrap());
    let with_events = count_ids(&read_to_string(events_dir.join("maps.txt")).unwrap());

    assert!(plain > 0, "no map ids at all");
    assert_eq!(plain, with_events, "map_events lost map ids");
}

/// A comment the user wrote under a section header survives repeated appends,
/// once, under the same section.
#[test]
fn user_comments_survive_appends() {
    const NOTE: &str = "<!> translator note";

    let translation_dir = workspace("user_comments").join("translation");
    read_rm2k(
        FileFlags::Database | FileFlags::Actors,
        FORCED_READ,
        false,
        &translation_dir,
    );

    let actors = translation_dir.join("actors.txt");
    let text = read_to_string(&actors).unwrap();
    let mut lines: Vec<&str> = text.lines().collect();
    let name_index = lines
        .iter()
        .position(|line| line.starts_with("<!>NAME"))
        .expect("no name comment");
    lines.insert(name_index + 1, NOTE);
    std::fs::write(&actors, lines.join("\n")).unwrap();

    let section_of_note = |text: &str| {
        let lines: Vec<&str> = text.lines().collect();
        let note = lines.iter().position(|line| *line == NOTE).expect("the note is gone");
        let id = lines[..note]
            .iter()
            .rev()
            .find(|line| line.starts_with("<!>ID"))
            .expect("note before any id");
        (lines.iter().filter(|line| **line == NOTE).count(), id.to_string())
    };

    let expected = section_of_note(&lines.join("\n"));

    for _ in 0..2 {
        read_rm2k(
            FileFlags::Database | FileFlags::Actors,
            Mode::Read {
                append: true,
                force: true,
            },
            false,
            &translation_dir,
        );

        assert_eq!(section_of_note(&read_to_string(&actors).unwrap()), expected);
    }
}

/// An RM2K message box holds at most four lines, so an entry with more is two boxes glued together.
#[test]
fn rm2k_message_boxes_stay_separate() {
    let translation_dir = workspace("message_boxes").join("translation");
    read_rm2k(FileFlags::Map, FORCED_READ, false, &translation_dir);

    for line in read_to_string(translation_dir.join("maps.txt")).unwrap().lines() {
        if let Some((source, _)) = line.split_once("<#>") {
            assert!(source.matches(r"\#").count() < 4, "more than one box in: {source}");
        }
    }
}

/// RM2K keeps its title in `RPG_RT.ini` only, so a write hands the translated title back through the processor.
#[test]
fn rm2k_write_returns_translated_title() {
    let workspace = workspace("translated_title");
    let translation_dir = workspace.join("translation");

    let mut processor = Processor {
        mode: FORCED_READ,
        file_flags: FileFlags::Database,
        game_title: "My Game".to_owned(),
        read_encoding: Some(encoding_rs::WINDOWS_1252),
        ..Default::default()
    };

    processor
        .process(EngineType::RM2K, source_dir(), &translation_dir, None)
        .expect("read should not fail");

    let terms = translation_dir.join("terms.txt");
    let text = read_to_string(&terms)
        .unwrap()
        .replace("My Game<#>", "My Game<#>Моя игра");
    std::fs::write(&terms, text).unwrap();

    processor.mode = Mode::Write;
    processor
        .process(
            EngineType::RM2K,
            source_dir(),
            &translation_dir,
            Some(&workspace.join("output")),
        )
        .expect("write should not fail");

    assert_eq!(processor.game_title, "Моя игра");
}
