//! End-to-end lifecycles over the bundled default projects, one per engine.
//!
//! `tests/RM*` are stock RPG Maker projects, so between them they cover every
//! kind of extractable data the library knows about. Each test drives
//! [`Processor`] the way a CLI would - through the filesystem - into a scratch
//! directory under `target/lifecycle`, and never writes inside the fixtures.

use rvpacker_txt_rs_lib::{
    BaseFlags, DuplicateMode, EngineType, FileFlags, Mode, Processor, RPGMFileType, get_ini_title,
};
use std::{
    collections::{BTreeMap, BTreeSet},
    fs::{copy, create_dir_all, read_dir, read_to_string, remove_dir_all, write},
    path::{Path, PathBuf},
};

#[derive(Clone, Copy)]
struct Fixture {
    /// Directory under `tests/`.
    dir: &'static str,
    /// The project's data directory, lowercase on MV/MZ and capitalised before.
    data: &'static str,
    engine: EngineType,
    /// Whether the project ships `js/plugins.js`.
    ///
    /// Only MV does. The plugin format did not change for MZ, so one fixture
    /// covers both.
    plugins: bool,
    /// Whether the project ships `Game.ini`, which is where XP, VX and VX Ace
    /// keep the game title.
    ini: bool,
}

impl Fixture {
    fn root(self) -> PathBuf {
        Path::new(env!("CARGO_MANIFEST_DIR")).join("tests").join(self.dir)
    }

    fn source(self) -> PathBuf {
        self.root().join(self.data)
    }

    /// Everything the fixture actually contains.
    fn flags(self) -> FileFlags {
        if self.engine.is_mvmz() && !self.plugins {
            FileFlags::all() & !FileFlags::Scripts
        } else {
            FileFlags::all()
        }
    }

    /// The game title `Game.ini` carries, decoded.
    ///
    /// The file is not necessarily UTF-8 - that is the whole reason the title
    /// is the caller's job - but these fixtures are ASCII.
    fn ini_title(self) -> String {
        let content = std::fs::read(self.root().join("Game.ini")).expect("no Game.ini");

        String::from_utf8(get_ini_title(&content).expect("no title in Game.ini")).expect("the title is not UTF-8")
    }

    /// Translation files every engine produces.
    fn expected_files(self) -> Vec<&'static str> {
        let mut files = Vec::from([
            "actors.txt",
            "armors.txt",
            "classes.txt",
            "commonevents.txt",
            "enemies.txt",
            "items.txt",
            "maps.txt",
            "skills.txt",
            "states.txt",
            "system.txt",
            "troops.txt",
            "weapons.txt",
        ]);

        if self.engine.is_mvmz() {
            if self.plugins {
                files.push("plugins.txt");
            }
        } else {
            files.push("scripts.txt");
        }

        files.sort_unstable();
        files
    }
}

/// A scratch directory of its own per test, so tests can run in parallel.
fn workspace(tag: &str) -> PathBuf {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("target/lifecycle").join(tag);

    let _ = remove_dir_all(&path);
    create_dir_all(&path).expect("could not create the workspace");
    path
}

fn run(
    fixture: Fixture,
    mode: Mode,
    flags: BaseFlags,
    duplicate_mode: DuplicateMode,
    translation: &Path,
    output: Option<&Path>,
) {
    let mut processor = Processor {
        mode,
        file_flags: fixture.flags(),
        flags,
        duplicate_mode,
        ..Default::default()
    };

    if let Err(error) = processor.process(fixture.engine, fixture.source(), translation, output) {
        panic!("{mode:?} failed for {}: {error}", fixture.dir);
    }
}

/// A forced read, which is what every scenario starts from.
fn read(fixture: Fixture, translation: &Path) {
    run(
        fixture,
        Mode::Read {
            append: false,
            force: true,
        },
        BaseFlags::empty(),
        DuplicateMode::Allow,
        translation,
        None,
    );
}

fn append(fixture: Fixture, flags: BaseFlags, translation: &Path) {
    run(
        fixture,
        Mode::Read {
            append: true,
            force: true,
        },
        flags,
        DuplicateMode::Allow,
        translation,
        None,
    );
}

/// Runs a processor the caller configured, for the options `run` does not take.
fn run_with(fixture: Fixture, processor: &mut Processor, translation: &Path, output: Option<&Path>) {
    let mode = processor.mode;

    if let Err(error) = processor.process(fixture.engine, fixture.source(), translation, output) {
        panic!("{mode:?} failed for {}: {error}", fixture.dir);
    }
}

/// A processor with the fixture's file flags and nothing else set.
fn processor(fixture: Fixture, mode: Mode) -> Processor {
    Processor {
        mode,
        file_flags: fixture.flags(),
        ..Default::default()
    }
}

const FORCED_READ: Mode = Mode::Read {
    append: false,
    force: true,
};

const FORCED_APPEND: Mode = Mode::Read {
    append: true,
    force: true,
};

/// A read that records event ids, names and positions before each event's text.
fn read_with_map_events(fixture: Fixture, mode: Mode, translation: &Path) {
    let mut processor = Processor {
        mode,
        file_flags: FileFlags::Map,
        map_events: true,
        ..Default::default()
    };

    if let Err(error) = processor.process(fixture.engine, fixture.source(), translation, None) {
        panic!("{mode:?} failed for {}: {error}", fixture.dir);
    }
}

/// Every structural rule a translation file has to keep, whichever pass wrote it.
///
/// Chiefly: a line is either metadata or a `source<#>translation` pair. A bare
/// line is a translation that lost its source, which is how a corrupted append
/// shows up.
fn check_shape(text: &str, name: &str) {
    for (number, line) in text.lines().enumerate() {
        assert!(!line.is_empty(), "{name}:{number} is blank");

        if !is_metadata(line) {
            assert!(
                line.contains("<#>"),
                "{name}:{number} is neither metadata nor an entry: {line}"
            );
        }
    }
}

fn check_all_shapes(dir: &Path) {
    for path in txt_files(dir) {
        let name = path.file_name().unwrap().to_string_lossy().into_owned();
        check_shape(&slurp(&path), &name);
    }
}

fn txt_files(dir: &Path) -> Vec<PathBuf> {
    let mut files: Vec<PathBuf> = read_dir(dir)
        .expect("no translation directory")
        .flatten()
        .map(|entry| entry.path())
        .filter(|path| path.extension().is_some_and(|ext| ext == "txt"))
        .collect();

    files.sort();
    files
}

fn file_names(dir: &Path) -> Vec<String> {
    txt_files(dir)
        .iter()
        .map(|path| path.file_name().unwrap().to_string_lossy().into_owned())
        .collect()
}

fn slurp(path: &Path) -> String {
    read_to_string(path).unwrap_or_else(|e| panic!("{}: {e}", path.display()))
}

fn is_metadata(line: &str) -> bool {
    line.starts_with("<!>")
}

/// Whether a translation file holds map or "other" text, as opposed to the
/// system, script and plugin files, which several options leave alone.
fn is_main_file(name: &str) -> bool {
    !matches!(name, "system.txt" | "scripts.txt" | "plugins.txt")
}

/// Every `source<#>translation` pair in a translation file, metadata aside.
fn entries(text: &str) -> Vec<(&str, &str)> {
    text.lines()
        .filter(|line| !is_metadata(line))
        .filter_map(|line| line.split_once("<#>"))
        .collect()
}

fn sources(text: &str) -> BTreeSet<&str> {
    entries(text).into_iter().map(|(source, _)| source).collect()
}

fn translations(text: &str) -> BTreeMap<&str, &str> {
    entries(text).into_iter().collect()
}

/// Sources that occur exactly once in a file.
///
/// A default project repeats plenty of text - "Event Item." across half the
/// item list - and a source-keyed assertion cannot say anything about those.
fn unique_sources(text: &str) -> BTreeSet<&str> {
    let mut counts: BTreeMap<&str, usize> = BTreeMap::new();

    for (source, _) in entries(text) {
        *counts.entry(source).or_default() += 1;
    }

    counts
        .into_iter()
        .filter(|(_, count)| *count == 1)
        .map(|(source, _)| source)
        .collect()
}

/// Entries outside the section whose name comment is `section`.
///
/// The game title is the one entry a write cannot put back from the translation
/// file - it comes from the caller, since it also names the output directory.
fn entries_outside(text: &str, section: &str) -> BTreeMap<String, String> {
    let mut result = BTreeMap::new();
    let mut skipping = false;

    for line in text.lines() {
        if let Some(name) = line.strip_prefix("<!>NAME<#>") {
            skipping = name == section;
        } else if line.starts_with("<!>ID<#>") {
            skipping = false;
        } else if !skipping
            && !is_metadata(line)
            && let Some((source, translation)) = line.split_once("<#>")
        {
            result.insert(source.to_owned(), translation.to_owned());
        }
    }

    result
}

/// Fills in every empty translation column with a marked-up copy of its source.
///
/// The display name is metadata that carries its own translation after the
/// separator, so its marker is stripped before the source is wrapped.
fn translate(text: &str) -> String {
    const DISPLAY_NAME: &str = "<!>IN-GAME DISPLAYED NAME: ";

    let mut output = String::with_capacity(text.len() * 2);

    for line in text.lines() {
        output.push_str(line);

        if let Some(source) = line.strip_suffix("<#>")
            && !source.is_empty()
        {
            let source = source.strip_prefix(DISPLAY_NAME).unwrap_or(source);
            output.push_str(&format!("[{source}]"));
        }

        output.push('\n');
    }

    output
}

/// Blanks the translation column of the lines whose source is in `sources`.
fn untranslate(text: &str, sources: &[&str]) -> String {
    let mut output = String::with_capacity(text.len());

    for line in text.lines() {
        match line.split_once("<#>") {
            Some((source, _)) if !is_metadata(line) && sources.contains(&source) => {
                output.push_str(source);
                output.push_str("<#>");
            }
            _ => output.push_str(line),
        }

        output.push('\n');
    }

    output
}

/// Drops every `nth` entry line, leaving metadata alone.
fn drop_every(text: &str, nth: usize) -> (String, Vec<String>) {
    let mut output = String::with_capacity(text.len());
    let mut dropped = Vec::new();
    let mut seen = 0;

    for line in text.lines() {
        if !is_metadata(line)
            && let Some((source, _)) = line.split_once("<#>")
        {
            seen += 1;

            if seen % nth == 0 {
                dropped.push(source.to_owned());
                continue;
            }
        }

        output.push_str(line);
        output.push('\n');
    }

    (output, dropped)
}

fn copy_dir(from: &Path, to: &Path) {
    create_dir_all(to).expect("could not create the destination");

    for entry in read_dir(from).expect("no source directory").flatten() {
        let path = entry.path();

        if path.is_file() {
            copy(&path, to.join(entry.file_name())).expect("copy failed");
        }
    }
}

/// The lines of one id's section, header included.
fn block(text: &str, id: u16) -> String {
    let header = format!("<!>ID<#>{id}");
    let mut inside = false;

    text.lines()
        .filter(|line| {
            if line.starts_with("<!>ID<#>") {
                inside = *line == header;
            }

            inside
        })
        .collect::<Vec<_>>()
        .join("\n")
}

/// The id of the first section in a translation file.
fn first_id(text: &str) -> u16 {
    text.lines()
        .find_map(|line| line.strip_prefix("<!>ID<#>"))
        .and_then(|id| id.parse().ok())
        .expect("no sections in the file")
}

/// Drops the entry line whose source is `source`.
fn drop_entry(text: &str, source: &str) -> String {
    let mut output = String::with_capacity(text.len());

    for line in text.lines() {
        if !is_metadata(line) && line.split_once("<#>").is_some_and(|(s, _)| s == source) {
            continue;
        }

        output.push_str(line);
        output.push('\n');
    }

    output
}

/// A file present in every engine's output, with plenty of short entries.
const SUBJECT: &str = "items.txt";

mod scenarios {
    use super::*;

    /// A plain read produces a complete, wholly untranslated set of files, and a
    /// second plain read leaves them alone.
    pub fn plain_read(fixture: Fixture, tag: &str) {
        let workspace = workspace(tag);
        let translation = workspace.join("translation");

        run(
            fixture,
            Mode::read(),
            BaseFlags::empty(),
            DuplicateMode::Allow,
            &translation,
            None,
        );

        assert_eq!(file_names(&translation), fixture.expected_files());

        for path in txt_files(&translation) {
            let text = slurp(&path);
            let name = path.file_name().unwrap().to_string_lossy();

            assert!(!text.is_empty(), "{name} is empty");
            check_shape(&text, &name);

            for (source, translation) in entries(&text) {
                assert!(
                    translation.is_empty(),
                    "{name}: {source} came out of a read already translated"
                );
            }
        }

        // A second plain read must not clobber work in progress.
        let subject = translation.join(SUBJECT);
        let edited = translate(&slurp(&subject));
        write(&subject, &edited).unwrap();

        run(
            fixture,
            Mode::read(),
            BaseFlags::empty(),
            DuplicateMode::Allow,
            &translation,
            None,
        );

        assert_eq!(slurp(&subject), edited, "a plain read overwrote {SUBJECT}");
    }

    /// Two forced reads of the same project produce the same bytes.
    pub fn reads_are_reproducible(fixture: Fixture, tag: &str) {
        let workspace = workspace(tag);
        let first = workspace.join("first");
        let second = workspace.join("second");

        read(fixture, &first);
        read(fixture, &second);

        for name in fixture.expected_files() {
            assert_eq!(
                slurp(&first.join(name)),
                slurp(&second.join(name)),
                "{name} differs between two reads"
            );
        }
    }

    /// Deleting entries and appending brings them back without disturbing the
    /// translations that were already there.
    pub fn append_restores_deleted_entries(fixture: Fixture, tag: &str) {
        let workspace = workspace(tag);
        let translation = workspace.join("translation");

        read(fixture, &translation);

        let subject = translation.join(SUBJECT);
        let original = translate(&slurp(&subject));
        write(&subject, &original).unwrap();

        let (thinned, dropped) = drop_every(&original, 3);
        assert!(!dropped.is_empty(), "nothing was dropped from {SUBJECT}");
        write(&subject, &thinned).unwrap();

        append(fixture, BaseFlags::empty(), &translation);
        check_all_shapes(&translation);

        let appended = slurp(&subject);
        let restored = translations(&appended);
        let unique = unique_sources(&original);

        for (source, translation) in translations(&original) {
            // Repeated sources collapse in a source-keyed map, so they cannot
            // be told apart here.
            if !unique.contains(source) {
                continue;
            }

            let found = restored
                .get(source)
                .unwrap_or_else(|| panic!("{SUBJECT}: {source} did not come back"));

            if dropped.iter().any(|d| d == source) {
                // Re-read from the game files, so untranslated again.
                assert!(found.is_empty(), "{SUBJECT}: {source} came back already translated");
            } else {
                assert_eq!(*found, translation, "{SUBJECT}: {source} lost its translation");
            }
        }

        // Nothing was invented along the way.
        assert_eq!(sources(&appended), sources(&original));
    }

    /// Purging drops the entries left untranslated, records them in the ignore
    /// file, and a later append honours it.
    pub fn purge_then_ignore(fixture: Fixture, tag: &str) {
        let workspace = workspace(tag);
        let translation = workspace.join("translation");

        read(fixture, &translation);

        for path in txt_files(&translation) {
            let translated = translate(&slurp(&path));
            write(&path, translated).unwrap();
        }

        let subject = translation.join(SUBJECT);
        let translated = slurp(&subject);

        // Blank out a few translations, which is what makes them purgeable.
        let abandoned: Vec<&str> = sources(&translated)
            .into_iter()
            .filter(|source| !source.is_empty())
            .take(3)
            .collect();
        assert_eq!(abandoned.len(), 3, "{SUBJECT} has too few entries to test");

        let abandoned: Vec<String> = abandoned.iter().map(|s| (*s).to_owned()).collect();
        let abandoned_refs: Vec<&str> = abandoned.iter().map(String::as_str).collect();

        write(&subject, untranslate(&translated, &abandoned_refs)).unwrap();

        run(
            fixture,
            Mode::Purge,
            BaseFlags::CreateIgnore,
            DuplicateMode::Allow,
            &translation,
            None,
        );

        check_all_shapes(&translation);

        let purged = slurp(&subject);
        let purged_sources = sources(&purged);

        for source in &abandoned {
            assert!(
                !purged_sources.contains(source.as_str()),
                "{SUBJECT}: {source} survived the purge"
            );
        }

        // Everything else kept both halves.
        for (source, translation) in translations(&purged) {
            assert!(
                !translation.is_empty(),
                "{SUBJECT}: {source} was left untranslated by the purge"
            );
        }

        let ignore_file = translation.join(".rvpacker-ignore");
        let ignored = slurp(&ignore_file);

        for source in &abandoned {
            assert!(
                ignored.lines().any(|line| line == source),
                "{source} is missing from the ignore file"
            );
        }

        append(fixture, BaseFlags::Ignore, &translation);
        check_all_shapes(&translation);

        let reappended = slurp(&subject);
        let reappended_sources = sources(&reappended);

        for source in &abandoned {
            assert!(
                !reappended_sources.contains(source.as_str()),
                "{SUBJECT}: {source} came back despite the ignore file"
            );
        }

        for (source, translation) in translations(&purged) {
            assert_eq!(
                translations(&reappended).get(source),
                Some(&translation),
                "{SUBJECT}: {source} changed across the append"
            );
        }
    }

    /// Event headers survive a translate-and-append round trip.
    ///
    /// They are carried through the pipeline as an entry with no source, which
    /// is the one place a translation file is allowed to look unbalanced - and
    /// the place an append used to emit somebody else's translation instead.
    pub fn map_events_round_trip(fixture: Fixture, tag: &str) {
        let workspace = workspace(tag);
        let translation = workspace.join("translation");

        read_with_map_events(
            fixture,
            Mode::Read {
                append: false,
                force: true,
            },
            &translation,
        );

        let maps = translation.join("maps.txt");
        let text = slurp(&maps);
        check_shape(&text, "maps.txt");

        let headers: Vec<&str> = text.lines().filter(|line| line.starts_with("<!>EVENT ID<#>")).collect();
        assert!(!headers.is_empty(), "no event headers were written");

        assert!(
            text.lines().any(|line| line.starts_with("<!>EVENT NAME<#>")),
            "event headers carry no name"
        );
        assert!(
            text.lines().any(|line| line.starts_with("<!>EVENT POS<#>")),
            "event headers carry no position"
        );

        let translated = translate(&text);
        write(&maps, &translated).unwrap();

        read_with_map_events(
            fixture,
            Mode::Read {
                append: true,
                force: true,
            },
            &translation,
        );

        let appended = slurp(&maps);
        check_shape(&appended, "maps.txt");

        let appended_headers: Vec<&str> = appended
            .lines()
            .filter(|line| line.starts_with("<!>EVENT ID<#>"))
            .collect();
        assert_eq!(appended_headers, headers, "the event headers changed across an append");

        for (source, translation) in translations(&translated) {
            assert_eq!(
                translations(&appended).get(source),
                Some(&translation),
                "maps.txt: {source} lost its translation"
            );
        }
    }

    /// Translate everything, write it into the game files, and read the result
    /// back: the translations become the sources.
    pub fn write_then_reread(fixture: Fixture, tag: &str) {
        let workspace = workspace(tag);
        let translation = workspace.join("translation");
        let output = workspace.join("output");

        read(fixture, &translation);

        for path in txt_files(&translation) {
            let translated = translate(&slurp(&path));
            write(&path, translated).unwrap();
        }

        // The writer only emits the files it translates, so the rest of the
        // project - MapInfos above all - has to be there already.
        copy_dir(&fixture.source(), &output.join(fixture.data));

        run(
            fixture,
            Mode::Write,
            BaseFlags::empty(),
            DuplicateMode::Allow,
            &translation,
            Some(&output),
        );

        let reread = workspace.join("reread");

        let mut processor = Processor {
            mode: Mode::Read {
                append: false,
                force: true,
            },
            file_flags: fixture.flags(),
            ..Default::default()
        };
        processor
            .process(fixture.engine, output.join(fixture.data), &reread, None)
            .expect("could not read the written project back");

        for name in fixture.expected_files() {
            // Scripts are re-encoded wholesale rather than per entry.
            if name == "scripts.txt" {
                continue;
            }

            let before = slurp(&translation.join(name));
            let after = slurp(&reread.join(name));
            let after_sources = sources(&after);

            for (source, translation) in entries_outside(&before, "Game Title") {
                let (source, translation) = (source.as_str(), translation.as_str());
                assert!(
                    after_sources.contains(translation),
                    "{name}: {source} was translated to {translation}, which is not in the rewritten project"
                );
            }
        }
    }

    /// `js/plugins.js` reads, writes and reads back.
    ///
    /// Only MV ships one here; MZ's format is the same, so one fixture covers
    /// the pair.
    pub fn plugins_round_trip(fixture: Fixture, tag: &str) {
        if !fixture.plugins {
            return;
        }

        let workspace = workspace(tag);
        let translation = workspace.join("translation");
        let output = workspace.join("output");

        let mut reader = Processor {
            file_flags: FileFlags::Scripts,
            ..processor(fixture, FORCED_READ)
        };
        run_with(fixture, &mut reader, &translation, None);

        let plugins = translation.join("plugins.txt");
        let text = slurp(&plugins);
        check_shape(&text, "plugins.txt");

        assert!(!entries(&text).is_empty(), "no plugin text was extracted");
        assert!(text.contains("<!>NAME<#>"), "plugin sections carry no plugin name");

        for (source, translation) in entries(&text) {
            assert!(
                translation.is_empty(),
                "plugins.txt: {source} came out of a read already translated"
            );
        }

        write(&plugins, translate(&text)).unwrap();

        let mut writer = Processor {
            file_flags: FileFlags::Scripts,
            ..processor(fixture, Mode::Write)
        };
        run_with(fixture, &mut writer, &translation, Some(&output));

        let written = slurp(&output.join("js/plugins.js"));
        assert!(
            written.starts_with("var $plugins ="),
            "the rewritten plugins.js lost its assignment"
        );

        // Read the rewritten file back: the translations are now the sources.
        let reread = workspace.join("reread");
        let mut rereader = Processor {
            file_flags: FileFlags::Scripts,
            ..processor(fixture, FORCED_READ)
        };
        rereader
            .process(fixture.engine, output.join(fixture.data), &reread, None)
            .expect("could not read the rewritten plugins back");

        let after_text = slurp(&reread.join("plugins.txt"));
        let after = sources(&after_text);

        for (source, translation) in translations(&text) {
            assert!(
                after.contains(format!("[{source}]").as_str()),
                "plugins.txt: {source} was translated to {translation}, which is not in the rewritten file"
            );
        }
    }

    /// The title from `Game.ini` reaches the translation file, and comes back
    /// out on a write.
    ///
    /// XP keeps no title in its system file at all, so without this the section
    /// does not exist; VX and VX Ace have one, and the caller's title wins.
    pub fn ini_title(fixture: Fixture, tag: &str) {
        if !fixture.ini {
            return;
        }

        let workspace = workspace(tag);
        let translation = workspace.join("translation");
        let output = workspace.join("output");
        let title = fixture.ini_title();

        assert!(!title.is_empty(), "Game.ini carries an empty title");

        let mut reader = Processor {
            game_title: title.clone(),
            file_flags: FileFlags::System,
            ..processor(fixture, FORCED_READ)
        };
        run_with(fixture, &mut reader, &translation, None);

        let system = translation.join("system.txt");
        let text = slurp(&system);

        assert!(
            text.contains("<!>NAME<#>Game Title"),
            "system.txt has no game title section"
        );
        assert!(
            block(&text, 8).contains(&format!("{title}<#>")),
            "the game title section does not hold the title from Game.ini"
        );

        write(&system, translate(&text)).unwrap();
        copy_dir(&fixture.source(), &output.join(fixture.data));

        let translated_title = format!("[{title}]");
        let mut writer = Processor {
            game_title: translated_title.clone(),
            file_flags: FileFlags::System,
            ..processor(fixture, Mode::Write)
        };
        run_with(fixture, &mut writer, &translation, Some(&output));

        let reread = workspace.join("reread");
        let mut rereader = Processor {
            file_flags: FileFlags::System,
            ..processor(fixture, FORCED_READ)
        };
        rereader
            .process(fixture.engine, output.join(fixture.data), &reread, None)
            .expect("could not read the rewritten system file back");

        let after = slurp(&reread.join("system.txt"));

        if fixture.engine.is_xp() {
            // XP's system file has no title field of its own
            assert!(
                !block(&after, 8).contains(&format!("{translated_title}<#>")),
                "XP has no game-title field of its own; the writer should not have invented one"
            );
        } else {
            assert!(
                block(&after, 8).contains(&format!("{translated_title}<#>")),
                "the translated title did not reach the system file"
            );
        }
    }

    /// `SkipObsolete` drops entries that the game files no longer have.
    pub fn skip_obsolete(fixture: Fixture, tag: &str) {
        const OBSOLETE: &str = "A line no game file has<#>[gone]";

        let workspace = workspace(tag);
        let translation = workspace.join("translation");

        read(fixture, &translation);

        let subject = translation.join(SUBJECT);
        let text = translate(&slurp(&subject));
        let id = first_id(&text);

        // Slipped into the first section, so it belongs to a real id.
        let mut planted = String::with_capacity(text.len() + OBSOLETE.len());
        for line in text.lines() {
            planted.push_str(line);
            planted.push('\n');

            if line == format!("<!>ID<#>{id}") {
                planted.push_str(OBSOLETE);
                planted.push('\n');
            }
        }
        write(&subject, &planted).unwrap();

        // A plain append keeps it: an entry the reader cannot see is still the
        // translator's work.
        append(fixture, BaseFlags::empty(), &translation);

        let kept = slurp(&subject);
        check_shape(&kept, SUBJECT);
        assert!(
            kept.contains(OBSOLETE),
            "{SUBJECT}: a plain append dropped an obsolete entry"
        );

        append(fixture, BaseFlags::SkipObsolete, &translation);

        let pruned = slurp(&subject);
        check_shape(&pruned, SUBJECT);
        assert!(
            !pruned.contains("A line no game file has"),
            "{SUBJECT}: SkipObsolete kept an obsolete entry"
        );

        // The entries the game files do have are all still there.
        for source in sources(&translate(&slurp(&subject))) {
            assert!(!source.is_empty());
        }
        assert_eq!(
            sources(&pruned).len() + 1,
            sources(&kept).len(),
            "{SUBJECT}: SkipObsolete dropped more than the obsolete entry"
        );
    }

    /// A skipped entry is passed through from the translation file rather than
    /// re-read, so an append leaves it exactly as it was.
    pub fn skipped_entries_pass_through(fixture: Fixture, tag: &str) {
        let workspace = workspace(tag);
        let translation = workspace.join("translation");

        read(fixture, &translation);

        let subject = translation.join(SUBJECT);
        let text = translate(&slurp(&subject));
        write(&subject, &text).unwrap();

        let id = first_id(&text);
        let victim = entries(&block(&text, id))
            .first()
            .expect("the first section has no entries")
            .0
            .to_owned();

        write(&subject, drop_entry(&text, &victim)).unwrap();

        let mut skipping = processor(fixture, FORCED_APPEND);
        skipping.skip_events = Vec::from([(RPGMFileType::Items, Vec::from([id]))]);
        run_with(fixture, &mut skipping, &translation, None);

        let skipped = slurp(&subject);
        check_shape(&skipped, SUBJECT);
        assert!(
            !sources(&block(&skipped, id)).contains(victim.as_str()),
            "{SUBJECT}: a skipped section was re-read anyway"
        );

        // Without the skip the same append puts it back.
        append(fixture, BaseFlags::empty(), &translation);

        assert!(
            sources(&block(&slurp(&subject), id)).contains(victim.as_str()),
            "{SUBJECT}: the entry did not come back once the skip was gone"
        );
    }

    /// The same, for maps - which have their own skip list.
    pub fn skipped_maps_pass_through(fixture: Fixture, tag: &str) {
        let workspace = workspace(tag);
        let translation = workspace.join("translation");

        read(fixture, &translation);

        let maps = translation.join("maps.txt");
        let text = translate(&slurp(&maps));
        write(&maps, &text).unwrap();

        let skipped_id = first_id(&text);
        let victim = entries(&block(&text, skipped_id))
            .first()
            .expect("the first map has no entries")
            .0
            .to_owned();

        write(&maps, drop_entry(&text, &victim)).unwrap();

        let mut skipping = processor(fixture, FORCED_APPEND);
        skipping.skip_maps = Vec::from([skipped_id]);
        run_with(fixture, &mut skipping, &translation, None);

        let after = slurp(&maps);
        check_shape(&after, "maps.txt");

        assert!(
            !sources(&block(&after, skipped_id)).contains(victim.as_str()),
            "maps.txt: a skipped map was re-read anyway"
        );

        append(fixture, BaseFlags::empty(), &translation);

        assert!(
            sources(&block(&slurp(&maps), skipped_id)).contains(victim.as_str()),
            "maps.txt: the entry did not come back once the skip was gone"
        );
    }

    /// A file whose contents have not changed since the last read is skipped.
    pub fn unchanged_files_are_skipped(fixture: Fixture, tag: &str) {
        let workspace = workspace(tag);
        let translation = workspace.join("translation");

        let mut first = processor(fixture, FORCED_READ);
        run_with(fixture, &mut first, &translation, None);

        let subject = translation.join(SUBJECT);
        let text = translate(&slurp(&subject));
        let victim = entries(&text).first().expect("no entries to drop").0.to_owned();

        write(&subject, drop_entry(&text, &victim)).unwrap();

        // Appending without forcing, carrying the hashes of the read above:
        // nothing in the game files changed, so nothing is re-read.
        let mut hashed = Processor {
            hashes: std::mem::take(&mut first.hashes),
            ..processor(
                fixture,
                Mode::Read {
                    append: true,
                    force: false,
                },
            )
        };
        run_with(fixture, &mut hashed, &translation, None);

        assert!(
            !sources(&slurp(&subject)).contains(victim.as_str()),
            "{SUBJECT}: an unchanged file was re-read"
        );

        // The same append with no hashes to compare against does re-read it.
        let mut unhashed = processor(
            fixture,
            Mode::Read {
                append: true,
                force: false,
            },
        );
        run_with(fixture, &mut unhashed, &translation, None);

        assert!(
            sources(&slurp(&subject)).contains(victim.as_str()),
            "{SUBJECT}: the entry did not come back without the hashes"
        );

        assert!(!unhashed.hashes.is_empty(), "the processor recorded no hashes to reuse");
    }

    /// Reading and writing with duplicates removed exercises the flattened
    /// lookup instead of the per-entry maps.
    pub fn duplicates_removed(fixture: Fixture, tag: &str) {
        let workspace = workspace(tag);
        let translation = workspace.join("translation");
        let output = workspace.join("output");

        run(
            fixture,
            Mode::Read {
                append: false,
                force: true,
            },
            BaseFlags::empty(),
            DuplicateMode::Remove,
            &translation,
            None,
        );

        for path in txt_files(&translation) {
            let text = slurp(&path);
            let name = path.file_name().unwrap().to_string_lossy();
            // Only maps and the "other" files are deduplicated; the system,
            // script and plugin files are exempt, as `DuplicateMode`
            // documents.
            if is_main_file(&name) {
                let all: Vec<&str> = entries(&text).into_iter().map(|(s, _)| s).collect();

                assert_eq!(all.len(), sources(&text).len(), "{name} still has duplicate sources");
            }

            write(&path, translate(&text)).unwrap();
        }

        copy_dir(&fixture.source(), &output.join(fixture.data));

        run(
            fixture,
            Mode::Write,
            BaseFlags::empty(),
            DuplicateMode::Remove,
            &translation,
            Some(&output),
        );
    }
}

macro_rules! engines {
    ($($name:ident => (
        $dir:literal,
        $data:literal,
        $engine:expr,
        $plugins:literal,
        $ini:literal
    )),* $(,)?) => {
        $(
            mod $name {
                use super::*;

                const FIXTURE: Fixture = Fixture {
                    dir: $dir,
                    data: $data,
                    engine: $engine,
                    plugins: $plugins,
                    ini: $ini,
                };

                #[test]
                fn plain_read() {
                    scenarios::plain_read(
                        FIXTURE,
                        concat!($dir, "-plain-read"),
                    );
                }

                #[test]
                fn reads_are_reproducible() {
                    scenarios::reads_are_reproducible(
                        FIXTURE,
                        concat!($dir, "-reproducible"),
                    );
                }

                #[test]
                fn append_restores_deleted_entries() {
                    scenarios::append_restores_deleted_entries(
                        FIXTURE,
                        concat!($dir, "-append"),
                    );
                }

                #[test]
                fn purge_then_ignore() {
                    scenarios::purge_then_ignore(
                        FIXTURE,
                        concat!($dir, "-purge"),
                    );
                }

                #[test]
                fn map_events_round_trip() {
                    scenarios::map_events_round_trip(
                        FIXTURE,
                        concat!($dir, "-map-events"),
                    );
                }

                #[test]
                fn plugins_round_trip() {
                    scenarios::plugins_round_trip(
                        FIXTURE,
                        concat!($dir, "-plugins"),
                    );
                }

                #[test]
                fn ini_title() {
                    scenarios::ini_title(
                        FIXTURE,
                        concat!($dir, "-ini-title"),
                    );
                }

                #[test]
                fn skip_obsolete() {
                    scenarios::skip_obsolete(
                        FIXTURE,
                        concat!($dir, "-skip-obsolete"),
                    );
                }

                #[test]
                fn skipped_entries_pass_through() {
                    scenarios::skipped_entries_pass_through(
                        FIXTURE,
                        concat!($dir, "-skip-events"),
                    );
                }

                #[test]
                fn skipped_maps_pass_through() {
                    scenarios::skipped_maps_pass_through(
                        FIXTURE,
                        concat!($dir, "-skip-maps"),
                    );
                }

                #[test]
                fn unchanged_files_are_skipped() {
                    scenarios::unchanged_files_are_skipped(
                        FIXTURE,
                        concat!($dir, "-hashes"),
                    );
                }

                #[test]
                fn write_then_reread() {
                    scenarios::write_then_reread(
                        FIXTURE,
                        concat!($dir, "-write"),
                    );
                }

                #[test]
                fn duplicates_removed() {
                    scenarios::duplicates_removed(
                        FIXTURE,
                        concat!($dir, "-duplicates"),
                    );
                }
            }
        )*
    };
}

engines! {
    mz => ("RMMZ", "data", EngineType::MVMZ, false, false),
    mv => ("RMMV", "data", EngineType::MVMZ, true, false),
    vxace => ("RMVXACE", "Data", EngineType::VXAce, false, true),
    vx => ("RMVX", "Data", EngineType::VX, false, true),
    xp => ("RMXP", "Data", EngineType::XP, false, true),
}
