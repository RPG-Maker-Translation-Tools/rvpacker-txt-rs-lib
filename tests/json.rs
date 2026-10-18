//! Round trips through the `json` module: every Marshal file in a project turned
//! into JSON (or, for `Scripts`, into Ruby) and written back.
//!
//! Only the older engines have Marshal files; MV and MZ ship JSON already.

use rvpacker_txt_rs_lib::{
    EngineType, Error,
    json::{generate, generate_file, write, write_file},
};
use std::{
    fs::{create_dir_all, read, read_dir, read_to_string, remove_dir_all, write as write_file_to},
    path::{Path, PathBuf},
};

#[derive(Clone, Copy)]
struct Fixture {
    dir: &'static str,
    engine: EngineType,
}

impl Fixture {
    fn source(self) -> PathBuf {
        Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("tests")
            .join(self.dir)
            .join("Data")
    }
}

fn workspace(tag: &str) -> PathBuf {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("target/json").join(tag);

    let _ = remove_dir_all(&path);
    create_dir_all(&path).expect("could not create the workspace");
    path
}

/// The same document with every `__id` dropped.
///
/// `__id` is the object's position in the Marshal link table, which shifts when
/// the file is re-encoded - the dumper tags strings with their encoding, and
/// that changes how many objects the table holds. It says nothing about the
/// data, so comparisons ignore it.
fn without_ids(json: &str) -> serde_json::Value {
    fn strip(value: &mut serde_json::Value) {
        match value {
            serde_json::Value::Object(object) => {
                object.remove("__id");

                for (_, value) in object.iter_mut() {
                    strip(value);
                }
            }
            serde_json::Value::Array(array) => {
                for value in array {
                    strip(value);
                }
            }
            _ => {}
        }
    }

    let mut value: serde_json::Value = serde_json::from_str(json).expect("not valid JSON");
    strip(&mut value);
    value
}

/// The files in a directory, by name, sorted.
fn names(dir: &Path) -> Vec<String> {
    let mut names: Vec<String> = read_dir(dir)
        .expect("no directory")
        .flatten()
        .map(|entry| entry.file_name().to_string_lossy().into_owned())
        .collect();

    names.sort();
    names
}

mod scenarios {
    use super::*;

    /// Every data file becomes a JSON document, and `Scripts` becomes Ruby.
    pub fn generate_covers_the_project(fixture: Fixture, tag: &str) {
        let workspace = workspace(tag);
        let json = workspace.join("json");

        generate(fixture.source().as_path(), json.as_path(), true, None).expect("generate failed");

        let extension = fixture.engine.extension();
        let expected: Vec<String> = names(&fixture.source())
            .into_iter()
            .map(|name| {
                let stem = name.trim_end_matches(extension).trim_end_matches('.');

                if stem == "Scripts" {
                    format!("{stem}.rb")
                } else {
                    format!("{stem}.json")
                }
            })
            .collect();

        assert_eq!(names(&json), expected);

        for name in expected {
            let content = read_to_string(json.join(&name)).unwrap();
            assert!(!content.is_empty(), "{name} is empty");

            if name.ends_with(".json") {
                serde_json::from_str::<serde_json::Value>(&content)
                    .unwrap_or_else(|e| panic!("{name} is not valid JSON: {e}"));
            } else {
                assert!(content.starts_with("<!>SCRIPT<#>"), "{name} has no script header");
            }
        }
    }

    /// Writing the JSON back reproduces the data it came from.
    ///
    /// The comparison is at the data level, not byte for byte: the dumper
    /// always tags a string with its encoding, while RGSS1 and RGSS2 - Ruby
    /// 1.8 - wrote them plain, so a faithful reload of an XP or VX file does
    /// not reproduce the original bytes. Regenerating the JSON is what says
    /// whether anything was actually lost.
    pub fn round_trip_is_lossless(fixture: Fixture, tag: &str) {
        let workspace = workspace(tag);
        let json = workspace.join("json");
        let output = workspace.join("output");
        let json_again = workspace.join("json-again");

        generate(fixture.source().as_path(), json.as_path(), true, None).expect("generate failed");
        write(json.as_path(), output.as_path(), fixture.engine, None).expect("write failed");

        assert_eq!(names(&output), names(&fixture.source()));

        generate(output.as_path(), json_again.as_path(), true, None).expect("the written files could not be read back");

        assert_eq!(names(&json_again), names(&json));

        for name in names(&json) {
            let before = read_to_string(json.join(&name)).unwrap();
            let after = read_to_string(json_again.join(&name)).unwrap();

            if name.ends_with(".rb") {
                assert_eq!(after, before, "{name} changed across a round trip");
            } else {
                assert!(
                    without_ids(&before) == without_ids(&after),
                    "{name} lost data across a JSON round trip"
                );
            }
        }
    }

    /// The per-file entry points behave like the directory ones.
    pub fn single_files_round_trip(fixture: Fixture, tag: &str) {
        let _ = tag;

        let name = format!("Actors.{}", fixture.engine.extension());
        let before = read(fixture.source().join(&name)).unwrap();

        let json = generate_file(&before, &name, None).expect("generate_file failed");
        let written = write_file(&json).expect("write_file failed");
        let json_again = generate_file(&written, &name, None).expect("generate_file failed");

        assert!(
            without_ids(&json) == without_ids(&json_again),
            "{name} lost data across a round trip"
        );
    }

    /// Scripts take the Ruby path rather than the JSON one.
    pub fn scripts_round_trip(fixture: Fixture, tag: &str) {
        let workspace = workspace(tag);
        let json = workspace.join("json");
        let output = workspace.join("output");

        generate(fixture.source().as_path(), json.as_path(), true, None).expect("generate failed");

        let ruby = read_to_string(json.join("Scripts.rb")).unwrap();
        let headers = ruby.lines().filter(|line| line.starts_with("<!>SCRIPT<#>")).count();
        assert!(headers > 0, "Scripts.rb has no script headers");
        assert!(ruby.contains("class "), "Scripts.rb carries no Ruby source");

        write(json.as_path(), output.as_path(), fixture.engine, None).expect("write failed");

        let name = format!("Scripts.{}", fixture.engine.extension());
        let written = read(output.join(&name)).unwrap();
        let ruby_again = generate_file(&written, &name, None).expect("generate_file failed");

        assert_eq!(
            ruby_again.lines().filter(|l| l.starts_with("<!>SCRIPT<#>")).count(),
            headers,
            "a script was lost across the round trip"
        );
        assert_eq!(ruby_again, ruby, "Scripts.rb changed across a round trip");
    }

    /// Without `force`, an existing JSON file is left alone.
    pub fn generate_does_not_clobber(fixture: Fixture, tag: &str) {
        let workspace = workspace(tag);
        let json = workspace.join("json");

        generate(fixture.source().as_path(), json.as_path(), true, None).expect("generate failed");

        let subject = json.join("Actors.json");
        write_file_to(&subject, "edited by hand").unwrap();

        // The script file is `Scripts.rb`, so an edited one has to survive too. Only engines with scripts have it.
        let script = json.join("Scripts.rb");
        let has_script = script.exists();
        if has_script {
            write_file_to(&script, "edited by hand").unwrap();
        }

        generate(fixture.source().as_path(), json.as_path(), false, None).expect("generate failed");
        assert_eq!(read_to_string(&subject).unwrap(), "edited by hand");

        if has_script {
            assert_eq!(read_to_string(&script).unwrap(), "edited by hand");
        }

        generate(fixture.source().as_path(), json.as_path(), true, None).expect("generate failed");
        assert_ne!(read_to_string(&subject).unwrap(), "edited by hand");

        if has_script {
            assert_ne!(read_to_string(&script).unwrap(), "edited by hand");
        }
    }
}

macro_rules! engines {
    ($($name:ident => ($dir:literal, $engine:expr)),* $(,)?) => {
        $(
            mod $name {
                use super::*;

                const FIXTURE: Fixture = Fixture {
                    dir: $dir,
                    engine: $engine,
                };

                #[test]
                fn generate_covers_the_project() {
                    scenarios::generate_covers_the_project(
                        FIXTURE,
                        concat!($dir, "-generate"),
                    );
                }

                #[test]
                fn round_trip_is_lossless() {
                    scenarios::round_trip_is_lossless(
                        FIXTURE,
                        concat!($dir, "-round-trip"),
                    );
                }

                #[test]
                fn single_files_round_trip() {
                    scenarios::single_files_round_trip(
                        FIXTURE,
                        concat!($dir, "-single"),
                    );
                }

                #[test]
                fn scripts_round_trip() {
                    scenarios::scripts_round_trip(
                        FIXTURE,
                        concat!($dir, "-scripts"),
                    );
                }

                #[test]
                fn generate_does_not_clobber() {
                    scenarios::generate_does_not_clobber(
                        FIXTURE,
                        concat!($dir, "-force"),
                    );
                }
            }
        )*
    };
}

engines! {
    vxace => ("RMVXACE", EngineType::VXAce),
    vx => ("RMVX", EngineType::VX),
    xp => ("RMXP", EngineType::XP),
}

#[test]
fn negative_integers_survive_a_round_trip() {
    for value in [-1, -123, -124, -150, -250, -256, -257, -1000, 0, 150] {
        // The tagged form `generate_file` produces: an array object holding
        // one integer.
        let json = format!(r#"{{"__type": "array", "__value": [{value}]}}"#);
        let dumped = write_file(&json).expect("write_file failed");
        let loaded = generate_file(&dumped, "Numbers.rvdata2", None).expect("generate failed");

        let back: i64 = loaded
            .lines()
            .find_map(|line| line.trim().trim_end_matches(',').parse().ok())
            .unwrap_or_else(|| panic!("{value} came back as {loaded}"));

        assert_eq!(back, value, "{value} did not survive the round trip");
    }
}

#[test]
fn stray_files_in_the_data_folder_are_skipped() {
    let workspace = workspace("stray-files");
    let source = workspace.join("source");
    let output = workspace.join("output");
    create_dir_all(&source).unwrap();

    // Neither is Marshal, and neither has a data file's extension, so neither may stop the conversion.
    write_file_to(source.join("desktop.ini"), "[.ShellClassInfo]").unwrap();
    write_file_to(source.join("Actors.rxdata.bak"), "not marshal").unwrap();

    generate(&source, &output, false, None).expect("a stray file stopped the conversion");
    assert_eq!(read_dir(&output).unwrap().count(), 0);
}

#[test]
fn write_refuses_what_it_cannot_write() {
    let nowhere = Path::new("nowhere");

    assert!(matches!(
        write(nowhere, nowhere, EngineType::RM2K, None),
        Err(Error::UnsupportedEngine(_))
    ));
    assert!(matches!(
        write(nowhere, nowhere, EngineType::XP, Some(encoding_rs::UTF_16LE)),
        Err(Error::UnsupportedWriteEncoding { .. })
    ));
}

/// Scripts in a legacy codepage convert to Ruby and back when told the codepage, and fail without it.
#[test]
fn scripts_follow_the_given_encodings() {
    let workspace = workspace("script-encodings");
    let ruby = workspace.join("ruby");
    let output = workspace.join("output");
    create_dir_all(&ruby).unwrap();

    write_file_to(ruby.join("Scripts.rb"), "<!>SCRIPT<#>1<#>Main\nputs '勇者'\n").unwrap();
    write(&ruby, &output, EngineType::XP, Some(encoding_rs::SHIFT_JIS)).expect("write failed");

    let scripts = read(output.join("Scripts.rxdata")).unwrap();

    assert!(matches!(
        generate_file(&scripts, "Scripts.rxdata", None),
        Err(Error::InvalidText { .. })
    ));
    assert!(
        generate_file(&scripts, "Scripts.rxdata", Some(encoding_rs::SHIFT_JIS))
            .unwrap()
            .contains("勇者")
    );

    // Not representable in the codepage (Shift_JIS has Cyrillic, but no emoji): a hard error, not a replacement character.
    write_file_to(ruby.join("Scripts.rb"), "<!>SCRIPT<#>1<#>Main\nputs '😀'\n").unwrap();
    assert!(matches!(
        write(&ruby, &output, EngineType::XP, Some(encoding_rs::SHIFT_JIS)),
        Err(Error::UnmappableText { .. })
    ));
}
