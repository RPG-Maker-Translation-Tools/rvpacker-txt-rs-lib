//! Round trips through the `serde` module: a translation file exported to
//! another format and imported back has to come out unchanged.
//!
//! The JSON schema is always available; the rest are behind their features, so
//! this file only covers them under `--all-features`.

use rvpacker_txt_rs_lib::{
    EngineType, FileFlags, Mode, Processor,
    serde::{export_json, import_json},
};
use std::{
    fs::{create_dir_all, read_to_string, remove_dir_all},
    path::{Path, PathBuf},
    sync::OnceLock,
};

/// Real translation files, read once for the whole test binary.
fn translations() -> &'static PathBuf {
    static DIR: OnceLock<PathBuf> = OnceLock::new();

    DIR.get_or_init(|| {
        let root = Path::new(env!("CARGO_MANIFEST_DIR"));
        let out = root.join("target/serde/translation");

        let _ = remove_dir_all(&out);
        create_dir_all(&out).expect("could not create the directory");

        let mut processor = Processor {
            mode: Mode::Read {
                append: false,
                force: true,
            },
            file_flags: FileFlags::all(),
            ..Default::default()
        };

        processor
            .process(
                EngineType::XP,
                root.join("tests/RMXP/Data"),
                &out,
                None,
            )
            .expect("could not read the fixture");

        out
    })
}

fn sample(name: &str) -> String {
    read_to_string(translations().join(name))
        .unwrap_or_else(|e| panic!("{name}: {e}"))
}

/// The same file with every entry translated, for the columns to be non-empty.
fn translated(name: &str) -> String {
    let text = sample(name);
    let mut output = String::with_capacity(text.len() * 2);

    for line in text.lines() {
        output.push_str(line);

        if let Some(source) = line.strip_suffix("<#>")
            && !source.is_empty()
        {
            output.push_str("перевод");
        }

        output.push('\n');
    }

    // The reader leaves no trailing newline, so neither does this.
    output.pop();
    output
}

/// Files that between them cover comments, metadata, multi-line entries and
/// Ruby source.
const SAMPLES: [&str; 4] =
    ["items.txt", "maps.txt", "system.txt", "scripts.txt"];

/// Hand-written cases the fixtures do not produce.
const EDGE_CASES: [(&str, &str); 6] = [
    ("a single entry", "source<#>"),
    ("a filled entry", "source<#>translation"),
    (
        "several translation columns",
        "source<#>first<#>second<#>third",
    ),
    ("an empty middle column", "source<#><#>third"),
    (
        "a line break marker",
        r"first line\#second line<#>первая\#вторая",
    ),
    (
        "text that looks like the old comment syntax",
        "<!-- not a comment --><#>translated",
    ),
];

fn cases() -> Vec<(String, String)> {
    let mut cases: Vec<(String, String)> = SAMPLES
        .into_iter()
        .flat_map(|name| {
            [
                (name.to_owned(), sample(name)),
                (format!("{name} (translated)"), translated(name)),
            ]
        })
        .collect();

    cases.extend(
        EDGE_CASES
            .into_iter()
            .map(|(name, content)| (name.to_owned(), content.to_owned())),
    );

    cases
}

#[test]
fn json_round_trips() {
    for (name, content) in cases() {
        let exported = export_json(&content).expect("export failed");
        let imported = import_json(&exported).expect("import failed");

        assert_eq!(imported, content, "{name} changed across a JSON round trip");
    }
}

#[test]
fn the_json_export_is_valid_json() {
    let exported = export_json(&sample("items.txt")).expect("export failed");
    let value: serde_json::Value =
        serde_json::from_str(&exported).expect("not valid JSON");

    let entries = value.as_array().expect("the export is not an array");
    assert!(!entries.is_empty());

    for entry in entries {
        let kind = entry["type"].as_str().expect("an entry has no type");
        assert!(
            kind == "comment" || kind == "translation",
            "unexpected entry type {kind}"
        );
    }
}

#[cfg(feature = "serde-yaml")]
#[test]
fn yaml_round_trips() {
    use rvpacker_txt_rs_lib::serde::{export_yaml, import_yaml};

    for (name, content) in cases() {
        let exported = export_yaml(&content).expect("export failed");
        let imported = import_yaml(&exported).expect("import failed");

        assert_eq!(imported, content, "{name} changed across a YAML round trip");
    }
}

#[cfg(feature = "serde-csv")]
#[test]
fn csv_round_trips() {
    use rvpacker_txt_rs_lib::serde::{export_csv, import_csv};

    for (name, content) in cases() {
        let exported = export_csv(&content).expect("export failed");
        let imported = import_csv(&exported).expect("import failed");

        assert_eq!(imported, content, "{name} changed across a CSV round trip");
    }
}

#[cfg(feature = "serde-xml")]
#[test]
fn xml_round_trips() {
    use rvpacker_txt_rs_lib::serde::{export_xml, import_xml};

    for (name, content) in cases() {
        let exported = export_xml(&content).expect("export failed");
        let imported = import_xml(&exported).expect("import failed");

        assert_eq!(imported, content, "{name} changed across an XML round trip");
    }
}

#[cfg(feature = "serde-xlsx")]
#[test]
fn xlsx_round_trips() {
    use rvpacker_txt_rs_lib::serde::{export_xlsx, import_xlsx};

    for (name, content) in cases() {
        let exported = export_xlsx(&content).expect("export failed");
        let imported = import_xlsx(&exported).expect("import failed");

        assert_eq!(
            imported, content,
            "{name} changed across an XLSX round trip"
        );
    }
}
