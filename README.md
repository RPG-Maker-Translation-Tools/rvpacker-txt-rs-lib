# rvpacker-txt-rs-lib

A Rust library for extracting the translatable text out of RPG Maker 2000/2003/XP/VX/VX Ace/MV/MZ data files (`.ldb`/`.lmt`/`.lmu`, `.rxdata`, `.rvdata`, `.rvdata2`, `.json`) into plain `.txt` files, and writing translated `.txt` files back into the game's original format.

It also ships a `json` module that converts the older engines' binary data files to and from JSON, independent of the translation workflow.

This library backs [RPGMTranslate GUI](https://github.com/RPG-Maker-Translation-Tools/rpgmtranslate-qt) and [rvpacker-txt-rs CLI](https://github.com/RPG-Maker-Translation-Tools/rvpacker-txt-rs). If you just want to translate a game, use one of those; use this crate to build your own tool.

## Installation

```bash
cargo add rvpacker-txt-rs-lib
```

## Features

`gxhash` makes the crate's [`HashMap`] and [`HashSet`] use gxhash, which needs AES-NI on x86 or the Armv8 crypto extensions. Without it, they use rapidhash.

The optional `serde-*` features add [`serde`](https://docs.rs/rvpacker-txt-rs-lib/latest/rvpacker_txt_rs_lib/serde/index.html) converters between `.txt` files and other formats:

- `serde-csv` - CSV.
- `serde-xml` - XML.
- `serde-xlsx` - XLSX.
- `serde-yaml` - YAML.

JSON export/import (`export_json`/`import_json`) is always available. See [Serialization](#serialization-converters).

## Quick start

[`Processor`](#processor---the-high-level-api) is the entry point for most uses: set the fields you need, then call [`Processor::process`] once per operation.

```rust no_run
use rvpacker_txt_rs_lib::{EngineType, Error, FileFlags, Mode, Processor};
use std::path::Path;

fn main() -> Result<(), Error> {
    let mut processor = Processor {
        mode: Mode::read(),
        file_flags: FileFlags::Map | FileFlags::other(),
        ..Default::default()
    };

    // Extract text from `C:/Game/Data` into `.txt` files under `C:/Game/translation`.
    processor.process(EngineType::VXAce, "C:/Game/Data", "C:/Game/translation", None)?;

    // Translate the `.txt` files by hand, then write them back into the game's format.
    processor.mode = Mode::Write;
    processor.process(
        EngineType::VXAce,
        "C:/Game/Data",
        "C:/Game/translation",
        Some(Path::new("C:/Game/output")),
    )?;

    // Drop any translation entry that was never filled in.
    processor.mode = Mode::Purge;
    processor.process(EngineType::VXAce, "C:/Game/Data", "C:/Game/translation", None)?;
    Ok(())
}
```

## Concepts

### Engine types

[`EngineType`] selects the binary format and JSON field naming: `MVMZ`, `VXAce`, `VX`, `XP` and `RM2K`. Nothing is auto-detected; the caller supplies it, typically by checking which of `System.json`/`System.rvdata2`/`System.rvdata`/`System.rxdata` exists, or - for `RM2K` - whether `RPG_RT.ldb` exists. [`EngineType::from_extension`] maps a data file's extension to its engine.

### File kinds

[`FileFlags`] selects which files are processed:

- `Map` - `Mapxxx.ext`.
- `System` - the system file. Ignored for RM2K, whose vocabulary is under `Database`.
- `Scripts` - `Scripts.ext` for XP/VX/VX Ace, `plugins.js` (read from `../js` next to the data directory) for MV/MZ. Ignored for RM2K.
- [`FileFlags::other()`] - `Actors`, `Armors`, `Classes`, `CommonEvents`, `Enemies`, `Items`, `Skills`, `States`, `Troops` and `Weapons`. Each of these also has its own flag. For RM2K, `Actors`, `Skills`, `Items`, `Enemies`, `Troops`, `Classes`, `CommonEvents` and `States` select the matching list of `RPG_RT.ldb`; `Armors` and `Weapons` have no list there.
- `Database` - RM2K's vocabulary (`terms.txt`, with the game title as its second section). The other lists of `RPG_RT.ldb` have their own flags above. `RPG_RT.ldb` is read on every RM2K run, because its `system.ldb_id` decides the build to write.

### Text encoding

Reading and writing use separate settings: [`core::Base::set_read_encoding`]/[`core::Base::set_write_encoding`] ([`Processor::read_encoding`]/[`Processor::write_encoding`] at the higher level). Don't assume one codepage fits both.

**Reading.** XP/VX and RM2K text has no in-file encoding tag, so it is read as UTF-8 by default. For XP/VX that is almost always right, since RGSS sets Ruby's `$KCODE` to `"UTF8"`; a game whose text was saved in a legacy codepage is the exception. Text that isn't valid UTF-8 fails the read, so set the codepage once you know it. RM2K text is usually in a legacy codepage, so set it there too; nothing is guessed. VX Ace tags each string with its encoding. The library trusts that tag over `read_encoding` for text that isn't ASCII, and an unrecognized tag falls back to `read_encoding`. MV/MZ files are JSON, so they are always UTF-8 and both settings are ignored for them.

For XP/VX, [`get_ini_title`] helps find the codepage: `Game.ini`'s title is the only non-ASCII field. Decode the returned bytes with each candidate codepage and pick the one that yields sensible text. A purely ASCII title gives no signal. [`get_ini_title_rm2k`] does the same for RM2K's `RPG_RT.ini`, and [`get_system_title`] reads the title out of a `System.json` for MV/MZ.

**Writing defaults to UTF-8 on every engine.** Text that doesn't fit the source codepage (e.g. Russian written into a Shift-JIS game) would otherwise be silently replaced with numeric character references. Set the write encoding only when the translation fits a legacy codepage and the game reads that codepage. A translation the write encoding can't represent fails its file's write, and UTF-16 can't be written at all.

RM2K/2003 display text using the player's "language for non-Unicode programs" setting, not the file's declared encoding. For RM2K/2003, and for an XP/VX exception:

- If the target machine's non-Unicode codepage is UTF-8 (Windows 10 1903+, Region settings → "Beta: Use Unicode UTF-8 for worldwide language support"), the default works.
- Otherwise, set `write_encoding` to the _target language's_ codepage (e.g. Windows-1251 for Russian), and have players match it or run the game through a locale emulator such as Locale Emulator.

VX Ace (RGSS3) is Unicode-native, so the UTF-8 default works without either step.

### Modes

[`Mode`] picks the operation:

- `Mode::read()` (`Mode::Read { append: false, force: false }`) - extract text into `.txt` files. An existing translation file is left alone, and the run logs why. Use `append` or `force` to change that.
- `Mode::Read { append: true, .. }` - `append` keeps existing translations and adds new lines, for a game that received a content update. A file whose content hash matches the previous read is skipped as unchanged. Persist [`Processor::hashes`] between runs for this to work.
- `Mode::Read { force: true, .. }` - `force` overwrites existing translation files. With `append`, it also bypasses the hash check.
- `Mode::Write` - write translated `.txt` files back into the game's original format.
- `Mode::Purge` - drop translation entries that were never filled in.

In every mode except a plain read, a translation file that doesn't exist is skipped with a warning, rather than failing the whole run.

### Duplicate handling

[`DuplicateMode`] controls repeated source lines. It only affects map and "other" files:

- `Allow` - the default. Each map/event keeps its own entries, so repeated text can have different translations per context.
- `Remove` - every occurrence shares one entry and one translation. Less repetition in the `.txt` files, but no per-context translations. The translation is still written back everywhere the line occurs.

Reads and writes must use the same setting.

## API

### `Processor` - the high-level API

[`Processor`] wraps the `core` module and handles all filesystem access: reading source files, calling into `core`, and writing `.txt`/game files. It's a plain struct: set the fields you need, `..Default::default()` the rest, and call [`Processor::process`]. [`Processor::new`] is the same as `Processor { mode, ..Default::default() }`. Each field is documented on the struct. See [Quick start](#quick-start).

What [`Processor::process`] does with the paths:

- Translation files are named after the data file in lowercase: `maps.txt`, `actors.txt`, `commonevents.txt`, `system.txt`, `scripts.txt` (XP/VX/VX Ace) or `plugins.txt` (MV/MZ). RM2K uses `maps.txt`, `actors.txt`, `skills.txt`, `items.txt`, `enemies.txt`, `troops.txt`, `classes.txt`, `commonevents.txt`, `states.txt` and `terms.txt`. `.rvpacker-ignore` sits beside them.
- `Mode::Write` puts rebuilt files under `output_path`, or `./output` if it is `None`: in `data` (MV/MZ) or `Data` (XP/VX/VX Ace), `js/plugins.js` for MV/MZ, and the project root for RM2K. Every other mode writes only into the translation directory.
- A file whose translation `.txt` is missing is skipped with a warning on every mode but a plain read. A plain read skips a `.txt` that already exists unless `force` is set.
- Source files are processed in parallel, and the output is the same on any thread count. Files are written through a temporary file and renamed, so a crash doesn't leave one cut off.
- If a run fails, `hashes` is put back as it was, so a failed read can't make the next append skip files whose text never reached a translation.
- `RM2K` needs `RPG_RT.ldb` in `source_path` on every run, and `RPG_RT.lmt` when maps are included.

### `core` module - the low-level API

[`core::Base`] is what [`Processor`] is built on. It has one method per file kind - `process_map`, `process_other`, `process_system`, `process_scripts`, `process_plugins` - each taking raw bytes and returning `Result<Option<ProcessedData>, Error>`, with no filesystem access. `None` means the file had nothing to process, such as a map that `MapInfos` doesn't list. Use it when [`Processor`]'s all-at-once shape doesn't fit, such as a GUI that processes one file per user action.

Maps are processed as a run, because every map shares one translation file: call `begin_maps`, then `process_map` per file, then `finish_maps`.

```rust no_run
use rvpacker_txt_rs_lib::{core::Base, Mode, EngineType};
use std::fs::read;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut base = Base::new(Mode::Read { append: false, force: true }, EngineType::VXAce);

    let mapinfos = read("C:/Game/Data/MapInfos.rvdata2")?;
    base.begin_maps(&mapinfos, None)?;

    let map_file_content = read("C:/Game/Data/Map001.rvdata2")?;
    base.process_map("Map001.rvdata2", &map_file_content)?;

    // The translation is only available once every map has been processed.
    let translation_data = base.finish_maps();

    Ok(())
}
```

[`ProcessedData`] holds either the game file's bytes (`RPGMData`, from write) or the `.txt` bytes (`TranslationData`, from read and purge). It implements `AsRef<[u8]>`, so you can get the bytes without matching on it.

### RPG Maker 2000/2003

RM2K data is parsed by [`rm2k-lib`](https://crates.io/crates/rm2k-lib) into typed structs. [`core::Base`] has its own `begin_rm2k_maps`/`process_rm2k_map`/`finish_rm2k_maps` and `process_rm2k_*` methods for them, so you only need `rm2k-lib` to load those structs yourself (it is `rm2k` in code).

RM2K's database methods mutate the struct you pass in and return `Ok(None)` on write. Serialize `RPG_RT.ldb` once with `rm2k::file::save_database` after every section has run. [`Processor::process`] does this for `RM2K` and is the reference for the full sequence.

The engine build matters. Some fields exist only on RPG Maker 2000 or only on 2003, and `Base` writes the R2K layout unless you say otherwise. Set it from the database with [`core::Base::set_rm2k_engine`], as [`Processor`] does:

```rust no_run
use rvpacker_txt_rs_lib::{core::Base, Mode, EngineType};
use rm2k::{engine::Engine, file::load_database};
use std::fs::read;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let ldb_content = read("C:/Game/RPG_RT.ldb")?;
    let mut database = load_database(&ldb_content)?;

    let mut base = Base::new(Mode::read(), EngineType::RM2K);
    base.set_rm2k_engine(Engine::from_ldb_id(database.value.system.ldb_id));

    let translation_data = base.process_rm2k_actors(&mut database.value.actors, None)?;

    Ok(())
}
```

RM2K's layout differs: `RPG_RT.ldb`, `RPG_RT.lmt` and every `MapNNNN.lmu` sit at the project root, so [`core::filter_rm2k_maps`] finds the map files there, in place of [`core::filter_maps`].

### Reading one value out of a raw file

[`core::get_entity_values`] reads scalar values out of a raw data file (MV/MZ and the Marshal engines) without running the translation pipeline. The `path` segments lead to an object or array, and the `leaves` are the fields or indices read from there. Each leaf must resolve to a boolean, number or string. It returns one string per leaf, in order. RM2K isn't a JSON or Marshal file, so use the RM2K functions below instead.

```rust no_run
use rvpacker_txt_rs_lib::{EngineType, core::{PathSegment, get_entity_values}};
use std::fs::read;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let content = read("C:/Game/Data/System.json")?;

    // Walk to `variables`, then read its element at index 3.
    let values = get_entity_values(
        &content,
        EngineType::MVMZ,
        None,
        &[PathSegment::Key("variables")],
        &[PathSegment::Index(3)],
    )?;

    Ok(())
}
```

RM2K keeps its database in one binary `RPG_RT.ldb`, so there is no path to walk. [`core::get_rm2k_entity_name`] returns the name of one entry instead (actors and variables, from [`core::Rm2kEntity`]), decoded with the read encoding (UTF-8 if you pass `None`):

```rust no_run
use rvpacker_txt_rs_lib::core::{Rm2kEntity, get_rm2k_entity_name};
use std::fs::read;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let content = read("C:/Game/RPG_RT.ldb")?;

    // What `\V[3]` stands for.
    let name = get_rm2k_entity_name(&content, Rm2kEntity::Variables, 3, Some(encoding_rs::SHIFT_JIS))?;

    Ok(())
}
```

Each of these loads the database on every call. To look up several names, load it once with [`core::Rm2kDatabase::load`] and call its methods.

[`core::get_rm2k_system_graphic_name`] returns the system graphic (`System/<name>.png`) that the database selects, where `\C[n]` text colors come from:

```rust no_run
use rvpacker_txt_rs_lib::core::get_rm2k_system_graphic_name;
use std::fs::read;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let content = read("C:/Game/RPG_RT.ldb")?;
    let name = get_rm2k_system_graphic_name(&content, None)?;

    Ok(())
}
```

### `save` module

[`save`] reads save files of every supported engine, independent of translation. [`save::get_save_summary`] takes a save's raw bytes and its [`EngineType`], and returns a normalized [`save::SaveSummary`]:

- MV/MZ - the LZString-compressed `.rpgsave` (pass the file as is).
- XP/VX/VX Ace - several `Marshal.dump` streams in one file, located by class name.
- RM2K/RM2K3 - the LCF `.lsd`.

The summary covers playtime, map, gold, party, actors, items, weapons, armors, switches and variables. Fields an engine doesn't store stay at their default. RM2K keeps all items in `items`, and an actor's `name` is empty unless the game changed it, meaning "use the database name".

`read_encoding` decodes strings the same way [Text encoding](#text-encoding) describes, so pass the game's codepage for a legacy save with non-ASCII names. MV/MZ saves are JSON and ignore it.

```rust no_run
use rvpacker_txt_rs_lib::{EngineType, save::get_save_summary};
use std::fs::read;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let content = read("C:/Game/save/file1.rvdata2")?;
    let summary = get_save_summary(&content, EngineType::VXAce, None)?;

    println!("{} gold on map {}", summary.gold, summary.map_id);
    Ok(())
}
```

### `json` module

The `json` module converts RPG Maker data files to JSON and back, independent of the `.txt` workflow.

For XP/VX/VX Ace, [`json::generate`] converts `.rxdata`/`.rvdata`/`.rvdata2` files to JSON and [`json::write`] converts them back. `Scripts.ext` generates as a `.rb` file with the decompiled Ruby source instead of JSON. Existing output is skipped unless `force` is set. RM2K has [`json::generate_rm2k`] and [`json::write_rm2k`] for an RM2K folder; `write_rm2k` takes the build from `RPG_RT.ldb.json` and writes RPG Maker 2000 files, logging a warning, if that file is missing.

```rust no_run
use rvpacker_txt_rs_lib::{json::{generate, write}, EngineType, Error};

fn main() -> Result<(), Error> {
    generate("C:/Game/Data", "C:/Game/json", false, None)?;
    write("C:/Game/json", "C:/Game/json-output", EngineType::VXAce, None)?;
    Ok(())
}
```

For one file at a time, without touching the filesystem, use the `*_file` variants: [`json::generate_file`] and [`json::write_file`] for XP/VX/VX Ace, and the `*_rm2k_*_file` functions for RM2K. A save file is generated as an array of documents, which is written back as the same `Marshal.dump` streams. RM2K saves can be converted to JSON with [`json::generate_rm2k_save_file`], but there is no way back.

## Translation file format

A `.txt` translation file has three kinds of lines:

1. **Comment** - starts with the comment prefix (`<!>` by default). Leave it as-is.
2. **Translatable comment** - the exception: `{COMMENT_PREFIX}IN-GAME DISPLAYED NAME: Source{LINE_SEPARATOR}Translation`. It names something the player sees, so it is translated.
3. **Translatable line** - `Source{LINE_SEPARATOR}Translation`. Everything before the first separator is source; everything after is translation.

A line can carry several translations: `Source{LINE_SEPARATOR}Translation 1{LINE_SEPARATOR}Translation 2`. On write, the last non-empty one is used.

Line breaks in the source are normalized to a marker (`\#` by default). Use the same marker in translations rather than pressing Enter.

How the files are read:

- Lines before the first `{COMMENT_PREFIX}ID` line belong to no section and are ignored, with a warning.
- A section id that appears twice fails the read, and an unreadable id line fails it too.
- A translatable line with no separator is skipped with a warning. On write, a line with an empty translation is left untranslated.
- A UTF-8 byte order mark at the start of the file is ignored.
- Other comments you add are kept on append, with the entry that follows them.
- The `{COMMENT_PREFIX}EVENT ID`, `EVENT NAME` and `EVENT POS` lines appear only with `map_events`, and are regenerated on every read.

Sample `maps.txt` with the default separator, break marker and prefix:

```txt
<!>ID<#>2
<!>NAME<#>City
<!>ORDER<#>157
<!>IN-GAME DISPLAYED NAME: City<#>Translated City
This is sample single-line text<#>This is translated sample single-line text
This is sample\#multiline text<#>This is translated sample\#multiline text
Multiple translations<#>The first one<#>The second one
```

With `DuplicateMode::Remove`, a line that repeats across `{COMMENT_PREFIX}ID` sections is kept only at its first appearance. Translations are still written back to every occurrence. This doesn't apply to system, scripts, plugin or RM2K terms files.

### Customizing the format

The comment prefix, line separator and line break marker are per-project. Set them once at startup, before processing any file:

| Setting           | Default | Setter                 |
| ----------------- | ------- | ---------------------- |
| Comment prefix    | `<!>`   | [`set_comment_prefix`] |
| Line separator    | `<#>`   | [`set_line_separator`] |
| Line break marker | `\#`    | [`set_line_break`]     |

These are process-global, not per-`Processor`/`Base`. Don't change them mid-run. Apply the same values on every run against a project, since files written under one set won't parse under another.

## `.rvpacker-ignore`

Lines listed in a `.rvpacker-ignore` file are skipped on read. The file lives in the translation directory, next to the `.txt` files. Set [`BaseFlags::Ignore`] to apply it. A missing file is an error in that case. Set [`BaseFlags::CreateIgnore`] on purge to collect untranslated entries into one. An existing file is loaded first and rewritten with the new entries added, and sections left empty are dropped. A missing file is fine with `CreateIgnore` alone. It has no effect outside a purge.

[`BaseFlags::SkipObsolete`] only matters on an append read: it drops translation entries, and whole sections, that are no longer in the game files, instead of keeping them at the end of their section.

The file is a sequence of sections. A header names the target file (and, unless duplicates are removed, the entry id); every line until the next header is an entry:

```text
<!>Ignore Entry<#>Items: 1
Torch
Silver shilling
<!>Glob<#>*soul
<!>Ignore Entry<#>Weapons: 1
makeshift2
```

A plain line matches exactly. A line written `<!>Glob<#>pattern` matches as a shell-style pattern: `*` for any run of characters, `?` for one character, everything else literal.

With `DuplicateMode::Remove` the `: id` suffix is ignored on read, and the section applies to the whole file (system, scripts and plugin sections always keep their id). With `DuplicateMode::Allow` the id must match the entry's id. A purge always writes the id, so the same file works under either mode. RM2K headers use the RM2K kind names, such as `Rm2kItems: 1`. There are no comments. Blank lines and lines before the first header are dropped; every other line that isn't a header is an entry.

An ignore file can only deny, so an allowlist (translate only these names) has no equivalent.

[`examples/.rvpacker-ignore`](https://github.com/RPG-Maker-Translation-Tools/rvpacker-txt-rs-lib/blob/main/examples/.rvpacker-ignore) is a worked example.

## Hashing

[`content_hash`] hashes a file's bytes, seeded by the duplicate mode. It always uses rapidhash, so the result is the same on every CPU and whether or not `gxhash` is enabled. [`Processor::hashes`] stores these values between runs, which is why they must stay stable.

## Serialization (converters)

The [`serde`](crate::serde) module converts a translation `.txt` file to and from other formats. Each `export_*` takes the `.txt` content and returns the format, and each `import_*` takes the format and returns `.txt` content:

- `export_json`/`import_json` - always available. An array of entries, each tagged `comment` or `translation`.
- `export_csv`/`import_csv` - `serde-csv`. One row per entry, with the translation count and then each translation as a column. A text cell a spreadsheet would read as a formula (starting with `=`, `+`, `-`, `@`) gets a `'` in front, which the import removes, so opening the CSV in Excel can't mangle or run it.
- `export_xlsx`/`import_xlsx` - `serde-xlsx`. The CSV layout, on sheet 1, with every cell stored as text, so none needs the `'`. Export returns the workbook's bytes.
- `export_xml`/`import_xml` - `serde-xml`. An `<entries>` document.
- `export_yaml`/`import_yaml` - `serde-yaml`. The JSON structure in YAML.

Each function's docs give the exact schema. Blank lines are not kept: an exported and re-imported file comes back without them.

## Tests

Tests use default RPG Maker projects for XP, VX, VX Ace, MV and MZ. The RM2K tests use [OFF English Translation 3.0](https://www.mediafire.com/file/sx1kyo89yu0dko1/OFF_English_3.0.zip/file).

## Support

[Me](https://github.com/savannstm), the maintainer of this project, is a poor college student from Eastern Europe.

If you could, please consider supporting us through:

- [Boosty](https://boosty.to/mcdeimos) (has lower fees)
- [Ko-fi](https://ko-fi.com/savannstm)

Even if you don't, we will still continue working on the project anyways.

## License

Licensed under [WTFPL](https://www.wtfpl.net/).
