# rvpacker-txt-rs-lib

A Rust library for extracting the translatable text out of RPG Maker XP/VX/VX Ace/MV/MZ data files (`.rxdata`, `.rvdata`, `.rvdata2`, `.json`) into plain `.txt` files, and writing translated `.txt` files back into the game's original format.

It also ships a `json` module that converts the older engines' binary data files to and from JSON, independent of the translation workflow.

This library backs [RPGMTranslate GUI](https://github.com/RPG-Maker-Translation-Tools/rpgmtranslate-qt) and [rvpacker-txt-rs CLI](https://github.com/RPG-Maker-Translation-Tools/rvpacker-txt-rs); either is a more practical starting point if you just want to translate a game. Reach for this crate directly when you're building your own tool on top of it.

## Installation

```bash
cargo add rvpacker-txt-rs-lib
```

## Quick start

[`Processor`](#processor---the-high-level-api) is the entry point for most uses: configure the fields you care about, then call [`Processor::process`] once per operation.

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

[`EngineType`] tells the library which binary format and JSON field naming to expect: `MVMZ`, `VXAce`, `VX` and `XP`. Nothing is auto-detected - the caller supplies it, typically by checking which of `System.json`/`System.rvdata2`/`System.rvdata`/`System.rxdata` exists.

### File kinds

[`FileFlags`] is a bitflag selecting which RPG Maker data files get processed: `Map` (`Mapxxx.ext`), `System`, `Scripts` (`Scripts.ext`/`plugins.js`, depending on engine), and [`FileFlags::other()`] - everything else (`Actors`, `Armors`, `Classes`, `CommonEvents`, `Enemies`, `Items`, `Skills`, `States`, `Troops`, `Weapons`). `FileFlags::default()` is `FileFlags::all()`.

### Modes

[`Mode`] picks the operation:

- `Mode::Read { append, force }` - extract text into `.txt` files. `append` keeps existing translations and adds anything new since the last read (for a game that received a content update); without it, an existing translation file is left untouched. `force` overwrites files that already exist and skips the hash check that would otherwise leave unchanged files alone.
- `Mode::Write` - write translated `.txt` files back into the game's original format.
- `Mode::Purge` - drop translation entries that were never filled in.

### Duplicate handling

[`DuplicateMode`] controls what happens when the same source line appears more than once, and only affects map and "other" files:

- `Allow` (default) - each map/event gets its own entry, so text that happens to repeat can still get different translations depending on context.
- `Remove` - every occurrence of a line shares one entry and one translation. Cuts down on repetition in the `.txt` files at the cost of losing per-context translations; the translation is still written back everywhere the source line occurs.

## API

### `Processor` - the high-level API

[`Processor`] wraps the `core` module and handles every system call - reading source files, calling into `core`, and writing `.txt`/game files back out. It's a plain struct: set the fields you need, `..Default::default()` the rest, and call [`Processor::process`]. See [Quick start](#quick-start) for a full example.

Key fields:

| Field | Purpose |
| --- | --- |
| `mode` | Which operation to run - see [Modes](#modes). |
| `file_flags` | Which files to process - see [File kinds](#file-kinds). |
| `flags` | [`BaseFlags`] - `.rvpacker-ignore` handling and skipping obsolete entries. |
| `duplicate_mode` | See [Duplicate handling](#duplicate-handling). Must match what was used on read when writing or purging. |
| `game_title` | Overrides the title extracted from the system file - needed for XP/VX/VX Ace, whose title may only live in `Game.ini`, not necessarily as UTF-8. |
| `hashes` | Content hashes keyed by lowercased file stem, letting `Mode::Read` skip files unchanged since the last read. Persist [`Processor::hashes`] after a read and feed it back in next time. |
| `skip_maps` / `skip_events` | Map ids, or per-file entry ids, to leave untouched on read. |
| `map_events` | Whether to record each event's id, name and coordinates ahead of its text. |

### `core` module - the low-level API

[`core::Base`] is what [`Processor`] is built on: one method per RPG Maker file kind - `process_map`, `process_other`, `process_system`, `process_scripts`, `process_plugins` - each taking raw file bytes and returning [`ProcessedData`] or translation data, with no filesystem access of its own. Reach for it when [`Processor`]'s all-files-at-once shape doesn't fit - a GUI processing one file per user action, for instance.

Maps are the one kind processed as a run rather than one shot, because every map shares a single translation file: call `begin_maps`, then `process_map` per file, then `finish_maps`.

```rust no_run
use rvpacker_txt_rs_lib::{core::Base, Mode, EngineType};
use std::fs::read;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut base = Base::new(Mode::Read { append: false, force: true }, EngineType::VXAce);
    base.begin_maps();

    let mapinfos = read("C:/Game/Data/MapInfos.rvdata2")?;
    let map_file_content = read("C:/Game/Data/Map001.rvdata2")?;
    base.process_map("Map001.rvdata2", &map_file_content, &mapinfos, None)?;

    // The translation is only available once every map has been processed.
    let translation_data = base.finish_maps();

    Ok(())
}
```

### `json` module

For the older engines (XP/VX/VX Ace), `json` provides [`json::generate`] and [`json::write`] to convert `.rxdata`/`.rvdata`/`.rvdata2` files to JSON and back - independent of the `.txt` translation workflow above. `Scripts.ext` is special-cased: it generates as a `.rb` file holding the decompiled, human-readable Ruby source of every script, rather than JSON.

```rust no_run
use rvpacker_txt_rs_lib::{json::{generate, write}, EngineType, Error};

fn main() -> Result<(), Error> {
    generate("C:/Game/Data", "C:/Game/json", false)?;
    write("C:/Game/json", "C:/Game/json-output", EngineType::VXAce)?;
    Ok(())
}
```

## Translation file format

A `.txt` translation file is made of three kinds of lines:

1. **Comment** - starts with the comment prefix (`<!>` by default). Untranslatable; leave it as-is and don't add new translations for it.
2. **Translatable comment** - the one exception: a `{COMMENT_PREFIX}IN-GAME DISPLAYED NAME: Source{LINE_SEPARATOR}Translation` line. It looks like a comment but names something the player actually sees, so it does need translating - and only in exactly that shape.
3. **Translatable line** - `Source{LINE_SEPARATOR}Translation`. Everything before the first separator is source text; everything after is translation.

A single line can carry more than one translation, separated the same way: `Source{LINE_SEPARATOR}Translation 1{LINE_SEPARATOR}Translation 2`, and so on.

To avoid line-break ambiguity inside a `.txt` file, every line break in the source text is normalized to a single marker (`\#` by default) rather than left as a literal newline. Translations should use that same marker instead of pressing `Enter`.

Here's a sample `maps.txt` with the default separator/break/prefix:

```txt
<!>ID<#>2
<!>NAME<#>City
<!>ORDER<#>157
<!>IN-GAME DISPLAYED NAME: City<#>Translated City
This is sample single-line text<#>This is translated sample single-line text
This is sample\#multiline text<#>This is translated sample\#multiline text
Multiple translations<#>The first one<#>The second one
```

`DuplicateMode::Remove` (see [Duplicate handling](#duplicate-handling)) deduplicates lines that repeat across multiple `{COMMENT_PREFIX}ID` sections, keeping only the first appearance in the `.txt` file - e.g. if "text" appears in both `map002` and `map003`, only `map002`'s copy is kept, though the translation is still written back to both on write. This does not apply to system, scripts or plugin files, since this behavior may possibly brerak them.

### Customizing the format

The comment prefix, line separator and line break marker are per-project values, not fixed constants - set once, before any processing, and they hold for every file the library touches afterward:

| Setting | Default | Setter | Getter |
| --- | --- | --- | --- |
| Comment prefix | `<!>` | [`set_comment_prefix`] | [`get_comment_prefix`] |
| Line separator | `<#>` | [`set_line_separator`] | [`get_line_separator`] |
| Line break marker | `\#` | [`set_line_break`] | [`get_line_break`] |

These are process-global, not per-`Processor`/`Base` instance - call the setters once at startup, before touching any file, and don't change them mid-run. A consumer that persists a project's settings (as [rvpacker-txt-rs CLI](https://github.com/RPG-Maker-Translation-Tools/rvpacker-txt-rs) does, in `.rvpacker-metadata`) should apply the same values on every subsequent run against that project - translation files written under one set of values will not parse correctly under another.

## `.rvpacker-ignore`

Lines listed in a `.rvpacker-ignore` file are skipped when reading. Set [`BaseFlags::Ignore`] to apply the file, or [`BaseFlags::CreateIgnore`] on a purge to collect untranslated entries into one instead of dropping them.

The file is a sequence of sections. A section header names the target file (and, unless duplicates are removed, the entry id); every line until the next header is an entry:

```text
<!>Ignore Entry<#>Items: 1
Torch
Silver shilling
<!>Glob<#>*soul
<!>Ignore Entry<#>Weapons: 1
makeshift2
```

A plain line matches exactly. A line written `<!>Glob<#>pattern` matches as a shell-style pattern - `*` for any run of characters, `?` for exactly one, everything else literal - for text only recognisable by shape, like a shared prefix or suffix, rather than a fixed string.

With `DuplicateMode::Remove` the `: id` suffix is ignored and a section applies to the whole file, so the id you write is arbitrary; with `DuplicateMode::Allow` it must match the entry's actual id. There is no comment syntax - any line that isn't a section header is an entry.

[`examples/.rvpacker-ignore`](examples/.rvpacker-ignore) is a worked example, covering unused items, classes, enemies, armors and weapons - filtering that used to be hardcoded in this crate. Note that not every rule can be expressed this way: an *allowlist* (translate only these names, drop every other actor) has no equivalent here, since an ignore file can only deny. Leaving it out costs nothing but a few extra entries to skip past.

## Serialization

Every public enum and struct in this crate implements `serde`'s `Serialize`/`Deserialize`. Flat enums with only number variants (and [`FileFlags`]/[`BaseFlags`]) use `#[serde(into = "u8", try_from = "u8")]` (or `u16` for `FileFlags`), serializing as a single integer rather than a tagged representation.

## Support

[Me](https://github.com/savannstm), the maintainer of this project, is a poor college student from Eastern Europe.

If you could, please consider supporting us through:

- [Ko-fi](https://ko-fi.com/savannstm)
- [Patreon](https://www.patreon.com/cw/savannstm)
- [Boosty](https://boosty.to/mcdeimos)

Even if you don't, it's fine. We'll continue to do as we right now.

## License

Project is licensed under WTFPL.
