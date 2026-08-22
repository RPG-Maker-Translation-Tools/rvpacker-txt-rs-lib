# rvpacker-txt-rs-lib

`rvpacker-txt-rs-lib` that provides functions to extract the text from RPG Maker `.rxdata`, `.rvdata`, `.rvdata2` and `.json` files to `.txt` format for easy translation.

It also provides the `json` module to convert `.rxdata`, `.rvdata` and `.rvdata2` files to JSON and back.

This library is used in [RPGMTranslate GUI](https://github.com/RPG-Maker-Translation-Tools/rpgmtranslate-qt) and [rvpacker-txt-rs CLI](https://github.com/RPG-Maker-Translation-Tools/rvpacker-txt-rs).

## Installation

`cargo add rvpacker-txt-rs-lib`

## Features

This crate provides core structs and functions in `core` module, but also exports `Processor`, a wrapper around those.

### `Processor`

`Processor` abstracts over the `core` module and processes files, handling all system calls. It's a plain struct with public fields: set the ones you need, and `mode` decides whether it reads, writes or purges.

#### Example

```rust no_run
use rvpacker_txt_rs_lib::{EngineType, Error, FileFlags, Mode, Processor};
use std::path::Path;

fn main() -> Result<(), Error> {
    let mut processor = Processor {
        mode: Mode::read(),
        file_flags: FileFlags::Map | FileFlags::other(),
        ..Default::default()
    };

    processor.process(EngineType::VXAce, "C:/Game/Data", "C:/Game/translation", None)?;

    processor.mode = Mode::Write;
    processor.process(
        EngineType::VXAce,
        "C:/Game/Data",
        "C:/Game/translation",
        Some(Path::new("C:/Game/output")),
    )?;

    processor.mode = Mode::Purge;
    processor.process(EngineType::VXAce, "C:/Game/Data", "C:/Game/translation", None)?;
    Ok(())
}
```

### `core` module

This module provides the `Base` struct, which exposes one method per RPG Maker file kind: `process_map`, `process_other`, `process_system`, `process_scripts` and `process_plugins`.

Maps are the one kind processed as a run rather than one shot, because they all share a single translation file: call `begin_maps`, then `process_map` per file, then `finish_maps`.

#### Example

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

`json` module provides `generate` and `write` functions to generate JSON representations of older engines' files and write them back respectively.

#### Example

```rust no_run
use rvpacker_txt_rs_lib::{json::{generate, write}, EngineType, Error};

fn main() -> Result<(), Error> {
    generate("C:/Game/Data", "C:/Game/json", false)?;
    write("C:/Game/json", "C:/Game/json-output", EngineType::VXAce)?;
    Ok(())
}
```

### `.rvpacker-ignore`

Lines listed in `.rvpacker-ignore` are skipped when reading. Set `BaseFlags::Ignore` to apply the file, or `BaseFlags::CreateIgnore` on a purge to have untranslated entries collected into one.

The file is a sequence of sections. A section header names the file (and, unless duplicates are removed, the entry id); every following line until the next header is an entry:

```text
<!>Ignore Entry<#>Items: 1
Torch
Silver shilling
<!>Glob<#>*soul
<!>Ignore Entry<#>Weapons: 1
makeshift2
```

A plain line matches exactly. A line written `<!>Glob<#>pattern` matches as a shell-style pattern, where `*` stands for any run of characters and `?` for exactly one; everything else is literal. Globs exist for text that can only be recognised by shape — a shared prefix or suffix — rather than by a fixed string.

With `DuplicateMode::Remove` the `: id` suffix is ignored and a section applies to its whole file, so the id you write is arbitrary. With `DuplicateMode::Allow` it must match the entry.

There is no comment syntax: any line that is not a section header is an entry.

`examples/.rvpacker-ignore` is a worked example, covering the unused items, classes, enemies, armors and weapons of Fear & Hunger 2: Termina — filtering that used to be hardcoded in this crate. Note that one of the original rules cannot be expressed here: Termina's actor filter was an *allowlist* (translate these eighteen names, drop every other actor), and an ignore file can only deny. Leaving it out costs nothing but a few extra entries to skip past.

### Serialization/Deserialization

All public enums and structs in this crate are serializable with `serde`.

Flat enums that contain only number variants are serialized with `#[serde(into = "u8", try_from = "u8")]` attribute, which converts enums to a single u8 integer, representing the variant. The same applies to `FileFlags` struct.

## Support

[Me](https://github.com/savannstm), the maintainer of this project, is a poor college student from Eastern Europe.

If you could, please consider supporting us through:

- [Ko-fi](https://ko-fi.com/savannstm)
- [Patreon](https://www.patreon.com/cw/savannstm)
- [Boosty](https://boosty.to/mcdeimos)

Even if you don't, it's fine. We'll continue to do as we right now.

## License

Project is licensed under WTFPL.
