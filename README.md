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
use rvpacker_txt_rs_lib::{EngineType, Error, FileFlags, Mode, Processor, ReadMode};
use std::path::Path;

fn main() -> Result<(), Error> {
    let mut processor = Processor {
        mode: Mode::Read(ReadMode::Default { force: false }),
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
use rvpacker_txt_rs_lib::{core::Base, Mode, EngineType, ReadMode};
use std::fs::read;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut base = Base::new(Mode::Read(ReadMode::Default { force: true }), EngineType::VXAce);
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
