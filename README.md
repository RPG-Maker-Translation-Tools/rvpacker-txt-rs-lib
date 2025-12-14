# rvpacker-txt-rs-lib

`rvpacker-txt-rs-lib` that provides functions to extract the text from RPG Maker `.rxdata`, `.rvdata`, `.rvdata2` and `.json` files to `.txt` format for easy translation.

It also provides the `json` module to convert `.rxdata`, `.rvdata` and `.rvdata2` files to JSON and back.

This library is used in [RPGMTranslate GUI](https://github.com/rpg-maker-translation-tools/rpgmtranslate) and [rvpacker-txt-rs CLI](https://github.com/rpg-maker-translation-tools/rvpacker-txt-rs).

## Installation

`cargo add rvpacker-txt-rs-lib`

## Features

This crate provides core structs and functions in `core` module, but also exports wrappers around those, like `Purger`, `Writer`, `Reader`.

### `Reader`/`Writer`/`Purger`

These structs abstract over the `core` module and process files, handling all system calls.

#### Example

```rust no_run
use rvpacker_txt_rs_lib::{Reader, Writer, Purger, Error, FileFlags, EngineType};

fn main() -> Result<(), Error> {
    let mut reader = Reader::new();
    reader.set_files(FileFlags::Map | FileFlags::other());
    reader.read("C:/Game/Data", "C:/Game/translation", EngineType::VXAce)?;

    let mut writer = Writer::new();
    writer.set_files(FileFlags::Map | FileFlags::other());
    writer.write("C:/Game/Data", "C:/Game/translation", "C:/Game/output", EngineType::VXAce)?;

    let mut purger = Purger::new();
    purger.set_files(FileFlags::Map | FileFlags::other());
    purger.purge("C:/Game/Data", "C:/Game/translation", EngineType::VXAce)?;
    Ok(())
}
```

### `core` module

This module provides structs `Base`, `MapBase`, `OtherBase`, `SystemBase`, `PluginBase` and `ScriptBase`.

#### Example

```rust no_run
use rvpacker_txt_rs_lib::{core::{Base, MapBase}, Mode, EngineType, ReadMode};
use std::fs::read;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut base = Base::new(Mode::Read(ReadMode::Default(true)), EngineType::VXAce);
    let mut map_base = MapBase::new(&mut base);

    let mapinfos = read("C:/Game/Data/Mapinfos.rvdata2")?;
    let map_file_content = read("C:/Game/Data/Map001.rvdata2")?;
    map_base.process("Map001.rvdata2", &map_file_content, &mapinfos, None)?;

    // To get the translation, you must use `translation` after processing all the maps.
    let translation_data = map_base.translation();

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

-   [Ko-fi](https://ko-fi.com/savannstm)
-   [Patreon](https://www.patreon.com/cw/savannstm)
-   [Boosty](https://boosty.to/mcdeimos)

Even if you don't, it's fine. We'll continue to do as we right now.

## License

Project is licensed under WTFPL.
