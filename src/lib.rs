/*!
# rvpacker-txt-rs-lib

`rvpacker-txt-rs-lib` is a library for [rvpacker-txt-rs](https://github.com/savannstm/rvpacker-txt-rs) CLI, that provides the function to extract the text from RPG Maker `.rxdata`, `.rvdata`, `.rvdata2` and `.json` files to `.txt` format.

It also provides the `json` module to convert `.rxdata`, `.rvdata` and `.rvdata2` files to JSON and back.

## Installation

`cargo add rvpacker-txt-rs-lib`

## Features

This crate provides core structs and functions in `core` module, but also exports wrappers around those, like `Purger`, `Writer`, `Reader`.

### `Reader`/`Writer`/`Purger`

These structs abstract over the `core` module and process files, handling all system calls.

#### Example

```no_run
use rvpacker_txt_rs_lib::{Reader, Writer, Purger, Error, FileFlags, EngineType};

fn main() -> Result<(), Error> {
    let mut reader = Reader::new();
    reader.set_flags(FileFlags::Map | FileFlags::Other);
    reader.read("C:/Game/Data", "C:/Game/translation", EngineType::VXAce)?;

    let mut writer = Writer::new();
    writer.set_flags(FileFlags::Map | FileFlags::Other);
    writer.write("C:/Game/Data", "C:/Game/translation", "C:/Game/output", EngineType::VXAce)?;

    let mut purger = Purger::new();
    purger.set_flags(FileFlags::Map | FileFlags::Other);
    purger.purge("C:/Game/Data", "C:/Game/translation", EngineType::VXAce)?;
    Ok(())
}
```

### `core` module

This module provides structs `Base`, `MapBase`, `OtherBase`, `SystemBase`, `PluginBase` and `ScriptBase`.

#### Example

```no_run
use rvpacker_txt_rs_lib::{core::{Base, MapBase}, Mode, EngineType, ReadMode, Error};
use std::fs::read;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut base = Base::new(Mode::Read, EngineType::VXAce);
    base.read_mode = ReadMode::Force;

    let mut map_base = MapBase::new(&mut base);

    let mapinfos = read("C:/Game/Data/Mapinfos.rvdata2")?;
    map_base.initialize_mapinfos(&mapinfos)?;

    let map_file_content = read("C:/Game/Data/Map001.rvdata2")?;
    map_base.process("Map001.rvdata2", &map_file_content)?;

    // To get the translation, you must use `translation` after processing all the maps.
    let translation_data = map_base.translation();

    Ok(())
}
```

### `json` module

`json` module provides `generate` and `write` functions to generate JSON representations of older engines' files and write them back respectively.

#### Example

```no_run
use rvpacker_txt_rs_lib::{json::{generate, write}, EngineType, Error};

fn main() -> Result<(), Error> {
    generate("C:/Game/Data", "C:/Game/json", false)?;
    write("C:/Game/json", "C:/Game/json-output", EngineType::VXAce)?;
    Ok(())
}
```

## License

Project is licensed under WTFPL.
*/
#![allow(clippy::collapsible_else_if)]
#![deny(clippy::use_self)]
#![deny(clippy::redundant_clone)]
#![deny(clippy::branches_sharing_code)]

mod processors;

pub mod constants;
pub mod core;
pub mod json;
pub mod types;

pub use constants::{
    NEW_LINE, RVPACKER_IGNORE_FILE, RVPACKER_METADATA_FILE, SEPARATOR,
};
pub use core::{
    filter_maps, filter_other, get_engine_extension, get_ini_title,
    get_system_title, parse_ignore,
};
pub use processors::{
    Purger, PurgerBuilder, Reader, ReaderBuilder, Writer, WriterBuilder,
};
pub use types::*;
