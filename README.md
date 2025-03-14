# rvpacker-txt-rs-lib

Library providing functions for [rvpacker-txt-rs](https://github.com/savannstm/rvpacker-txt-rs).

Can be used in other projects, although it's specifically written for `rvpacker-txt-rs`.

## Installation

`cargo add rvpacker-txt-rs-lib`

## Examples

```rust
use rvpacker_txt_rs_lib::{read::MapReader, purge::MapPurger, write::MapWriter, types::EngineType};

fn main() {
    // Read
    let reader = MapReader::new("data", "translation", EngineType::New).logging(true);
    reader.read();

    // Purge (just for example)
    let purger = MapPurger::new("data", "translation", EngineType::New).logging(true);
    purger.purge(None, None);

    // Write
    let writer = MapWriter::new("data", "translation", "output/data", EngineType::New).logging(true);
    writer.write();
}
```

## License

Project is licensed under WTFPL.
