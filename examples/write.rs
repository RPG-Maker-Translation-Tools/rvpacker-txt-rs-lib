use rvpacker_txt_rs_lib::{EngineType, Error, FileFlags, Mode, Processor};
use std::path::Path;

fn main() -> Result<(), Error> {
    let mut processor = Processor {
        mode: Mode::Write,
        file_flags: FileFlags::Map | FileFlags::other(),
        ..Default::default()
    };

    processor.process(
        EngineType::New,
        "C:/Game/www/data",
        "C:/Game/translation",
        Some(Path::new("C:/Game/output")),
    )?;
    Ok(())
}
