use rvpacker_txt_rs_lib::{EngineType, Error, FileFlags, Mode, Processor};

fn main() -> Result<(), Error> {
    let mut processor = Processor {
        mode: Mode::Purge,
        file_flags: FileFlags::Map | FileFlags::other(),
        ..Default::default()
    };

    processor.process(
        EngineType::New,
        "C:/Game/www/data",
        "C:/Game/translation",
        None,
    )?;
    Ok(())
}
