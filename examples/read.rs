use rvpacker_txt_rs_lib::{
    EngineType, Error, FileFlags, Mode, Processor, ReadMode,
};

fn main() -> Result<(), Error> {
    let mut processor = Processor {
        mode: Mode::Read(ReadMode::Default { force: false }),
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
