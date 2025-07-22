use rvpacker_txt_rs_lib::{EngineType, Error, FileFlags, WriterBuilder};

fn main() -> Result<(), Error> {
    let mut writer = WriterBuilder::new()
        .with_flags(FileFlags::Map | FileFlags::Other)
        .build();

    writer.write(
        "C:/Game/www/data",
        "C:/Game/translation",
        "C:/Game/output",
        EngineType::New,
    )?;
    Ok(())
}
