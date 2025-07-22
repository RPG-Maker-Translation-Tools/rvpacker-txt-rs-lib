use rvpacker_txt_rs_lib::{EngineType, Error, FileFlags, ReaderBuilder};

fn main() -> Result<(), Error> {
    let mut reader = ReaderBuilder::new()
        .with_flags(FileFlags::Map | FileFlags::Other)
        .build();

    reader.read("C:/Game/www/data", "C:/Game/translation", EngineType::New)?;
    Ok(())
}
