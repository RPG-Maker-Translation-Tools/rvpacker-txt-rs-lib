use rvpacker_txt_rs_lib::{EngineType, Error, FileFlags, PurgerBuilder};

fn main() -> Result<(), Error> {
    let mut purger = PurgerBuilder::new()
        .with_flags(FileFlags::Map | FileFlags::Other)
        .build();

    purger.purge("C:/Game/www/data", "C:/Game/translation", EngineType::New)?;
    Ok(())
}
