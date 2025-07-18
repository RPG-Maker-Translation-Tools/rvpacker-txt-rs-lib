use rvpacker_txt_rs_lib::{
    read::ReaderBuilder,
    types::{EngineType, FileFlags},
};

fn main() {
    let reader = ReaderBuilder::new()
        .with_flags(FileFlags::Map | FileFlags::Other)
        .logging(true)
        .build();

    let result = reader.read("data", "translation", EngineType::New);
}
