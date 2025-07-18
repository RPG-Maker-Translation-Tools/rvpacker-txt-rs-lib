use rvpacker_txt_rs_lib::{
    types::{EngineType, FileFlags},
    write::WriterBuilder,
};

fn main() {
    let writer = WriterBuilder::new()
        .with_flags(FileFlags::Map | FileFlags::Other)
        .logging(true)
        .build();

    let result =
        writer.write("data", "translation", "output/data", EngineType::New);
}
