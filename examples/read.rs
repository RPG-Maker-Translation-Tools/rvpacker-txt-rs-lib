use rvpacker_txt_rs_lib::{read::MapReader, types::EngineType};

fn main() {
    let reader = MapReader::new("data", "translation", EngineType::New).logging(true);

    reader.read();
}
