use rvpacker_txt_rs_lib::{types::EngineType, write::MapWriter};

fn main() {
    let writer = MapWriter::new("data", "translation", "output/data", EngineType::New).logging(true);

    writer.write();
}
