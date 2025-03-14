use rvpacker_txt_rs_lib::{purge::MapPurger, types::EngineType};

fn main() {
    let purger = MapPurger::new("data", "translation", EngineType::New).logging(true);

    purger.purge(None, None);
}
