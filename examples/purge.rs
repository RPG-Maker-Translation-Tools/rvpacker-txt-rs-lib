use rvpacker_txt_rs_lib::{
    purge::PurgerBuilder,
    types::{EngineType, FileFlags},
};

fn main() {
    let purger = PurgerBuilder::new()
        .with_flags(FileFlags::Map | FileFlags::Other)
        .logging(true)
        .build();

    purger.purge("data", "translation", EngineType::New);
}
