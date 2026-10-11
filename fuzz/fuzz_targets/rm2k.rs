#![no_main]
use libfuzzer_sys::fuzz_target;
use rm2k::engine::Engine;
use rvpacker_txt_rs_lib::json::{
    generate_rm2k_database_file, generate_rm2k_map_file, generate_rm2k_save_file, generate_rm2k_tree_map_file,
    write_rm2k_database_file, write_rm2k_map_file, write_rm2k_tree_map_file,
};

fuzz_target!(|input: &[u8]| {
    let Some((&selector, file)) = input.split_first() else {
        return;
    };
    let engine = if selector & 8 == 0 { Engine::R2K } else { Engine::R2K3 };

    match selector & 3 {
        0 => {
            if let Ok(json) = generate_rm2k_database_file(file) {
                write_rm2k_database_file(&json).expect("own JSON must convert back");
            }
        }
        1 => {
            if let Ok(json) = generate_rm2k_map_file(file) {
                write_rm2k_map_file(&json, engine).expect("own JSON must convert back");
            }
        }
        2 => {
            if let Ok(json) = generate_rm2k_tree_map_file(file) {
                write_rm2k_tree_map_file(&json, engine).expect("own JSON must convert back");
            }
        }
        _ => {
            let _ = generate_rm2k_save_file(file);
        }
    }
});
