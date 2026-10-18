//! `Processor` promises the same output on any number of threads. Maps are loaded in chunks of
//! `current_num_threads()`, so these use more maps than threads and a line shared between maps,
//! which makes the chunk boundaries and the duplicate removal order visible.

use rvpacker_txt_rs_lib::{EngineType, FileFlags, Mode, Processor};
use std::{
    fs::{create_dir_all, read_to_string, remove_dir_all, write},
    path::{Path, PathBuf},
};

const MAP_COUNT: usize = 9;

fn scratch(tag: &str) -> PathBuf {
    let dir = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("target")
        .join("threads")
        .join(tag);
    let _ = remove_dir_all(&dir);
    create_dir_all(&dir).unwrap();
    dir
}

fn project(root: &Path) -> PathBuf {
    let data = root.join("data");
    create_dir_all(&data).unwrap();

    let mut infos = String::from("[null");

    for id in 1..=MAP_COUNT {
        infos.push_str(&format!(
            r#",{{"id":{id},"name":"Map {id}","order":{id},"parentId":0}}"#
        ));

        // "Shared line." repeats in every map; the other line is unique to it.
        let map = format!(
            r#"{{"displayName":"Name {id}","events":[null,{{"id":1,"name":"EV","x":1,"y":2,"pages":[{{"list":[
{{"code":401,"parameters":["Shared line."]}},{{"code":401,"parameters":["Only in map {id}."]}},{{"code":0,"parameters":[]}}]}}]}}]}}"#
        );

        write(data.join(format!("Map{id:03}.json")), map).unwrap();
    }

    infos.push(']');
    write(data.join("MapInfos.json"), infos).unwrap();
    data
}

fn read_maps(threads: usize, tag: &str) -> String {
    let root = scratch(tag);
    let source = project(&root);
    let translation = root.join("translation");

    let pool = rayon::ThreadPoolBuilder::new().num_threads(threads).build().unwrap();

    pool.install(|| {
        Processor {
            mode: Mode::Read {
                append: false,
                force: true,
            },
            file_flags: FileFlags::Map,
            ..Default::default()
        }
        .process(EngineType::MVMZ, &source, &translation, None)
        .unwrap();
    });

    read_to_string(translation.join("maps.txt")).unwrap()
}

#[test]
fn reading_maps_gives_the_same_output_on_any_thread_count() {
    let single = read_maps(1, "one");

    assert!(single.contains("Only in map 9."));
    assert_eq!(single, read_maps(4, "four"));
    assert_eq!(single, read_maps(MAP_COUNT + 3, "many"));
}

/// Reads, then writes back unchanged, the RM2K fixture's maps. Returns `maps.txt` and every written map's bytes.
fn rm2k_maps(threads: usize, tag: &str) -> (String, Vec<(String, Vec<u8>)>) {
    let root = scratch(tag);
    let source = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/RM2K");
    let translation = root.join("translation");
    let output = root.join("output");

    let pool = rayon::ThreadPoolBuilder::new().num_threads(threads).build().unwrap();

    let run = |mode| {
        pool.install(|| {
            Processor {
                mode,
                file_flags: FileFlags::Map,
                map_events: true,
                read_encoding: Some(encoding_rs::WINDOWS_1252),
                ..Default::default()
            }
            .process(EngineType::RM2K, &source, &translation, Some(&output))
            .unwrap();
        });
    };

    run(Mode::Read {
        append: false,
        force: true,
    });

    // Untranslated maps are not written back, so every entry gets a translation.
    let maps_file = translation.join("maps.txt");
    let filled: String = read_to_string(&maps_file)
        .unwrap()
        .lines()
        .map(|line| {
            if line.ends_with("<#>") && !line.starts_with("<!>") {
                format!("{line}translated\n")
            } else {
                format!("{line}\n")
            }
        })
        .collect();
    std::fs::write(&maps_file, filled).unwrap();

    run(Mode::Write);

    let mut written: Vec<_> = std::fs::read_dir(&output)
        .unwrap()
        .map(|entry| {
            let path = entry.unwrap().path();
            (
                path.file_name().unwrap().to_string_lossy().into_owned(),
                std::fs::read(&path).unwrap(),
            )
        })
        .collect();
    written.sort();

    (read_to_string(translation.join("maps.txt")).unwrap(), written)
}

#[test]
fn rm2k_maps_give_the_same_output_on_any_thread_count() {
    let single = rm2k_maps(1, "rm2k-one");

    assert!(!single.1.is_empty());
    assert!(single == rm2k_maps(4, "rm2k-four"));
}
