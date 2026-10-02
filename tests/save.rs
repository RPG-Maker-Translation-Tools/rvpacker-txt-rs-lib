use rvpacker_txt_rs_lib::{
    EngineType,
    save::{dump, get_save_summary, load},
};

/// Output of `LZString.compressToBase64` (lz-string 1.5.0), the reference implementation MV/MZ use.
const REFERENCE: &[(&str, &str)] = &[
    ("", "Q==="),
    ("a", "IZA="),
    ("hello", "BYUwNmD2Q==="),
    ("hello hello hello hello", "BYUwNmD2AEoTcq3FIA=="),
    (
        "Привет, мир! 日本語 😀",
        "vgggEQQcIITCCKwghCIANAAkDwgYQEIGFPTQNOaDyohgvBuAAe0A",
    ),
];

#[test]
fn dump_matches_lz_string() {
    for &(plain, compressed) in REFERENCE {
        assert_eq!(dump(plain), compressed, "dump({plain:?})");
    }
}

#[test]
fn load_matches_lz_string() {
    for &(plain, compressed) in REFERENCE {
        assert_eq!(load(compressed).as_deref(), Some(plain), "load({compressed:?})");
    }
}

#[test]
fn lz_string_round_trips_large_input() {
    let repetitive = r#"{"a":1,"b":[1,2,3,null,"x"]}"#.repeat(5000);
    let zeros = "0".repeat(100_000);
    let mixed: String = (0u32..20_000)
        .map(|i| char::from_u32((i * 7919) % 0xD000).unwrap_or('?'))
        .collect();

    for plain in [&repetitive, &zeros, &mixed] {
        assert_eq!(load(&dump(plain)).as_deref(), Some(plain.as_str()));
    }
}

#[test]
fn load_rejects_garbage() {
    assert_eq!(load(""), None);
    assert_eq!(load("not base64!"), None);
    // Valid alphabet, but ends before the end-of-stream marker.
    assert_eq!(load(&dump("hello world")[..4]), None);
}

#[test]
fn mvmz_summary() {
    let json = r#"{
        "system": {"_framesOnSave": 7260},
        "map": {"_mapId": 12},
        "switches": {"_data": [null, true, false]},
        "variables": {"_data": {"@a": [null, 5, 7]}},
        "actors": {"_data": [null, {"_actorId": 1, "_name": "Harold", "_nickname": "Hero", "_classId": 3, "_level": 9,
                                    "_hp": 40, "_mp": 12, "_tp": 5},
                             {"_actorId": 2, "_name": "Marsha", "_nickname": "", "_classId": 4, "_level": 2,
                              "_hp": 1, "_mp": 2, "_tp": 3}]},
        "party": {"_gold": 99, "_actors": [2, 1], "_items": {"1": 3}, "_weapons": {"4": 1}, "_armors": {}}
    }"#;

    let summary = get_save_summary(dump(json).as_bytes(), EngineType::MVMZ, None).unwrap();

    assert_eq!(summary.frames, 7260);
    assert_eq!(summary.map_id, 12);
    assert_eq!(summary.gold, 99);
    assert_eq!(summary.party, [2, 1]);
    assert_eq!(summary.actors.len(), 2);
    assert_eq!(summary.actors[0].name, "Harold");
    assert_eq!(summary.actors[0].tp, 5);
    assert_eq!((summary.items[0].id, summary.items[0].count), (1, 3));
    assert_eq!(summary.weapons[0].id, 4);
    assert_eq!(summary.switches, [false, true, false]);
    assert_eq!(summary.variables, [0, 5, 7]);
}

#[test]
fn summary_rejects_garbage() {
    assert!(get_save_summary(b"not a save", EngineType::MVMZ, None).is_err());
}
