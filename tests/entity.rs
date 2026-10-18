//! Reading single values out of a raw data file by path, and the RM2K entry
//! names a message code needs.

use rvpacker_txt_rs_lib::{
    EngineType, Error,
    core::{
        PathSegment, Rm2kDatabase, Rm2kEntity, get_entity_values, get_rm2k_entity_name, get_rm2k_system_graphic_name,
    },
};

use PathSegment::{Index, Key};
use marshal_rs::arena::Arena;

const SYSTEM: &[u8] = include_bytes!("RMMV/data/System.json");
const ACTORS: &[u8] = include_bytes!("RMMV/data/Actors.json");
const VX_ACE_ACTORS: &[u8] = include_bytes!("RMVXACE/Data/Actors.rvdata2");
const LDB: &[u8] = include_bytes!("RM2K/RPG_RT.ldb");

#[test]
fn values_are_read_from_the_root_of_a_json_file() {
    let values = get_entity_values(
        SYSTEM,
        EngineType::MVMZ,
        None,
        &[],
        &[Key("gameTitle"), Key("optDisplayTp"), Key("startMapId")],
    )
    .unwrap();

    assert_eq!(values, ["Project1", "true", "1"]);
}

#[test]
fn values_are_read_from_a_nested_object() {
    let values = get_entity_values(
        SYSTEM,
        EngineType::MVMZ,
        None,
        &[Key("ship")],
        &[Key("startMapId"), Key("characterName")],
    )
    .unwrap();

    assert_eq!(values, ["0", "Vehicle"]);
}

#[test]
fn values_are_read_from_an_array_element() {
    let values = get_entity_values(ACTORS, EngineType::MVMZ, None, &[Index(1)], &[Key("name"), Key("id")]).unwrap();

    assert_eq!(values, ["Harold", "1"]);
}

#[test]
fn values_are_read_from_a_marshal_object() {
    let values = get_entity_values(VX_ACE_ACTORS, EngineType::VXAce, None, &[Index(1)], &[Key("name")]).unwrap();

    assert_eq!(values, ["Eric"]);
}

#[test]
fn a_path_that_does_not_resolve_is_invalid() {
    assert!(matches!(
        get_entity_values(SYSTEM, EngineType::MVMZ, None, &[Key("nope")], &[Key("gameTitle")]),
        Err(Error::InvalidPath(path)) if path == "nope"
    ));
    assert!(matches!(
        get_entity_values(ACTORS, EngineType::MVMZ, None, &[Index(999)], &[Key("name")]),
        Err(Error::InvalidPath(path)) if path == "999"
    ));
    assert!(matches!(
        get_entity_values(SYSTEM, EngineType::MVMZ, None, &[], &[Key("nope")]),
        Err(Error::InvalidPath(_))
    ));
}

#[test]
fn a_leaf_that_is_not_a_scalar_is_invalid() {
    // Actor 1 carries a `traits` array, which is not a boolean, number or string.
    assert!(matches!(
        get_entity_values(ACTORS, EngineType::MVMZ, None, &[Index(1)], &[Key("traits")]),
        Err(Error::InvalidPath(_))
    ));
}

#[test]
fn content_that_is_not_a_data_file_is_rejected() {
    assert!(get_entity_values(b"not a data file", EngineType::MVMZ, None, &[], &[]).is_err());
    // RM2K's database is a binary file of its own, not Marshal.
    assert!(get_entity_values(LDB, EngineType::RM2K, None, &[], &[]).is_err());
}

#[test]
fn a_missing_rm2k_entry_names_its_list_and_id() {
    assert!(matches!(
        get_rm2k_entity_name(LDB, Rm2kEntity::Variables, i32::MAX, None),
        Err(Error::InvalidPath(path)) if path == "variables[2147483647]"
    ));
}

#[test]
fn untagged_text_is_decoded_in_the_read_encoding() {
    let (bytes, _, _) = encoding_rs::SHIFT_JIS.encode("勇者");

    let mut arena = Arena::builder();
    let name = arena.push_bytes(bytes.into_owned());
    let actor = arena
        .push_object(b"RPG::Actor".to_vec(), &[(b"@name".to_vec(), name)])
        .unwrap();
    arena.set_root(actor);
    let content = marshal_rs::dump(&arena);

    let read = |encoding| get_entity_values(&content, EngineType::XP, encoding, &[], &[Key("@name")]);

    assert!(matches!(read(None), Err(Error::InvalidText { .. })));
    assert_eq!(read(Some(encoding_rs::SHIFT_JIS)).unwrap(), ["勇者"]);
}

#[test]
fn a_loaded_rm2k_database_answers_like_the_one_shot_functions() {
    let database = Rm2kDatabase::load(LDB, Some(encoding_rs::WINDOWS_1252)).unwrap();

    assert_eq!(
        database.system_graphic_name().unwrap(),
        get_rm2k_system_graphic_name(LDB, Some(encoding_rs::WINDOWS_1252)).unwrap()
    );
    assert!(matches!(
        database.entity_name(Rm2kEntity::Variables, i32::MAX),
        Err(Error::InvalidPath(_))
    ));
}
