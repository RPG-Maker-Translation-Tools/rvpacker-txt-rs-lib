//! RM2K/2003 save summaries (`SaveNN.lsd`). The saves are built here with rm2k-lib's own writer, so no game file is needed:
//! a save holding only the magic header loads with every field at its default, and the test sets the fields it reads.

use rm2k::{
    engine::{Engine, SaveOpt},
    field::{BitArray, Packed},
};
use rvpacker_txt_rs_lib::{EngineType, Error, save::get_save_summary};

/// The LSD magic, as written: its length as a varint, then `LcfSaveData`.
const LSD_MAGIC: &[u8] = b"\x0bLcfSaveData";

fn lsd(build: impl FnOnce(&mut rm2k::rpg::Save<'static>)) -> Vec<u8> {
    let mut save = rm2k::file::load_save(LSD_MAGIC)
        .expect("the magic is a valid header")
        .value;
    build(&mut save);

    let mut out = Vec::new();
    rm2k::file::save_save(&save, &mut out, Engine::R2K, SaveOpt::default(), &[]).expect("writing into a Vec");
    out
}

#[test]
fn an_rm2k_save_is_summarized() {
    let bytes = lsd(|save| {
        save.system.frame_count = 5400;
        save.party_location.map_id = 7;
        save.inventory.gold = 150;
        save.inventory.party = Packed(vec![2, 1]);
        save.inventory.item_ids = Packed(vec![3]);
        save.inventory.item_counts = Packed(vec![4]);
        // One byte per switch: switch 1 on, switch 2 off.
        save.system.switches = BitArray::from_vec(vec![1, 0]);
        save.system.variables = Packed(vec![5, 7]);
    });

    let summary = get_save_summary(&bytes, EngineType::RM2K, None).unwrap();

    assert_eq!(summary.frames, 5400);
    assert_eq!(summary.map_id, 7);
    assert_eq!(summary.gold, 150);
    assert_eq!(summary.party, [2, 1]);
    assert_eq!((summary.items[0].id, summary.items[0].count), (3, 4));
    // Switch and variable 1 come first in the file, and index 0 is unused.
    assert_eq!(summary.switches, [false, true, false]);
    assert_eq!(summary.variables, [0, 5, 7]);
}

#[test]
fn a_file_that_is_not_an_rm2k_save_is_rejected() {
    assert!(get_save_summary(b"not a save", EngineType::RM2K, None).is_err());
    assert!(matches!(
        get_save_summary(b"\x0bNotLcfData!", EngineType::RM2K, None),
        Err(Error::Rm2kLoad(_) | Error::InvalidSave)
    ));
}

/// An actor the save holds. Every field the summary doesn't read keeps a neutral default, since serde needs them all.
fn actor(
    id: i32,
    name: &str,
    title: &str,
    class_id: i32,
    level: i32,
    hp: i32,
    sp: i32,
) -> rm2k::rpg::SaveActor<'static> {
    serde_json::from_value(serde_json::json!({
        "id": id,
        "name": name,
        "title": title,
        "sprite_name": "",
        "sprite_id": 0,
        "transparency": 0,
        "face_name": "",
        "face_id": 0,
        "level": level,
        "exp": 0,
        "hp_mod": 0,
        "sp_mod": 0,
        "attack_mod": 0,
        "defense_mod": 0,
        "spirit_mod": 0,
        "agility_mod": 0,
        "skills": [],
        "equipped": [0, 0, 0, 0, 0],
        "current_hp": hp,
        "current_sp": sp,
        "battle_commands": [],
        "status": [],
        "changed_battle_commands": false,
        "class_id": class_id,
        "row": 0,
        "two_weapon": false,
        "lock_equipment": false,
        "auto_battle": false,
        "super_guard": false,
        "battler_animation": 0,
    }))
    .expect("the actor should deserialize")
}

#[test]
fn an_rm2k_save_summarizes_its_actors() {
    let bytes = lsd(|save| {
        save.actors = rm2k::field::List(vec![actor(1, "Hero", "The Brave", 2, 7, 30, 8)]);
    });

    let summary = get_save_summary(&bytes, EngineType::RM2K, None).unwrap();
    let hero = &summary.actors[0];

    assert_eq!(
        (hero.actor_id, hero.name.as_str(), hero.nickname.as_str()),
        (1, "Hero", "The Brave")
    );
    assert_eq!((hero.class_id, hero.level, hero.hp, hero.mp), (2, 7, 30, 8));
}

#[test]
fn a_name_the_game_never_changed_is_empty_in_the_summary() {
    // LCF stores `\x01` for a name the game never changed, so the summary leaves it empty.
    let bytes = lsd(|save| {
        save.actors = rm2k::field::List(vec![actor(2, "\u{1}", "\u{1}", 0, 1, 0, 0)]);
    });

    let summary = get_save_summary(&bytes, EngineType::RM2K, None).unwrap();

    assert_eq!(
        (summary.actors[0].name.as_str(), summary.actors[0].nickname.as_str()),
        ("", "")
    );
}
