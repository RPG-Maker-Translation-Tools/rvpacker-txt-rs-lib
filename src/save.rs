//! Reading the save files of every supported engine.
//!
//! - MV/MZ: LZString-compressed JSON (`.rpgsave`), see [`load`] and [`dump`] for the compression itself.
//! - XP/VX/VX Ace: several `Marshal.dump` streams in one file.
//! - RM2K/RM2K3: the LCF `.lsd`.
//!
//! [`get_save_summary`] reads any of them into a normalized [`SaveSummary`], for a consumer (a GUI tooltip, say) that
//! wants to show what a message code like `\P[1]` or `\playtime` would print without knowing how each engine stores a
//! save. Objects of the Marshal engines are located by their class name instead of their position, since what the
//! streams hold (and in which order) differs between the three engines.
//!
//! Anything an engine does not store (nicknames and TP before VX Ace, say) is left at its default.

use crate::{
    core::Base,
    types::{EngineType, Error},
};
use gxhash::{HashMap, HashMapExt, HashSet, HashSetExt};
use marshal_rs::{Arena, Kind, ValueRef};
use serde::Serialize;
use serde_json::Value as Json;

// LZString (`compressToBase64`/`decompressFromBase64`), which is what MV/MZ run `JSON.stringify(save)` through.

const ALPHABET: &[u8; 65] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=";
const BITS_PER_CHAR: u32 = 6;

/// Writes values bit by bit, least significant bit first, packing [`BITS_PER_CHAR`] of them into every output char.
struct Encoder {
    output: Vec<u8>,
    /// Codes of the single-unit phrases that were not written out literally yet.
    pending: HashSet<u32>,
    value: u32,
    position: u32,
    enlarge_in: u32,
    num_bits: u32,
}

impl Encoder {
    fn push_bit(&mut self, bit: u32) {
        self.value = (self.value << 1) | (bit & 1);
        self.position += 1;

        if self.position == BITS_PER_CHAR {
            // SAFETY: `value` holds exactly `BITS_PER_CHAR` bits here, so it is below 64.
            self.output
                .push(unsafe { *ALPHABET.get_unchecked(self.value as usize) });
            self.value = 0;
            self.position = 0;
        }
    }

    fn push_number(&mut self, mut value: u32, bits: u32) {
        for _ in 0..bits {
            self.push_bit(value & 1);
            value >>= 1;
        }
    }

    /// Widens the codes by a bit whenever the dictionary outgrows the current width.
    fn enlarge(&mut self) {
        self.enlarge_in -= 1;

        if self.enlarge_in == 0 {
            self.enlarge_in = 1 << self.num_bits;
            self.num_bits += 1;
        }
    }

    /// Writes `phrase`, whose first unit is `first`: a literal on its first occurrence, its dictionary code after.
    fn emit(&mut self, phrase: u32, first: u16) {
        if self.pending.remove(&phrase) {
            // The marker is `0` for a unit of a byte, `1` for one of two, both `num_bits` wide.
            if first < 256 {
                self.push_number(0, self.num_bits);
                self.push_number(u32::from(first), 8);
            } else {
                self.push_number(1, self.num_bits);
                self.push_number(u32::from(first), 16);
            }

            self.enlarge();
        } else {
            self.push_number(phrase, self.num_bits);
        }

        self.enlarge();
    }
}

fn compress_units(units: &[u16]) -> String {
    let mut encoder = Encoder {
        output: Vec::with_capacity(units.len() / 2),
        pending: HashSet::new(),
        value: 0,
        position: 0,
        enlarge_in: 2,
        num_bits: 2,
    };

    // A phrase is its prefix's code plus one more unit, `0` standing for the empty prefix: the dictionary is a trie, so
    // growing a phrase is one lookup instead of building (and hashing) a `Vec` of its units.
    let mut dictionary: HashMap<(u32, u16), u32> = HashMap::new();
    let mut dict_size: u32 = 3;
    let mut phrase: u32 = 0;
    let mut first: u16 = 0;

    for &unit in units {
        let single = *dictionary.entry((0, unit)).or_insert_with(|| {
            encoder.pending.insert(dict_size);
            dict_size += 1;
            dict_size - 1
        });

        if phrase == 0 {
            phrase = single;
            first = unit;
        } else if let Some(&code) = dictionary.get(&(phrase, unit)) {
            phrase = code;
        } else {
            encoder.emit(phrase, first);
            dictionary.insert((phrase, unit), dict_size);
            dict_size += 1;
            phrase = single;
            first = unit;
        }
    }

    if phrase != 0 {
        encoder.emit(phrase, first);
    }

    // End of the stream, then zero bits up to the end of the char.
    encoder.push_number(2, encoder.num_bits);

    loop {
        encoder.push_bit(0);

        if encoder.position == 0 {
            break;
        }
    }

    // Padded to a multiple of 4, like `btoa`.
    encoder.output.resize(encoder.output.len().next_multiple_of(4), b'=');

    // SAFETY: only bytes of `ALPHABET` were pushed, and it is ASCII.
    unsafe { String::from_utf8_unchecked(encoder.output) }
}

fn base64_value(byte: u8) -> Option<u32> {
    match byte {
        b'A'..=b'Z' => Some(u32::from(byte - b'A')),
        b'a'..=b'z' => Some(u32::from(byte - b'a') + 26),
        b'0'..=b'9' => Some(u32::from(byte - b'0') + 52),
        b'+' => Some(62),
        b'/' => Some(63),
        b'=' => Some(64),
        _ => None,
    }
}

/// The reading counterpart of [`Encoder`]. Every read fails when the input ends or holds a byte that is not of the
/// [`ALPHABET`].
struct Decoder<'a> {
    input: &'a [u8],
    index: usize,
    value: u32,
    position: u32,
}

impl<'a> Decoder<'a> {
    const RESET: u32 = 1 << (BITS_PER_CHAR - 1);

    fn new(input: &'a [u8]) -> Option<Self> {
        Some(Self {
            input,
            index: 1,
            value: base64_value(*input.first()?)?,
            position: Self::RESET,
        })
    }

    fn read_bits(&mut self, bits: u32) -> Option<u32> {
        let mut result = 0;

        for shift in 0..bits {
            if self.value & self.position != 0 {
                result |= 1 << shift;
            }

            self.position >>= 1;

            if self.position == 0 {
                self.position = Self::RESET;
                self.value = base64_value(*self.input.get(self.index)?)?;
                self.index += 1;
            }
        }

        Some(result)
    }
}

fn decompress_units(input: &[u8]) -> Option<Vec<u16>> {
    let mut data = Decoder::new(input)?;

    // Codes 0, 1 and 2 are the literal markers and the end of the stream, never phrases.
    let mut dictionary: Vec<Vec<u16>> = vec![Vec::new(); 3];
    let mut result: Vec<u16> = Vec::new();
    let mut enlarge_in: u32 = 4;
    let mut num_bits: u32 = 3;

    let first = match data.read_bits(2)? {
        0 => data.read_bits(8)? as u16,
        1 => data.read_bits(16)? as u16,
        2 => return Some(result),
        _ => return None,
    };

    dictionary.push(vec![first]);
    let mut previous = vec![first];
    result.push(first);

    loop {
        let code = match data.read_bits(num_bits)? {
            marker @ (0 | 1) => {
                dictionary.push(vec![data.read_bits(if marker == 0 { 8 } else { 16 })? as u16]);
                enlarge_in -= 1;
                dictionary.len() - 1
            }
            2 => return Some(result),
            code => code as usize,
        };

        if enlarge_in == 0 {
            enlarge_in = 1 << num_bits;
            num_bits += 1;
        }

        let entry = match code.cmp(&dictionary.len()) {
            std::cmp::Ordering::Less => dictionary[code].clone(),
            // The phrase being defined by this very code.
            std::cmp::Ordering::Equal => {
                let mut entry = previous.clone();
                entry.push(previous[0]);
                entry
            }
            std::cmp::Ordering::Greater => return None,
        };

        result.extend_from_slice(&entry);

        let mut new_entry = previous;
        new_entry.push(entry[0]);
        dictionary.push(new_entry);
        enlarge_in -= 1;
        previous = entry;

        if enlarge_in == 0 {
            enlarge_in = 1 << num_bits;
            num_bits += 1;
        }
    }
}

/// Decompresses the content of an MV/MZ save (`LZString.decompressFromBase64`) into the JSON it holds.
///
/// Returns `None` if `content` is not valid `LZString` output.
#[must_use]
pub fn load(content: &str) -> Option<String> {
    String::from_utf16(&decompress_units(content.as_bytes())?).ok()
}

/// Compresses `json` the way MV/MZ do on saving (`LZString.compressToBase64`), the inverse of [`load`].
#[must_use]
pub fn dump(json: &str) -> String {
    compress_units(&json.encode_utf16().collect::<Vec<_>>())
}

// ---------------------------------------------------------------------------------------------------------------------
// Summary
// ---------------------------------------------------------------------------------------------------------------------

/// One actor, as stored in the save. `name` is empty when the save holds no name for it (RM2K stores a name only when
/// the game changed it), so the caller falls back to the database.
#[derive(Debug, Default, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SaveActorSummary {
    pub actor_id: i32,
    pub name: String,
    pub nickname: String,
    pub class_id: i32,
    pub level: i32,
    pub hp: i32,
    pub mp: i32,
    pub tp: i32,
}

/// `count` of the item (or weapon, or armor) `id` the party carries.
#[derive(Debug, Default, Serialize)]
pub struct SaveStack {
    pub id: i32,
    pub count: i32,
}

/// See the [module docs](self). `switches` and `variables` are indexed by the in-game ID, so index 0 is unused.
#[derive(Debug, Default, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SaveSummary {
    /// Playtime in frames (60 per second).
    pub frames: u32,
    pub map_id: i32,
    pub gold: i32,
    /// IDs of the actors in the party, in order.
    pub party: Vec<i32>,
    /// Every actor the save holds, the party's or not.
    pub actors: Vec<SaveActorSummary>,
    /// RM2K has no weapons and armors apart from items; all of them are in here.
    pub items: Vec<SaveStack>,
    pub weapons: Vec<SaveStack>,
    pub armors: Vec<SaveStack>,
    pub switches: Vec<bool>,
    pub variables: Vec<i32>,
}

/// Reads `content` (a save file of `engine_type`) into a [`SaveSummary`]. `read_encoding` decodes the strings the
/// same way processing the game does; `None` guesses between the common codepages.
///
/// # Errors
///
/// - [`Error::InvalidSave`] - if `content` is not a save of this engine.
/// - [`Error::MarshalLoad`]/[`Error::Rm2kLoad`] - if `content` fails to parse.
pub fn get_save_summary(
    content: &[u8],
    engine_type: EngineType,
    read_encoding: Option<&'static encoding_rs::Encoding>,
) -> Result<SaveSummary, Error> {
    match engine_type {
        EngineType::MVMZ => from_json(content),
        EngineType::RM2K => from_lsd(content, read_encoding),
        _ => {
            let arenas = marshal_rs::load_many(content)?;
            Ok(from_marshal(&arenas, read_encoding))
        }
    }
}

fn from_lsd(content: &[u8], read_encoding: Option<&'static encoding_rs::Encoding>) -> Result<SaveSummary, Error> {
    let save = rm2k::file::load_save(content)?;
    let mut summary = SaveSummary {
        frames: save.system.frame_count.max(0) as u32,
        map_id: save.party_location.map_id,
        gold: save.inventory.gold,
        ..SaveSummary::default()
    };

    summary.party = save.inventory.party.iter().map(|&id| i32::from(id)).collect();

    // `\x01` is how LCF marks a name the game never changed.
    let decode = |bytes: &[u8]| {
        if bytes == b"\x01" {
            String::new()
        } else {
            Base::decode_bytes_with(bytes, read_encoding)
        }
    };

    summary.actors = save
        .actors
        .iter()
        .map(|actor| SaveActorSummary {
            actor_id: actor.id,
            name: decode(actor.name.as_bytes()),
            nickname: decode(actor.title.as_bytes()),
            class_id: actor.class_id,
            level: actor.level,
            hp: actor.current_hp,
            mp: actor.current_sp,
            tp: 0,
        })
        .collect();

    summary.items = save
        .inventory
        .item_ids
        .iter()
        .zip(save.inventory.item_counts.iter())
        .map(|(&id, &count)| SaveStack {
            id: i32::from(id),
            count: i32::from(count),
        })
        .collect();

    // LCF stores switch/variable 1 first.
    summary.switches = std::iter::once(false).chain(save.system.switches.iter()).collect();
    summary.variables = std::iter::once(0)
        .chain(save.system.variables.iter().copied())
        .collect();

    Ok(summary)
}

fn json_i32(value: Option<&Json>) -> i32 {
    value.and_then(Json::as_i64).unwrap_or_default() as i32
}

fn json_str(value: Option<&Json>) -> String {
    value.and_then(Json::as_str).unwrap_or_default().to_owned()
}

//// `JsonEx` writes an array that carries extra properties as `{"@a": [...]}`.
fn json_array(value: Option<&Json>) -> &[Json] {
    value
        .and_then(|value| value.as_array().or_else(|| value.get("@a")?.as_array()))
        .map_or(&[], Vec::as_slice)
}

fn json_stacks(value: Option<&Json>) -> Vec<SaveStack> {
    value
        .and_then(Json::as_object)
        .map(|map| {
            map.iter()
                .filter_map(|(id, count)| {
                    Some(SaveStack {
                        id: id.parse().ok()?,
                        count: count.as_i64()? as i32,
                    })
                })
                .collect()
        })
        .unwrap_or_default()
}

fn from_json(content: &[u8]) -> Result<SaveSummary, Error> {
    let compressed = std::str::from_utf8(content).map_err(|_| Error::InvalidSave)?;
    let raw = load(compressed).ok_or(Error::InvalidSave)?;
    let root = serde_json::from_str::<Json>(&raw)?;

    let party = root.get("party");
    let actors = json_array(root.pointer("/actors/_data"));

    Ok(SaveSummary {
        frames: json_i32(root.pointer("/system/_framesOnSave")).max(0) as u32,
        map_id: json_i32(root.pointer("/map/_mapId")),
        gold: json_i32(party.and_then(|party| party.get("_gold"))),
        party: json_array(party.and_then(|party| party.get("_actors")))
            .iter()
            .filter_map(|id| Some(id.as_i64()? as i32))
            .collect(),
        // The slot of an actor that never was instantiated is `null`.
        actors: actors
            .iter()
            .filter(|actor| actor.is_object())
            .map(|actor| SaveActorSummary {
                actor_id: json_i32(actor.get("_actorId")),
                name: json_str(actor.get("_name")),
                nickname: json_str(actor.get("_nickname")),
                class_id: json_i32(actor.get("_classId")),
                level: json_i32(actor.get("_level")),
                hp: json_i32(actor.get("_hp")),
                mp: json_i32(actor.get("_mp")),
                tp: json_i32(actor.get("_tp")),
            })
            .collect(),
        items: json_stacks(party.and_then(|party| party.get("_items"))),
        weapons: json_stacks(party.and_then(|party| party.get("_weapons"))),
        armors: json_stacks(party.and_then(|party| party.get("_armors"))),
        switches: json_array(root.pointer("/switches/_data"))
            .iter()
            .map(|value| value.as_bool().unwrap_or_default())
            .collect(),
        variables: json_array(root.pointer("/variables/_data"))
            .iter()
            .map(|value| value.as_i64().unwrap_or_default() as i32)
            .collect(),
    })
}

/// The first object of class `class`, either a stream of its own (XP/VX) or a value of the contents hash (VX Ace).
fn find_class<'r, 'a>(arenas: &'r [Arena<'a>], class: &[u8]) -> Option<ValueRef<'r, 'a>> {
    arenas.iter().find_map(|arena| {
        let root = ValueRef::new(arena, arena.root());

        if root.class_name() == Some(class) {
            return Some(root);
        }

        root.entries()
            .map(|(_, value)| value)
            .find(|value| value.class_name() == Some(class))
    })
}

fn ivar_i32(object: &ValueRef<'_, '_>, name: &str) -> i32 {
    object.get(name).and_then(|value| value.as_i64()).unwrap_or_default() as i32
}

fn ivar_string(object: &ValueRef<'_, '_>, name: &str, read_encoding: Option<&'static encoding_rs::Encoding>) -> String {
    object
        .get(name)
        .and_then(|value| value.as_bytes())
        .map(|bytes| Base::decode_bytes_with(bytes, read_encoding))
        .unwrap_or_default()
}

fn marshal_stacks(object: &ValueRef<'_, '_>, name: &str) -> Vec<SaveStack> {
    object
        .get(name)
        .map(|hash| {
            hash.entries()
                .filter_map(|(id, count)| {
                    Some(SaveStack {
                        id: id.as_i64()? as i32,
                        count: count.as_i64()? as i32,
                    })
                })
                .collect()
        })
        .unwrap_or_default()
}

fn from_marshal(arenas: &[Arena<'_>], read_encoding: Option<&'static encoding_rs::Encoding>) -> SaveSummary {
    let mut summary = SaveSummary::default();

    // XP/VX dump `Graphics.frame_count` as a stream of its own; VX Ace keeps the playtime in seconds in its header.
    for arena in arenas {
        let root = ValueRef::new(arena, arena.root());

        if root.kind() == Kind::Fixnum {
            summary.frames = root.as_i64().unwrap_or_default().max(0) as u32;
            break;
        }

        if let Some(seconds) = root.lookup_symbol("playtime_s").and_then(|value| value.as_i64()) {
            summary.frames = (seconds.max(0) as u32).saturating_mul(60);
            break;
        }
    }

    if let Some(map) = find_class(arenas, b"Game_Map") {
        summary.map_id = ivar_i32(&map, "map_id");
    }

    let summarize = |actor: &ValueRef<'_, '_>| SaveActorSummary {
        actor_id: ivar_i32(actor, "actor_id"),
        name: ivar_string(actor, "name", read_encoding),
        nickname: ivar_string(actor, "nickname", read_encoding),
        class_id: ivar_i32(actor, "class_id"),
        level: ivar_i32(actor, "level"),
        hp: ivar_i32(actor, "hp"),
        // XP calls it SP.
        mp: actor
            .get("mp")
            .or_else(|| actor.get("sp"))
            .and_then(|value| value.as_i64())
            .unwrap_or_default() as i32,
        tp: ivar_i32(actor, "tp"),
    };

    // An actor that never was instantiated is `nil`.
    if let Some(data) = find_class(arenas, b"Game_Actors").and_then(|actors| actors.get("data")) {
        summary.actors = data
            .array()
            .filter(|actor| actor.kind() == Kind::Object)
            .map(|actor| summarize(&actor))
            .collect();
    }

    if let Some(party) = find_class(arenas, b"Game_Party") {
        summary.gold = ivar_i32(&party, "gold");
        summary.items = marshal_stacks(&party, "items");
        summary.weapons = marshal_stacks(&party, "weapons");
        summary.armors = marshal_stacks(&party, "armors");

        // VX and VX Ace parties only know the IDs of their actors; the XP party holds the `Game_Actor`s themselves.
        for member in party.get("actors").into_iter().flat_map(|members| members.array()) {
            if member.kind() == Kind::Fixnum {
                summary.party.push(member.as_i64().unwrap_or_default() as i32);
            } else {
                let actor = summarize(&member);
                summary.party.push(actor.actor_id);

                if !summary.actors.iter().any(|known| known.actor_id == actor.actor_id) {
                    summary.actors.push(actor);
                }
            }
        }
    }

    if let Some(data) = find_class(arenas, b"Game_Switches").and_then(|switches| switches.get("data")) {
        summary.switches = data.array().map(|value| value.as_bool().unwrap_or_default()).collect();
    }

    if let Some(data) = find_class(arenas, b"Game_Variables").and_then(|variables| variables.get("data")) {
        summary.variables = data
            .array()
            .map(|value| value.as_i64().unwrap_or_default() as i32)
            .collect();
    }

    summary
}
