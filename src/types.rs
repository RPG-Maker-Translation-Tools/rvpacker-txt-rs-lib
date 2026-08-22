use crate::constants::{
    MAP_DISPLAY_NAME_COMMENT_PREFIX, MAP_ORDER_COMMENT, NAME_COMMENT,
};
use bitflags::bitflags;
use gxhash::GxBuildHasher;
use indexmap::{IndexMap, IndexSet};
use num_enum::{FromPrimitive, IntoPrimitive, TryFromPrimitive};
use serde::{Deserialize, Serialize, Serializer};
use smallvec::SmallVec;
use std::{
    convert::Infallible, hash::BuildHasher, io, ops::Deref, path::PathBuf,
    str::FromStr,
};
use strum_macros::{Display, EnumIs, VariantNames};
use thiserror::Error;

pub(crate) type IndexSetGx<K> = IndexSet<K, GxBuildHasher>;
pub(crate) type IndexMapGx<K, V> = IndexMap<K, V, GxBuildHasher>;

pub(crate) type IgnoreMap = IndexMapGx<String, crate::core::IgnoreEntry>;

pub(crate) type Comments = SmallVec<[String; 3]>;
pub(crate) type Lines = IndexSetGx<String>;
pub(crate) type TranslationMap = IndexMapGx<String, TranslationEntry>;

/// 401 - Dialogue line.
///
/// 101 - Start of the dialogue line. (**XP ENGINE ONLY!**)
///
/// 102 - Dialogue choices array.
///
/// 402 - One of the dialogue choices from the array. (**WRITE ONLY!**)
///
/// 405 - Credits lines. (**probably NEWER ENGINES ONLY!**)
///
/// 356 - System line, special text. (TODO: that one needs clarification)
///
/// 655 - Line displayed in shop - from an external script. (**OLDER ENGINES ONLY!**)
///
/// 324, 320 - Some used in-game line. (**probably NEWER ENGINES ONLY!**)
#[derive(Clone, Copy, EnumIs, FromPrimitive)]
#[repr(u16)]
pub(crate) enum Code {
    Dialogue = 401,
    DialogueStart = 101,
    Credit = 405,
    ChoiceArray = 102,
    Choice = 402,
    System = 356,
    Misc1 = 320,
    Misc2 = 324,
    Shop = 655,
    #[num_enum(default)]
    Bad = 0,
}

impl Code {
    pub const fn is_any_misc(self) -> bool {
        matches!(self, Self::Misc1 | Self::Misc2)
    }

    pub const fn is_any_dialogue(self) -> bool {
        matches!(self, Self::Dialogue | Self::DialogueStart | Self::Credit)
    }
}

#[derive(Clone, Copy, EnumIs)]
#[repr(u8)]
pub(crate) enum Variable {
    Name,
    Nickname,
    Description,
    Message1,
    Message2,
    Message3,
    Message4,
    Note,
}

impl Variable {
    pub const fn is_any_message(self) -> bool {
        matches!(
            self,
            Self::Message1 | Self::Message2 | Self::Message3 | Self::Message4
        )
    }
}

#[derive(Clone, Copy)]
pub(crate) struct Labels {
    pub display_name: &'static str,
    pub events: &'static str,
    pub pages: &'static str,
    pub list: &'static str,
    pub code: &'static str,
    pub parameters: &'static str,
    pub name: &'static str,
    pub nickname: &'static str,
    pub description: &'static str,
    pub message1: &'static str,
    pub message2: &'static str,
    pub message3: &'static str,
    pub message4: &'static str,
    pub note: &'static str,
    pub armor_types: &'static str,
    pub elements: &'static str,
    pub skill_types: &'static str,
    pub terms: &'static str,
    pub weapon_types: &'static str,
    pub game_title: &'static str,
    pub equip_types: &'static str,
    pub currency_unit: &'static str,
}

impl Labels {
    /// The fifteen labels that are identical on every engine, plus the seven
    /// that are not.
    const fn with_varying(
        display_name: &'static str,
        armor_types: &'static str,
        skill_types: &'static str,
        terms: &'static str,
        weapon_types: &'static str,
        game_title: &'static str,
        currency_unit: &'static str,
    ) -> Self {
        Self {
            display_name,
            armor_types,
            skill_types,
            terms,
            weapon_types,
            game_title,
            currency_unit,

            events: "events",
            pages: "pages",
            list: "list",
            code: "code",
            parameters: "parameters",
            name: "name",
            nickname: "nickname",
            description: "description",
            message1: "message1",
            message2: "message2",
            message3: "message3",
            message4: "message4",
            note: "note",
            elements: "elements",
            equip_types: "equipTypes",
        }
    }

    pub const fn new(engine_type: EngineType) -> Self {
        match engine_type {
            // MV/MZ name their JSON fields in camelCase.
            EngineType::New => Self::with_varying(
                "displayName",
                "armorTypes",
                "skillTypes",
                "terms",
                "weaponTypes",
                "gameTitle",
                "currencyUnit",
            ),
            // XP calls the terms section "words"; VX and VX Ace call it "terms".
            EngineType::XP => Self::with_varying(
                "display_name",
                "armor_types",
                "skill_types",
                "words",
                "weapon_types",
                "game_title",
                "currency_unit",
            ),
            _ => Self::with_varying(
                "display_name",
                "armor_types",
                "skill_types",
                "terms",
                "weapon_types",
                "game_title",
                "currency_unit",
            ),
        }
    }
}

impl Default for Labels {
    fn default() -> Self {
        Self::new(EngineType::New)
    }
}

#[derive(
    Debug,
    Default,
    Clone,
    Copy,
    PartialEq,
    Eq,
    Hash,
    EnumIs,
    Display,
    TryFromPrimitive,
    IntoPrimitive,
    Serialize,
    Deserialize,
)]
#[serde(into = "u8", try_from = "u8")]
#[repr(u8)]
pub enum RPGMFileType {
    #[default]
    Invalid,
    Actors,
    Armors,
    Classes,
    Events,
    Enemies,
    Items,
    Map,
    Skills,
    States,
    System,
    Troops,
    Weapons,
    Scripts,
    Plugins,
}

impl RPGMFileType {
    #[must_use]
    pub fn from_filename(filename: &str) -> Self {
        unsafe { Self::from_str(filename).unwrap_unchecked() }
    }

    #[must_use]
    pub const fn is_other(self) -> bool {
        matches!(
            self,
            Self::Actors
                | Self::Armors
                | Self::Classes
                | Self::Events
                | Self::Enemies
                | Self::Items
                | Self::Skills
                | Self::States
                | Self::Troops
                | Self::Weapons
        )
    }

    #[must_use]
    pub const fn is_main(self) -> bool {
        self.is_map() || self.is_other()
    }

    #[must_use]
    pub const fn is_misc(self) -> bool {
        matches!(self, Self::System | Self::Plugins | Self::Scripts)
    }
}

impl FromStr for RPGMFileType {
    type Err = Infallible;

    /// Matches on the first three bytes lowercased in place. Byte-wise rather than
    /// `value[0..3].to_lowercase()`, which allocated per call and panicked outright
    /// if byte 3 was not a character boundary.
    fn from_str(value: &str) -> Result<Self, Self::Err> {
        let bytes = value.as_bytes();

        if bytes.len() < 3 {
            return Ok(Self::Invalid);
        }

        let prefix = [
            bytes[0].to_ascii_lowercase(),
            bytes[1].to_ascii_lowercase(),
            bytes[2].to_ascii_lowercase(),
        ];

        Ok(match &prefix {
            b"act" => Self::Actors,
            b"arm" => Self::Armors,
            b"cla" => Self::Classes,
            b"com" => Self::Events,
            b"ene" => Self::Enemies,
            b"ite" => Self::Items,
            b"map" => Self::Map,
            b"ski" => Self::Skills,
            b"sta" => Self::States,
            b"sys" => Self::System,
            b"tro" => Self::Troops,
            b"wea" => Self::Weapons,
            b"scr" => Self::Scripts,
            b"plu" => Self::Plugins,
            _ => Self::Invalid,
        })
    }
}

pub(crate) trait IndexSetExt {
    fn with_capacity(capacity: usize) -> Self;
}

impl<K, S: BuildHasher + Default> IndexSetExt for IndexSet<K, S> {
    fn with_capacity(capacity: usize) -> Self {
        Self::with_capacity_and_hasher(capacity, S::default())
    }
}

pub(crate) trait IndexMapExt {
    fn with_capacity(capacity: usize) -> Self;
}

impl<K, V, S: BuildHasher + Default> IndexMapExt for IndexMap<K, V, S> {
    fn with_capacity(capacity: usize) -> Self {
        Self::with_capacity_and_hasher(capacity, S::default())
    }
}

#[derive(Clone, Debug, Default)]
pub(crate) struct TranslationEntry {
    pub comments: Vec<String>,
    pub translation: String,
}

impl From<&str> for TranslationEntry {
    fn from(translation: &str) -> Self {
        TranslationEntry {
            translation: translation.to_string(),
            ..Default::default()
        }
    }
}

impl From<String> for TranslationEntry {
    fn from(translation: String) -> Self {
        TranslationEntry {
            translation,
            ..Default::default()
        }
    }
}

impl Deref for TranslationEntry {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.translation
    }
}

#[derive(Debug, Error)]
pub enum Error {
    #[error("{0}: IO error occurred: {1}")]
    Io(PathBuf, io::Error),
    #[error("Loading RPG Maker data failed with: {0}")]
    MarshalLoad(#[from] marshal_rs::LoadError),
    #[error("Parsing JSON data failed with: {0}")]
    JsonParse(#[from] serde_json::Error),
    #[error(
        "Title couldn't be found. Ensure you've passed right `Game.ini` or \
         `System.json` file."
    )]
    NoTitle,
    #[error(
        "Processing mode is not default read, but no translation was supplied."
    )]
    NoTranslation,
}

impl Serialize for Error {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.to_string().serialize(serializer)
    }
}

/// What to do with a game's files.
///
/// Reading is the only operation with options, so they sit on that variant
/// rather than in a second enum:
///
/// - `append` keeps the translations already in the `.txt` files and adds text
///   that is new since the last read, for a game that got a content update.
///   Without it, an existing translation file is left alone.
/// - `force` overwrites files that already exist and bypasses the hash check
///   that would otherwise skip unchanged files.
#[derive(Clone, Copy, Debug, Deserialize, EnumIs, Serialize, VariantNames)]
#[serde(into = "u8", try_from = "u8")]
#[strum(serialize_all = "lowercase")]
#[repr(u8)]
pub enum Mode {
    /// Extract text from the game's files into `.txt` files.
    Read { append: bool, force: bool },
    /// Write translated text back into the game's files.
    Write,
    /// Drop entries that have no translation.
    Purge,
}

impl Mode {
    /// A plain read: no appending, no forcing.
    #[must_use]
    pub const fn read() -> Self {
        Self::Read {
            append: false,
            force: false,
        }
    }

    /// Reading without appending, forced or not.
    #[must_use]
    pub const fn is_default(self) -> bool {
        matches!(self, Self::Read { append: false, .. })
    }

    /// Reading with appending, forced or not.
    #[must_use]
    pub const fn is_append(self) -> bool {
        matches!(self, Self::Read { append: true, .. })
    }

    /// Reading without appending and without forcing.
    #[must_use]
    pub const fn is_default_default(self) -> bool {
        matches!(
            self,
            Self::Read {
                append: false,
                force: false
            }
        )
    }

    /// Reading with appending but without forcing.
    #[must_use]
    pub const fn is_append_default(self) -> bool {
        matches!(
            self,
            Self::Read {
                append: true,
                force: false
            }
        )
    }
}

impl Default for Mode {
    fn default() -> Self {
        Self::read()
    }
}

/// Reads occupy 0..=3, with bit 0 as `force` and bit 1 as `append`.
///
/// The previous encoding gave `Write` the value 3, which a force-append read
/// also produced, so that combination could not survive a round trip.
impl From<Mode> for u8 {
    fn from(value: Mode) -> Self {
        match value {
            Mode::Read { append, force } => {
                u8::from(force) | (u8::from(append) << 1)
            }
            Mode::Write => 4,
            Mode::Purge => 5,
        }
    }
}

impl TryFrom<u8> for Mode {
    type Error = &'static str;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Ok(match value {
            0..=3 => Self::Read {
                append: value & 0b10 != 0,
                force: value & 0b01 != 0,
            },
            4 => Self::Write,
            5 => Self::Purge,
            _ => return Err("Expected a number from 0 to 5"),
        })
    }
}

impl FromStr for Mode {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "default" => Self::read(),
            "append" => Self::Read {
                append: true,
                force: false,
            },
            "force" => Self::Read {
                append: false,
                force: true,
            },
            "force-append" => Self::Read {
                append: true,
                force: true,
            },
            "write" => Self::Write,
            "purge" => Self::Purge,
            _ => {
                return Err("Expected `default`, `append`, `force`, \
                            `force-append`, `write` or `purge` string");
            }
        })
    }
}

#[derive(
    Debug,
    Default,
    Clone,
    Copy,
    EnumIs,
    TryFromPrimitive,
    IntoPrimitive,
    Deserialize,
    Serialize,
    VariantNames,
)]
#[serde(into = "u8", try_from = "u8")]
#[strum(serialize_all = "lowercase")]
#[repr(u8)]
/// Sets, what to do with duplicates. Works only for map and other files.
///
/// - [`DuplicateMode::Allow`]: Default and recommended. Each map/event is parsed into its own hashmap. That won't likely cause much clashes between the same lines which require different translations.
/// - [`DuplicateMode::Remove`]: Not recommended. This mode is stable and works perfectly, but it will write the same translation into multiple places where source text is used. Recommended only when duplicates cause too much bloat.
pub enum DuplicateMode {
    #[default]
    Allow,
    Remove,
}

impl FromStr for DuplicateMode {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "allow" => Self::Allow,
            "remove" => Self::Remove,
            _ => return Err("Expected `allow` or `remove` string"),
        })
    }
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    EnumIs,
    Default,
    TryFromPrimitive,
    IntoPrimitive,
    Deserialize,
    Serialize,
)]
#[serde(into = "u8", try_from = "u8")]
#[repr(u8)]
/// Defines engine type of the processed game.
///
/// - [`EngineType::New`] - used for MV/MZ.
/// - [`EngineType::VXAce`], [`EngineType::VX`] and [`EngineType::XP`] are self-explanatory.
pub enum EngineType {
    #[default]
    /// MV/MZ
    New,
    VXAce,
    VX,
    XP,
}

impl EngineType {
    pub fn from_extension(extension: &str) -> Option<Self> {
        match extension {
            "json" => Some(EngineType::New),
            "rxdata" => Some(EngineType::XP),
            "rvdata" => Some(EngineType::VX),
            "rvdata2" => Some(EngineType::VXAce),
            _ => None,
        }
    }

    pub fn to_str(self) -> &'static str {
        match self {
            EngineType::New => "MV/MZ",
            EngineType::VX => "VX",
            EngineType::VXAce => "VX Ace",
            EngineType::XP => "XP",
        }
    }

    pub fn extension(self) -> &'static str {
        match self {
            EngineType::New => "json",
            EngineType::VXAce => "rvdata2",
            EngineType::VX => "rvdata",
            EngineType::XP => "rxdata",
        }
    }
}

impl std::fmt::Display for EngineType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.to_str())
    }
}

bitflags! {
    #[derive(Clone, Copy, Debug, PartialEq, Eq, Deserialize, Serialize)]
    #[serde(into = "u16", try_from = "u16")]
    #[repr(transparent)]
    /// There's four [`FileFlags`] variants:
    /// - [`FileFlags::Map`] - enables `Mapxxx.ext` files processing.
    /// - [`FileFlags::other`] - enables processing files other than `Map`, `System`, `Scripts` and `plugins`.
    /// - [`FileFlags::System`] - enables `System.txt` file processing.
    /// - [`FileFlags::Scripts`] - enables `Scripts.ext`/`plugins.js` file processing, based on engine type.
    pub struct FileFlags: u16 {
        /// `Mapxxx.ext` files.
        const Map = 1 << 0;

        /// `Actors.ext` file.
        const Actors = 1 << 1;

        /// `Armors.ext` file.
        const Armors = 1 << 2;

        /// `Classes.ext` file.
        const Classes = 1 << 3;

        /// `CommonEvents.ext` file.
        const CommonEvents = 1 << 4;

        /// `Enemies.ext` file.
        const Enemies = 1 << 5;

        /// `Items.ext` file.
        const Items = 1 << 6;

        /// `Skills.ext` file.
        const Skills = 1 << 7;

        /// `States.ext` file.
        const States = 1 << 8;

        /// `Troops.ext` file.
        const Troops = 1 << 9;

        /// `Weapons.ext` file.
        const Weapons = 1 << 10;

        /// `System.ext` file.
        const System = 1 << 11;

        /// `Scripts.ext`/`plugins.js` file.
        const Scripts = 1 << 12;
    }
}

pub trait FieldNames {
    const FIELDS: &[&str];
}

impl FieldNames for FileFlags {
    const FIELDS: &[&str] = &[
        "map",
        "actors",
        "armors",
        "classes",
        "commonevents",
        "enemies",
        "items",
        "skills",
        "states",
        "troops",
        "weapons",
        "system",
        "scripts",
    ];
}

/// Gives each flag a function form, so a single file kind reads the same at a call
/// site as the [`FileFlags::other`] group does.
macro_rules! flag_aliases {
    ($($name:ident => $flag:ident),* $(,)?) => {
        $(
            #[doc = concat!("Alias for [`FileFlags::", stringify!($flag), "`].")]
            #[must_use]
            pub const fn $name() -> Self {
                Self::$flag
            }
        )*
    };
}

impl FileFlags {
    /// Everything except [`FileFlags::Map`], [`FileFlags::System`] and
    /// [`FileFlags::Scripts`].
    #[must_use]
    pub const fn other() -> Self {
        Self::Actors
            .union(Self::Armors)
            .union(Self::Classes)
            .union(Self::CommonEvents)
            .union(Self::Enemies)
            .union(Self::Items)
            .union(Self::Skills)
            .union(Self::States)
            .union(Self::Troops)
            .union(Self::Weapons)
    }

    flag_aliases! {
        map => Map,
        actors => Actors,
        armors => Armors,
        classes => Classes,
        common_events => CommonEvents,
        enemies => Enemies,
        items => Items,
        skills => Skills,
        states => States,
        troops => Troops,
        weapons => Weapons,
        system => System,
        scripts => Scripts,
    }
}

impl FromStr for FileFlags {
    type Err = &'static str;

    /// Derived from [`RPGMFileType`] rather than repeating its filename table.
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // SAFETY: `RPGMFileType`'s error type is `Infallible`.
        let file_type = unsafe { RPGMFileType::from_str(s).unwrap_unchecked() };

        Ok(match file_type {
            RPGMFileType::Actors => Self::Actors,
            RPGMFileType::Armors => Self::Armors,
            RPGMFileType::Classes => Self::Classes,
            RPGMFileType::Events => Self::CommonEvents,
            RPGMFileType::Enemies => Self::Enemies,
            RPGMFileType::Items => Self::Items,
            RPGMFileType::Map => Self::Map,
            RPGMFileType::Skills => Self::Skills,
            RPGMFileType::States => Self::States,
            RPGMFileType::System => Self::System,
            RPGMFileType::Troops => Self::Troops,
            RPGMFileType::Weapons => Self::Weapons,
            RPGMFileType::Scripts | RPGMFileType::Plugins => Self::Scripts,
            RPGMFileType::Invalid => {
                return Err("FileFlags require valid RPG Maker data file \
                            name to parse from.");
            }
        })
    }
}

impl TryFrom<u16> for FileFlags {
    type Error = String;

    fn try_from(value: u16) -> Result<Self, Self::Error> {
        Ok(Self::from_bits_truncate(value))
    }
}

impl From<FileFlags> for u16 {
    fn from(value: FileFlags) -> Self {
        value.bits()
    }
}

impl Default for FileFlags {
    fn default() -> Self {
        Self::all()
    }
}

bitflags! {
    #[derive(Clone, Copy, Debug, PartialEq, Eq, Deserialize, Serialize)]
    #[serde(into = "u8", try_from = "u8")]
    #[repr(transparent)]
    /// Indicates different modes of processing the text.
    ///
    /// Check each flag to see what it does.
    pub struct BaseFlags: u8 {
        /// Trim leading and trailing whitespace from all encountered text.
        ///
        /// This flag **must be set on write or purge** if it was set on read.
        const Trim = 1 << 1;

        /// Use ignore entries from `.rvpacker-ignore` file.
        ///
        /// Prior to using this function, you may need to create `.rvpacker-ignore` file by purging with [`BaseFlags::CreateIgnore`] argument.
        ///
        /// Only used on reads with [`Mode::Read`] to bypass entries that were previously purged.
        const Ignore = 1 << 2;

        /// Create `.rvpacker-ignore` file with ignore entries from purged entries.
        ///
        /// Only used on purge.
        const CreateIgnore = 1 << 3;

        /// No effect, for convenience.
        const DisableCustomProcessing = 1 << 4;

        /// Skip obsolete entries that are not in game files anymore on reads with [`Mode::Read`].
        const SkipObsolete = 1 << 5;
    }
}

impl Default for BaseFlags {
    fn default() -> Self {
        Self::empty()
    }
}

impl TryFrom<u8> for BaseFlags {
    type Error = String;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Ok(Self::from_bits_truncate(value))
    }
}

impl From<BaseFlags> for u8 {
    fn from(value: BaseFlags) -> Self {
        value.bits()
    }
}

/// Holds either RPG Maker file data ([`ProcessedData::RPGMData`]) or translation data ([`ProcessedData::TranslationData`]).
///
/// [`AsRef<[u8]>`] is implemented for this type, so that you can get the data without `match`ing this enum.
pub enum ProcessedData {
    RPGMData(Vec<u8>),
    TranslationData(Vec<u8>),
}

impl AsRef<[u8]> for ProcessedData {
    fn as_ref(&self) -> &[u8] {
        match self {
            Self::RPGMData(vec) | Self::TranslationData(vec) => vec,
        }
    }
}

/// Holds magic numbers, contents, and script names from the `Scripts.ext` file.
pub struct Scripts {
    pub numbers: Vec<i32>,
    pub contents: Vec<String>,
    pub names: Vec<String>,
}

impl Scripts {
    #[must_use]
    pub fn new(
        numbers: Vec<i32>,
        contents: Vec<String>,
        names: Vec<String>,
    ) -> Self {
        Self {
            numbers,
            contents,
            names,
        }
    }
}

#[derive(Clone, Copy, PartialEq)]
pub(crate) enum CommentPos {
    None = -1,
    Name,
    Order,
    DisplayName,
}

impl CommentPos {
    pub fn from_str(str: &str) -> Self {
        if str.starts_with(NAME_COMMENT) {
            Self::Name
        } else if str.starts_with(MAP_ORDER_COMMENT) {
            Self::Order
        } else if str.starts_with(MAP_DISPLAY_NAME_COMMENT_PREFIX) {
            Self::DisplayName
        } else {
            Self::None
        }
    }
}
