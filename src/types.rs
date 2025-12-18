use crate::constants::{
    MAP_DISPLAY_NAME_COMMENT_PREFIX, MAP_ORDER_COMMENT, NAME_COMMENT,
};
use bitflags::bitflags;
use gxhash::{GxBuildHasher, HashSet};
use indexmap::{IndexMap, IndexSet};
use num_enum::{FromPrimitive, IntoPrimitive, TryFromPrimitive};
use serde::{Deserialize, Serialize, Serializer};
use smallvec::SmallVec;
use std::{
    convert::Infallible, hash::BuildHasher, io, mem::take, ops::Deref,
    path::PathBuf, str::FromStr,
};
use strum_macros::{Display, EnumIs, VariantNames};
use thiserror::Error;

pub(crate) type IndexSetGx<K> = IndexSet<K, GxBuildHasher>;
pub(crate) type IndexMapGx<K, V> = IndexMap<K, V, GxBuildHasher>;

pub(crate) type IgnoreEntry = HashSet<String>;
pub(crate) type IgnoreMap = IndexMapGx<String, HashSet<String>>;

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

pub(crate) trait EachLine {
    fn each_line(&self) -> Vec<String>;
}

impl EachLine for str {
    #[inline]
    /// Returns a [`Vec`] of strings splitted by lines (inclusive), akin to `each_line` in Ruby
    fn each_line(&self) -> Vec<String> {
        let mut result = Vec::with_capacity(1024);
        let mut current_line = String::new();

        for char in self.chars() {
            current_line.push(char);

            if char == '\n' {
                result.push(take(&mut current_line));
            }
        }

        if !current_line.is_empty() {
            result.push(take(&mut current_line));
        }

        result
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

impl Default for Labels {
    fn default() -> Self {
        Self {
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
            currency_unit: "currency_unit",

            game_title: "",
            display_name: "",
            armor_types: "",
            skill_types: "",
            terms: "",
            weapon_types: "",

            equip_types: "equipTypes",
        }
    }
}

impl Labels {
    pub fn new(engine_type: EngineType) -> Self {
        match engine_type {
            EngineType::New => Self {
                display_name: "displayName",
                armor_types: "armorTypes",
                skill_types: "skillTypes",
                terms: "terms",
                weapon_types: "weaponTypes",
                game_title: "gameTitle",
                ..Default::default()
            },
            _ => Self {
                display_name: "display_name",
                armor_types: "armor_types",
                skill_types: "skill_types",
                terms: if engine_type.is_xp() {
                    "words"
                } else {
                    "terms"
                },
                weapon_types: "weapon_types",
                game_title: "game_title",
                ..Default::default()
            },
        }
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

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        Ok(if value.len() >= 3 {
            let letters: &str = &value[0..3].to_lowercase();

            match letters {
                "act" => Self::Actors,
                "arm" => Self::Armors,
                "cla" => Self::Classes,
                "com" => Self::Events,
                "ene" => Self::Enemies,
                "ite" => Self::Items,
                "map" => Self::Map,
                "ski" => Self::Skills,
                "sta" => Self::States,
                "sys" => Self::System,
                "tro" => Self::Troops,
                "wea" => Self::Weapons,
                "scr" => Self::Scripts,
                "plu" => Self::Plugins,
                _ => Self::Invalid,
            }
        } else {
            Self::Invalid
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

#[derive(Default, Debug, Clone)]
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
        "Title couldn't be found. Ensure you've passed right `Game.ini` or `System.json` file."
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

#[derive(Debug, Clone, Copy, EnumIs, Deserialize, Serialize, VariantNames)]
#[serde(into = "u8", try_from = "u8")]
#[strum(serialize_all = "lowercase")]
#[repr(u8)]
/// Defines how to read file.
///
/// - [`Mode::Read`] holds a [`ReadMode`] that defines the read mode.
/// - [`Mode::Write`] is used to write files back.
/// - [`Mode::Purge`] is used to purge lines with empty translation.
pub enum Mode {
    Read(ReadMode),
    Write = 3,
    Purge = 4,
}

impl Mode {
    /// Checks if [`Mode`] is [`ReadMode::Default`] with any force boolean.
    #[must_use]
    pub const fn is_default(self) -> bool {
        matches!(self, Self::Read(ReadMode::Default(_)))
    }

    /// Checks if [`Mode`] is [`ReadMode::Append`] with any force boolean.
    #[must_use]
    pub const fn is_append(self) -> bool {
        matches!(self, Self::Read(ReadMode::Append(_)))
    }

    /// Checks if [`Mode`] is [`ReadMode::Default`] without a force boolean.
    #[must_use]
    pub const fn is_default_default(self) -> bool {
        matches!(self, Self::Read(ReadMode::Default(false)))
    }

    /// Checks if [`Mode`] is [`ReadMode::Append`] without a force boolean.
    #[must_use]
    pub const fn is_append_default(self) -> bool {
        matches!(self, Self::Read(ReadMode::Append(false)))
    }

    /// Checks if [`Mode`] is [`ReadMode::Default`] with a force boolean.
    #[must_use]
    pub const fn is_force(self) -> bool {
        matches!(self, Self::Read(ReadMode::Default(true)))
    }

    /// Checks if [`Mode`] is [`ReadMode::Append`] with a force boolean.
    #[must_use]
    pub const fn is_force_append(self) -> bool {
        matches!(self, Self::Read(ReadMode::Append(true)))
    }
}

impl Default for Mode {
    fn default() -> Self {
        Self::Read(ReadMode::Default(false))
    }
}

impl From<Mode> for u8 {
    fn from(val: Mode) -> Self {
        match val {
            Mode::Read(m) => m.into(),
            Mode::Write => 3,
            Mode::Purge => 4,
        }
    }
}

impl TryFrom<u8> for Mode {
    type Error = &'static str;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            3 => Ok(Mode::Write),
            4 => Ok(Mode::Purge),
            v => {
                let r: ReadMode =
                    v.try_into().map_err(|_| "invalid ReadMode value")?;
                Ok(Mode::Read(r))
            }
        }
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
    Default,
    Clone,
    Copy,
    EnumIs,
    TryFromPrimitive,
    IntoPrimitive,
    Deserialize,
    Serialize,
)]
#[serde(into = "u8", try_from = "u8")]
#[repr(u8)]
/// Game type for custom processing.
///
/// Right now, custom processing is implement for Fear & Hunger 2: Termina ([`GameType::Termina`]), and `LisaRPG` series games ([`GameType::LisaRPG`]).
///
/// There's no single definition for "custom processing", but the current implementations filter out unnecessary text and improve the readability of output `.txt` files.
///
/// For example, in `LisaRPG` games, `\nbt` prefix is used in dialogues to mark the tile, above which textbox should appear. When `game_type` is set to [`GameType::LisaRPG`], this prefix is not included to the output `.txt` files.
pub enum GameType {
    #[default]
    None,
    Termina,
    LisaRPG,
}

#[derive(Debug, Clone, Copy, Deserialize, Serialize, VariantNames)]
#[serde(into = "u8", try_from = "u8")]
#[strum(serialize_all = "lowercase")]
#[repr(u8)]
/// There's two read modes:
///
/// - [`ReadMode::Default`] - parses the text from the RPG Maker files, aborts if translation files already exist. `bool` indicates whether mode is force.
/// - [`ReadMode::Append`] - appends the new text to the translation files. That's particularly helpful if the game received content update. `bool` indicates whether mode is force.
///
/// Each of the modes holds a [`bool`]. It defines whether to read in force mode (overwrite existing files/bypass hashes).
pub enum ReadMode {
    Default(bool),
    Append(bool),
}

impl Default for ReadMode {
    fn default() -> Self {
        Self::Default(false)
    }
}

impl From<ReadMode> for u8 {
    fn from(val: ReadMode) -> Self {
        match val {
            ReadMode::Default(force) => u8::from(force),
            ReadMode::Append(force) => {
                if force {
                    3
                } else {
                    2
                }
            }
        }
    }
}

impl TryFrom<u8> for ReadMode {
    type Error = &'static str;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Ok(match value {
            0..=1 => Self::Default(value != 0),
            2..=3 => Self::Append(value != 2),
            _ => return Err("Expected a number from 0 to 3"),
        })
    }
}

impl FromStr for ReadMode {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "default" => Self::Default(false),
            "append" => Self::Append(false),
            "force" => Self::Default(true),
            "force-append" => Self::Append(true),
            _ => {
                return Err(
                    "Expected `default`, `append`, `force` or `force-append` string",
                );
            }
        })
    }
}

impl ReadMode {
    /// Checks if [`ReadMode`] is [`ReadMode::Default`] with any force boolean.
    #[must_use]
    pub const fn is_default(self) -> bool {
        matches!(self, ReadMode::Default(_))
    }

    /// Checks if [`ReadMode`] is [`ReadMode::Append`] with any force boolean.
    #[must_use]
    pub const fn is_append(self) -> bool {
        matches!(self, ReadMode::Append(_))
    }

    /// Checks if [`ReadMode`] is [`ReadMode::Default`] without a force boolean.
    #[must_use]
    pub const fn is_default_default(self) -> bool {
        matches!(self, ReadMode::Default(false))
    }

    /// Checks if [`ReadMode`] is [`ReadMode::Append`] without a force boolean.
    #[must_use]
    pub const fn is_append_default(self) -> bool {
        matches!(self, ReadMode::Append(false))
    }

    /// Checks if [`ReadMode`] is [`ReadMode::Default`] with a force boolean.
    #[must_use]
    pub const fn is_force(self) -> bool {
        matches!(self, ReadMode::Default(true))
    }

    /// Checks if [`ReadMode`] is [`ReadMode::Append`] with a force boolean.
    #[must_use]
    pub const fn is_force_append(self) -> bool {
        matches!(self, ReadMode::Append(true))
    }
}

#[derive(
    Debug,
    Clone,
    Copy,
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

bitflags! {
    #[derive(Debug, Clone, Copy, Deserialize, Serialize)]
    #[serde(into = "u16", try_from = "u16")]
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

impl FileFlags {
    #[must_use]
    /// Other entries. Those include [`FileFlags::Armors`], [`FileFlags::Classes`], [`FileFlags::CommonEvents`], [`FileFlags::Enemies`], [`FileFlags::Items`], [`FileFlags::Skills`], [`FileFlags::States`], [`FileFlags::Troops`], [`FileFlags::Weapons`].
    pub fn other() -> Self {
        Self::Actors
            | Self::Armors
            | Self::Classes
            | Self::CommonEvents
            | Self::Enemies
            | Self::Items
            | Self::Skills
            | Self::States
            | Self::Troops
            | Self::Weapons
    }
}

impl FromStr for FileFlags {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.len() >= 3 {
            let letters: &str = &s[0..3].to_lowercase();

            Ok(match letters {
                "act" => Self::Actors,
                "arm" => Self::Armors,
                "cla" => Self::Classes,
                "com" => Self::CommonEvents,
                "ene" => Self::Enemies,
                "ite" => Self::Items,
                "map" => Self::Map,
                "ski" => Self::Skills,
                "sta" => Self::States,
                "sys" => Self::System,
                "tro" => Self::Troops,
                "wea" => Self::Weapons,
                "scr" | "plu" => Self::Scripts,
                _ => {
                    return Err(
                        "FileFlags require valid RPG Maker data file name to parse from.",
                    );
                }
            })
        } else {
            Err(
                "FileFlags require valid RPG Maker data file name to parse from.",
            )
        }
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
    #[derive(Debug, Clone, Copy, Deserialize, Serialize)]
    #[serde(into = "u8", try_from = "u8")]
    /// Indicates different modes of processing the text.
    ///
    /// Check each flag to see what it does.
    pub struct BaseFlags: u8 {
        /// Convert all encountered Unicode/CJK typographic/punctuation symbols to their Western/ASCII equivalents.
        ///
        /// That includes characters like Japanese quotation marks, for example `「」` are converted to `''` single quotes.
        ///
        /// This flag **must be set on write or purge** if it was set on read.
        const Romanize = 1 << 0;

        /// Trim leading and trailing whitespace from all encountered text.
        ///
        /// This flag **must be set on write or purge** if it was set on read.
        const Trim = 1 << 1;

        /// Use ignore entries from `.rvpacker-ignore` file.
        ///
        /// Prior to using this function, you may need to create `.rvpacker-ignore` file by purging with [`BaseFlags::CreateIgnore`] argument.
        ///
        /// Only used on reads with [`ReadMode::Append`] to bypass entries that were previously purged.
        const Ignore = 1 << 2;

        /// Create `.rvpacker-ignore` file with ignore entries from purged entries.
        ///
        /// Only used on purge.
        const CreateIgnore = 1 << 3;

        /// No effect, for convenience.
        const DisableCustomProcessing = 1 << 4;
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

#[derive(PartialEq, Clone, Copy)]
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
