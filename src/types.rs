use bitflags::bitflags;
use getset::{Getters, MutGetters, Setters};
use gxhash::{GxBuildHasher, HashSet};
use indexmap::{IndexMap, IndexSet};
use num_enum::FromPrimitive;
use num_enum::{IntoPrimitive, TryFromPrimitive};
use serde::{Deserialize, Serialize, Serializer};
use std::{hash::BuildHasher, io, mem::take, ops::Deref, path::PathBuf};
use strum_macros::{Display, EnumIs};
use thiserror::Error;

pub(crate) type IndexSetGx<K> = IndexSet<K, GxBuildHasher>;
pub(crate) type IndexMapGx<K, V> = IndexMap<K, V, GxBuildHasher>;
pub(crate) type IgnoreEntry = HashSet<String>;
pub(crate) type IgnoreMap = IndexMapGx<String, HashSet<String>>;
pub(crate) type TranslationMap = IndexMapGx<String, TranslationEntry>;
pub(crate) type Lines = IndexSetGx<String>;
pub(crate) type TranslationDuplicateMap = Vec<(String, TranslationEntry)>;

pub(crate) trait Comments {
    fn comments(&self) -> &Vec<String>;
    fn comments_mut(&mut self) -> &mut Vec<String>;
}

impl Comments for IndexMapGx<String, TranslationEntry> {
    fn comments(&self) -> &Vec<String> {
        unsafe { &self.first().unwrap_unchecked().1.comments }
    }

    fn comments_mut(&mut self) -> &mut Vec<String> {
        unsafe { &mut self.first_mut().unwrap_unchecked().1.comments }
    }
}

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
    /// Return a [`Vec`] of strings splitted by lines (inclusive), akin to `each_line` in Ruby
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

#[derive(Default, Clone, Copy)]
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
    pub const fn new(engine_type: EngineType) -> Self {
        match engine_type {
            EngineType::New => Self {
                display_name: "displayName",
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
                armor_types: "armorTypes",
                elements: "elements",
                skill_types: "skillTypes",
                terms: "terms",
                weapon_types: "weaponTypes",
                game_title: "gameTitle",
                equip_types: "equipTypes",
                currency_unit: "currency_unit",
            },
            _ => Self {
                display_name: "display_name",
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
                armor_types: "armor_types",
                elements: "elements",
                skill_types: "skill_types",
                terms: if engine_type.is_xp() {
                    "words"
                } else {
                    "terms"
                },
                weapon_types: "weapon_types",
                game_title: "game_title",
                equip_types: "equipTypes",
                currency_unit: "currency_unit",
            },
        }
    }
}

#[derive(Default, Clone, Copy, EnumIs, Display)]
#[repr(u8)]
pub(crate) enum RPGMFileType {
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
    pub fn from_filename(filename: &str) -> Self {
        if filename.len() > 3 {
            let letters: &str = &filename[0..3].to_lowercase();

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
        }
    }

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
}

pub(crate) trait IndexMapExt {
    fn with_capacity(capacity: usize) -> Self;
}

impl<K, V, S: BuildHasher + Default> IndexMapExt for IndexMap<K, V, S> {
    fn with_capacity(capacity: usize) -> Self {
        Self::with_capacity_and_hasher(capacity, S::default())
    }
}

#[derive(Debug, Default, Clone, Getters, Setters, MutGetters)]
pub(crate) struct TranslationEntry {
    #[getset(get = "pub", set = "pub")]
    translation: String,
    #[getset(get = "pub", set = "pub", get_mut = "pub")]
    comments: Vec<String>,
}

impl TranslationEntry {
    pub fn new(translation: String, comments: Vec<String>) -> Self {
        Self {
            translation,
            comments,
        }
    }

    pub fn parts(&self) -> (&String, &[String]) {
        (&self.translation, &self.comments)
    }
}

impl From<&str> for TranslationEntry {
    fn from(value: &str) -> Self {
        let mut entry = Self::default();
        entry.set_translation(value.to_string());
        entry
    }
}

impl From<String> for TranslationEntry {
    fn from(value: String) -> Self {
        let mut entry = Self::default();
        entry.set_translation(value);
        entry
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
        "Base mode is not `ReadMode::Default` or `ReadMode::Force`, but no translation was supplied."
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

#[derive(Debug, Clone, Copy, EnumIs, Deserialize, Serialize)]
#[serde(into = "u8", try_from = "u8")]
#[repr(u8)]
pub enum Mode {
    Read(ReadMode),
    Write = 3,
    Purge = 4,
}

impl Mode {
    #[must_use]
    pub const fn is_append(self) -> bool {
        matches!(self, Self::Read(ReadMode::Append))
    }

    #[must_use]
    pub const fn is_default(self) -> bool {
        matches!(self, Self::Read(ReadMode::Default | ReadMode::Force))
    }

    #[must_use]
    pub const fn is_force(self) -> bool {
        matches!(self, Self::Read(ReadMode::Force))
    }
}

impl Default for Mode {
    fn default() -> Self {
        Self::Read(ReadMode::Default)
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
)]
#[serde(into = "u8", try_from = "u8")]
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
/// There's three read modes:
///
/// - [`ReadMode::Default`] - parses the text from the RPG Maker files, aborts if translation files already exist.
/// - [`ReadMode::Append`] - appends the new text to the translation files. That's particularly helpful if the game received content update.
/// - [`ReadMode::Force`] - parses the text from the RPG Maker files, overwrites the existing translation. **DANGEROUS!**
pub enum ReadMode {
    #[default]
    Default,
    Append,
    Force,
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
    #[serde(into = "u8", try_from = "u8")]
    /// There's four [`FileFlags`] variants:
    /// - [`FileFlags::Map`] - enables `Mapxxx.ext` files processing.
    /// - [`FileFlags::Other`] - enables processing files other than `Map`, `System`, `Scripts` and `plugins`.
    /// - [`FileFlags::System`] - enables `System.txt` file processing.
    /// - [`FileFlags::Scripts`] - enables `Scripts.ext`/`plugins.js` file processing, based on engine type.
    pub struct FileFlags: u8 {
        /// `Mapxxx.ext` files.
        const Map = 1 << 0;

        /// All files, other than map, system, and scripts/plugins.
        const Other = 1 << 1;

        /// `System.ext` file.
        const System = 1 << 2;

        /// `Scripts.ext`/`plugins.js` file.
        const Scripts = 1 << 3;
    }
}

impl TryFrom<u8> for FileFlags {
    type Error = String;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Ok(Self::from_bits_truncate(value))
    }
}

impl From<FileFlags> for u8 {
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

pub enum ProcessedData {
    RPGMData(Vec<u8>),
    TranslationData(Vec<u8>),
}

impl std::convert::AsRef<[u8]> for ProcessedData {
    fn as_ref(&self) -> &[u8] {
        match self {
            Self::RPGMData(vec) | Self::TranslationData(vec) => vec,
        }
    }
}

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
