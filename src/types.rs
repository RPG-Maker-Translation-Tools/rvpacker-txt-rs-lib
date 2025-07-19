use bitflags::bitflags;
use derive_more::{Deref, DerefMut, From, Into, IntoIterator};
use getset::{Getters, MutGetters, Setters};
use gxhash::{GxBuildHasher, HashSet};
use indexmap::{IndexMap, IndexSet};
use num_enum::FromPrimitive;
use std::{hash::BuildHasher, mem::take, ops::Deref, path::PathBuf};
use strum_macros::{Display, EnumIs};
use thiserror::Error;

#[cfg(feature = "serde")]
use num_enum::{IntoPrimitive, TryFromPrimitive};
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

pub(crate) type IndexSetGx<K> = IndexSet<K, GxBuildHasher>;
pub(crate) type IndexMapGx<K, V> = IndexMap<K, V, GxBuildHasher>;
pub(crate) type IgnoreEntry = HashSet<String>;
pub(crate) type IgnoreMap = IndexMapGx<String, HashSet<String>>;
pub(crate) type TranslationMap = IndexMapGx<String, TranslationEntry>;
pub(crate) type Lines = IndexSetGx<String>;
pub(crate) type TranslationDuplicateMap = Vec<(String, TranslationEntry)>;

#[derive(From, Into, Deref, DerefMut, Debug, Default, IntoIterator)]
#[must_use = "this `Vec` of `Result`s may contain an `Err` variant, which should be handled"]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct Results(Vec<Result<Outcome, Error>>);

#[derive(Debug)]
#[must_use = "this `FileResults` struct of `Vec` of `Result`s may contain an `Err` variant, which should be handled"]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct FileResults {
    pub map: Results,
    pub other: Results,
    pub system: Results,
    pub scripts: Results,
}

impl Default for FileResults {
    fn default() -> Self {
        Self {
            map: vec![Ok(Outcome::NotInFileFlags(FileFlags::Map))].into(),
            other: vec![Ok(Outcome::NotInFileFlags(FileFlags::Other))].into(),
            system: vec![Ok(Outcome::NotInFileFlags(FileFlags::System))].into(),
            scripts: vec![Ok(Outcome::NotInFileFlags(FileFlags::Scripts))]
                .into(),
        }
    }
}

impl IntoIterator for FileResults {
    type Item = Result<Outcome, Error>;
    type IntoIter = std::vec::IntoIter<Result<Outcome, Error>>;

    fn into_iter(self) -> Self::IntoIter {
        [self.map, self.other, self.system, self.scripts]
            .into_iter()
            .flatten()
            .collect::<Vec<_>>()
            .into_iter()
    }
}

#[derive(Debug, Error)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum Error {
    /// Error for failed `fs::read_dir`.
    #[error("Reading `source_path` {path} failed with: {err}")]
    ReadDirFailed { path: PathBuf, err: String },
    /// Error for failed `fs::create_dir`.
    #[error("Create directory {path} failed with: {err}")]
    CreateDirFailed { path: PathBuf, err: String },
    /// Error for failed `fs::write`.
    #[error("Writing file {file} failed with: {err}")]
    WriteFileFailed { file: PathBuf, err: String },
    /// Error for failed `fs::read`.
    #[error("Read file {file} failed with: {err}")]
    ReadFileFailed { file: PathBuf, err: String },
    /// Error for failed `marshal_rs::load`.
    #[error("Loading RPG Maker file {file} failed with: {err}")]
    LoadFailed { file: PathBuf, err: String },
    /// Error for failed `serde_json::from_str`.
    #[error("Parsing `.json` file {file} failed with: {err}")]
    JSONParseFailed { file: PathBuf, err: String },
    /// Error for non-existant `source_path/../js/plugins.js`.
    #[error(
        "When operating on `MV/MZ` (`EngineType::New`) games and `FileFlags::Scripts` flag is set, parent directory of `source_path` must contain `js` directory with `plugins.js` file."
    )]
    PluginsFileMissing,
    /// Error for not supported `Append` `read_mode` in `json::generate`.
    #[error("`Append` `read_mode` is not supported.")]
    AppendModeIsNotSupported,
}

#[derive(Debug, Display)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum Outcome {
    /// Outcome for already existing `.txt` file when `read_mode` is set to `Default`.
    #[strum(
        to_string = "{0:?}: File already exist. Set `read_mode` to `Append` to append new lines, or to `Force` to forcefully overwrite."
    )]
    TXTAlreadyExist(PathBuf),
    /// Outcome for when `Mapxxx` file is not presented in `Mapinfos` file, therefore making it unused.
    #[strum(
        to_string = "{0}: The ID of this map is not presented in `Mapinfos` file, therefore it's never appears in game and unused, so it is skipped."
    )]
    MapIsUnused(String),
    /// Outcome for when translation entry doesn't have any translation.
    #[strum(
        to_string = "{entry} ({file}): No translation exists for this entry, so it is skipped."
    )]
    NoTranslationForEntry { file: String, entry: String },
    /// Outcome for successfully written file.
    #[strum(to_string = "{0:?}: Successfully written file.")]
    WrittenFile(PathBuf),
    /// Outcome for successfully purged file.
    #[strum(to_string = "{0:?}: Successfully purged file.")]
    PurgedFile(PathBuf),
    /// Outcome for successfully read file.
    #[strum(to_string = "{0:?}: Successfully read file.")]
    ReadFile(PathBuf),
    /// Outcome for when file is not in `file_flags`, so it's skipped.
    #[strum(
        to_string = "Processing of {0:?} skipped, because it's not in the file flags."
    )]
    NotInFileFlags(FileFlags),
    /// Outcome for when `json::generate` is called, `JSON` file already exist, and `read_mode` is set to default.
    #[strum(
        to_string = "{0:?}: JSON already exist. Set `read_mode` to `Force` to overwrite."
    )]
    JSONAlreadyExist(PathBuf),
    /// Outcome for when `json::generate` is called on a MV/MZ `source_path` directory.
    #[strum(
        to_string = "MV/MZ engines are already JSON, so processing is skipped."
    )]
    MVMZAlreadyJSON,
    /// Outcome for successfully generated JSON.
    #[strum(to_string = "{0:?}: Succesfully generated JSON.")]
    GeneratedJSON(PathBuf),
    /// Outcome for successfully written JSON.
    #[strum(to_string = "{0:?}: Succesfully written JSON.")]
    WrittenJSON(PathBuf),
}

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

pub trait IndexSetExt {
    fn new() -> Self;
    fn with_capacity(capacity: usize) -> Self;
}

impl<K, S: BuildHasher + Default> IndexSetExt for IndexSet<K, S> {
    fn new() -> Self {
        Self::with_hasher(S::default())
    }

    fn with_capacity(capacity: usize) -> Self {
        Self::with_capacity_and_hasher(capacity, S::default())
    }
}

pub trait IndexMapExt {
    fn new() -> Self;
    fn with_capacity(capacity: usize) -> Self;
}

impl<K, V, S: BuildHasher + Default> IndexMapExt for IndexMap<K, V, S> {
    fn new() -> Self {
        Self::with_hasher(S::default())
    }

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

#[derive(Clone, Copy, EnumIs, Default)]
#[cfg_attr(
    feature = "serde",
    derive(TryFromPrimitive, IntoPrimitive, Deserialize, Serialize)
)]
#[cfg_attr(feature = "serde", serde(into = "u8", try_from = "u8"))]
#[repr(u8)]
pub enum ProcessingMode {
    #[default]
    Read,
    Write,
    Purge,
}

#[derive(Clone, Copy, EnumIs, Default)]
#[cfg_attr(
    feature = "serde",
    derive(TryFromPrimitive, IntoPrimitive, Deserialize, Serialize)
)]
#[cfg_attr(feature = "serde", serde(into = "u8", try_from = "u8"))]
#[repr(u8)]
pub enum DuplicateMode {
    #[default]
    Allow,
    Remove,
}

#[derive(PartialEq, Clone, Copy, EnumIs, Default)]
#[cfg_attr(
    feature = "serde",
    derive(TryFromPrimitive, IntoPrimitive, Deserialize, Serialize)
)]
#[cfg_attr(feature = "serde", serde(into = "u8", try_from = "u8"))]
#[repr(u8)]
pub enum GameType {
    #[default]
    None,
    Termina,
    LisaRPG,
}

#[derive(PartialEq, Clone, Copy, EnumIs, Default)]
#[cfg_attr(
    feature = "serde",
    derive(TryFromPrimitive, IntoPrimitive, Deserialize, Serialize)
)]
#[cfg_attr(feature = "serde", serde(into = "u8", try_from = "u8"))]
#[repr(u8)]
pub enum ReadMode {
    #[default]
    None,
    Default,
    Append,
    Force,
}

#[derive(PartialEq, Clone, Copy, EnumIs, Default)]
#[cfg_attr(
    feature = "serde",
    derive(TryFromPrimitive, IntoPrimitive, Deserialize, Serialize)
)]
#[cfg_attr(feature = "serde", serde(into = "u8", try_from = "u8"))]
#[repr(u8)]
pub enum EngineType {
    #[default]
    /// MV/MZ
    New,
    VXAce,
    VX,
    XP,
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
#[derive(PartialEq, Clone, Copy, EnumIs, FromPrimitive)]
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

#[derive(PartialEq, Clone, Copy, EnumIs)]
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
    /// Return a Vec of strings splitted by lines (inclusive), akin to each_line in Ruby
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

#[derive(PartialEq, Clone, Copy, EnumIs, Display, Default)]
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

bitflags! {
    #[derive(PartialEq, Debug, Clone, Copy)]
    #[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
    pub struct FileFlags: u8 {
        const None = 0;
        /// `Mapxxx.ext` files.
        const Map = 1;
        /// All files, other than map, system, and scripts/plugins.
        const Other = 2;
        /// `System.ext` file.
        const System = 4;
        /// `Scripts.ext`/`plugins.js` file.
        const Scripts = 8;
    }
}

impl Default for FileFlags {
    fn default() -> Self {
        Self::all()
    }
}
