use bitflags::bitflags;
use getset::{Getters, MutGetters, Setters};
use gxhash::{GxBuildHasher, HashSet};
use indexmap::{IndexMap, IndexSet};
use num_enum::FromPrimitive;
use std::{mem::take, ops::Deref};
use strum_macros::{Display, EnumIs};

#[cfg(feature = "serde")]
use num_enum::{IntoPrimitive, TryFromPrimitive};
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

#[cfg(feature = "log")]
use log;

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
        &self.first().unwrap_log().1.comments
    }

    fn comments_mut(&mut self) -> &mut Vec<String> {
        &mut self.first_mut().unwrap_log().1.comments
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

pub(crate) trait TrimReplace {
    fn trim_replace(&self) -> String;
}

impl TrimReplace for str {
    #[inline(always)]
    fn trim_replace(&self) -> String {
        self.trim().into()
    }
}

pub(crate) trait EachLine {
    fn each_line(&self) -> Vec<String>;
}

impl EachLine for str {
    #[inline]
    /// Return a Vec of strings splitted by lines (inclusive), akin to each_line in Ruby
    fn each_line(&self) -> Vec<String> {
        let mut result = Vec::new();
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
    #[derive(PartialEq, Clone, Copy)]
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
        const All = Self::Map.bits()
                | Self::Other.bits()
                | Self::System.bits()
                | Self::Scripts.bits();
    }
}

impl Default for FileFlags {
    fn default() -> Self {
        Self::All
    }
}

pub(crate) trait OptionExt<T> {
    fn unwrap_log(self) -> T;
}

pub(crate) trait ResultExt<T, E> {
    fn unwrap_log(self) -> T
    where
        E: std::fmt::Debug;
}

impl<T> OptionExt<T> for Option<T> {
    #[track_caller]
    #[inline(always)]
    fn unwrap_log(self) -> T {
        let Some(value) = self else {
            #[cfg(feature = "log")]
            {
                let location = std::panic::Location::caller();
                log::error!(
                    "called `Option::unwrap_log()` on a `None` value in {} at {}:{}",
                    location.file(),
                    location.line(),
                    location.column()
                );
            }

            panic!("called `Option::unwrap_log()` on a `None` value");
        };

        value
    }
}

impl<T, E> ResultExt<T, E> for Result<T, E> {
    #[track_caller]
    #[inline(always)]
    fn unwrap_log(self) -> T
    where
        E: std::fmt::Debug,
    {
        match self {
            Ok(value) => value,
            Err(err) => {
                #[cfg(feature = "log")]
                {
                    let location = std::panic::Location::caller();
                    log::error!(
                        "called `Result::unwrap_log()` on an `Err` value: {err:?} in {} at {}:{}",
                        location.file(),
                        location.line(),
                        location.column()
                    );
                }

                panic!(
                    "called `Result::unwrap_log()` on an `Err` value: {err:?}"
                );
            }
        }
    }
}

#[cfg(feature = "log")]
#[macro_export]
macro_rules! println {
    ($($arg:tt)*) => {{
        log::info!($($arg)*);
    }};
}

#[cfg(feature = "log")]
#[macro_export]
macro_rules! eprintln {
    ($($arg:tt)*) => {{
        log::error!($($arg)*);
    }};
}
