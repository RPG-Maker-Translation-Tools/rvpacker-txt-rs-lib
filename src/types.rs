use bitflags::bitflags;
use gxhash::*;
use indexmap::*;
#[cfg(feature = "log")]
use log;
#[cfg(feature = "serde")]
use serde::{Deserialize, Deserializer};
#[cfg(feature = "serde")]
use std::mem::transmute;
use strum_macros::EnumIs;

pub type IndexSetGx<K> = IndexSet<K, GxBuildHasher>;
pub type IndexMapGx<K, V> = IndexMap<K, V, GxBuildHasher>;
pub type IgnoreEntry = HashSet<String>;
pub type IgnoreMap = IndexMapGx<String, HashSet<String>>;
pub type TranslationMap = IndexMapGx<String, String>;
pub type TranslationSet = IndexSetGx<String>;
pub type TranslationDuplicateMap = Vec<(String, String)>;
pub type TranslationVec = Vec<String>;
pub type PurgeIndices = HashSet<usize>;
pub type StatVec = Vec<(String, String)>;

#[derive(Clone, Copy, EnumIs)]
pub enum ProcessingMode {
    Read,
    Write,
    Purge,
}

#[derive(PartialEq, Clone, Copy, EnumIs)]
pub enum GameType {
    None,
    Termina,
    LisaRPG,
}

#[derive(PartialEq, Clone, Copy, EnumIs)]
#[repr(u8)]
pub enum ReadMode {
    None,
    Default,
    Append,
    Force,
}

#[derive(PartialEq, Clone, Copy, EnumIs)]
pub enum EngineType {
    New,
    VXAce,
    VX,
    XP,
}

#[derive(PartialEq, Clone, Copy, EnumIs)]
#[repr(u16)]
pub enum Code {
    Dialogue = 401,
    DialogueStart = 101,
    Credit = 405,
    ChoiceArray = 102,
    Choice = 402,
    System = 356,
    Misc1 = 320,
    Misc2 = 324,
    Shop = 655,
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
pub enum Variable {
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

pub trait TrimReplace {
    fn trim_replace(&self) -> String;
}

impl TrimReplace for str {
    #[inline(always)]
    fn trim_replace(&self) -> String {
        self.trim().to_owned()
    }
}

pub trait EachLine {
    fn each_line(&self) -> Vec<String>;
}

impl EachLine for str {
    #[inline]
    /// Return a Vec of strings splitted by lines (inclusive), akin to each_line in Ruby
    fn each_line(&self) -> Vec<String> {
        let mut result: Vec<String> = Vec::new();
        let mut current_line: String = String::new();

        for char in self.chars() {
            current_line.push(char);

            if char == '\n' {
                result.push(std::mem::take(&mut current_line));
            }
        }

        if !current_line.is_empty() {
            result.push(std::mem::take(&mut current_line));
        }

        result
    }
}

#[derive(Clone, Copy)]
pub struct Labels {
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
                currency_unit: "__symbol__currency_unit",
            },
            _ => Self {
                display_name: "__symbol__display_name",
                events: "__symbol__events",
                pages: "__symbol__pages",
                list: "__symbol__list",
                code: "__symbol__code",
                parameters: "__symbol__parameters",
                name: "__symbol__name",
                nickname: "__symbol__nickname",
                description: "__symbol__description",
                message1: "__symbol__message1",
                message2: "__symbol__message2",
                message3: "__symbol__message3",
                message4: "__symbol__message4",
                note: "__symbol__note",
                armor_types: "__symbol__armor_types",
                elements: "__symbol__elements",
                skill_types: "__symbol__skill_types",
                terms: if engine_type.is_xp() {
                    "__symbol__words"
                } else {
                    "__symbol__terms"
                },
                weapon_types: "__symbol__weapon_types",
                game_title: "__symbol__game_title",
                equip_types: "equipTypes",
                currency_unit: "__symbol__currency_unit",
            },
        }
    }
}

#[derive(PartialEq, Clone, Copy, EnumIs)]
pub enum RPGMFileType {
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
        const Map = 1;
        const Other = 2;
        const System = 4;
        const Scripts = 8;
        const All = Self::Map.bits()
                | Self::Other.bits()
                | Self::System.bits()
                | Self::Scripts.bits();
    }
}

pub trait OptionExt<T> {
    fn unwrap_log(self) -> T;
}

pub trait ResultExt<T, E> {
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
                    "called `Result::unwrap_log()` on an `Err` value: {:?}",
                    err
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

#[cfg(feature = "serde")]
impl<'de> Deserialize<'de> for EngineType {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let value: u8 = Deserialize::deserialize(deserializer)?;
        Ok(unsafe { transmute::<u8, EngineType>(value) })
    }
}

#[cfg(feature = "serde")]
impl<'de> Deserialize<'de> for ReadMode {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let value: u8 = Deserialize::deserialize(deserializer)?;
        Ok(unsafe { transmute::<u8, ReadMode>(value) })
    }
}
