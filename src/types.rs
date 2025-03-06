#[cfg(feature = "serde")]
use serde::{Deserialize, Deserializer};
#[cfg(feature = "serde")]
use std::mem::transmute;

#[cfg(feature = "log")]
use log;

pub type IgnoreMap =
    indexmap::IndexMap<String, std::collections::HashSet<String, gxhash::GxBuildHasher>, gxhash::GxBuildHasher>;
pub type HashMapGx = std::collections::HashMap<String, String, gxhash::GxBuildHasher>;
pub type IndexSetGx = indexmap::IndexSet<String, gxhash::GxBuildHasher>;
pub type IndexMapGx = indexmap::IndexMap<String, String, gxhash::GxBuildHasher>;
pub type IgnoreEntry = std::collections::HashSet<String, gxhash::GxBuildHasher>;

#[derive(PartialEq, Clone, Copy)]
pub enum GameType {
    Termina,
    LisaRPG,
}

#[derive(PartialEq, Clone, Copy)]
#[repr(u8)]
pub enum ProcessingMode {
    Default,
    Append,
    Force,
}

impl ProcessingMode {
    pub const fn is_default(self) -> bool {
        matches!(self, ProcessingMode::Default)
    }

    pub const fn is_append(self) -> bool {
        matches!(self, ProcessingMode::Append)
    }

    pub const fn is_force(self) -> bool {
        matches!(self, ProcessingMode::Force)
    }
}

#[derive(PartialEq, Clone, Copy)]
pub enum EngineType {
    New,
    VXAce,
    VX,
    XP,
}

impl EngineType {
    pub const fn is_new(self) -> bool {
        matches!(self, EngineType::New)
    }

    pub const fn is_ace(self) -> bool {
        matches!(self, EngineType::VXAce)
    }

    pub const fn is_vx(self) -> bool {
        matches!(self, EngineType::VX)
    }

    pub const fn is_xp(self) -> bool {
        matches!(self, EngineType::XP)
    }
}

#[derive(PartialEq, Clone, Copy)]
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
    pub const fn is_dialogue(self) -> bool {
        matches!(self, Code::Dialogue)
    }

    pub const fn is_dialogue_start(self) -> bool {
        matches!(self, Code::DialogueStart)
    }

    pub const fn is_credit(self) -> bool {
        matches!(self, Code::Credit)
    }

    pub const fn is_choice_array(self) -> bool {
        matches!(self, Code::ChoiceArray)
    }

    pub const fn is_choice(self) -> bool {
        matches!(self, Code::Choice)
    }

    pub const fn is_system(self) -> bool {
        matches!(self, Code::System)
    }

    pub const fn is_misc1(self) -> bool {
        matches!(self, Code::Misc1)
    }

    pub const fn is_misc2(self) -> bool {
        matches!(self, Code::Misc2)
    }

    pub const fn is_any_misc(self) -> bool {
        matches!(self, Code::Misc1 | Code::Misc2)
    }

    pub const fn is_shop(self) -> bool {
        matches!(self, Code::Shop)
    }

    pub const fn is_bad(self) -> bool {
        matches!(self, Code::Bad)
    }

    pub const fn is_any_dialogue(self) -> bool {
        matches!(self, Code::Dialogue | Code::DialogueStart | Code::Credit)
    }
}

#[derive(PartialEq, Clone, Copy)]
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
    pub const fn is_name(self) -> bool {
        matches!(self, Variable::Name)
    }

    pub const fn is_nickname(self) -> bool {
        matches!(self, Variable::Nickname)
    }

    pub const fn is_desc(self) -> bool {
        matches!(self, Variable::Description)
    }

    pub const fn is_message1(self) -> bool {
        matches!(self, Variable::Message1)
    }

    pub const fn is_message2(self) -> bool {
        matches!(self, Variable::Message2)
    }

    pub const fn is_message3(self) -> bool {
        matches!(self, Variable::Message3)
    }

    pub const fn is_message4(self) -> bool {
        matches!(self, Variable::Message4)
    }

    pub const fn is_any_message(self) -> bool {
        matches!(
            self,
            Variable::Message1 | Variable::Message2 | Variable::Message3 | Variable::Message4
        )
    }

    pub const fn is_note(self) -> bool {
        matches!(self, Variable::Note)
    }
}

#[derive(PartialEq, Clone, Copy)]
#[repr(u8)]
pub enum MapsProcessingMode {
    Default,
    Separate,
    Preserve,
}

impl MapsProcessingMode {
    pub const fn is_default(self) -> bool {
        matches!(self, MapsProcessingMode::Default)
    }

    pub const fn is_separate(self) -> bool {
        matches!(self, MapsProcessingMode::Separate)
    }

    pub const fn is_preserve(self) -> bool {
        matches!(self, MapsProcessingMode::Preserve)
    }
}

pub trait TrimReplace {
    fn trim_replace(&self) -> String;
}

impl TrimReplace for str {
    #[inline(always)]
    fn trim_replace(&self) -> String {
        // okay i shouldn't have reinvented the wheel and just do this from the start
        self.trim().to_owned()
    }
}

pub trait EachLine {
    fn each_line(&self) -> Vec<String>;
}

// Return a Vec of strings splitted by lines (inclusive), akin to each_line in Ruby
impl EachLine for str {
    #[inline]
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
        match self {
            Some(value) => value,
            None => {
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
            }
        }
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

                panic!("called `Result::unwrap_log()` on an `Err` value: {:?}", err);
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
impl<'de> Deserialize<'de> for MapsProcessingMode {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let value: u8 = Deserialize::deserialize(deserializer)?;
        Ok(unsafe { transmute::<u8, MapsProcessingMode>(value) })
    }
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
impl<'de> Deserialize<'de> for ProcessingMode {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let value: u8 = Deserialize::deserialize(deserializer)?;
        Ok(unsafe { transmute::<u8, ProcessingMode>(value) })
    }
}
