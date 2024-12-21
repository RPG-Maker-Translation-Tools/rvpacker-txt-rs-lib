#[cfg(feature = "serde")]
use serde::{Deserialize, Deserializer};
#[cfg(feature = "serde")]
use std::mem::transmute;

#[cfg(feature = "log")]
use log;

#[derive(PartialEq, Clone, Copy)]
pub enum GameType {
    Termina,
    LisaRPG,
}

#[derive(PartialEq, Clone, Copy)]
#[repr(u8)]
pub enum ProcessingMode {
    Force,
    Append,
    Default,
}

#[derive(PartialEq, Clone, Copy)]
pub enum EngineType {
    New,
    VXAce,
    VX,
    XP,
}

#[derive(PartialEq)]
pub enum Code {
    Dialogue, // also goes for credit
    Choice,
    System,
    Misc,
    Shop,
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

#[derive(PartialEq, Clone, Copy)]
#[repr(u8)]
pub enum MapsProcessingMode {
    Default,
    Separate,
    Preserve,
}

pub trait EachLine {
    fn each_line(&self) -> Vec<String>;
}

// Return a Vec of strings splitted by lines (inclusive), akin to each_line in Ruby
impl EachLine for str {
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
    fn unwrap_log(self) -> T {
        match self {
            Some(value) => value,
            None => {
                #[cfg(feature = "log")]
                log::error!("called `Option::unwrap_log()` on a `None` value",);
                panic!("called `Option::unwrap_log()` on a `None` value");
            }
        }
    }
}

impl<T, E> ResultExt<T, E> for Result<T, E> {
    #[track_caller]
    fn unwrap_log(self) -> T
    where
        E: std::fmt::Debug,
    {
        match self {
            Ok(value) => value,
            Err(err) => {
                #[cfg(feature = "log")]
                log::error!("called `Result::unwrap_log()` on an `Err` value: {:?}", err);
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
