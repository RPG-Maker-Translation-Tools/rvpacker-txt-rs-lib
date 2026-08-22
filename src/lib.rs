#![warn(clippy::all, clippy::pedantic)]
#![allow(clippy::needless_doctest_main)]
#![allow(clippy::cast_possible_truncation)]
#![allow(clippy::cast_possible_wrap)]
#![allow(clippy::cast_sign_loss)]
#![allow(clippy::deref_addrof)]
#![doc = include_str!("../README.md")]

pub mod constants;
pub mod core;
pub mod json;
pub mod processors;
pub mod serde;
pub mod types;

pub use constants::{
    NEW_LINE, RVPACKER_IGNORE_FILE, RVPACKER_METADATA_FILE, SEPARATOR,
};
pub use core::{
    filter_maps, filter_other, get_ini_title, get_system_title, parse_ignore,
};
pub use processors::Processor;
pub use types::*;
