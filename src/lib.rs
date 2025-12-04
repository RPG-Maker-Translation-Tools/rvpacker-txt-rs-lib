#![warn(clippy::all, clippy::pedantic)]
#![allow(clippy::needless_doctest_main)]
#![allow(clippy::cast_possible_truncation)]
#![allow(clippy::cast_possible_wrap)]
#![allow(clippy::cast_sign_loss)]
#![allow(clippy::deref_addrof)]
#![doc = include_str!("../README.md")]

mod processors;

pub mod constants;
pub mod core;
pub mod json;
pub mod types;

pub use constants::{
    NEW_LINE, RVPACKER_IGNORE_FILE, RVPACKER_METADATA_FILE, SEPARATOR,
};
pub use core::{
    filter_maps, filter_other, get_engine_extension, get_ini_title,
    get_system_title, parse_ignore,
};
pub use processors::{
    Purger, PurgerBuilder, Reader, ReaderBuilder, Writer, WriterBuilder,
};
pub use types::*;
