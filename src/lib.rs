//! # rvpacker-txt-rs-lib
//!
//! `rvpacker-txt-rs-lib` is a library for [rvpacker-txt-rs](https://github.com/savannstm/rvpacker-txt-rs) CLI, that provides the function to extract the text from RPG Maker `.rxdata`, `.rvdata`, `.rvdata2` and `.json` files to `.txt` format.
//!
//! It also provides the `json` module to convert `.rxdata`, `.rvdata` and `.rvdata2` files to JSON and back.
//!
//! ## Installation
//!
//! `cargo add rvpacker-txt-rs-lib`
//!
//! ## Features
//!
//! This crate exposes the main modules, that do all the heavy-lifting: `read`, `purge`, `write`, and `json`.
//!
//! ### `read` module
//!
//! `read` module provides the `Reader` struct, along with its `ReaderBuilder` builder version.
//!
//! `Reader` struct is used to parse the text of RPG Maker files from the specified directory to the `.txt` files in specified output directory.
//!
//! #### Example
//!
//! ```rust
//! use rvpacker_txt_rs_lib::{Reader, FileFlags, EngineType};
//!
//! let mut reader = Reader::new();
//! reader.set_flags(FileFlags::Map | FileFlags::Other);
//! reader.read("C:/Game/Data", "C:/Game/translation", EngineType::VXAce);
//! ```
//!
//! ### `write` module
//!
//! `write` module provides the `Writer` struct, along with its `WriterBuilder` builder version.
//!
//! `Writer` struct is used to write the translation from the `.txt` files back to RPG Maker files, and output them to the specified output directory.
//!
//! #### Example
//!
//! ```rust
//! use rvpacker_txt_rs_lib::{Writer, FileFlags, EngineType};
//!
//! let mut writer = Writer::new();
//! writer.set_flags(FileFlags::Map | FileFlags::Other);
//! writer.write("C:/Game/Data", "C:/Game/translation", "C:/Game/output", EngineType::VXAce);
//! ```
//!
//! ### `purge` module
//!
//! `purge` module provides the `Purger` struct, along with its `PurgerBuilder` builder version.
//!
//! `Purger` struct is used to purge the translation from the `.txt` files based on settings, and output them to the specified output directory.
//!
//! #### Example
//!
//! ```rust
//! use rvpacker_txt_rs_lib::{Purger, FileFlags, EngineType};
//!
//! let mut purger = Purger::new();
//! purger.set_flags(FileFlags::Map | FileFlags::Other);
//! purger.write("C:/Game/Data", "C:/Game/translation", EngineType::VXAce);
//! ```
//!
//! ### `json` module
//!
//! Currently broken.
//!
//! ## License
//!
//! Project is licensed under WTFPL.
//!

#![allow(clippy::collapsible_else_if)]
#![deny(clippy::use_self)]
#![deny(clippy::redundant_clone)]
#![deny(clippy::branches_sharing_code)]

mod bases;
mod functions;

pub mod constants;
pub mod json;
pub mod purge;
pub mod read;
pub mod types;
pub mod write;

pub use functions::{get_engine_extension, read_to_string_without_bom};
pub use purge::{Purger, PurgerBuilder};
pub use read::{Reader, ReaderBuilder};
pub use types::{EngineType, FileFlags, GameType, ReadMode};
pub use write::{Writer, WriterBuilder};
