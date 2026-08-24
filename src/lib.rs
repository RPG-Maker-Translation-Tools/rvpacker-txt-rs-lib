#![warn(clippy::all, clippy::pedantic)]
#![allow(clippy::needless_doctest_main)]
#![allow(clippy::cast_possible_truncation)]
#![allow(clippy::cast_possible_wrap)]
#![allow(clippy::cast_sign_loss)]
#![allow(clippy::deref_addrof)]
#![allow(static_mut_refs)]
#![doc = include_str!("../README.md")]

use std::sync::LazyLock;

pub(crate) static mut LINE_SEPARATOR: &'static str = DEFAULT_LINE_SEPARATOR;
pub(crate) static mut LINE_BREAK: &'static str = DEFAULT_LINE_BREAK;
pub(crate) static mut COMMENT_PREFIX: &'static str = DEFAULT_COMMENT_PREFIX;

pub(crate) static mut ID_COMMENT: LazyLock<String> = LazyLock::new(|| unsafe { format!("{COMMENT_PREFIX}ID") });
pub(crate) static mut NAME_COMMENT: LazyLock<String> = LazyLock::new(|| unsafe { format!("{COMMENT_PREFIX}NAME") });

pub(crate) static mut EVENT_ID_COMMENT: LazyLock<String> =
    LazyLock::new(|| unsafe { format!("{COMMENT_PREFIX}EVENT ID") });
pub(crate) static mut EVENT_NAME_COMMENT: LazyLock<String> =
    LazyLock::new(|| unsafe { format!("{COMMENT_PREFIX}EVENT NAME") });
pub(crate) static mut EVENT_POS_COMMENT: LazyLock<String> =
    LazyLock::new(|| unsafe { format!("{COMMENT_PREFIX}EVENT POS") });

pub(crate) static mut MAP_ORDER_COMMENT: LazyLock<String> =
    LazyLock::new(|| unsafe { format!("{COMMENT_PREFIX}ORDER") });
pub(crate) static mut MAP_DISPLAY_NAME_COMMENT_PREFIX: LazyLock<String> =
    LazyLock::new(|| unsafe { format!("{COMMENT_PREFIX}IN-GAME DISPLAYED NAME: ") });
pub(crate) static mut IGNORE_ENTRY_COMMENT: LazyLock<String> =
    LazyLock::new(|| unsafe { format!("{COMMENT_PREFIX}Ignore Entry") });

pub(crate) static mut GLOB_ENTRY_COMMENT: LazyLock<String> =
    LazyLock::new(|| unsafe { format!("{COMMENT_PREFIX}Glob") });

pub(crate) static mut SCRIPT_COMMENT: LazyLock<String> = LazyLock::new(|| unsafe { format!("{COMMENT_PREFIX}SCRIPT") });

pub fn set_line_separator(sep: &'static str) {
    unsafe {
        LINE_SEPARATOR = sep;
    }
}

pub fn set_line_break(brk: &'static str) {
    unsafe {
        LINE_BREAK = brk;
    }
}

pub fn set_comment_prefix(prefix: &'static str) {
    unsafe {
        COMMENT_PREFIX = prefix;

        ID_COMMENT = LazyLock::new(|| format!("{COMMENT_PREFIX}ID"));
        NAME_COMMENT = LazyLock::new(|| format!("{COMMENT_PREFIX}NAME"));

        EVENT_ID_COMMENT = LazyLock::new(|| format!("{COMMENT_PREFIX}EVENT ID"));
        EVENT_NAME_COMMENT = LazyLock::new(|| format!("{COMMENT_PREFIX}EVENT NAME"));
        EVENT_POS_COMMENT = LazyLock::new(|| format!("{COMMENT_PREFIX}EVENT POS"));

        MAP_ORDER_COMMENT = LazyLock::new(|| format!("{COMMENT_PREFIX}ORDER"));
        MAP_DISPLAY_NAME_COMMENT_PREFIX = LazyLock::new(|| format!("{COMMENT_PREFIX}IN-GAME DISPLAYED NAME: "));
        IGNORE_ENTRY_COMMENT = LazyLock::new(|| format!("{COMMENT_PREFIX}Ignore Entry"));

        GLOB_ENTRY_COMMENT = LazyLock::new(|| format!("{COMMENT_PREFIX}Glob"));

        SCRIPT_COMMENT = LazyLock::new(|| format!("{COMMENT_PREFIX}SCRIPT"));
    }
}

pub fn get_line_separator() -> &'static str {
    unsafe { LINE_SEPARATOR }
}

pub fn get_line_break() -> &'static str {
    unsafe { LINE_BREAK }
}

pub fn get_comment_prefix() -> &'static str {
    unsafe { COMMENT_PREFIX }
}

pub(crate) fn get_id_comment() -> &'static str {
    unsafe { &ID_COMMENT }
}

pub(crate) fn get_name_comment() -> &'static str {
    unsafe { &NAME_COMMENT }
}

pub(crate) fn get_event_id_comment() -> &'static str {
    unsafe { &EVENT_ID_COMMENT }
}

pub(crate) fn get_event_name_comment() -> &'static str {
    unsafe { &EVENT_NAME_COMMENT }
}

pub(crate) fn get_event_pos_comment() -> &'static str {
    unsafe { &EVENT_POS_COMMENT }
}

pub(crate) fn get_map_order_comment() -> &'static str {
    unsafe { &MAP_ORDER_COMMENT }
}

pub(crate) fn get_map_display_name_comment_prefix() -> &'static str {
    unsafe { &MAP_DISPLAY_NAME_COMMENT_PREFIX }
}

pub(crate) fn get_ignore_entry_comment() -> &'static str {
    unsafe { &IGNORE_ENTRY_COMMENT }
}

pub(crate) fn get_glob_entry_comment() -> &'static str {
    unsafe { &GLOB_ENTRY_COMMENT }
}

pub(crate) fn get_script_comment() -> &'static str {
    unsafe { &SCRIPT_COMMENT }
}

pub mod constants;
pub mod core;
pub mod json;
pub mod processors;
pub mod serde;
pub mod types;

pub use constants::{
    DEFAULT_COMMENT_PREFIX, DEFAULT_LINE_BREAK, DEFAULT_LINE_SEPARATOR, RVPACKER_IGNORE_FILE, RVPACKER_METADATA_FILE,
};
pub use core::{filter_maps, filter_other, get_ini_title, get_system_title, parse_ignore};
pub use processors::Processor;
pub use types::*;
