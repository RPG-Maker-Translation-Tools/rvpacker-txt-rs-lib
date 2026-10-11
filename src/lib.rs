#![warn(clippy::all, clippy::pedantic, clippy::nursery)]
#![allow(clippy::cast_possible_truncation)]
#![allow(clippy::cast_sign_loss)]
#![doc = include_str!("../README.md")]

use std::{
    ptr,
    sync::{
        LazyLock, Mutex, PoisonError,
        atomic::{AtomicPtr, Ordering},
    },
};

/// The separators and the comment lines derived from the comment prefix.
#[derive(Clone)]
struct Config {
    line_separator: &'static str,
    line_break: &'static str,
    comment_prefix: &'static str,

    id_comment: String,
    name_comment: String,
    event_id_comment: String,
    event_name_comment: String,
    event_pos_comment: String,
    map_order_comment: String,
    map_display_name_comment_prefix: String,
    ignore_entry_comment: String,
    glob_entry_comment: String,
    script_comment: String,
}

impl Config {
    fn new(line_separator: &'static str, line_break: &'static str, comment_prefix: &'static str) -> Self {
        let with_prefix = |tail: &str| format!("{comment_prefix}{tail}");

        Self {
            line_separator,
            line_break,
            comment_prefix,
            id_comment: with_prefix("ID"),
            name_comment: with_prefix("NAME"),
            event_id_comment: with_prefix("EVENT ID"),
            event_name_comment: with_prefix("EVENT NAME"),
            event_pos_comment: with_prefix("EVENT POS"),
            map_order_comment: with_prefix("ORDER"),
            map_display_name_comment_prefix: with_prefix("IN-GAME DISPLAYED NAME: "),
            ignore_entry_comment: with_prefix("Ignore Entry"),
            glob_entry_comment: with_prefix("Glob"),
            script_comment: with_prefix("SCRIPT"),
        }
    }
}

static DEFAULT_CONFIG: LazyLock<Config> =
    LazyLock::new(|| Config::new(DEFAULT_LINE_SEPARATOR, DEFAULT_LINE_BREAK, DEFAULT_COMMENT_PREFIX));

/// Null until a setter runs, then the newest `Config`. Configs are published once and never freed or
/// mutated, so references to them (and to the strings inside) stay valid for the rest of the process.
static CONFIG: AtomicPtr<Config> = AtomicPtr::new(ptr::null_mut());

/// Serializes setters, so two of them cannot both start from the same old config and lose an update.
static CONFIG_WRITE: Mutex<()> = Mutex::new(());

fn config() -> &'static Config {
    let current = CONFIG.load(Ordering::Acquire);

    if current.is_null() {
        &DEFAULT_CONFIG
    } else {
        // SAFETY: the only non-null value ever stored in `CONFIG` is a `Box::leak`ed `Config` from
        // `update_config`, which is never freed or mutated afterwards.
        unsafe { &*current }
    }
}

/// Publishes a changed copy of the current config. Each call leaks one small `Config`: setters are
/// meant to run at startup, and leaking is what keeps `&'static str` results of earlier getter calls
/// valid.
fn update_config(change: impl FnOnce(&mut Config)) {
    let _guard = CONFIG_WRITE.lock().unwrap_or_else(PoisonError::into_inner);

    let mut next = config().clone();
    change(&mut next);

    CONFIG.store(Box::leak(Box::new(next)), Ordering::Release);
}

pub fn set_line_separator(sep: &'static str) {
    update_config(|config| config.line_separator = sep);
}

pub fn set_line_break(brk: &'static str) {
    update_config(|config| config.line_break = brk);
}

pub fn set_comment_prefix(prefix: &'static str) {
    update_config(|config| *config = Config::new(config.line_separator, config.line_break, prefix));
}

#[must_use]
pub fn get_line_separator() -> &'static str {
    config().line_separator
}

#[must_use]
pub fn get_line_break() -> &'static str {
    config().line_break
}

#[must_use]
pub fn get_comment_prefix() -> &'static str {
    config().comment_prefix
}

pub(crate) fn get_id_comment() -> &'static str {
    &config().id_comment
}

pub(crate) fn get_name_comment() -> &'static str {
    &config().name_comment
}

pub(crate) fn get_event_id_comment() -> &'static str {
    &config().event_id_comment
}

pub(crate) fn get_event_name_comment() -> &'static str {
    &config().event_name_comment
}

pub(crate) fn get_event_pos_comment() -> &'static str {
    &config().event_pos_comment
}

pub(crate) fn get_map_order_comment() -> &'static str {
    &config().map_order_comment
}

pub(crate) fn get_map_display_name_comment_prefix() -> &'static str {
    &config().map_display_name_comment_prefix
}

pub(crate) fn get_ignore_entry_comment() -> &'static str {
    &config().ignore_entry_comment
}

pub(crate) fn get_glob_entry_comment() -> &'static str {
    &config().glob_entry_comment
}

pub(crate) fn get_script_comment() -> &'static str {
    &config().script_comment
}

pub mod constants;
pub mod core;
pub mod json;
pub(crate) mod marshal_compat;
pub mod processors;
pub mod save;
pub mod serde;
pub mod types;

pub use constants::{
    DEFAULT_COMMENT_PREFIX, DEFAULT_LINE_BREAK, DEFAULT_LINE_SEPARATOR, RVPACKER_IGNORE_FILE, RVPACKER_METADATA_FILE,
};
pub use core::{filter_maps, filter_other, get_ini_title, get_ini_title_rm2k, get_system_title, parse_ignore};
pub use processors::Processor;
pub use types::*;
