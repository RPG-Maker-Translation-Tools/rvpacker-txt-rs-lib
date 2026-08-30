//! Building blocks for reading, writing and purging RPG Maker files.
//!
//! [`Base`] holds the state shared by every file kind - mode, flags, the parsed
//! translation, the accumulated output - and exposes one method per file kind:
//! [`Base::process_map`], [`Base::process_other`], [`Base::process_system`],
//! [`Base::process_scripts`] and [`Base::process_plugins`].
//!
//! Maps are the one kind processed as a run rather than one shot, because they
//! share a single translation file: [`Base::begin_maps`], then
//! [`Base::process_map`] per file, then [`Base::finish_maps`].
//!
//! For the usual case of processing a whole game directory, use
//! [`Processor`](crate::Processor) instead; it drives all of these and handles
//! the file system.

mod base;
pub mod entity;
mod file;
mod ignore;
mod list;
mod map;
mod other;
mod plugin;
mod plugins;
/// `rm2000` to avoid colliding with the `rm2k` crate name
mod rm2000;
mod script;
mod system;
pub mod text;
mod translation;

pub use base::Base;
pub use entity::{PathSegment, get_entity_values};
pub use file::{
    filter_maps, filter_other, filter_rm2k_maps, get_ini_title, get_ini_title_rm2k, get_system_title, parse_ignore,
};
pub use ignore::{Glob, IgnoreEntry};

pub(crate) use crate::marshal_compat::parse_rpgm_file;
pub(crate) use text::{
    CustomReplace, TranslationLine, ends_with_if_index, push_entries, push_metadata, split_translation_line,
    string_is_only_symbols,
};
pub(crate) use translation::FlushedLines;

use crate::types::CommentPos;

/// Index of the display-name slot in a metadata [`Comments`](crate::Comments).
pub(crate) const DISPLAY_NAME_POS: usize = CommentPos::DisplayName as usize;
