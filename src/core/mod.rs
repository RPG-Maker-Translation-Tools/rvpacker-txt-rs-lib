//! Building blocks for reading, writing and purging RPG Maker files.
//!
//! [`Base`] holds the state shared by every file kind - mode, flags, the parsed
//! translation, the accumulated output - and each file kind wraps it:
//! [`MapBase`], [`OtherBase`], [`SystemBase`], [`ScriptBase`] and [`PluginBase`].
//!
//! For the usual case of processing a whole game directory, use
//! [`Processor`](crate::Processor) instead; it drives all of these and handles
//! the file system.

mod base;
mod file;
mod list;
mod map;
mod other;
mod plugin;
mod script;
mod system;
mod text;
mod translation;

pub use base::Base;
pub use file::{
    filter_maps, filter_other, get_ini_title, get_system_title, parse_ignore,
    parse_rpgm_file,
};
pub use map::MapBase;
pub use other::OtherBase;
pub use plugin::PluginBase;
pub use script::ScriptBase;
pub use system::SystemBase;
pub use text::latinize_string;

pub(crate) use text::{
    CustomReplace, TranslationLine, push_entries, push_metadata,
    split_translation_line, string_is_only_symbols,
};

pub(crate) use text::ends_with_if_index;
pub(crate) use translation::FlushedLines;

use crate::types::CommentPos;

/// Index of the display-name slot in a metadata [`Comments`](crate::Comments).
pub(crate) const DISPLAY_NAME_POS: usize = CommentPos::DisplayName as usize;
