//! RPG Maker 2000/2003 support, via `rm2k-lib`.
//!
//! Structurally different enough from the JSON/Marshal engines - typed structs
//! with `Cow<[u8]>` text instead of a dynamic cursor, one `.ldb` database file
//! instead of per-entity files, no `Scripts`/`Plugins` - that it doesn't route
//! through [`crate::marshal_compat::Value`] at all. These modules implement
//! their own extraction/injection directly against `rm2k-lib`'s structs, while
//! reusing everything engine-agnostic on [`super::Base`]: translation
//! bookkeeping, ignore files, duplicate handling, txt serialization.

mod database;
mod event_list;
mod map;

use super::*;
use crate::ProcessedData;
use std::borrow::Cow;

impl Base {
    /// Returns the RPG Maker data bytes if `self.mode` is [`Mode::Write`], else
    /// returns translation data.
    ///
    /// The RM2K counterpart of [`Base::finish`]: there is no [`crate::marshal_compat::RpgmData`]
    /// to wrap since the caller already has the re-serialized bytes in hand.
    pub(super) fn finish_rm2k(&mut self, bytes: Vec<u8>) -> ProcessedData {
        if self.mode.is_write() {
            ProcessedData::RPGMData(bytes)
        } else {
            self.finish_translation()
        }
    }

    /// Processes one `Cow<[u8]>` text field shared by an rm2k struct: extracts
    /// it on read, or looks up and writes its translation back on write.
    ///
    /// Skips empty/symbols-only text the same way [`string_is_only_symbols`] guards
    /// every other engine's simple fields.
    pub(super) fn process_rm2k_string_field(&mut self, field: &mut rm2k::field::DbStr<'_>) {
        if field.is_empty() {
            return;
        }

        let text = self.decode_with_fallback(field.as_bytes());
        let trimmed = text.trim();

        if trimmed.is_empty() || string_is_only_symbols(trimmed) {
            return;
        }

        if self.mode.is_write() {
            if let Some(translated) = self.get_key(&text) {
                let bytes = self.encode_with_fallback(&translated.translation);
                *field = rm2k::field::DbStr::from_vec(bytes);
            }
        } else {
            self.insert_string(Cow::Owned(text));
        }
    }
}

/// Emits a public `process_*` method that walks an indexed rm2k entity list,
/// translating `name` (used for the section's metadata comment too) plus
/// whichever other text fields the caller lists - the same shape as
/// `core::other::process_array`'s `variable_pairs` table, just generated per
/// entity kind instead of shared, since rm2k's entities don't share a common
/// field set the way MV/VX's do.
///
/// Unlike `process_other`, translations are read back per section rather than
/// per physical file: `RPG_RT.ldb` bundles every entity kind into one file, so
/// each section resets and re-initializes `self.translation` from its own
/// `translation` argument, exactly as if it were its own file. On
/// [`Mode::Write`] the entity list is mutated in place and `Ok(None)` is
/// returned - the caller collects every section's mutations onto one shared
/// [`rm2k::rpg::Database`] and re-serializes it once, after every section ran.
macro_rules! rm2k_entity_pass {
    ($(#[$meta:meta])* $fn_name:ident, $file_type:expr, $ty:ty, { $($field:ident),+ $(,)? }) => {
        impl Base {
            $(#[$meta])*
            pub fn $fn_name(
                &mut self,
                list: &mut rm2k::field::List<$ty>,
                translation: Option<&str>,
            ) -> Result<Option<ProcessedData>, Error> {
                self.reset();
                self.file_type = $file_type;
                self.initialize_translation(translation)?;

                let mut processed = false;

                for entity in list.iter_mut() {
                    let id = entity.id as u16;

                    if self.get_translation_map(id).is_break() {
                        if self.mode.is_purge() {
                            processed = true;
                        }

                        continue;
                    }

                    processed = true;

                    let name_text = self.decode_with_fallback(entity.name.as_bytes());
                    self.update_metadata(id, Vec::from([(CommentPos::Name, name_text.as_str())]));

                    $( self.process_rm2k_string_field(&mut entity.$field); )+

                    self.flush_translation(id);
                }

                if !processed {
                    return Ok(None);
                }

                if self.mode.is_write() {
                    Ok(None)
                } else {
                    Ok(Some(self.finish_translation()))
                }
            }
        }
    };
}

pub(crate) use rm2k_entity_pass;
