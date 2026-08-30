use super::*;
use crate::{
    CommentPos, ProcessedData,
    get_event_id_comment, get_event_name_comment, get_event_pos_comment, get_line_separator,
    types::{Error, RPGMFileType, TranslationEntry, TranslationMap},
};
use rm2k::{engine::SaveOpt, file, rpg::TreeMap};
use smallvec::SmallVec;

impl Base {
    /// Prepares this base to process a run of `MapNNNN.lmu` files.
    ///
    /// Mirrors [`Base::begin_maps`] - maps share one `maps.txt`, so they're
    /// processed as a run: call this, then [`Base::process_rm2k_map`] per file,
    /// then [`Base::finish_rm2k_maps`].
    pub fn begin_rm2k_maps(&mut self) {
        self.reset();
        self.file_type = RPGMFileType::Rm2kMap;
    }

    /// Returns the translation data accumulated after processing multiple maps.
    ///
    /// Returns the actual data only with [`Mode::Read`] or [`Mode::Purge`].
    pub fn finish_rm2k_maps(&mut self) -> ProcessedData {
        self.finish_translation()
    }

    /// Processes one `MapNNNN.lmu` file's content.
    ///
    /// # Parameters
    ///
    /// - `filename` - Filename of the file that's being processed.
    /// - `content` - Content of the file that's being processed.
    /// - `tree` - The parsed `RPG_RT.lmt` map tree, supplying each map's name and
    ///   order - the rm2k counterpart of `MapInfos.*`.
    /// - `translation` - Contents of the translation file corresponding to maps.
    ///   Isn't used with [`Mode::Read`]. Requires to be set with any other [`Mode`].
    ///
    /// # Errors
    ///
    /// - [`Error::Rm2kLoad`] - if unable to load the map data.
    /// - [`Error::NoTranslation`] - if mode is not [`Mode::Read`], and no translation was passed.
    pub fn process_rm2k_map(
        &mut self,
        filename: &str,
        content: &[u8],
        tree: &TreeMap<'_>,
        translation: Option<&str>,
    ) -> Result<Option<ProcessedData>, Error> {
        self.initialize_translation(translation)?;

        let id = Self::parse_map_id(filename);

        // A map absent from the tree is unused, same as a map missing from
        // `MapInfos` on MV/VX.
        let Some(map_info) = tree.maps.iter().find(|m| m.id == i32::from(id)) else {
            return Ok(None);
        };

        if self.get_translation_map(id).is_break() {
            return Ok(None);
        }

        let loaded = file::load_map(content)?;
        let mut map = loaded.value;

        if self.mode.is_read() {
            let name = self.decode_with_fallback(map_info.name.as_bytes());
            let order = tree
                .tree_order
                .iter()
                .position(|&i| i == map_info.id)
                .unwrap_or_default()
                .to_string();

            self.update_metadata(
                id,
                Vec::from([(CommentPos::Name, name.as_str()), (CommentPos::Order, order.as_str())]),
            );
        }

        let mut visited = false;

        for event in map.events.iter_mut() {
            if event.pages.iter().all(|page| page.event_commands.0.is_empty()) {
                continue;
            }

            if self.map_events {
                let event_name = self.decode_with_fallback(event.name.as_bytes());

                self.flush_translation(id);

                self.output.accumulated.push((
                    id,
                    SmallVec::default(),
                    FlushedLines::EMPTY,
                    TranslationMap::from_iter([(
                        String::new(),
                        TranslationEntry {
                            comments: vec![
                                format!(
                                    "{comment}{sep}{event_id}",
                                    sep = get_line_separator(),
                                    comment = get_event_id_comment(),
                                    event_id = event.id
                                ),
                                format!(
                                    "{comment}{sep}{event_name}",
                                    sep = get_line_separator(),
                                    comment = get_event_name_comment()
                                ),
                                format!(
                                    "{comment}{sep}{x},{y}",
                                    sep = get_line_separator(),
                                    comment = get_event_pos_comment(),
                                    x = event.x,
                                    y = event.y
                                ),
                            ],
                            translation: String::new(),
                        },
                    )]),
                ));
            }

            for page in event.pages.iter_mut() {
                if page.event_commands.0.is_empty() {
                    continue;
                }

                visited = true;
                self.process_rm2k_event_list(&mut page.event_commands);
            }
        }

        if !visited {
            return Ok(None);
        }

        if self.mode.is_write() {
            let mut bytes = Vec::new();
            let opt = SaveOpt { preserve_header: true };

            // SAFETY-equivalent: `Vec<u8>` is a statically infallible `Sink`.
            file::save_map(&map, &mut bytes, self.rm2k_engine, opt, loaded.header).unwrap();

            Ok(Some(self.finish_rm2k(bytes)))
        } else {
            self.flush_translation(id);
            Ok(None)
        }
    }
}
