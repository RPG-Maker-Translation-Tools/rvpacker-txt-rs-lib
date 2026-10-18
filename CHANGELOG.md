## 19.0.0

### Breaking

- `core::Base::new` uses `DuplicateMode::Allow`, as `Processor` does. Set `duplicate_mode` to `Remove` if you relied on the old default.
- Map and event ids are `u32`, so `Processor::skip_maps`, `Processor::skip_events`, `Base::skip_maps`, `Base::skip_events` and `Base::parse_map_id` use `u32` instead of `u16`. Ids above 65,535 work.
- `content_hash` always uses rapidhash instead of gxhash, so every hash you stored from an earlier version is invalid: the first append after upgrading re-reads every file once, and the new hashes must be stored in place of the old.
- `Mode::Write` with no `output_path` writes to `./output` instead of the current directory.
- `read_encoding: None` used to guess each text's encoding, for XP/VX and RM2K/2003. It now means UTF-8 everywhere, and nothing is guessed. That includes `get_rm2k_entity_name` and `get_rm2k_system_graphic_name`, which decode UTF-8 for `None`. Text that doesn't decode fails with `Error::InvalidText`.
- `Error` has seven new variants: `InvalidText`, `UnmappableText`, `InFile`, `DuplicateSection`, `MapsNotBegun`, `UnsupportedWriteEncoding` and `UnsupportedEngine`. Exhaustive `match` statements need new arms.
- `core::Base::set_write_encoding` returns `Result`, and fails with `Error::UnsupportedWriteEncoding` for an encoding that can only decode, such as UTF-16. `Processor::process` returns the same error for such a `write_encoding`.
- `core::get_entity_values` takes a `read_encoding` after the engine and decodes untagged Marshal text with it.
- `json::generate_file` and `json::generate` take a `read_encoding` for `Scripts`, and `json::write` takes a `write_encoding`. `json::write` with `EngineType::RM2K` returns `Error::UnsupportedEngine` instead of panicking.
- A translation file with two sections of the same id fails with `Error::DuplicateSection`, instead of the second replacing the first.
- The sections of `Scripts` and `plugins.js` are matched to the game's scripts and plugins by name, not position, so inserting or reordering one no longer shifts the translations after it. New entries get ids past the existing ones. A translation file made by an earlier version is matched by its `<!>NAME` lines.
- A trailing `if(...)` on XP/VX/VX Ace text is part of the text instead of being cut off, so existing translations of such lines no longer match. `core::text::ends_with_if_index` is removed.
- VX Ace `\et[n]` speaker codes stay in the source text instead of being cut off, so existing translations of those lines no longer match. Append, then translate them again.
- `set_line_separator`, `set_line_break` and `set_comment_prefix` panic on an empty value or one with a line feed.

### Translation files

- A line before the first `<!>ID` line is logged instead of ignored silently.
- A file saved with a byte order mark, as Notepad does, no longer loses its first section.
- Append keeps what the game no longer has, instead of dropping it: translations of removed entries (under their own `<!>ID` header, not the previous section's), whole sections of removed entries and maps (after the others, unless `BaseFlags::SkipObsolete` is set), and comments after a section's last entry or in a section without entries.
- Files are written through a temporary file, flushed, then renamed, so an interrupted write keeps the previous file. This applies to translation files, game data files and `json` output.

### Reading and writing

- XP/VX text, map names and display names that happen to be valid UTF-8 honour `read_encoding`. A VX Ace string's declared encoding wins over bytes that happen to be valid UTF-8.
- Text that can't be encoded in `write_encoding` fails the file with `Error::UnmappableText`, instead of writing `&#NNNN;`.
- XP/VX message boxes, choices, event text and script names are written as raw bytes, not encoding-tagged strings, and `write_encoding` applies to them.
- Fixed untranslated event text being erased on write, a map's display name being blanked when it has no translation, `shop_talk` lines losing the space after `=`, and a translation that already starts with a VX Ace speaker prefix getting it twice.
- Fixed a map or entry whose only text is a line shared with another one (duplicates removed) being left untranslated on write.
- Fixed the last message box of every XP event list being skipped, and an entry with no name not being processed.
- The game title in `system.txt` is written back for engines that keep it in the system file. A `game_title` still wins, and is written in `write_encoding`.

### Ignore file

- Fixed `BaseFlags::CreateIgnore` rewriting `.rvpacker-ignore` outside a purge.
- Fixed RM2K terms entries not matching when duplicates are removed.
- Fixed a purge with duplicates removed and `BaseFlags::Ignore` writing a collapsed section next to the per-id ones.
- Fixed `?` in a glob matching one byte instead of one character, and a literal `*` in the text consuming a wildcard.
- Lines are listed in a stable order.

### Scripts and plugins

- `plugins.js` is written back with its layout and comments, changing only the translated strings. A `=` in a comment above the list, or a missing `var $plugins`, no longer breaks it.
- Plugin parameters nested more than 256 levels fail with `Error::InvalidPlugins`. They used to fail at 128 levels with `Error::JsonParse`.
- Fixed a quote in a translated script string ending the Ruby literal early. It is escaped now.
- Fixed `plugins.js` not being found for source paths `.` and `..`, and a panic when the path has no parent.

### MV/MZ

- Files are written back with their original formatting, changing only the translated strings, instead of minified, and keep a byte order mark.
- Fixed `MapNNN.json` files left over from deleted maps (`null` in `MapInfos`) being processed.

### RM2K/2003

- Hero names and titles set by events (`Change Hero Name`, `Change Hero Title`) are extracted and translated.
- Message boxes that hold only symbols are no longer extracted, as on the other engines.
- `skip_maps`, `skip_events` and the ignore flags (`BaseFlags::Ignore`, `CreateIgnore`) now work.
- Fixed append dropping the translations of unchanged maps, user comments under a section header moving or duplicating on each append, two consecutive message boxes merging into one entry, and `maps.txt` losing a map's `<!>ID` with `map_events`.
- After a write, `Processor::game_title` holds the translated title from `terms.txt`, for `RPG_RT.ini`. It is no longer forgotten after the first read on the same `Base`.
- `RPG_RT.ldb` and `RPG_RT.lmt` load errors name the file.
- Added `core::Rm2kDatabase`, which loads `RPG_RT.ldb` once for many name lookups.

### Processor and core

- `Processor::process` returns data file parse errors as `Error::InFile`, naming the file. A purge no longer creates an empty output directory.
- `Processor::process` no longer empties `skip_maps` and `skip_events`, so a second call keeps them. A failed run puts `hashes` back, so the next append doesn't skip files that never reached a translation.
- `Base::process_map` before `begin_maps` returns `Error::MapsNotBegun` instead of panicking.
- Fixed `set_line_break` having no effect on `denormalize` once it had been used.
- `filter_other` no longer picks up `Statistics.json` or `Items_backup.json`. The `Game.ini` title is no longer read from a longer key such as `TitleFont=`.
- Added a `gxhash` feature: without it the crate builds without AES-NI/Armv8 crypto and maps and sets use rapidhash.

### json

- `json::generate` checks `Scripts.rb`, not `Scripts.json`, before overwriting without `force`, skips stray files such as `desktop.ini`, and takes separate types for its source and output paths (as do `generate_rm2k` and `write_rm2k`).
- `Scripts.rb` round trips exactly: CRLF line endings and empty scripts are no longer turned into LF and a lone newline. JSON made by an earlier version loses one trailing newline per script that ended with one.
- `json::write_rm2k` warns when `RPG_RT.ldb.json` is missing, since the maps are then written as RPG Maker 2000.

### save

- A file without the `LcfSaveData` header fails with `Error::InvalidSave` instead of reading as an empty RM2K/2003 save.
- `get_save_summary` honours the encoding a VX Ace string declares.

### serde

- CSV and XLSX import fail when a row's `translation_count` is not a number. XML import fails on an `<entry>` with no `<source>`, and keeps comment text that isn't wrapped in CDATA.
- A carriage return at the end of the text survives an export and import.
- The CSV export puts a `'` in front of cells a spreadsheet would read as a formula (starting with `=`, `+`, `-`, `@`, a tab or a carriage return), and the import removes it.
- Fixed an XML export of a comment containing `]]>` being malformed.

### Performance and robustness

- Files are processed in parallel on rayon's pool (wrap calls in `rayon::ThreadPool::install` to limit threads).
- Writing no longer parses maps that have no translation.
- Fixed panics on malformed event commands and event lists in map and other data files. Such commands are skipped.
