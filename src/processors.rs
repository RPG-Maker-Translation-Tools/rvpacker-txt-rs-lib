use crate::{
    ProcessedData, RPGMFileType,
    constants::RVPACKER_IGNORE_FILE,
    core::{Base, MapWalk, Rm2kMapWalk, Rm2kTree, filter_maps, filter_other, filter_rm2k_maps, parse_ignore},
    marshal_compat::{RpgmData, parse_rpgm_file},
    types::{BaseFlags, DuplicateMode, EngineType, Error, FileFlags, Mode},
    types::{HashMap, HashSet},
};
use log::{debug, info, warn};
use rayon::prelude::*;
use rm2k::engine::{Engine, SaveOpt};
use std::{
    fs::{DirEntry, File, create_dir_all, read, read_dir, read_to_string, remove_file, rename},
    io::Write,
    mem::take,
    ops::{ControlFlow, Range},
    path::{Path, PathBuf},
    str::FromStr,
};

/// Reads, writes and purges RPG Maker translation files, handling all system calls.
///
/// This is a plain struct with public fields: set the ones you need and call
/// [`Processor::process`]. [`Processor::mode`] decides which operation runs, so
/// the same struct covers all three.
///
/// Files are read, parsed and walked on rayon's global thread pool, with the
/// results recorded in file order, so the output is the same on any number of
/// threads. To limit the threads, call [`Processor::process`] inside
/// `rayon::ThreadPool::install`.
///
/// # Example
///
/// ```no_run
/// use rvpacker_txt_rs_lib::{EngineType, FileFlags, Mode, Processor};
///
/// fn main() -> Result<(), Box<dyn std::error::Error>> {
///     let mut processor = Processor {
///         mode: Mode::Read { append: false, force: true },
///         file_flags: FileFlags::Map | FileFlags::other(),
///         ..Default::default()
///     };
///
///     processor.process(
///         EngineType::VXAce,
///         "C:/Game/Data",
///         "C:/Game/translation",
///         None,
///     )?;
///
///     // Writing back needs an output directory and the same options.
///     processor.mode = Mode::Write;
///     processor.process(
///         EngineType::VXAce,
///         "C:/Game/Data",
///         "C:/Game/translation",
///         Some("C:/Game/output".as_ref()),
///     )?;
///
///     Ok(())
/// }
/// ```
#[derive(Default)]
pub struct Processor {
    /// Which operation to perform. See [`Mode`].
    pub mode: Mode,

    /// Which RPG Maker files to process. See [`FileFlags`].
    pub file_flags: FileFlags,

    /// How to process the text. See [`BaseFlags`].
    ///
    /// Must match the flags used on read when writing or purging.
    pub flags: BaseFlags,

    /// What to do with duplicate lines. See [`DuplicateMode`].
    ///
    /// Must match the mode used on read when writing or purging.
    pub duplicate_mode: DuplicateMode,

    /// Overrides the game title in the system file.
    ///
    /// XP/VX/VX Ace games may only carry their title in `Game.ini`, which is not
    /// necessarily UTF-8. Decode it yourself - see
    /// [`get_ini_title`](crate::get_ini_title) - and set it here. On read it is
    /// recorded as the title's source text. On write it replaces the title in the
    /// system file, so set it to the translated title (an empty one leaves the
    /// file's title alone). RM2K differs: a write replaces it with its translation
    /// from `terms.txt`, since only the caller can put it into `RPG_RT.ini`.
    pub game_title: String,

    /// Content hashes keyed by lowercased file stem.
    ///
    /// With [`Mode::Read`] in append mode (without `force`), a file whose hash matches is skipped as unchanged. Set these
    /// from a previous read to enable that, and read them back afterwards to persist them.
    pub hashes: HashMap<String, u64>,

    /// Map ids to leave untouched. On append and purge the existing translation of these maps is carried over as it is;
    /// on a first read and on write they are skipped.
    pub skip_maps: Vec<u32>,

    /// Per-file entry ids to leave untouched, like [`Processor::skip_maps`]. Has no effect on [`RPGMFileType::Map`].
    pub skip_events: Vec<(RPGMFileType, Vec<u32>)>,

    /// Whether to record event metadata - id, name and coordinates - before each
    /// event's text.
    pub map_events: bool,

    /// The codepage untagged source text is decoded in, UTF-8 if [`None`].
    ///
    /// XP/VX (pre-1.9 Ruby) and RM2K text carries no in-file encoding
    /// indicator. For XP/VX the default is almost always right: RGSS sets
    /// Ruby's `$KCODE` to `"UTF8"`, so the engine stores and reads its text as
    /// UTF-8. There can be exceptions, such as a game whose text was saved in a
    /// legacy codepage - then set this from a codepage the caller already knows.
    /// RM2K text is usually in a legacy codepage, so set it (e.g. from `RPG_RT.ini`). Text that isn't valid in it
    /// fails the run with [`Error::InvalidText`]; nothing is guessed.
    ///
    /// Independent of [`Processor::write_encoding`] - see that field's docs
    /// for why reusing one setting for both directions would be wrong.
    pub read_encoding: Option<&'static encoding_rs::Encoding>,

    /// Forces *encoding* of translated text written back to the source
    /// format, instead of always writing plain UTF-8.
    ///
    /// `None` (the default) always writes UTF-8, which is the only choice
    /// that cannot silently corrupt a translation written in a different
    /// script than the source game's own text. For XP and VX that is also what
    /// the engine expects: RGSS sets Ruby's `$KCODE` to `"UTF8"`, so it stores
    /// and reads its text as UTF-8. There can be exceptions, such as a game
    /// whose text is in a legacy codepage. RM2K/2003 has no such default and
    /// renders through the OS's legacy ANSI codepage. Override this only when
    /// the target game reads a legacy codepage and the translation stays within
    /// a script that codepage can represent - the player then also needs their
    /// system (or a locale emulator) set to that same codepage. See the "Text
    /// encoding" section of the crate documentation.
    pub write_encoding: Option<&'static encoding_rs::Encoding>,
}

impl Processor {
    /// Creates a [`Processor`] for `mode`, with every other option left at its default.
    ///
    /// # Example
    ///
    /// ```
    /// use rvpacker_txt_rs_lib::{Mode, Processor};
    ///
    /// let processor = Processor::new(Mode::Purge);
    /// ```
    #[must_use]
    pub fn new(mode: Mode) -> Self {
        Self {
            mode,
            ..Default::default()
        }
    }

    /// Runs the operation selected by [`Processor::mode`].
    ///
    /// # Parameters
    ///
    /// - `engine_type` - engine the source files belong to.
    /// - `source_path` - directory holding the RPG Maker data files.
    /// - `translation_path` - directory holding (or to hold) the `.txt` files.
    /// - `output_path` - where to write rebuilt data files. Only used by
    ///   [`Mode::Write`]. `None` means `./output`.
    ///
    /// # Errors
    ///
    /// - [`Error::Io`] - if any I/O operation fails.
    /// - [`Error::JsonParse`] - if parsing any JSON fails.
    /// - [`Error::MarshalLoad`] - if loading any Marshal byte stream fails.
    /// - [`Error::NoTranslation`] - if the mode needs a translation file and none was found.
    /// - [`Error::InFile`] - wraps a parse error of a data file with the file's name.
    /// - [`Error::InvalidTranslation`]/[`Error::DuplicateSection`] - if a translation file is malformed.
    /// - [`Error::InvalidText`]/[`Error::UnmappableText`] - if source text doesn't decode in [`Processor::read_encoding`],
    ///   or a translation doesn't encode in [`Processor::write_encoding`].
    /// - [`Error::UnsupportedWriteEncoding`] - if `write_encoding` can't be written, such as UTF-16.
    pub fn process(
        &mut self,
        engine_type: EngineType,
        source_path: impl AsRef<Path>,
        translation_path: impl AsRef<Path>,
        output_path: Option<&Path>,
    ) -> Result<(), Error> {
        // Hashes are recorded as files load, before the run is known to succeed. A caller that persists them after a
        // failed run would make the next append skip files whose text never reached a translation.
        let hashes = self.hashes.clone();
        let result = self.process_inner(
            engine_type,
            source_path.as_ref(),
            translation_path.as_ref(),
            output_path,
        );

        if result.is_err() {
            self.hashes = hashes;
        }

        result
    }

    /// A [`Base`] set up from this processor's options.
    fn new_base(&self, engine_type: EngineType) -> Result<Base, Error> {
        let mut base = Base::new(self.mode, engine_type);
        base.flags = self.flags;
        base.map_events = self.map_events;
        base.duplicate_mode = self.duplicate_mode;
        base.set_read_encoding(self.read_encoding);
        base.set_write_encoding(self.write_encoding)?;

        // The ignore file is only created by a purge.
        if !self.mode.is_purge() {
            base.flags.remove(BaseFlags::CreateIgnore);
        }

        Ok(base)
    }

    fn process_inner(
        &mut self,
        engine_type: EngineType,
        source_path: &Path,
        translation_path: &Path,
        output_path: Option<&Path>,
    ) -> Result<(), Error> {
        if self.file_flags.is_empty() {
            return Ok(());
        }

        let output_path = output_path.unwrap_or_else(|| Path::new(DEFAULT_OUTPUT_DIR));

        let mut base = self.new_base(engine_type)?;
        base.skip_events = self
            .skip_events
            .iter()
            .map(|(id, vec)| (*id, vec.iter().copied().collect::<HashSet<u32>>()))
            .collect();

        let mode = base.mode;
        let flags = base.flags;

        let mut ignore_file_path = PathBuf::new();

        if flags.intersects(BaseFlags::CreateIgnore | BaseFlags::Ignore) {
            ignore_file_path = translation_path.join(RVPACKER_IGNORE_FILE);
            read_ignore_file(&mut base, &ignore_file_path)?;
        }

        if engine_type == EngineType::RM2K {
            self.process_rm2k(&mut base, source_path, translation_path, output_path)?;

            if flags.contains(BaseFlags::CreateIgnore) {
                write_ignore_file(&mut base, &ignore_file_path)?;
            }

            return Ok(());
        }

        // Only a write touches the output directory; everything else lands in the translation one.
        let output_dir = if mode.is_write() { output_path } else { translation_path };

        create_dir_all(output_dir).map_err(|e| Error::Io(output_dir.to_path_buf(), e))?;

        let data_output_path = output_path.join(if engine_type.is_mvmz() { "data" } else { "Data" });

        if mode.is_write() {
            create_dir_all(&data_output_path).map_err(|e| Error::Io(data_output_path.clone(), e))?;
        }

        let run = Run {
            mode,
            extension: engine_type.extension(),
            source_path,
            translation_path,
            output_path,
            data_output_path,
            pre_msg: mode_pre_msg(mode),
            post_msg: mode_post_msg(mode),
        };

        let entries: Vec<DirEntry> = read_dir(source_path)
            .map_err(|e| Error::Io(source_path.to_path_buf(), e))?
            .flatten()
            .collect();

        if self.file_flags.contains(FileFlags::Map) {
            self.process_maps(&mut base, &run, engine_type, &entries)?;
        }

        if self.file_flags.intersects(FileFlags::other()) {
            self.process_other(&mut base, &run, engine_type, &entries)?;
        }

        if self.file_flags.contains(FileFlags::System) {
            let filename = format!("System.{}", run.extension);
            base.set_game_title(&self.game_title);

            self.process_single(
                &mut base,
                &run,
                "system.txt",
                run.source_path.join(&filename),
                &run.data_output_path.join(&filename),
                &filename,
                Base::process_system,
            )?;
        }

        if self.file_flags.contains(FileFlags::Scripts) {
            if engine_type.is_mvmz() {
                // `plugins.js` sits in `js`, next to the data directory.
                self.process_single(
                    &mut base,
                    &run,
                    "plugins.txt",
                    source_path.join("../js/plugins.js"),
                    &run.output_path.join("js/plugins.js"),
                    "plugins.js",
                    |base, content, translation| {
                        if mode.is_write() {
                            let js_output_path = run.output_path.join("js");
                            create_dir_all(&js_output_path).map_err(|e| Error::Io(js_output_path, e))?;
                        }

                        base.process_plugins(content, translation)
                    },
                )?;
            } else {
                let filename = format!("Scripts.{}", run.extension);

                self.process_single(
                    &mut base,
                    &run,
                    "scripts.txt",
                    run.source_path.join(&filename),
                    &run.data_output_path.join(&filename),
                    &filename,
                    Base::process_scripts,
                )?;
            }
        }

        if flags.contains(BaseFlags::CreateIgnore) {
            write_ignore_file(&mut base, &ignore_file_path)?;
        }

        Ok(())
    }

    /// Reads, walks and records every map, then writes the maps or `maps.txt`.
    fn process_maps(
        &mut self,
        base: &mut Base,
        run: &Run<'_>,
        engine_type: EngineType,
        entries: &[DirEntry],
    ) -> Result<(), Error> {
        let mode = run.mode;
        let translation_file_path = run.translation_path.join("maps.txt");

        if skip_translation_file(mode, &translation_file_path) {
            return Ok(());
        }

        let mapinfos_path = run.source_path.join(format!("MapInfos.{}", run.extension));
        let mapinfos = read(&mapinfos_path).map_err(|e| Error::Io(mapinfos_path, e))?;

        let translation = load_translation(mode, &translation_file_path)?;

        base.skip_maps = self.skip_maps.iter().copied().collect();

        base.begin_maps(&mapinfos, translation.as_deref())?;

        let maps: Vec<PathBuf> = filter_maps(entries.iter(), run.extension).map(DirEntry::path).collect();

        // Decided up front for every map, from the state before any of them is processed.
        let needs_parse: Vec<bool> = maps
            .iter()
            .map(|path| {
                let filename = path.file_name().and_then(|name| name.to_str()).unwrap_or_default();
                base.map_needs_parse(Base::parse_map_id(filename))
            })
            .collect();

        // The loaders compare against the hashes from before this run, while the sequential stage records
        // the new ones.
        let old_hashes = self.hashes.clone();
        let duplicate_mode = self.duplicate_mode;

        // Reading, parsing and walking a map only borrow `base`, so maps load in parallel. Walking is most of
        // the work: on read it collects the map's text, on write it patches the map.
        let load_chunk = |base: &Base, range: Range<usize>| -> Result<Vec<Loaded<(RpgmData, MapWalk)>>, Error> {
            maps[range.clone()]
                .par_iter()
                .zip(&needs_parse[range])
                .map(|(path, &needs)| {
                    Loaded::load(path, duplicate_mode, mode, &old_hashes, |filename, content| {
                        if !needs {
                            return Ok(None);
                        }

                        let mut map_data = parse_rpgm_file(content, engine_type)?;
                        let walk = base.walk_map(Base::parse_map_id(filename), &mut map_data);
                        Ok(Some((map_data, walk)))
                    })
                })
                .collect()
        };

        // So is serializing and writing a patched map, and freeing a read one: dropping a big parsed
        // document costs nearly half as much as parsing it.
        let finish_chunk = |done: Vec<(String, RpgmData)>| -> Result<(), Error> {
            done.into_par_iter().try_for_each(|(filename, map_data)| {
                if !mode.is_write() {
                    return Ok(());
                }

                let output_path = run.data_output_path.join(filename);
                write_data_file(&output_path, map_data.into_bytes())
            })
        };

        // Recording what the walks found stays sequential and in map order, which keeps the output
        // identical.
        run_chunked(
            base,
            maps.len(),
            load_chunk,
            |base, map| {
                let filename = map.filename;
                let id = Base::parse_map_id(&filename);

                debug!("{filename}: {}", run.pre_msg);

                let skipped = record_hash(&mut self.hashes, mode, &filename, map.hash).is_break();

                if skipped {
                    base.skip_maps.insert(id);
                }

                let map_data = base
                    .process_parsed_map(id, &map.content, map.prepared)
                    .map_err(|e| e.in_file(&filename))?;

                if skipped {
                    info!("{filename}: Skipped.");
                } else {
                    info!("{filename}: {}", run.post_msg);
                }

                Ok(map_data.map(|map_data| (filename, map_data)))
            },
            finish_chunk,
        )?;

        if !mode.is_write() {
            let contents = match base.finish_maps() {
                ProcessedData::TranslationData(t) => t,
                ProcessedData::RPGMData(_) => unreachable!(),
            };

            write_translation_file(&translation_file_path, contents)?;
        }

        Ok(())
    }

    /// Processes the one-file-per-entity data files (`Actors`, `Items`, ...) selected by the file flags.
    fn process_other(
        &mut self,
        base: &mut Base,
        run: &Run<'_>,
        engine_type: EngineType,
        entries: &[DirEntry],
    ) -> Result<(), Error> {
        let mode = run.mode;
        let mut jobs = Vec::new();

        for entry in filter_other(entries.iter(), run.extension) {
            let path = entry.path();
            let filename = path.file_name().and_then(|p| p.to_str()).unwrap();

            let file_flag = FileFlags::from_str(filename).unwrap();

            if !self.file_flags.contains(file_flag) {
                continue;
            }

            debug!("{filename}: {}", run.pre_msg);

            let translation_file_path = run
                .translation_path
                .join(Path::new(&filename.to_ascii_lowercase()).with_extension("txt"));

            if skip_translation_file(mode, &translation_file_path) {
                continue;
            }

            jobs.push((path, translation_file_path));
        }

        // A handful of files, so all of them load at once, in parallel; processing stays sequential.
        let loaded = {
            let hashes = &self.hashes;
            let duplicate_mode = self.duplicate_mode;

            jobs.into_par_iter()
                .map(|(path, translation_file_path)| {
                    let translation = load_translation(mode, &translation_file_path)?;
                    let file = Loaded::load(&path, duplicate_mode, mode, hashes, |_, content| {
                        parse_rpgm_file(content, engine_type).map(Some)
                    })?;
                    Ok((file, translation, translation_file_path))
                })
                .collect::<Result<Vec<_>, Error>>()?
        };

        for (file, translation, translation_file_path) in loaded {
            let filename = file.filename;

            if record_hash(&mut self.hashes, mode, &filename, file.hash).is_break() {
                continue;
            }

            let data = base
                .process_parsed_other(&filename, &file.content, file.prepared, translation.as_deref())
                .map_err(|e| e.in_file(&filename))?;

            run.emit(data, &run.data_output_path.join(&filename), &translation_file_path)?;

            info!("{filename}: {}", run.post_msg);
        }

        Ok(())
    }

    /// Processes one file that has a translation file of its own: `System`, `Scripts` or `plugins.js`.
    ///
    /// `hash_name` is both the name the file's hash is kept under and the name in logs and errors.
    #[allow(clippy::too_many_arguments)]
    fn process_single(
        &mut self,
        base: &mut Base,
        run: &Run<'_>,
        translation_name: &str,
        source_file_path: PathBuf,
        output_file_path: &Path,
        hash_name: &str,
        process: impl FnOnce(&mut Base, &[u8], Option<&str>) -> Result<Option<ProcessedData>, Error>,
    ) -> Result<(), Error> {
        let mode = run.mode;
        let translation_file_path = run.translation_path.join(translation_name);

        if skip_translation_file(mode, &translation_file_path) {
            return Ok(());
        }

        debug!("{translation_name}: {}", run.pre_msg);

        let translation = load_translation(mode, &translation_file_path)?;
        let content = read(&source_file_path).map_err(|e| Error::Io(source_file_path, e))?;

        if hash_content(&mut self.hashes, self.duplicate_mode, mode, &content, hash_name).is_break() {
            return Ok(());
        }

        let data = process(base, &content, translation.as_deref()).map_err(|e| e.in_file(hash_name))?;

        run.emit(data, output_file_path, &translation_file_path)?;

        info!("{hash_name}: {}", run.post_msg);

        Ok(())
    }

    /// Runs the operation selected by [`Processor::mode`] against an RPG Maker
    /// 2000/2003 project.
    ///
    /// Rm2k's layout doesn't fit the rest of [`Processor::process`]: every file
    /// (`RPG_RT.ldb`, `RPG_RT.lmt`, `MapNNNN.lmu`) lives at the project root
    /// rather than in a `Data` subdirectory, there's no `Scripts`/`Plugins`
    /// equivalent, and the entire "other" file set is one `RPG_RT.ldb`
    /// producing several txt files instead of several source files each
    /// producing one. [`Processor::process`] dispatches here in a single
    /// branch instead of threading `engine_type == EngineType::RM2K` through
    /// the MV/VX flow below.
    ///
    /// # Errors
    ///
    /// As [`Processor::process`], plus [`Error::Rm2kLoad`] if loading any
    /// `.ldb`/`.lmt`/`.lmu` fails.
    // Splitting this would rewrite working code, and the lint is about length only.
    #[allow(clippy::too_many_lines)]
    fn process_rm2k(
        &mut self,
        base: &mut Base,
        source_path: &Path,
        translation_path: &Path,
        output_path: &Path,
    ) -> Result<(), Error> {
        let mode = base.mode;

        create_dir_all(translation_path).map_err(|e| Error::Io(translation_path.to_path_buf(), e))?;

        if mode.is_write() {
            create_dir_all(output_path).map_err(|e| Error::Io(output_path.to_path_buf(), e))?;
        }

        let entries: Vec<DirEntry> = read_dir(source_path)
            .map_err(|e| Error::Io(source_path.to_path_buf(), e))?
            .flatten()
            .collect();

        // Read unconditionally: even a map-only run needs the database's
        // `system.ldb_id` to know whether to write 2000 or 2003 fields.
        let ldb_path = source_path.join("RPG_RT.ldb");
        let ldb_content = read(&ldb_path).map_err(|e| Error::Io(ldb_path, e))?;
        let mut database = rm2k::file::load_database(&ldb_content).map_err(|e| Error::from(e).in_file("RPG_RT.ldb"))?;
        let engine = Engine::from_ldb_id(database.value.system.ldb_id);
        base.set_rm2k_engine(engine);
        base.set_game_title(&self.game_title);

        if self.file_flags.contains(FileFlags::Map) {
            let translation_file_path = translation_path.join("maps.txt");

            if !skip_translation_file(mode, &translation_file_path) {
                let lmt_path = source_path.join("RPG_RT.lmt");
                let lmt_content = read(&lmt_path).map_err(|e| Error::Io(lmt_path, e))?;
                let tree = rm2k::file::load_tree_map(&lmt_content).map_err(|e| Error::from(e).in_file("RPG_RT.lmt"))?;

                let translation = load_translation(mode, &translation_file_path)?;

                base.skip_maps = self.skip_maps.iter().copied().collect();
                base.begin_rm2k_maps();

                let maps: Vec<PathBuf> = filter_rm2k_maps(entries.iter()).map(DirEntry::path).collect();

                // Parsed once for the whole run, rather than checked again for every map.
                if !maps.is_empty() {
                    base.initialize_rm2k_maps(translation.as_deref())?;
                }

                let tree_index = Rm2kTree::indexed(&tree.value);

                // Decided up front for every map, from the state before any of them is processed.
                let needs_walk: Vec<bool> = maps
                    .iter()
                    .map(|path| {
                        let filename = path.file_name().and_then(|name| name.to_str()).unwrap_or_default();
                        base.rm2k_map_needs_walk(Base::parse_map_id(filename), &tree_index)
                    })
                    .collect();

                // The loaders compare against the hashes from before this run, while the sequential stage records
                // the new ones.
                let old_hashes = self.hashes.clone();
                let duplicate_mode = self.duplicate_mode;

                // Reading, parsing and walking a map only borrow the base, so maps load in parallel. On write the walk
                // also serializes the patched map.
                let load_chunk = |base: &Base, range: Range<usize>| -> Result<Vec<Loaded<Rm2kMapWalk>>, Error> {
                    maps[range.clone()]
                        .par_iter()
                        .zip(&needs_walk[range])
                        .map(|(path, &needs)| {
                            Loaded::load(path, duplicate_mode, mode, &old_hashes, |filename, content| {
                                if !needs {
                                    return Ok(None);
                                }

                                base.walk_rm2k_map(Base::parse_map_id(filename), content).map(Some)
                            })
                        })
                        .collect()
                };

                let finish_chunk = |done: Vec<(String, Vec<u8>)>| -> Result<(), Error> {
                    done.into_par_iter().try_for_each(|(filename, bytes)| {
                        let out_path = output_path.join(filename);
                        write_data_file(&out_path, bytes)
                    })
                };

                // Recording what the walks found stays sequential and in map order, which keeps the output
                // identical.
                run_chunked(
                    base,
                    maps.len(),
                    load_chunk,
                    |base, map| {
                        let filename = map.filename;
                        let id = Base::parse_map_id(&filename);

                        debug!("{filename}: {pre_msg}", pre_msg = mode_pre_msg(mode));

                        // Still processed when skipped, so the existing translation is carried into the maps file.
                        let skipped = record_hash(&mut self.hashes, mode, &filename, map.hash).is_break();

                        if skipped {
                            base.skip_maps.insert(id);
                        }

                        let bytes = base
                            .process_parsed_rm2k_map(id, &map.content, &tree_index, map.prepared)
                            .map_err(|e| e.in_file(&filename))?;

                        if skipped {
                            info!("{filename}: Skipped.");
                        } else {
                            info!("{filename}: {post_msg}", post_msg = mode_post_msg(mode));
                        }

                        Ok(bytes.map(|bytes| (filename, bytes)))
                    },
                    finish_chunk,
                )?;

                if !mode.is_write() {
                    let contents = match base.finish_rm2k_maps() {
                        ProcessedData::TranslationData(t) => t,
                        ProcessedData::RPGMData(_) => unreachable!(),
                    };

                    write_translation_file(&translation_file_path, contents)?;
                }
            }
        }

        // Each section is hashed on its own, under its stem: one read of some of them must not make the others look
        // unchanged to a later append.
        let database_flags = FileFlags::Database
            | FileFlags::Actors
            | FileFlags::Skills
            | FileFlags::Items
            | FileFlags::Enemies
            | FileFlags::Troops
            | FileFlags::Classes
            | FileFlags::CommonEvents
            | FileFlags::States;

        if self.file_flags.intersects(database_flags) {
            // Every section shares this shape: load its own txt (translations
            // aren't bundled - `RPG_RT.ldb` holds every entity kind, so each
            // kind gets treated as if it were its own file, same as MV/VX's
            // `Actors.*`/`Skills.*`/...), mutate the shared `database` in
            // place, and - on read/purge - write the section's own txt back.
            macro_rules! db_section {
                ($field:ident, $method:ident, $stem:literal, $flag:expr) => {{
                    let translation_file_path = translation_path.join(concat!($stem, ".txt"));

                    if self.file_flags.contains($flag)
                        && !skip_translation_file(mode, &translation_file_path)
                        && !hash_content(&mut self.hashes, self.duplicate_mode, mode, &ldb_content, $stem).is_break()
                    {
                        let translation = load_translation(mode, &translation_file_path)?;
                        let data = base
                            .$method(&mut database.value.$field, translation.as_deref())
                            .map_err(|e| e.in_file(concat!($stem, ".txt")))?;

                        if let Some(data) = data {
                            write_translation_file(&translation_file_path, data)?;
                        }

                        info!(concat!($stem, ".txt: {}"), mode_post_msg(mode));
                    }
                }};
            }

            db_section!(actors, process_rm2k_actors, "actors", FileFlags::Actors);
            db_section!(skills, process_rm2k_skills, "skills", FileFlags::Skills);
            db_section!(items, process_rm2k_items, "items", FileFlags::Items);
            db_section!(enemies, process_rm2k_enemies, "enemies", FileFlags::Enemies);
            db_section!(troops, process_rm2k_troops, "troops", FileFlags::Troops);
            db_section!(classes, process_rm2k_classes, "classes", FileFlags::Classes);
            db_section!(
                commonevents,
                process_rm2k_commonevents,
                "commonevents",
                FileFlags::CommonEvents
            );
            db_section!(states, process_rm2k_states, "states", FileFlags::States);
            db_section!(terms, process_rm2k_terms, "terms", FileFlags::Database);

            // `RPG_RT.ldb` has no title field; hand the translated title back for the caller's `RPG_RT.ini`.
            if mode.is_write() {
                base.game_title().clone_into(&mut self.game_title);
            }

            if mode.is_write() {
                let mut bytes = Vec::new();
                let opt = SaveOpt { preserve_header: true };

                // Writing into a `Vec` is infallible: its `Sink` impl has no error case.
                rm2k::file::save_database(&database.value, &mut bytes, engine, opt, database.header).unwrap();

                let out_path = output_path.join("RPG_RT.ldb");
                write_data_file(&out_path, bytes)?;
            }
        }

        Ok(())
    }
}

/// The key a file's hash is kept under: its stem, lowercased.
fn hash_key(filename: &str) -> String {
    filename[..filename.find('.').unwrap_or(filename.len())].to_ascii_lowercase()
}

/// Hashes a file's content, seeded by the duplicate mode.
///
/// Always rapidhash, never the map hasher: the sums are persisted, so they must not depend on the CPU (gxhash differs
/// between x86 and Arm) or on the `gxhash` feature.
#[must_use]
pub const fn content_hash(duplicate_mode: DuplicateMode, content: &[u8]) -> u64 {
    rapidhash::v3::rapidhash_v3_seeded(content, &rapidhash::v3::RapidSecrets::seed(duplicate_mode as u64))
}

/// Records `hash` for `filename`, signaling whether the caller should skip the file because it's unchanged since
/// the last read (only in append-without-force mode).
fn record_hash(hashes: &mut HashMap<String, u64>, mode: Mode, filename: &str, hash: u64) -> ControlFlow<()> {
    let unchanged = hashes.insert(hash_key(filename), hash) == Some(hash);

    if unchanged && mode.is_append_default() {
        info!(
            "{filename} hasn't changed since the last read. Skipping it. Set `force` on the read mode, if you want to \
             forcefully append data."
        );

        return ControlFlow::Break(());
    }

    ControlFlow::Continue(())
}

/// Writes a translation file through a temporary sibling and a rename, so a crash or a full disk mid-write leaves the
/// old file whole instead of cutting it short. Translations are the user's work, and only exist in these files.
fn write_translation_file(path: &Path, contents: impl AsRef<[u8]>) -> Result<(), Error> {
    write_file(path, contents, true)
}

/// A game data file, which may be the one the run read from. Not synced: there can be thousands, and a lost one is
/// written again by the next run.
pub(crate) fn write_data_file(path: &Path, contents: impl AsRef<[u8]>) -> Result<(), Error> {
    write_file(path, contents, false)
}

/// Writes beside `path` and renames over it, so a crash never leaves `path` cut off.
fn write_file(path: &Path, contents: impl AsRef<[u8]>, sync: bool) -> Result<(), Error> {
    let mut temp_path = path.as_os_str().to_owned();
    temp_path.push(".tmp");

    // Synced before the rename, or a power loss can keep the rename but not the data.
    File::create(&temp_path)
        .and_then(|mut file| {
            file.write_all(contents.as_ref())?;

            if sync { file.sync_all() } else { Ok(()) }
        })
        .map_err(|e| Error::Io(PathBuf::from(&temp_path), e))?;

    rename(&temp_path, path).map_err(|e| {
        let _ = remove_file(&temp_path);
        Error::Io(path.to_path_buf(), e)
    })
}

/// [`record_hash`] for content hashed right here.
fn hash_content(
    hashes: &mut HashMap<String, u64>,
    duplicate_mode: DuplicateMode,
    mode: Mode,
    content: &[u8],
    filename: &str,
) -> ControlFlow<()> {
    record_hash(hashes, mode, filename, content_hash(duplicate_mode, content))
}

/// Loads `total` files a chunk at a time - one file per thread, so only that many parsed documents are held at once -
/// and records them one by one, in order. The chunk after the one being recorded loads while the one before it
/// finishes, which is where a patched file is serialized and written.
fn run_chunked<P: Send, D: Send>(
    base: &mut Base,
    total: usize,
    load_chunk: impl Fn(&Base, Range<usize>) -> Result<Vec<Loaded<P>>, Error> + Sync,
    mut record: impl FnMut(&mut Base, Loaded<P>) -> Result<Option<D>, Error>,
    finish_chunk: impl Fn(Vec<D>) -> Result<(), Error> + Sync,
) -> Result<(), Error> {
    let chunk_len = rayon::current_num_threads();
    let chunk_count = total.div_ceil(chunk_len);
    let range = |index: usize| index * chunk_len..total.min((index + 1) * chunk_len);

    let mut loaded = if chunk_count == 0 {
        Vec::new()
    } else {
        load_chunk(base, range(0))?
    };

    for index in 0..chunk_count {
        let mut done = Vec::with_capacity(loaded.len());

        for file in take(&mut loaded) {
            if let Some(item) = record(base, file)? {
                done.push(item);
            }
        }

        let base = &*base;
        let (next, finished) = rayon::join(
            || {
                (index + 1 < chunk_count)
                    .then(|| load_chunk(base, range(index + 1)))
                    .transpose()
            },
            || finish_chunk(done),
        );

        finished?;
        loaded = next?.unwrap_or_default();
    }

    Ok(())
}

/// Loads the ignore file into `base`. A missing file is an error only when the flags ask to apply it.
fn read_ignore_file(base: &mut Base, path: &Path) -> Result<(), Error> {
    match read_to_string(path) {
        Ok(content) => {
            base.ignore.map = parse_ignore(&content, base.duplicate_mode, base.mode.is_read());
            Ok(())
        }

        Err(err) if base.flags.contains(BaseFlags::Ignore) => Err(Error::Io(path.to_path_buf(), err)),

        Err(_) => Ok(()),
    }
}

/// Writes the entries a purge collected into the ignore file.
fn write_ignore_file(base: &mut Base, path: &Path) -> Result<(), Error> {
    let mut contents = String::new();

    // A section is created for every id as it's processed, but only gains lines if something under it was actually
    // purged - most ids keep their translation and leave it empty. Writing those out produced a header with nothing
    // under it.
    for (file, lines) in take(&mut base.ignore.map)
        .into_iter()
        .filter(|(_, lines)| !lines.is_empty())
    {
        contents.push_str(&file);
        contents.push('\n');

        for line in lines.lines() {
            contents.push_str(&line);
            contents.push('\n');
        }
    }
    write_translation_file(path, contents)
}

/// Where [`Mode::Write`] puts rebuilt files when the caller names no output directory.
const DEFAULT_OUTPUT_DIR: &str = "output";

/// The paths and messages shared by every step of one non-RM2K run.
struct Run<'a> {
    mode: Mode,
    extension: &'static str,
    source_path: &'a Path,
    translation_path: &'a Path,
    output_path: &'a Path,
    data_output_path: PathBuf,
    pre_msg: &'static str,
    post_msg: &'static str,
}

impl Run<'_> {
    /// Writes land next to the source data, everything else lands in the `.txt`.
    fn emit(
        &self,
        data: Option<ProcessedData>,
        rpgm_output_path: &Path,
        translation_file_path: &Path,
    ) -> Result<(), Error> {
        let Some(data) = data else {
            return Ok(());
        };

        if self.mode.is_write() {
            write_data_file(rpgm_output_path, data)
        } else {
            write_translation_file(translation_file_path, data)
        }
    }
}

/// What the parallel stage of [`Processor::process`] hands to the sequential one, per file: the file and whatever
/// `prepare` made of it.
struct Loaded<P> {
    filename: String,
    content: Vec<u8>,
    hash: u64,
    /// [`None`] when the file is skipped as unchanged, or `prepare` had nothing to do.
    prepared: Option<P>,
}

impl<P> Loaded<P> {
    /// Reads and hashes one file, then lets `prepare` - parsing, say - work on it unless it is skipped as unchanged.
    /// Touches nothing shared but `hashes`, which it only reads, so files load in parallel.
    fn load(
        path: &Path,
        duplicate_mode: DuplicateMode,
        mode: Mode,
        hashes: &HashMap<String, u64>,
        prepare: impl FnOnce(&str, &[u8]) -> Result<Option<P>, Error>,
    ) -> Result<Self, Error> {
        let content = read(path).map_err(|e| Error::Io(path.to_path_buf(), e))?;
        // The filters only pass UTF-8 names.
        let filename = path
            .file_name()
            .and_then(|name| name.to_str())
            .unwrap_or_default()
            .to_owned();
        let hash = content_hash(duplicate_mode, &content);

        // Mirrors `record_hash`: each file has its own key, so reading the old hashes here sees what it would.
        let unchanged = mode.is_append_default() && hashes.get(&hash_key(&filename)) == Some(&hash);

        let prepared = if unchanged {
            None
        } else {
            prepare(&filename, &content).map_err(|e| e.in_file(&filename))?
        };

        Ok(Self {
            filename,
            content,
            hash,
            prepared,
        })
    }
}

fn load_translation(mode: Mode, path: &Path) -> Result<Option<String>, Error> {
    if mode.is_default() {
        return Ok(None);
    }

    read_to_string(path)
        .map_err(|e| Error::Io(path.to_path_buf(), e))
        .map(Some)
}

/// Whether to leave a translation file alone, logging why: a plain read without force must not clobber an existing
/// one, and every other mode needs one to exist. A missing file is skipped rather than failing the whole run.
fn skip_translation_file(mode: Mode, path: &Path) -> bool {
    let exists = path.exists();

    if mode.is_default_default() && exists {
        info!(
            "{}: File already exists. Use append mode to append text or force mode to overwrite.",
            path.display()
        );
        true
    } else if !mode.is_default() && !exists {
        warn!("{}: Translation file does not exist. Skipping.", path.display());
        true
    } else {
        false
    }
}

const fn mode_pre_msg(mode: Mode) -> &'static str {
    match mode {
        Mode::Read { .. } => "Started reading.",
        Mode::Write => "Started writing.",
        Mode::Purge => "Started purging.",
    }
}

const fn mode_post_msg(mode: Mode) -> &'static str {
    match mode {
        Mode::Read { .. } => "Successfully read.",
        Mode::Write => "Successfully written.",
        Mode::Purge => "Successfully purged.",
    }
}
