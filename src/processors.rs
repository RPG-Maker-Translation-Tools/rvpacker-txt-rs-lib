use crate::{
    ProcessedData, RPGMFileType,
    constants::RVPACKER_IGNORE_FILE,
    core::{Base, filter_maps, filter_other, parse_ignore},
    types::{BaseFlags, DuplicateMode, EngineType, Error, FileFlags, Mode},
};
use gxhash::{HashMap, HashSet, gxhash64};
use log::{debug, info};
use std::{
    fs::{DirEntry, create_dir_all, read, read_dir, read_to_string, write},
    mem::take,
    ops::ControlFlow,
    path::{Path, PathBuf},
    str::FromStr,
};

/// Reads, writes and purges RPG Maker translation files, handling all system calls.
///
/// This is a plain struct with public fields: set the ones you need and call
/// [`Processor::process`]. [`Processor::mode`] decides which operation runs, so
/// the same struct covers all three.
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

    /// How to process the text - trimming, ignore entries, and so on. See [`BaseFlags`].
    ///
    /// Must match the flags used on read when writing or purging.
    pub flags: BaseFlags,

    /// What to do with duplicate lines. See [`DuplicateMode`].
    ///
    /// Must match the mode used on read when writing or purging.
    pub duplicate_mode: DuplicateMode,

    /// Overrides the game title extracted from the system file.
    ///
    /// XP/VX/VX Ace games may only carry their title in `Game.ini`, which is not
    /// necessarily UTF-8. Decode it yourself - see
    /// [`get_ini_title`](crate::get_ini_title) - and set it here. Only used on read.
    pub game_title: String,

    /// Content hashes keyed by lowercased file stem.
    ///
    /// Set these from a previous read to let [`Mode::Read`]
    /// skip unchanged files, and read them back afterwards to persist them.
    pub hashes: HashMap<String, u64>,

    /// Map ids to skip. On [`Mode::Read`] the
    /// corresponding maps are written back unchanged.
    pub skip_maps: Vec<u16>,

    /// Per-file entry ids to skip. Has no effect on [`RPGMFileType::Map`].
    pub skip_events: Vec<(RPGMFileType, Vec<u16>)>,

    /// Whether to record event metadata - id, name and coordinates - before each
    /// event's text.
    pub map_events: bool,
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
    /// - `output_path` - where to write rebuilt data files. Required by
    ///   [`Mode::Write`], ignored otherwise.
    ///
    /// # Errors
    ///
    /// - [`Error::Io`] - if any I/O operation fails.
    /// - [`Error::JsonParse`] - if parsing any JSON fails.
    /// - [`Error::MarshalLoad`] - if loading any Marshal byte stream fails.
    /// - [`Error::NoTranslation`] - if the mode needs a translation file and none was found.
    pub fn process(
        &mut self,
        engine_type: EngineType,
        source_path: impl AsRef<Path>,
        translation_path: impl AsRef<Path>,
        output_path: Option<&Path>,
    ) -> Result<(), Error> {
        if self.file_flags.is_empty() {
            return Ok(());
        }

        let source_path = source_path.as_ref();
        let translation_path = translation_path.as_ref();
        let output_path = output_path.unwrap_or(Path::new(""));

        let mut base = Base::new(self.mode, engine_type);
        base.flags = self.flags;
        base.duplicate_mode = self.duplicate_mode;
        base.skip_events = take(&mut self.skip_events)
            .into_iter()
            .map(|(id, vec)| (id, HashSet::from_iter(vec)))
            .collect();

        let mode = base.mode;
        let flags = base.flags;

        let mut ignore_file_path = PathBuf::new();

        if flags.intersects(BaseFlags::CreateIgnore | BaseFlags::Ignore) {
            ignore_file_path = translation_path.join(RVPACKER_IGNORE_FILE);

            let ignore_file_content = read_to_string(&ignore_file_path)
                .map_err(|e| Error::Io(ignore_file_path.clone(), e));

            match ignore_file_content {
                Ok(content) => {
                    base.ignore.map = parse_ignore(
                        &content,
                        self.duplicate_mode,
                        mode.is_read(),
                    );
                }

                Err(err) if flags.contains(BaseFlags::Ignore) => {
                    return Err(err);
                }

                _ => {}
            }
        }

        let output_dir = if mode.is_read() {
            translation_path
        } else {
            output_path
        };

        create_dir_all(output_dir)
            .map_err(|e| Error::Io(output_dir.to_path_buf(), e))?;

        let data_output_path = output_path.join(if engine_type.is_new() {
            "data"
        } else {
            "Data"
        });

        if mode.is_write() {
            create_dir_all(&data_output_path)
                .map_err(|e| Error::Io(data_output_path.clone(), e))?;
        }

        let pre_msg = match mode {
            Mode::Read { .. } => "Started reading.",
            Mode::Write => "Started writing.",
            Mode::Purge => "Started purging.",
        };

        let post_msg = match mode {
            Mode::Read { .. } => "Successfully read.",
            Mode::Write => "Successfully written.",
            Mode::Purge => "Successfully purged.",
        };

        let load_translation = |p: &Path| -> Result<Option<String>, Error> {
            if mode.is_default() {
                return Ok(None);
            }

            read_to_string(p)
                .map_err(|e| Error::Io(p.to_path_buf(), e))
                .map(Some)
        };

        // `true` when a translation file is already there and the caller asked us
        // not to clobber it, i.e. plain default read mode without force.
        let already_exists = |p: &Path| {
            if mode.is_default_default() && p.exists() {
                info!(
                    "{}: File already exists. Use append mode to append text \
                     or force mode to overwrite.",
                    p.display()
                );
                true
            } else {
                false
            }
        };

        let mut hash = |content: &[u8], filename: &str| {
            let filename = &filename
                [0..filename.find('.').unwrap_or(filename.len())]
                .to_ascii_lowercase();
            let hash = gxhash64(content, self.duplicate_mode as i64);
            let mut unchanged = false;

            if let Some(&old_hash) = self.hashes.get(filename) {
                unchanged = old_hash == hash;
            }

            self.hashes.insert(filename.to_string(), hash);

            if unchanged && self.mode.is_append_default() {
                info!(
                    "{filename} hasn't changed since the last read. Skipping \
                     it. Set `force` on the read mode, if you want to \
                     forcefully append data."
                );

                return ControlFlow::Break(());
            }

            ControlFlow::Continue(())
        };

        // Writes land next to the source data, everything else lands in the `.txt`.
        let emit = |data: Option<ProcessedData>,
                    rpgm_output_path: PathBuf,
                    translation_file_path: PathBuf|
         -> Result<(), Error> {
            let Some(data) = data else {
                return Ok(());
            };

            let path = if mode.is_write() {
                rpgm_output_path
            } else {
                translation_file_path
            };

            write(&path, data).map_err(|e| Error::Io(path, e))
        };

        let entries: Vec<DirEntry> = read_dir(source_path)
            .map_err(|e| Error::Io(source_path.to_path_buf(), e))?
            .flatten()
            .collect();

        let engine_extension = engine_type.extension();

        if self.file_flags.contains(FileFlags::Map) {
            let translation_file_path = translation_path.join("maps.txt");

            if !already_exists(&translation_file_path) {
                let mapinfos_path =
                    source_path.join(format!("MapInfos.{engine_extension}"));
                let mapinfos = read(&mapinfos_path)
                    .map_err(|e| Error::Io(mapinfos_path, e))?;

                let translation = load_translation(&translation_file_path)?;

                base.map_events = self.map_events;
                base.skip_maps =
                    take(&mut self.skip_maps).into_iter().collect();

                base.begin_maps();

                for entry in filter_maps(entries.iter(), engine_extension) {
                    let path = entry.path();
                    let filename =
                        path.file_name().and_then(|p| p.to_str()).unwrap();

                    debug!("{filename}: {pre_msg}");

                    let content =
                        read(&path).map_err(|e| Error::Io(path.clone(), e))?;

                    let id = Base::parse_map_id(filename);

                    let mut skipped = false;

                    if hash(&content, filename).is_break() {
                        base.skip_maps.insert(id);
                        skipped = true;
                    }

                    let result = base.process_map(
                        filename,
                        &content,
                        &mapinfos,
                        translation.as_deref(),
                    )?;

                    if mode.is_write() {
                        if let Some(result) = result {
                            let output_path = data_output_path.join(filename);
                            write(&output_path, result)
                                .map_err(|e| Error::Io(output_path, e))?;
                        }
                    }

                    if skipped {
                        info!("{filename}: Skipped.");
                    } else {
                        info!("{filename}: {post_msg}");
                    }
                }

                if !mode.is_write() {
                    let contents = match base.finish_maps() {
                        ProcessedData::TranslationData(t) => t,
                        ProcessedData::RPGMData(_) => unreachable!(),
                    };

                    write(&translation_file_path, contents)
                        .map_err(|e| Error::Io(translation_file_path, e))?;
                }
            }
        }

        if self.file_flags.intersects(FileFlags::other()) {
            for entry in filter_other(entries.iter(), engine_extension) {
                let path = entry.path();
                let filename =
                    path.file_name().and_then(|p| p.to_str()).unwrap();

                debug!("{filename}: {pre_msg}");

                let file_flag = FileFlags::from_str(filename).unwrap();

                if !self.file_flags.contains(file_flag) {
                    continue;
                }

                let translation_file_path = translation_path.join(
                    Path::new(&filename.to_ascii_lowercase())
                        .with_extension("txt"),
                );

                if already_exists(&translation_file_path) {
                    continue;
                }

                let translation = load_translation(&translation_file_path)?;

                let content =
                    read(&path).map_err(|e| Error::Io(path.clone(), e))?;

                if hash(&content, filename).is_break() {
                    continue;
                }

                let data = base.process_other(
                    filename,
                    &content,
                    translation.as_deref(),
                )?;

                emit(
                    data,
                    data_output_path.join(filename),
                    translation_file_path,
                )?;

                info!("{filename}: {post_msg}");
            }
        }

        if self.file_flags.contains(FileFlags::System) {
            let translation_file_path = translation_path.join("system.txt");

            if !already_exists(&translation_file_path) {
                let translation = load_translation(&translation_file_path)?;
                let filename = format!("System.{engine_extension}");

                debug!("{filename}: {pre_msg}");

                let system_file_path = source_path.join(&filename);
                let content = read(&system_file_path)
                    .map_err(|e| Error::Io(system_file_path, e))?;

                if !hash(&content, &filename).is_break() {
                    base.set_game_title(&self.game_title);

                    let data =
                        base.process_system(&content, translation.as_deref())?;

                    emit(
                        data,
                        data_output_path.join(&filename),
                        translation_file_path,
                    )?;

                    info!("{filename}: {post_msg}");
                }
            }
        }

        if self.file_flags.contains(FileFlags::Scripts) {
            if engine_type.is_new() {
                let translation_file_path =
                    translation_path.join("plugins.txt");

                if !already_exists(&translation_file_path) {
                    debug!("plugins.txt: {pre_msg}");

                    let translation = load_translation(&translation_file_path)?;

                    let plugins_file_path =
                        source_path.parent().unwrap().join("js/plugins.js");
                    let content = read(&plugins_file_path)
                        .map_err(|e| Error::Io(plugins_file_path, e))?;

                    if !hash(&content, "plugins.js").is_break() {
                        let data = base.process_plugins(
                            &content,
                            translation.as_deref(),
                        )?;

                        if mode.is_write() {
                            let js_output_path = output_path.join("js");
                            create_dir_all(&js_output_path)
                                .map_err(|e| Error::Io(js_output_path, e))?;
                        }

                        emit(
                            data,
                            output_path.join("js/plugins.js"),
                            translation_file_path,
                        )?;

                        info!("plugins.js: {post_msg}");
                    }
                }
            } else {
                let translation_file_path =
                    translation_path.join("scripts.txt");

                if !already_exists(&translation_file_path) {
                    debug!("scripts.txt: {pre_msg}");

                    let translation = load_translation(&translation_file_path)?;

                    let filename = format!("Scripts.{engine_extension}");
                    let scripts_file_path = source_path.join(&filename);
                    let content = read(&scripts_file_path)
                        .map_err(|e| Error::Io(scripts_file_path, e))?;

                    if !hash(&content, &filename).is_break() {
                        let data = base.process_scripts(
                            &content,
                            translation.as_deref(),
                        )?;

                        emit(
                            data,
                            data_output_path.join(&filename),
                            translation_file_path,
                        )?;

                        info!("{filename}: {post_msg}");
                    }
                }
            }
        }

        if flags.contains(BaseFlags::CreateIgnore) {
            use std::fmt::Write;

            let contents: String = take(&mut base.ignore.map).into_iter().fold(
                String::new(),
                |mut output, (file, lines)| {
                    let _ = write!(
                        output,
                        "{}\n{}",
                        file,
                        lines
                            .lines()
                            .map(|line| line.into_owned() + "\n")
                            .collect::<String>()
                    );

                    output
                },
            );

            write(&ignore_file_path, contents)
                .map_err(|e| Error::Io(ignore_file_path, e))?;
        }

        Ok(())
    }
}
