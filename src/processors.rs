use crate::{
    constants::RVPACKER_IGNORE_FILE,
    core::{
        Base, MapBase, OtherBase, PluginBase, ScriptBase, SystemBase,
        filter_maps, filter_other, get_engine_extension, parse_ignore,
    },
    types::{
        BaseFlags, DuplicateMode, EngineType, Error, FileFlags, GameType, Mode,
        ReadMode,
    },
};
use gxhash::{GxHasher, HashSet, HashSetExt};
use log::info;
use std::{
    fs::{DirEntry, create_dir_all, read, read_dir, read_to_string, write},
    hash::Hash,
    mem::take,
    ops::ControlFlow,
    path::{Path, PathBuf},
    str::FromStr,
};

#[derive(Default)]
pub(crate) struct Processor {
    pub file_flags: FileFlags,
    pub mode: Mode,
    pub game_type: GameType,
    pub flags: BaseFlags,
    pub duplicate_mode: DuplicateMode,
    pub hashes: HashSet<u128>,
}

impl Processor {
    pub fn process<P: AsRef<Path>>(
        &mut self,
        engine_type: EngineType,
        source_path: P,
        translation_path: P,
        output_path: Option<&P>,
    ) -> Result<(), Error> {
        if self.file_flags.is_empty() {
            return Ok(());
        }

        let source_path = source_path.as_ref();
        let translation_path = translation_path.as_ref();
        let output_path =
            output_path.map_or_else(|| Path::new(""), |x| x.as_ref());

        let mut base = Base::new(self.mode, engine_type);
        base.flags = self.flags;
        base.duplicate_mode = self.duplicate_mode;
        base.game_type = self.game_type;

        let mut ignore_file_path = PathBuf::new();

        if base
            .flags
            .intersects(BaseFlags::CreateIgnore | BaseFlags::Ignore)
        {
            ignore_file_path = translation_path.join(RVPACKER_IGNORE_FILE);

            let ignore_file_content = read_to_string(&ignore_file_path)
                .map_err(|e| Error::Io(ignore_file_path.clone(), e));

            match ignore_file_content {
                Ok(content) => {
                    base.ignore_map = parse_ignore(
                        &content,
                        base.duplicate_mode,
                        base.mode.is_read(),
                    );
                }

                Err(err) if base.flags.contains(BaseFlags::Ignore) => {
                    return Err(err);
                }

                _ => {}
            }
        }

        let output_dir = if base.mode.is_read() {
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

        if base.mode.is_write() {
            create_dir_all(&data_output_path)
                .map_err(|e| Error::Io(data_output_path.clone(), e))?;
        }

        let msg = match base.mode {
            Mode::Read(_) => "Successfully read.",
            Mode::Write => "Successfully written.",
            Mode::Purge => "Successfully purged.",
        };

        let base_ref = unsafe { &mut *(&raw mut base) };
        let load_translation = |p: &Path| -> Result<Option<String>, Error> {
            if base_ref.mode.is_default() {
                return Ok(None);
            }

            read_to_string(p)
                .map_err(|e| Error::Io(p.to_path_buf(), e))
                .map(Some)
        };

        let mut new_hashes = HashSet::with_capacity(self.hashes.len());
        let mut hasher = GxHasher::with_seed(self.duplicate_mode as i64);
        let mut hash = |content: &[u8], filename: &str| {
            content.hash(&mut hasher);
            let result = hasher.finish_u128();

            let unchanged = self.hashes.contains(&result);
            new_hashes.insert(result);

            if unchanged && self.mode.is_append_default() {
                info!(
                    "{filename} hasn't changed since the last read. Skipping it. Use `ReadMode::ForceAppend`, if you want to forcefully append data."
                );

                return ControlFlow::Break(());
            }

            ControlFlow::Continue(())
        };

        let entries: Vec<DirEntry> = read_dir(source_path)
            .map_err(|e| Error::Io(source_path.to_path_buf(), e))?
            .flatten()
            .collect();

        let base_ref = unsafe { &mut *(&raw mut base) };
        let engine_extension = get_engine_extension(engine_type);

        if self.file_flags.contains(FileFlags::Map) {
            let mut map_base = MapBase::new(base_ref);

            let mapinfos_path =
                source_path.join(format!("Mapinfos.{engine_extension}"));
            let mapinfos = read(&mapinfos_path)
                .map_err(|e| Error::Io(mapinfos_path, e))?;

            let translation_file_path = translation_path.join("maps.txt");

            if base.mode.is_default_default() && translation_file_path.exists()
            {
                info!(
                    "{}: File already exists. Use append mode to append text or force mode to overwrite.",
                    translation_file_path.display()
                );
            } else {
                let translation = load_translation(&translation_file_path)?;
                let mut contents = Vec::new();

                for entry in filter_maps(entries.iter(), engine_extension) {
                    let path = entry.path();
                    let filename =
                        path.file_name().and_then(|p| p.to_str()).unwrap();

                    let content =
                        read(&path).map_err(|e| Error::Io(path.clone(), e))?;

                    // FIXME: If we just continue, it won't include the map in the output in append mode, but should
                    // if hash(&content, filename).is_break() {
                    //    continue;
                    // }

                    let result = map_base.process(
                        filename,
                        &content,
                        &mapinfos,
                        translation.as_deref(),
                    )?;

                    if base.mode.is_write() {
                        if let Some(result) = result {
                            let output_path = data_output_path.join(filename);
                            write(&output_path, result)
                                .map_err(|e| Error::Io(output_path, e))?;
                        }
                    }

                    info!("{filename}: {msg}");
                }

                if !base.mode.is_write() {
                    contents.extend(match map_base.translation() {
                        crate::ProcessedData::TranslationData(t) => t,
                        crate::ProcessedData::RPGMData(_) => unreachable!(),
                    });

                    write(&translation_file_path, contents)
                        .map_err(|e| Error::Io(translation_file_path, e))?;
                }
            }
        }

        if self.file_flags.intersects(FileFlags::other()) {
            let mut other_base = OtherBase::new(base_ref);

            for entry in
                filter_other(entries.iter(), engine_extension, base.game_type)
            {
                let path = entry.path();
                let filename =
                    path.file_name().and_then(|p| p.to_str()).unwrap();

                let file_flag = FileFlags::from_str(filename).unwrap();

                if !self.file_flags.contains(file_flag) {
                    continue;
                }

                let translation_file_path = translation_path.join(
                    Path::new(&filename.to_ascii_lowercase())
                        .with_extension("txt"),
                );

                if base.mode.is_default_default()
                    && translation_file_path.exists()
                {
                    info!(
                        "{}: File already exists. Use append mode to append text or force mode to overwrite.",
                        translation_file_path.display()
                    );
                } else {
                    let translation = load_translation(&translation_file_path)?;

                    let content =
                        read(&path).map_err(|e| Error::Io(path.clone(), e))?;

                    if hash(&content, filename).is_break() {
                        continue;
                    }

                    let data = other_base.process(
                        filename,
                        &content,
                        translation.as_deref(),
                    )?;

                    let output_file_path = if base.mode.is_write() {
                        data_output_path.join(filename)
                    } else {
                        translation_file_path
                    };

                    if let Some(data) = data {
                        write(&output_file_path, data)
                            .map_err(|e| Error::Io(output_file_path, e))?;
                    }

                    info!("{filename}: {msg}");
                }
            }
        }

        if self.file_flags.contains(FileFlags::System) {
            let system_base = SystemBase::new(base_ref);

            let translation_file_path = translation_path.join("system.txt");

            if base.mode.is_default_default() && translation_file_path.exists()
            {
                info!(
                    "{}: File already exists. Use append mode to append text or force mode to overwrite.",
                    translation_file_path.display()
                );
            } else {
                let translation = load_translation(&translation_file_path)?;

                let filename = format!("System.{engine_extension}");
                let system_file_path = source_path.join(&filename);
                let content = read(&system_file_path)
                    .map_err(|e| Error::Io(system_file_path, e))?;

                if !hash(&content, &filename).is_break() {
                    let data = system_base
                        .process(&content, translation.as_deref())?;

                    let output_path = if base.mode.is_write() {
                        data_output_path.join(&filename)
                    } else {
                        translation_file_path
                    };

                    if let Some(data) = data {
                        write(&output_path, data)
                            .map_err(|e| Error::Io(output_path, e))?;
                    }

                    info!("{filename}: {msg}");
                }
            }
        }

        if self.file_flags.contains(FileFlags::Scripts) {
            if engine_type.is_new() {
                let plugin_base = PluginBase::new(base_ref);

                let translation_file_path =
                    translation_path.join("plugins.txt");

                if base.mode.is_default_default()
                    && translation_file_path.exists()
                {
                    info!(
                        "{}: File already exists. Use append mode to append text or force mode to overwrite.",
                        translation_file_path.display()
                    );
                } else {
                    let translation = load_translation(&translation_file_path)?;

                    let plugins_file_path =
                        source_path.parent().unwrap().join("js/plugins.js");
                    let content = read(&plugins_file_path)
                        .map_err(|e| Error::Io(plugins_file_path, e))?;

                    if !hash(&content, "plugins.js").is_break() {
                        let data = plugin_base
                            .process(&content, translation.as_deref())?;

                        let output_path = if base.mode.is_write() {
                            let js_output_path = output_path.join("js");
                            create_dir_all(&js_output_path)
                                .map_err(|e| Error::Io(js_output_path, e))?;
                            output_path.join("js/plugins.js")
                        } else {
                            translation_file_path
                        };

                        if let Some(data) = data {
                            write(&output_path, data)
                                .map_err(|e| Error::Io(output_path, e))?;
                        }

                        info!("plugins.js: {msg}");
                    }
                }
            } else {
                let script_base = ScriptBase::new(base_ref);

                let translation_file_path =
                    translation_path.join("scripts.txt");

                if base.mode.is_default_default()
                    && translation_file_path.exists()
                {
                    info!(
                        "{}: File already exists. Use append mode to append text or force mode to overwrite.",
                        translation_file_path.display()
                    );
                } else {
                    let translation = load_translation(&translation_file_path)?;

                    let filename = format!("Scripts.{engine_extension}");
                    let scripts_file_path = source_path.join(&filename);
                    let content = read(&scripts_file_path)
                        .map_err(|e| Error::Io(scripts_file_path, e))?;

                    if !hash(&content, &filename).is_break() {
                        let data = script_base
                            .process(&content, translation.as_deref())?;

                        let output_path = if base.mode.is_write() {
                            data_output_path.join(&filename)
                        } else {
                            translation_file_path
                        };

                        if let Some(data) = data {
                            write(&output_path, data)
                                .map_err(|e| Error::Io(output_path, e))?;
                        }

                        info!("{filename}: {msg}");
                    }
                }
            }
        }

        self.hashes = take(&mut new_hashes);

        if base.flags.contains(BaseFlags::CreateIgnore) {
            use std::fmt::Write;

            let contents: String = take(&mut base.ignore_map).into_iter().fold(
                String::new(),
                |mut output, (file, lines)| {
                    let _ = write!(
                        output,
                        "{}\n{}",
                        file,
                        lines
                            .into_iter()
                            .map(|mut x| {
                                x.push('\n');
                                x
                            })
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

/// A struct used for parsing and extracting text from RPG Maker files into `.txt` format.
///
/// The [`Reader`] provides a configurable interface to control how files are parsed,
/// which files are selected, and how text content is filtered.
///
/// # Fields
///
/// - `file_flags`: Indicates which RPG Maker files should be processed. Use [`Reader::set_files`] to set.
/// - `mode`: Defines the read strategy. Use [`Reader::set_read_mode`] to set.
/// - `game_type`: Specifies which RPG Maker game type the data is from. Use [`Reader::set_game_type`] to set.
/// - `flags`: Indicates different modes of processing the text. Use [`Reader::set_flags`]. For more info, see [`BaseFlags`].
///
/// # Example
///
/// ```no_run
/// use rvpacker_txt_rs_lib::{Reader, FileFlags, EngineType};
///
/// let mut reader = Reader::new();
/// reader.set_files(FileFlags::Map | FileFlags::other());
/// reader.read("C:/Game/Data", "C:/Game/translation", EngineType::VXAce);
/// ```
pub struct Reader {
    processor: Processor,
}

impl Default for Reader {
    fn default() -> Self {
        Self {
            processor: Processor {
                mode: Mode::Read(ReadMode::Default(false)),
                ..Default::default()
            },
        }
    }
}

impl Reader {
    /// Creates a new [`Reader`] instance with default values.
    ///
    /// By default, all four file flags are set (all files will be read), the [`ReadMode::Default(false)`] read mode is used, duplicate mode is set to [`DuplicateMode::Allow`], and all other options are disabled.
    ///
    /// # Example
    /// ```
    /// use rvpacker_txt_rs_lib::Reader;
    ///
    /// let mut reader = Reader::new();
    /// ```
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Sets the file flags to determine which RPG Maker files will be parsed. See [`FileFlags`] for more info.
    ///
    /// # Parameters
    ///
    /// - `flags` - A [`FileFlags`] value indicating the file types to include.
    ///
    /// # Example
    ///
    /// ```
    /// use rvpacker_txt_rs_lib::{Reader, FileFlags};
    ///
    /// let mut reader = Reader::new();
    /// reader.set_files(FileFlags::Map | FileFlags::other());
    /// ```
    pub fn set_files(&mut self, flags: FileFlags) {
        self.processor.file_flags = flags;
    }

    /// Sets the read mode that affects how data is parsed. See [`ReadMode`] for more info.
    ///
    /// # Parameters
    ///
    /// - `mode` - A [`ReadMode`] variant.
    ///
    /// # Example
    ///
    /// ```
    /// use rvpacker_txt_rs_lib::{Reader, ReadMode};
    ///
    /// let mut reader = Reader::new();
    /// reader.set_read_mode(ReadMode::Default(false));
    /// ```
    pub fn set_read_mode(&mut self, mode: ReadMode) {
        self.processor.mode = Mode::Read(mode);
    }

    /// Sets the game type for custom processing.
    ///
    /// Sets the game type for custom processing. See [`GameType`] for more info.
    ///
    /// # Parameters
    ///
    /// - `game_type` - A [`GameType`] variant.
    ///
    /// # Example
    ///
    /// ```
    /// use rvpacker_txt_rs_lib::{Reader, GameType};
    ///
    /// let mut reader = Reader::new();
    /// reader.set_game_type(GameType::Termina);
    /// ```
    pub fn set_game_type(&mut self, game_type: GameType) {
        self.processor.game_type = game_type;
    }

    /// Sets the flags of the processor.
    ///
    /// Flags indicate, how to process text, and include options such as trimming, romanizing etc., for more info check [`BaseFlags`].
    ///
    /// # Parameters
    ///
    /// - `flags` - [`BaseFlags`] bitflags.
    ///
    /// # Example
    ///
    /// ```
    /// use rvpacker_txt_rs_lib::{Reader, BaseFlags};
    ///
    /// let mut reader = Reader::new();
    /// reader.set_flags(BaseFlags::Trim | BaseFlags::Romanize);
    /// ```
    pub fn set_flags(&mut self, flags: BaseFlags) {
        self.processor.flags = flags;
    }

    /// This function must have the same value that was passed to it in [`Reader`] struct.
    ///
    /// Sets, what to do with duplicates. Works only for map and other files. See [`DuplicateMode`] for more info.
    ///
    /// # Parameters
    ///
    /// - `mode` - A [`DuplicateMode`] variant.
    ///
    /// # Example
    ///
    /// ```
    /// use rvpacker_txt_rs_lib::{Reader, DuplicateMode};
    ///
    /// let mut reader = Reader::new();
    /// reader.set_duplicate_mode(DuplicateMode::Allow);
    /// ```
    pub fn set_duplicate_mode(&mut self, mode: DuplicateMode) {
        self.processor.duplicate_mode = mode;
    }

    /// Sets hashes from the previous read.
    ///
    /// Hashes are only used during [`ReadMode::Append`] read, and if a processed file matches the hashes, it's skipped.
    ///
    /// Note that you need to fetch the hashes again after reading in [`ReadMode::Append`] entries with [`Reader::hashes`], since files that don't match the hashes will cause hash recalculation.
    ///
    /// # Parameters
    ///
    /// - `hashes` - [`Vec<u128>`] of calculated hashes from the previous read
    ///
    pub fn set_hashes(&mut self, hashes: Vec<u128>) {
        self.processor.hashes = HashSet::from_iter(hashes);
    }

    /// Returns hashes, corresponding to the processed files.
    ///
    /// The purpose of this function is simple: on the subsequent append reads, you pass the hashes back to the reader by calling [`Reader::set_hashes`] or [`ReaderBuilder::hashes`], and if a processed file matches the hashes, it's skipped.
    ///
    /// It's done to avoid reading unchanged files, and therefore speed up the process.
    ///
    /// # Returns
    ///
    /// - [`Vec<u128>`] - vector of calculated hashes
    ///
    pub fn hashes(&mut self) -> Vec<u128> {
        Vec::from_iter(take(&mut self.processor.hashes))
    }

    /// Reads the RPG Maker files from `source_path` to `.txt` files in `translation_path`.
    ///
    /// Make sure you've configured the reader as you desire before calling it.
    ///
    /// # Parameters
    ///
    /// - `source_path` - Path to the directory containing RPG Maker files.
    /// - `translation_path` - Path to the directory where `.txt` files will be created.
    /// - `engine_type` - Engine type of the source RPG Maker files.
    ///
    /// # Errors
    ///
    /// - [`Error::Io`] - if any I/O operation fails.
    /// - [`Error::JsonParse`] - if parsing any JSON fails.
    /// - [`Error::MarshalLoad`] - if loading any Marshal byte stream fails.
    ///
    /// # Example
    ///
    /// ```no_run
    /// use rvpacker_txt_rs_lib::{Reader, EngineType};
    ///
    /// let mut reader = Reader::new();
    /// reader.read("C:/Game/Data", "C:/Game/translation", EngineType::VXAce);
    /// ```
    pub fn read<P: AsRef<Path>>(
        &mut self,
        source_path: P,
        translation_path: P,
        engine_type: EngineType,
    ) -> Result<(), Error> {
        self.processor.process(
            engine_type,
            source_path,
            translation_path,
            None,
        )?;
        Ok(())
    }
}

/// A builder struct for [`Reader`].
///
/// The [`Reader`] provides a configurable interface to control how files are parsed,
/// which files are selected, and how text content is filtered.
///
/// # Fields
///
/// - `file_flags`: Indicates which RPG Maker files should be processed. Use [`Reader::set_files`] to set. See [`FileFlags`] for more info.
/// - `flags`: Indicates different modes of processing the text. Use [`Reader::set_flags`] to set. See [`BaseFlags`] for more info.
/// - `read_mode`: Indicates the mode to read in. Use [`Reader::set_read_mode`] to set. See [`ReadMode`] for more info.
/// - `game_type`: Specifies which RPG Maker game type the data is from. Use [`Reader::set_game_type`] to set. See [`GameType`] for more info.
/// - `duplicate_mode` : Specifies, what to do with duplicates. Use [`Reader::set_duplicate_mode`] to set. See [`DuplicateMode`] for more info.
///
/// # Example
///
/// ```
/// use rvpacker_txt_rs_lib::{ReaderBuilder, FileFlags, GameType};
///
/// let mut reader = ReaderBuilder::new().with_files(FileFlags::Map | FileFlags::other()).build();
/// ```
#[derive(Default)]
pub struct ReaderBuilder {
    reader: Reader,
}

impl ReaderBuilder {
    /// Creates a new [`ReaderBuilder`] instance with default values.
    ///
    /// By default, all four file flags are set (all files will be read), the [`ReadMode::Default(false)`] read mode is used, duplicate mode is set to [`DuplicateMode::Allow`], and all other options are disabled.
    ///
    /// # Example
    ///
    /// ```
    /// use rvpacker_txt_rs_lib::ReaderBuilder;
    ///
    /// let mut reader = ReaderBuilder::new().build();
    /// ```
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Sets the file flags to determine which RPG Maker files will be parsed. See [`FileFlags`] for more info.
    ///
    /// # Parameters
    ///
    /// - `flags` - [`FileFlags`] bitflags indicating the file types to include.
    ///
    /// # Example
    ///
    /// ```
    /// use rvpacker_txt_rs_lib::{ReaderBuilder, FileFlags};
    ///
    /// let reader = ReaderBuilder::new().with_files(FileFlags::Map | FileFlags::other()).build();
    /// ```
    #[must_use]
    pub fn with_files(mut self, flags: FileFlags) -> Self {
        self.reader.processor.file_flags = flags;
        self
    }

    /// Sets the flags of the processor.
    ///
    /// Flags indicate, how to process text, and include options such as trimming, romanizing etc., for more info check [`BaseFlags`].
    ///
    /// # Parameters
    ///
    /// - `flags` - [`BaseFlags`] bitflags.
    ///
    /// # Example
    ///
    /// ```
    /// use rvpacker_txt_rs_lib::{ReaderBuilder, BaseFlags};
    ///
    /// let mut reader = ReaderBuilder::new().with_flags(BaseFlags::Trim | BaseFlags::Romanize);
    /// ```
    #[must_use]
    pub fn with_flags(mut self, flags: BaseFlags) -> Self {
        self.reader.processor.flags = flags;
        self
    }

    /// Sets the read mode that affects how data is parsed. See [`ReadMode`] for more info.
    ///
    /// # Parameters
    ///
    /// - `mode` - A [`ReadMode`] variant.
    ///
    /// # Example
    ///
    /// ```
    /// use rvpacker_txt_rs_lib::{ReaderBuilder, ReadMode};
    ///
    /// let reader = ReaderBuilder::new().read_mode(ReadMode::Default(false)).build();
    /// ```
    #[must_use]
    pub fn read_mode(mut self, mode: ReadMode) -> Self {
        self.reader.processor.mode = Mode::Read(mode);
        self
    }

    /// Sets, what to do with duplicates. Works only for map and other files. See [`DuplicateMode`] for more info.
    ///
    /// # Parameters
    ///
    /// - `mode` - A [`DuplicateMode`] variant.
    ///
    /// # Example
    ///
    /// ```
    /// use rvpacker_txt_rs_lib::{ReaderBuilder, DuplicateMode};
    ///
    /// let reader = ReaderBuilder::new().duplicate_mode(DuplicateMode::Allow).build();
    /// ```
    #[must_use]
    pub fn duplicate_mode(mut self, mode: DuplicateMode) -> Self {
        self.reader.processor.duplicate_mode = mode;
        self
    }

    /// Sets the game type for custom processing. See [`GameType`] for more info.
    ///
    /// # Parameters
    ///
    /// - `game_type` - A [`GameType`] variant.
    ///
    /// # Example
    ///
    /// ```
    /// use rvpacker_txt_rs_lib::{ReaderBuilder, GameType};
    ///
    /// let reader = ReaderBuilder::new().game_type(GameType::Termina).build();
    /// ```
    #[must_use]
    pub fn game_type(mut self, game_type: GameType) -> Self {
        self.reader.processor.game_type = game_type;
        self
    }

    /// Sets hashes from the previous read.
    ///
    /// Hashes are only used during [`ReadMode::Append`] read, and if a processed file matches the hashes, it's skipped.
    ///
    /// Note that you need to fetch the hashes again after reading in [`ReadMode::Append`] entries with [`Reader::hashes`], since files that don't match the hashes will cause hash recalculation.
    ///
    /// # Parameters
    ///
    /// - hashes - [`Vec<u128>`] of calculated hashes from the previous read
    ///
    #[must_use]
    pub fn hashes(mut self, hashes: Vec<u128>) -> Self {
        self.reader.processor.hashes = HashSet::from_iter(hashes);
        self
    }

    /// Builds and returns the [`Reader`].
    #[must_use]
    pub fn build(self) -> Reader {
        self.reader
    }
}

/// A struct used for writing translation from `.txt` files back to RPG Maker files.
///
/// The [`Writer`] struct, essentially, should receive the same options as [`Reader`], to ensure proper writing.
///
/// # Fields
///
/// - `file_flags`: Indicates which RPG Maker files should be processed. Use [`Writer::set_files`] to set. See [`FileFlags`] for more info.
/// - `flags`: Indicates different modes of processing the text. Use [`Writer::set_flags`] to set. See [`BaseFlags`] for more info.
/// - `game_type`: Specifies which RPG Maker game type the data is from. Use [`Writer::set_game_type`] to set. See [`GameType`] for more info.
/// - `duplicate_mode` : Specifies, what to do with duplicates. Use [`Writer::set_duplicate_mode`] to set. See [`DuplicateMode`] for more info.
///
/// # Example
///
/// ```no_run
/// use rvpacker_txt_rs_lib::{Writer, FileFlags, EngineType};
///
/// let mut writer = Writer::new();
/// writer.set_files(FileFlags::Map | FileFlags::other());
/// writer.write("C:/Game/Data", "C:/Game/translation", "C:/Game/output", EngineType::VXAce);
/// ```
pub struct Writer {
    processor: Processor,
}

impl Default for Writer {
    fn default() -> Self {
        Self {
            processor: Processor {
                mode: Mode::Write,
                ..Default::default()
            },
        }
    }
}

impl Writer {
    /// Creates a new [`Writer`] instance with default values.
    ///
    /// By default, all four file flags are set (all files will be written), duplicate mode is set to [`DuplicateMode::Allow`], and all other options are disabled.
    ///
    /// # Example
    ///
    /// ```
    /// use rvpacker_txt_rs_lib::Writer;
    ///
    /// let mut writer = Writer::new();
    /// ```
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Sets the file flags to determine which RPG Maker files will be parsed. See [`FileFlags`] for more info.
    ///
    /// # Parameters
    ///
    /// - `flags` - A [`FileFlags`] value indicating the file types to include.
    ///
    /// # Example
    ///
    /// ```
    /// use rvpacker_txt_rs_lib::{Writer, FileFlags};
    ///
    /// let mut writer = Writer::new();
    /// writer.set_files(FileFlags::Map | FileFlags::other());
    /// ```
    pub fn set_files(&mut self, file_flags: FileFlags) {
        self.processor.file_flags = file_flags;
    }

    /// Sets the flags of the processor.
    ///
    /// Flags indicate, how to process text, and include options such as trimming, romanizing etc., for more info check [`BaseFlags`].
    ///
    /// # Parameters
    ///
    /// - `flags` - [`BaseFlags`] bitflags.
    ///
    /// # Example
    ///
    /// ```
    /// use rvpacker_txt_rs_lib::{Writer, BaseFlags};
    ///
    /// let mut writer = Writer::new();
    /// writer.set_flags(BaseFlags::Trim | BaseFlags::Romanize);
    /// ```
    pub fn set_flags(&mut self, flags: BaseFlags) {
        self.processor.flags = flags;
    }

    /// This function must have the same value that was passed to it in [`Reader`] struct.
    ///
    /// Sets, what to do with duplicates. Works only for map and other files. See [`DuplicateMode`] for more info.
    ///
    /// # Parameters
    ///
    /// - `mode` - A [`DuplicateMode`] variant.
    ///
    /// # Example
    ///
    /// ```
    /// use rvpacker_txt_rs_lib::{Writer, DuplicateMode};
    ///
    /// let mut writer = Writer::new();
    /// writer.set_duplicate_mode(DuplicateMode::Allow);
    /// ```
    pub fn set_duplicate_mode(&mut self, mode: DuplicateMode) {
        self.processor.duplicate_mode = mode;
    }

    /// This function must have the same value that was passed to it in [`Reader`] struct.
    ///
    /// Sets the game type for custom processing. See [`GameType`] for more info.
    ///
    /// # Parameters
    ///
    /// - `game_type` - A [`GameType`] variant.
    ///
    /// # Example
    ///
    /// ```
    /// use rvpacker_txt_rs_lib::{Writer, GameType};
    ///
    /// let mut writer = Writer::new();
    /// writer.set_game_type(GameType::Termina);
    /// ```
    pub fn set_game_type(&mut self, game_type: GameType) {
        self.processor.game_type = game_type;
    }

    /// Writes the translation from `.txt` files in `translation_path`, and outputs modified
    /// files from `source_path` to `output_path`.
    ///
    /// Make sure you've configured the writer with the same options as reader before calling it.
    ///
    /// # Parameters
    ///
    /// - `source_path` - Path to the directory containing source RPG Maker files.
    ///
    ///   For `MV/MZ` engines, parent directory of `source_path` must contain `js` directory.
    ///
    /// - `translation_path` - Path to the directory where `.txt` translation files are located.
    /// - `output_path` - Path to the directory, where output RPG Maker files will be created.
    /// - `engine_type` - Engine type of the source RPG Maker files.
    ///
    /// # Example
    ///
    /// ```no_run
    /// use rvpacker_txt_rs_lib::{Writer, EngineType};
    ///
    /// let mut writer = Writer::new();
    /// writer.write("C:/Game/Data", "C:/Game/translation", "C:/Game/output", EngineType::VXAce);
    /// ```
    ///
    /// # Returns
    ///
    /// - Nothing on success.
    /// - [`Error`] otherwise.
    ///
    /// # Errors
    ///
    /// - [`Error::Io`] on I/O error.
    /// - [`Error::MarshalLoad`] on RPG Maker XP/VX/VXAce file parsing fail.
    /// - [`Error::JsonParse`] on JSON file parsing fail.
    ///
    pub fn write<P: AsRef<Path>>(
        &mut self,
        source_path: P,
        translation_path: P,
        output_path: P,
        engine_type: EngineType,
    ) -> Result<(), Error> {
        self.processor.process(
            engine_type,
            source_path,
            translation_path,
            Some(&output_path),
        )?;
        Ok(())
    }
}

/// A builder struct for [`Writer`].
///
/// The [`Writer`] struct, essentially, should receive the same options as [`Reader`], to ensure proper writing.
///
/// # Fields
/// - `file_flags`: Indicates which RPG Maker files should be processed. Use [`WriterBuilder::with_files`] to set.
/// - `game_type`: Specifies which RPG Maker game type the data is from. Use [`WriterBuilder::game_type`] to set.
/// - `flags`: Indicates different modes of processing the text. Use [`WriterBuilder::with_flags`]. For more info, see [`BaseFlags`].
///
/// # Example
///
/// ```
/// use rvpacker_txt_rs_lib::{WriterBuilder, FileFlags, GameType};
///
/// let mut writer = WriterBuilder::new().with_files(FileFlags::Map | FileFlags::other()).build();
/// ```
#[derive(Default)]
pub struct WriterBuilder {
    writer: Writer,
}

impl WriterBuilder {
    /// Creates a new [`WriterBuilder`] instance with default values.
    ///
    /// By default, all four file flags are set (all files will be written), duplicate mode is set to [`DuplicateMode::Allow`], and all other options are disabled.
    ///
    /// # Example
    ///
    /// ```
    /// use rvpacker_txt_rs_lib::WriterBuilder;
    ///
    /// let mut writer = WriterBuilder::new().build();
    /// ```
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Sets the file flags to determine which RPG Maker files will be parsed. See [`FileFlags`] for more info.
    ///
    /// # Parameters
    ///
    /// - `flags` - A [`FileFlags`] value indicating the file types to include.
    ///
    /// # Example
    ///
    /// ```
    /// use rvpacker_txt_rs_lib::{WriterBuilder, FileFlags};
    ///
    /// let writer = WriterBuilder::new().with_files(FileFlags::Map | FileFlags::other()).build();
    /// ```
    #[must_use]
    pub fn with_files(mut self, flags: FileFlags) -> Self {
        self.writer.processor.file_flags = flags;
        self
    }

    /// Sets the flags of the processor.
    ///
    /// Flags indicate, how to process text, and include options such as trimming, romanizing etc., for more info check [`BaseFlags`].
    ///
    /// # Parameters
    ///
    /// - `flags` - [`BaseFlags`] bitflags.
    ///
    /// # Example
    ///
    /// ```
    /// use rvpacker_txt_rs_lib::{WriterBuilder, BaseFlags};
    ///
    /// let mut writer = WriterBuilder::new().with_flags(BaseFlags::Trim | BaseFlags::Romanize);
    /// ```
    #[must_use]
    pub fn with_flags(mut self, flags: BaseFlags) -> Self {
        self.writer.processor.flags = flags;
        self
    }

    /// This function must have the same value that was passed to it in [`Reader`] struct.
    ///
    /// Sets, what to do with duplicates. Works only for map and other files. See [`DuplicateMode`] for more info.
    ///
    /// # Parameters
    ///
    /// - `mode` - A [`DuplicateMode`] variant.
    ///
    /// # Example
    ///
    /// ```
    /// use rvpacker_txt_rs_lib::{WriterBuilder, DuplicateMode};
    ///
    /// let writer = WriterBuilder::new().duplicate_mode(DuplicateMode::Remove).build();
    /// ```
    #[must_use]
    pub fn duplicate_mode(mut self, mode: DuplicateMode) -> Self {
        self.writer.processor.duplicate_mode = mode;
        self
    }

    /// This function must have the same value that was passed to it in [`Reader`] struct.
    ///
    /// Sets the game type for custom processing. See [`GameType`] for more info.
    ///
    /// # Parameters
    ///
    /// - `game_type` - A [`GameType`] variant.
    ///
    /// # Example
    ///
    /// ```
    /// use rvpacker_txt_rs_lib::{WriterBuilder, GameType};
    ///
    /// let writer = WriterBuilder::new().game_type(GameType::Termina).build();
    /// ```
    #[must_use]
    pub fn game_type(mut self, game_type: GameType) -> Self {
        self.writer.processor.game_type = game_type;
        self
    }

    /// Builds and returns the [`Writer`].
    #[must_use]
    pub fn build(self) -> Writer {
        self.writer
    }
}

/// A struct used for purging lines with no translation from `.txt` files.
///
/// The [`Purger`] struct, essentially, should receive the same options as [`Reader`], to ensure proper purging.
///
/// # Fields
/// - `file_flags`: Indicates which RPG Maker files should be processed. Use [`Purger::set_files`] to set. See [`FileFlags`] for more info.
/// - `flags`: Indicates different modes of processing the text. Use [`Purger::set_flags`] to set. See [`BaseFlags`] for more info.
/// - `game_type`: Specifies which RPG Maker game type the data is from. Use [`Purger::set_game_type`] to set. See [`GameType`] for more info.
/// - `duplicate_mode` : Specifies, what to do with duplicates. Use [`Purger::set_duplicate_mode`] to set. See [`DuplicateMode`] for more info.
///
/// # Example
///
/// ```no_run
/// use rvpacker_txt_rs_lib::{Purger, FileFlags, EngineType};
///
/// let mut purger = Purger::new();
/// purger.set_files(FileFlags::Map | FileFlags::other());
/// let result = purger.purge("C:/Game/Data", "C:/Game/translation", EngineType::VXAce);
/// ```
pub struct Purger {
    processor: Processor,
}

impl Default for Purger {
    fn default() -> Self {
        Self {
            processor: Processor {
                mode: Mode::Purge,
                ..Default::default()
            },
        }
    }
}

impl Purger {
    /// Creates a new [`Purger`] instance with default values.
    ///
    /// By default, all four file flags are set (all files will be purged), duplicate mode is set to [`DuplicateMode::Allow`], and all other options are disabled.
    ///
    /// # Example
    ///
    /// ```
    /// use rvpacker_txt_rs_lib::Purger;
    ///
    /// let mut purger = Purger::new();
    /// ```
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Sets the file flags to determine which RPG Maker files will be parsed. See [`FileFlags`] for more info.
    ///
    /// # Parameters
    ///
    /// - `flags` - A [`FileFlags`] value indicating the file types to include.
    ///
    /// # Example
    ///
    /// ```
    /// use rvpacker_txt_rs_lib::{Purger, FileFlags};
    ///
    /// let mut purger = Purger::new();
    /// purger.set_files(FileFlags::Map | FileFlags::other());
    /// ```
    pub fn set_files(&mut self, file_flags: FileFlags) {
        self.processor.file_flags = file_flags;
    }

    /// Sets the flags of the processor.
    ///
    /// Flags indicate, how to process text, and include options such as trimming, romanizing etc., for more info check [`BaseFlags`].
    ///
    /// # Parameters
    ///
    /// - `flags` - [`BaseFlags`] bitflags.
    ///
    /// # Example
    ///
    /// ```
    /// use rvpacker_txt_rs_lib::{Purger, BaseFlags};
    ///
    /// let mut purger = Purger::new();
    /// purger.set_flags(BaseFlags::Trim | BaseFlags::Romanize);
    /// ```
    pub fn set_flags(&mut self, flags: BaseFlags) {
        self.processor.flags = flags;
    }

    /// This function must have the same value that was passed to it in [`Reader`] struct.
    ///
    /// Sets, what to do with duplicates. Works only for map and other files. See [`DuplicateMode`] for more info.
    ///
    /// # Parameters
    ///
    /// - `mode` - A [`DuplicateMode`] variant.
    ///
    /// # Example
    ///
    /// ```
    /// use rvpacker_txt_rs_lib::{Purger, DuplicateMode};
    ///
    /// let mut purger = Purger::new();
    /// purger.set_duplicate_mode(DuplicateMode::Allow);
    /// ```
    pub fn set_duplicate_mode(&mut self, mode: DuplicateMode) {
        self.processor.duplicate_mode = mode;
    }

    /// This function must have the same value that was passed to it in [`Reader`] struct.
    ///
    /// Sets the game type for custom processing. See [`GameType`] for more info.
    ///
    /// # Parameters
    ///
    /// - `game_type` - A [`GameType`] variant.
    ///
    /// # Example
    ///
    /// ```
    /// use rvpacker_txt_rs_lib::{Purger, GameType};
    ///
    /// let mut purger = Purger::new();
    /// purger.set_game_type(GameType::Termina);
    /// ```
    pub fn set_game_type(&mut self, game_type: GameType) {
        self.processor.game_type = game_type;
    }

    /// Purges the lines with no translation from `.txt` files in `translation_path`, using source RPG Maker files from `source_path`.
    ///
    /// Make sure you've configured the purger with the same options as reader before calling it.
    ///
    /// # Parameters
    ///
    /// - `source_path` - Path to the directory containing RPG Maker files.
    /// - `translation_path` - Path to the directory containing `.txt` translation files.
    /// - `engine_type` - Engine type of the source RPG Maker files.
    ///
    /// # Errors
    ///
    /// - [`Error::Io`] - if any I/O operation fails.
    /// - [`Error::JsonParse`] - if parsing any JSON fails.
    /// - [`Error::MarshalLoad`] - if loading any Marshal byte stream fails.
    ///
    /// # Example
    ///
    /// ```no_run
    /// use rvpacker_txt_rs_lib::{Purger, EngineType};
    ///
    /// let mut purger = Purger::new();
    /// purger.purge("C:/Game/Data", "C:/Game/translation", EngineType::VXAce);
    /// ```
    pub fn purge<P: AsRef<Path>>(
        &mut self,
        source_path: P,
        translation_path: P,
        engine_type: EngineType,
    ) -> Result<(), Error> {
        self.processor.process(
            engine_type,
            source_path,
            translation_path,
            None,
        )?;
        Ok(())
    }
}

/// A builder struct for [`Purger`].
///
/// The `Purger` struct, essentially, should receive the same options as [`Reader`], to ensure proper purging.
///
/// # Fields
/// - `file_flags`: Indicates which RPG Maker files should be processed. Use [`PurgerBuilder::with_files`] to set.
/// - `flags`: Indicates different modes of processing the text. Use [`PurgerBuilder::with_flags`]. For more info, see [`BaseFlags`].
/// - `game_type`: Specifies which RPG Maker game type the data is from. Use [`PurgerBuilder::game_type`] to set.
/// - `duplicate_mode` : Specifies, what to do with duplicates. Use [`PurgerBuilder::duplicate_mode`] to set.
///
/// # Example
///
/// ```
/// use rvpacker_txt_rs_lib::{PurgerBuilder, FileFlags, GameType};
///
/// let mut purger = PurgerBuilder::new().with_files(FileFlags::Map | FileFlags::other()).build();
/// ```
#[derive(Default)]
pub struct PurgerBuilder {
    purger: Purger,
}

impl PurgerBuilder {
    /// Creates a new [`PurgerBuilder`] instance with default values.
    ///
    /// By default, all four file flags are set (all files will be purged), duplicate mode is set to [`DuplicateMode::Allow`], and all other options are disabled.
    ///
    /// # Example
    ///
    /// ```
    /// use rvpacker_txt_rs_lib::PurgerBuilder;
    ///
    /// let mut purger = PurgerBuilder::new().build();
    /// ```
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Sets the file flags to determine which RPG Maker files will be parsed. See [`FileFlags`] for more info.
    ///
    /// # Parameters
    ///
    /// - `flags` - A [`FileFlags`] value indicating the file types to include.
    ///
    /// # Example
    ///
    /// ```
    /// use rvpacker_txt_rs_lib::{PurgerBuilder, FileFlags};
    ///
    /// let purger = PurgerBuilder::new().with_files(FileFlags::Map | FileFlags::other()).build();
    /// ```
    #[must_use]
    pub fn with_files(mut self, file_flags: FileFlags) -> Self {
        self.purger.processor.file_flags = file_flags;
        self
    }

    /// Sets the flags of the processor.
    ///
    /// Flags indicate, how to process text, and include options such as trimming, romanizing etc., for more info check [`BaseFlags`].
    ///
    /// # Parameters
    ///
    /// - `flags` - [`BaseFlags`] bitflags.
    ///
    /// # Example
    ///
    /// ```
    /// use rvpacker_txt_rs_lib::{PurgerBuilder, BaseFlags};
    ///
    /// let mut purger = PurgerBuilder::new().with_flags(BaseFlags::Trim | BaseFlags::Romanize);
    /// ```
    #[must_use]
    pub fn with_flags(mut self, flags: BaseFlags) -> Self {
        self.purger.processor.flags = flags;
        self
    }

    /// This function must have the same value that was passed to it in [`Reader`] struct.
    ///
    /// Sets, what to do with duplicates. Works only for map and other files. See [`DuplicateMode`] for more info.
    ///
    /// # Parameters
    ///
    /// - `mode` - A [`DuplicateMode`] variant.
    ///
    /// # Example
    ///
    /// ```
    /// use rvpacker_txt_rs_lib::{PurgerBuilder, DuplicateMode};
    ///
    /// let purger = PurgerBuilder::new().duplicate_mode(DuplicateMode::Allow).build();
    /// ```
    #[must_use]
    pub fn duplicate_mode(mut self, mode: DuplicateMode) -> Self {
        self.purger.processor.duplicate_mode = mode;
        self
    }

    /// This function must have the same value that was passed to it in [`Reader`] struct.
    ///
    /// Sets the game type for custom processing. See [`GameType`] for more info.
    ///
    /// # Parameters
    ///
    /// - `game_type` - A [`GameType`] variant.
    ///
    /// # Example
    ///
    /// ```
    /// use rvpacker_txt_rs_lib::{PurgerBuilder, GameType};
    ///
    /// let purger = PurgerBuilder::new().game_type(GameType::Termina).build();
    /// ```
    #[must_use]
    pub fn game_type(mut self, game_type: GameType) -> Self {
        self.purger.processor.game_type = game_type;
        self
    }

    /// Builds and returns the [`Purger`].
    #[must_use]
    pub fn build(self) -> Purger {
        self.purger
    }
}
