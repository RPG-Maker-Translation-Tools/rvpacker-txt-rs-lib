use crate::{constants::*, core::*, types::*};
use std::{
    fs::{create_dir_all, read, read_dir, read_to_string, write},
    mem::take,
    path::{Path, PathBuf},
};

#[derive(Default)]
pub(crate) struct Processor {
    pub file_flags: FileFlags,
    pub base: Base,
}

impl Processor {
    pub fn process<P: AsRef<Path>>(
        &mut self,
        mode: Mode,
        engine_type: EngineType,
        source_path: P,
        translation_path: P,
        output_path: Option<P>,
    ) -> Result<(), Error> {
        if self.file_flags.is_empty() {
            return Ok(());
        }

        self.base.mode = mode;
        self.base.set_engine_type(engine_type);

        let default_path = PathBuf::default();

        let (source_path, translation_path, output_path) = (
            source_path.as_ref(),
            translation_path.as_ref(),
            if let Some(ref path) = output_path {
                path.as_ref()
            } else {
                default_path.as_path()
            },
        );

        let ignore_file_path = translation_path.join(RVPACKER_IGNORE_FILE);

        if self.base.ignore || self.base.create_ignore {
            match read_to_string(&ignore_file_path)
                .map_err(|e| Error::Io(ignore_file_path.clone(), e))
            {
                Ok(ignore_file_content) => {
                    self.base.ignore_map = parse_ignore(
                        &ignore_file_content,
                        self.base.duplicate_mode,
                        self.base.mode.is_read(),
                    );
                }
                Err(err) => {
                    if self.base.ignore {
                        return Err(err);
                    }
                }
            }
        }

        create_dir_all(if self.base.mode.is_read() {
            translation_path
        } else {
            output_path
        })
        .map_err(|e| {
            Error::Io(
                if self.base.mode.is_read() {
                    translation_path.to_path_buf()
                } else {
                    output_path.to_path_buf()
                },
                e,
            )
        })?;

        let data_output_path = output_path.join(if engine_type.is_new() {
            "data"
        } else {
            "Data"
        });

        if self.base.mode.is_write() {
            create_dir_all(&data_output_path)
                .map_err(|e| Error::Io(data_output_path.clone(), e))?;
        }

        let entries = read_dir(source_path)
            .map_err(|e| Error::Io(source_path.to_path_buf(), e))?
            .flatten();

        let msg = if self.base.mode.is_write() {
            "Successfully written."
        } else if self.base.mode.is_read() {
            "Successfully read."
        } else {
            "Successfully purged."
        };

        if self.file_flags.contains(FileFlags::Map) {
            let base_ref = unsafe { &mut *(&mut self.base as *mut Base) };
            let mut map_base = MapBase::new(base_ref);

            let mapinfos_path = source_path.join(format!(
                "Mapinfos.{}",
                get_engine_extension(engine_type)
            ));
            let mapinfos = read(&mapinfos_path)
                .map_err(|e| Error::Io(mapinfos_path, e))?;
            map_base.initialize_mapinfos(&mapinfos)?;

            if self.base.read_mode.is_append() || !self.base.mode.is_read() {
                let translation_file_path = translation_path.join("maps.txt");
                let translation = read_to_string(&translation_file_path)
                    .map_err(|e| Error::Io(translation_file_path, e))?;
                map_base.initialize_translation(&translation);
            }

            for map_entry in filter_maps(entries, engine_type) {
                let path = map_entry.path();
                let filename = path.file_name().unwrap().to_str().unwrap();
                let content = read(&path)
                    .map_err(|e| Error::Io(path.to_path_buf(), e))?;
                let data = map_base.process(filename, &content)?;

                if self.base.mode.is_write() {
                    if let Some(data) = data {
                        let output_file_path = data_output_path.join(filename);
                        write(&output_file_path, data)
                            .map_err(|e| Error::Io(output_file_path, e))?;
                    }
                }

                log::info!("{filename}: {msg}");
            }

            if !self.base.mode.is_write() {
                let translation_data = map_base.translation();
                let translation_file_path = translation_path.join("maps.txt");

                write(&translation_file_path, translation_data)
                    .map_err(|e| Error::Io(translation_file_path, e))?;
            }
        }

        let entries = read_dir(source_path)
            .map_err(|e| Error::Io(source_path.to_path_buf(), e))?
            .flatten();

        if self.file_flags.contains(FileFlags::Other) {
            let base_ref = unsafe { &mut *(&mut self.base as *mut Base) };
            let mut other_base = OtherBase::new(base_ref);

            for map_entry in
                filter_other(entries, engine_type, self.base.game_type)
            {
                let path = map_entry.path();
                let filename = path.file_name().unwrap().to_str().unwrap();

                if self.base.read_mode.is_append() || !self.base.mode.is_read()
                {
                    let translation_path = translation_path
                        .join(Path::new(filename).with_extension("txt"));
                    let translation = read_to_string(&translation_path)
                        .map_err(|e| Error::Io(translation_path, e))?;
                    other_base.initialize_translation(filename, &translation);
                }

                let content = read(&path)
                    .map_err(|e| Error::Io(path.to_path_buf(), e))?;
                let data = other_base.process(filename, &content)?;

                let output_file_path = if self.base.mode.is_write() {
                    data_output_path.join(filename)
                } else {
                    translation_path.join(
                        Path::new(&filename.to_ascii_lowercase())
                            .with_extension("txt"),
                    )
                };

                write(&output_file_path, data)
                    .map_err(|e| Error::Io(output_file_path, e))?;

                log::info!("{filename}: {msg}");
            }
        }

        if self.file_flags.contains(FileFlags::System) {
            let base_ref = unsafe { &mut *(&mut self.base as *mut Base) };
            let mut system_base = SystemBase::new(base_ref);

            if self.base.read_mode.is_append() || !self.base.mode.is_read() {
                let translation_path = translation_path.join("system.txt");
                let translation = read_to_string(&translation_path)
                    .map_err(|e| Error::Io(translation_path, e))?;
                system_base.initialize_translation(&translation);
            }

            let filename =
                format!("System.{}", get_engine_extension(engine_type));
            let system_file_path = source_path.join(&filename);
            let system_file_data = read(&system_file_path)
                .map_err(|e| Error::Io(system_file_path, e))?;
            let data = system_base.process(&system_file_data)?;

            let output_file_path = if self.base.mode.is_write() {
                data_output_path.join(&filename)
            } else {
                translation_path.join("system.txt")
            };

            write(&output_file_path, data)
                .map_err(|e| Error::Io(output_file_path, e))?;

            log::info!("{filename}: {msg}");
        }

        if self.file_flags.contains(FileFlags::Scripts) {
            let base_ref = unsafe { &mut *(&mut self.base as *mut Base) };

            if engine_type.is_new() {
                let mut plugin_base = PluginBase::new(base_ref);

                if self.base.read_mode.is_append() || !self.base.mode.is_read()
                {
                    let translation_path = translation_path.join("plugins.txt");
                    let translation = read_to_string(&translation_path)
                        .map_err(|e| Error::Io(translation_path, e))?;
                    plugin_base.initialize_translation(&translation);
                }

                let plugins_file_path = unsafe {
                    source_path
                        .parent()
                        .unwrap_unchecked()
                        .join("js/plugins.js")
                };
                let plugins_file_data = read(&plugins_file_path)
                    .map_err(|e| Error::Io(plugins_file_path, e))?;
                let data = plugin_base.process(&plugins_file_data)?;

                let output_file_path = if self.base.mode.is_write() {
                    let js_output_path = output_path.join("js");
                    create_dir_all(&js_output_path)
                        .map_err(|e| Error::Io(js_output_path, e))?;
                    output_path.join("js/plugins.js")
                } else {
                    translation_path.join("plugins.txt")
                };

                write(&output_file_path, data)
                    .map_err(|e| Error::Io(output_file_path, e))?;

                log::info!("plugins.js: {msg}");
            } else {
                let mut script_base = ScriptBase::new(base_ref);

                if self.base.read_mode.is_append() || !self.base.mode.is_read()
                {
                    let translation_path = translation_path.join("scripts.txt");
                    let translation = read_to_string(&translation_path)
                        .map_err(|e| Error::Io(translation_path, e))?;
                    script_base.initialize_translation(&translation);
                }

                let filename =
                    format!("Scripts.{}", get_engine_extension(engine_type));
                let scripts_file_path = source_path.join(&filename);
                let scripts_file_data = read(&scripts_file_path)
                    .map_err(|e| Error::Io(scripts_file_path, e))?;
                let data = script_base.process(&scripts_file_data)?;

                let output_file_path = if self.base.mode.is_write() {
                    data_output_path.join(&filename)
                } else {
                    translation_path.join("scripts.txt")
                };

                write(&output_file_path, data)
                    .map_err(|e| Error::Io(output_file_path, e))?;

                log::info!("{filename}: {msg}");
            }
        }

        if self.base.create_ignore {
            use std::fmt::Write;

            let contents: String = take(&mut self.base.ignore_map)
                .into_iter()
                .fold(String::new(), |mut output, (file, lines)| {
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
                });

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
/// - `file_flags`: Indicates which RPG Maker files should be processed. Use [`Reader::set_flags`] to set them.
/// - `read_mode`: Defines the read strategy. Use [`Reader::set_read_mode`] to set it.
/// - `game_type`: Specifies which RPG Maker game type the data is from. Use [`Reader::set_game_type`] to set it.
/// - `romanize`: Enables or disables romanization of parsed text. For more info, and to set it, see [`Reader::set_romanize`].
/// - `ignore`: Ignores entries from `.rvpacker-ignore` file. Use [`Reader::set_ignore`] to set it.
/// - `trim`: Removes leading and trailing whitespace from extracted strings. Use [`Reader::set_trim`] to set it.
///
/// # Example
/// ```no_run
/// use rvpacker_txt_rs_lib::{Reader, FileFlags, EngineType};
///
/// let mut reader = Reader::new();
/// reader.set_flags(FileFlags::Map | FileFlags::Other);
/// reader.read("C:/Game/Data", "C:/Game/translation", EngineType::VXAce);
/// ```
#[derive(Default)]
pub struct Reader {
    processor: Processor,
}

impl Reader {
    /// Creates a new [`Reader`] instance with default values.
    ///
    /// By default, all four file flags are set (all files will be read), the [`ReadMode::Default`] read mode is used, duplicate mode is set to [`DuplicateMode::Allow`], and all other options are disabled.
    ///
    /// # Example
    /// ```
    /// use rvpacker_txt_rs_lib::Reader;
    ///
    /// let mut reader = Reader::new();
    /// ```
    pub fn new() -> Self {
        Self::default()
    }

    /// Sets the file flags to determine which RPG Maker files will be parsed.
    ///
    /// There's four FileFlags variants:
    /// - [`FileFlags::Map`] - enables `Mapxxx.ext` files processing.
    /// - [`FileFlags::Other`] - enables processing files other than `Map`, `System`, `Scripts` and `plugins`.
    /// - [`FileFlags::System`] - enables `System.txt` file processing.
    /// - [`FileFlags::Scripts`] - enables `Scripts.ext`/`plugins.js` file processing, based on engine type.
    ///
    /// # Arguments
    /// - `flags` - A [`FileFlags`] value indicating the file types to include.
    ///
    /// # Example
    /// ```
    /// use rvpacker_txt_rs_lib::{Reader, FileFlags};
    ///
    /// let mut reader = Reader::new();
    /// reader.set_flags(FileFlags::Map | FileFlags::Other);
    /// ```
    pub fn set_flags(&mut self, flags: FileFlags) {
        self.processor.file_flags = flags;
    }

    /// Sets the read mode that affects how data is parsed.
    ///
    /// There's only three read modes:
    /// - [`ReadMode::Default`] - parses the text from the RPG Maker files, aborts if translation files already exist.
    /// - [`ReadMode::Append`] - appends the new text to the translation files. That's particularly helpful if the game received content update.
    /// - [`ReadMode::Force`] - parses the text from the RPG Maker files, overwrites the existing translation. **DANGEROUS!**
    ///
    /// # Arguments
    /// - `mode` - A [`ReadMode`] variant.
    ///
    /// # Example
    /// ```
    /// use rvpacker_txt_rs_lib::{Reader, ReadMode};
    ///
    /// let mut reader = Reader::new();
    /// reader.set_read_mode(ReadMode::Default);
    /// ```
    pub fn set_read_mode(&mut self, mode: ReadMode) {
        self.processor.base.read_mode = mode;
    }

    /// Sets the game type for custom processing.
    ///
    /// Right now, custom processing is implement for Fear & Hunger 2: Termina ([`GameType::Termina`]), and LisaRPG series games ([`GameType::LisaRPG`]).
    ///
    /// There's no single definition for "custom processing", but the current implementations filter out unnecessary text and improve the readability of output `.txt` files.
    ///
    /// For example, in LisaRPG games, `\nbt` prefix is used in dialogues to mark the tile, above which textbox should appear. When `game_type` is set to [`GameType::LisaRPG`], this prefix is not included to the output `.txt` files.
    ///
    /// # Arguments
    /// - `game_type` - A [`GameType`] variant.
    ///
    /// # Example
    /// ```
    /// use rvpacker_txt_rs_lib::{Reader, GameType};
    ///
    /// let mut reader = Reader::new();
    /// reader.set_game_type(GameType::Termina);
    /// ```
    pub fn set_game_type(&mut self, game_type: GameType) {
        self.processor.base.game_type = game_type;
    }

    /// Enables or disables romanization of the parsed text.
    ///
    /// Essentially, this flag just replaces Eastern symbols in strings to their Western equivalents.
    ///
    /// For example, 「」 Eastern (Japanese) quotation marks will be replaced by `''`.
    ///
    /// # Example
    /// ```
    /// use rvpacker_txt_rs_lib::Reader;
    ///
    /// let mut reader = Reader::new();
    /// reader.set_romanize(true);
    /// ```
    pub fn set_romanize(&mut self, enabled: bool) {
        self.processor.base.romanize = enabled;
    }

    /// Sets whether to ignore entries from `.rvpacker-ignore` file.
    ///
    /// # Example
    /// ```
    /// use rvpacker_txt_rs_lib::Reader;
    ///
    /// let mut reader = Reader::new();
    /// reader.set_ignore(true);
    /// ```
    pub fn set_ignore(&mut self, enabled: bool) {
        self.processor.base.ignore = enabled;
    }

    /// Sets whether to trim whitespace from strings.
    ///
    /// # Example
    /// ```
    /// use rvpacker_txt_rs_lib::{Reader, DuplicateMode};
    ///
    /// let mut reader = Reader::new();
    /// reader.set_trim(true);
    /// ```
    pub fn set_trim(&mut self, enabled: bool) {
        self.processor.base.trim = enabled;
    }

    /// This function must have the same value that was passed to it in [`Reader`] struct.
    ///
    /// Sets, what to do with duplicates.
    ///
    /// - [`DuplicateMode::Allow`]: Default and recommended. Each map/event is parsed into its own hashmap. That won't likely cause much clashes between the same lines which require different translations.
    /// - [`DuplicateMode::Remove`]: Not recommended. This mode is stable and works perfectly, but it will write the same translation into multiple places where source text is used. Recommended only when duplicates cause too much bloat.
    ///
    /// # Example
    /// ```
    /// use rvpacker_txt_rs_lib::{Reader, DuplicateMode};
    ///
    /// let mut reader = Reader::new();
    /// reader.set_duplicate_mode(DuplicateMode::Allow);
    /// ```
    pub fn set_duplicate_mode(&mut self, mode: DuplicateMode) {
        self.processor.base.duplicate_mode = mode;
    }

    /// Reads the RPG Maker files from `source_path` to `.txt` files in `translation_path`.
    ///
    /// Make sure you've configured the reader as you desire before calling it.
    ///
    /// # Arguments
    /// - `source_path` - Path to the directory containing RPG Maker files.
    /// - `translation_path` - Path to the directory where `.txt` files will be created.
    /// - `engine_type` - Engine type of the source RPG Maker files.
    ///
    /// # Example
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
            Mode::Read,
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
/// - `file_flags`: Indicates which RPG Maker files should be processed. Use [`ReaderBuilder::with_flags`] to set them.
/// - `read_mode`: Defines the read strategy. Use [`ReaderBuilder::read_mode`] to set it.
/// - `game_type`: Specifies which RPG Maker game type the data is from. Use [`ReaderBuilder::game_type`] to set it.
/// - `romanize`: Enables or disables romanization of parsed text. For more info, and to set it, see [`ReaderBuilder::romanize`].
/// - `ignore`: Ignores entries from `.rvpacker-ignore` file. Use [`ReaderBuilder::ignore`] to set it.
/// - `trim`: Removes leading and trailing whitespace from extracted strings. Use [`ReaderBuilder::trim`] to set it.
///
/// # Example
/// ```
/// use rvpacker_txt_rs_lib::{ReaderBuilder, FileFlags, GameType};
///
/// let mut reader = ReaderBuilder::new().with_flags(FileFlags::Map | FileFlags::Other).build();
/// ```
#[derive(Default)]
pub struct ReaderBuilder {
    reader: Reader,
}

impl ReaderBuilder {
    /// Creates a new [`ReaderBuilder`] instance with default values.
    ///
    /// By default, all four file flags are set (all files will be read), the [`ReadMode::Default`] read mode is used, duplicate mode is set to [`DuplicateMode::Allow`], and all other options are disabled.
    ///
    /// # Example
    /// ```
    /// use rvpacker_txt_rs_lib::ReaderBuilder;
    ///
    /// let mut reader = ReaderBuilder::new().build();
    /// ```
    pub fn new() -> Self {
        Self::default()
    }

    /// Sets the file flags to determine which RPG Maker files will be parsed.
    ///
    /// There's four FileFlags variants:
    /// - [`FileFlags::Map`] - enables `Mapxxx.ext` files processing.
    /// - [`FileFlags::Other`] - enables processing files other than `Map`, `System`, `Scripts` and `plugins`.
    /// - [`FileFlags::System`] - enables `System.txt` file processing.
    /// - [`FileFlags::Scripts`] - enables `Scripts.ext`/`plugins.js` file processing, based on engine type.
    ///
    /// # Arguments
    /// - `flags` - A [`FileFlags`] value indicating the file types to include.
    ///
    /// # Example
    /// ```
    /// use rvpacker_txt_rs_lib::{ReaderBuilder, FileFlags};
    ///
    /// let reader = ReaderBuilder::new().with_flags(FileFlags::Map | FileFlags::Other).build();
    /// ```
    pub fn with_flags(mut self, flags: FileFlags) -> Self {
        self.reader.processor.file_flags = flags;
        self
    }

    /// Sets the read mode that affects how data is parsed.
    ///
    /// There's only three read modes:
    /// - [`ReadMode::Default`] - parses the text from the RPG Maker files, aborts if translation files already exist.
    /// - [`ReadMode::Append`] - appends the new text to the translation files. That's particularly helpful if the game received content update.
    /// - [`ReadMode::Force`] - parses the text from the RPG Maker files, overwrites the existing translation. **DANGEROUS!**
    ///
    /// # Arguments
    /// - `mode` - A [`ReadMode`] variant.
    ///
    /// # Example
    /// ```
    /// use rvpacker_txt_rs_lib::{ReaderBuilder, ReadMode};
    ///
    /// let reader = ReaderBuilder::new().read_mode(ReadMode::Default).build();
    /// ```
    pub fn read_mode(mut self, mode: ReadMode) -> Self {
        self.reader.processor.base.read_mode = mode;
        self
    }

    /// Sets the game type for custom processing.
    ///
    /// Right now, custom processing is implement for Fear & Hunger 2: Termina ([`GameType::Termina`]), and LisaRPG series games ([`GameType::LisaRPG`]).
    ///
    /// There's no single definition for "custom processing", but the current implementations filter out unnecessary text and improve the readability of output `.txt` files.
    ///
    /// For example, in LisaRPG games, `\nbt` prefix is used in dialogues to mark the tile, above which textbox should appear. When `game_type` is set to [`GameType::LisaRPG`], this prefix is not included to the output `.txt` files.
    ///
    /// # Arguments
    /// - `game_type` - A [`GameType`] variant.
    ///
    /// # Example
    /// ```
    /// use rvpacker_txt_rs_lib::{ReaderBuilder, GameType};
    ///
    /// let reader = ReaderBuilder::new().game_type(GameType::Termina).build();
    /// ```
    pub fn game_type(mut self, game_type: GameType) -> Self {
        self.reader.processor.base.game_type = game_type;
        self
    }

    /// Enables or disables romanization of the parsed text.
    ///
    /// Essentially, this flag just replaces Eastern symbols in strings to their Western equivalents.
    ///
    /// For example, 「」 Eastern (Japanese) quotation marks will be replaced by `''`.
    ///
    /// # Example
    /// ```
    /// use rvpacker_txt_rs_lib::ReaderBuilder;
    ///
    /// let reader = ReaderBuilder::new().romanize(true).build();
    /// ```
    pub fn romanize(mut self, enabled: bool) -> Self {
        self.reader.processor.base.romanize = enabled;
        self
    }

    /// Sets whether to ignore entries from `.rvpacker-ignore` file.
    ///
    /// # Example
    /// ```
    /// use rvpacker_txt_rs_lib::ReaderBuilder;
    ///
    /// let reader = ReaderBuilder::new().ignore(true).build();
    /// ```
    pub fn ignore(mut self, enabled: bool) -> Self {
        self.reader.processor.base.ignore = enabled;
        self
    }

    /// Sets whether to trim whitespace from strings.
    ///
    /// # Example
    /// ```
    /// use rvpacker_txt_rs_lib::ReaderBuilder;
    ///
    /// let reader = ReaderBuilder::new().trim(true).build();
    /// ```
    pub fn trim(mut self, enabled: bool) -> Self {
        self.reader.processor.base.trim = enabled;
        self
    }

    /// Sets, what to do with duplicates. Works only for map and other files.
    ///
    /// - [`DuplicateMode::Allow`]: Default and recommended. Each map/event is parsed into its own hashmap. That won't likely cause much clashes between the same lines which require different translations.
    /// - [`DuplicateMode::Remove`]: Not recommended. This mode is stable and works perfectly, but it will write the same translation into multiple places where source text is used. Recommended only when duplicates cause too much bloat.
    ///
    /// # Example
    /// ```
    /// use rvpacker_txt_rs_lib::{ReaderBuilder, DuplicateMode};
    ///
    /// let reader = ReaderBuilder::new().duplicate_mode(DuplicateMode::Allow).build();
    /// ```
    pub fn duplicate_mode(mut self, mode: DuplicateMode) -> Self {
        self.reader.processor.base.duplicate_mode = mode;
        self
    }

    /// Builds and returns the [`Reader`].
    pub fn build(self) -> Reader {
        self.reader
    }
}

/// A struct used for writing translation from `.txt` files back to RPG Maker files.
///
/// The [`Writer`] struct, essentially, should receive the same options, as [`Reader`], to ensure proper writing.
///
/// # Fields
/// - `file_flags`: Indicates which RPG Maker files should be processed. Use [`Writer::set_flags`] to set them.
/// - `game_type`: Specifies which RPG Maker game type the data is from. Use [`Writer::set_game_type`] to set it.
/// - `romanize`: Enables or disables romanization of parsed text. For more info, and to set it, see [`Writer::set_romanize`].
/// - `trim`: Removes leading and trailing whitespace from extracted strings. Use [`Writer::set_trim`] to set it.
///
/// # Example
/// ```no_run
/// use rvpacker_txt_rs_lib::{Writer, FileFlags, EngineType};
///
/// let mut writer = Writer::new();
/// writer.set_flags(FileFlags::Map | FileFlags::Other);
/// writer.write("C:/Game/Data", "C:/Game/translation", "C:/Game/output", EngineType::VXAce);
/// ```
#[derive(Default)]
pub struct Writer {
    processor: Processor,
}

impl Writer {
    /// Creates a new [`Writer`] instance with default values.
    ///
    /// By default, all four file flags are set (all files will be written), duplicate mode is set to [`DuplicateMode::Allow`], and all other options are disabled.
    ///
    /// # Example
    /// ```
    /// use rvpacker_txt_rs_lib::Writer;
    ///
    /// let mut writer = Writer::new();
    /// ```
    pub fn new() -> Self {
        Self::default()
    }

    /// Sets the file flags to determine which RPG Maker files will be parsed.
    ///
    /// There's four FileFlags variants:
    /// - [`FileFlags::Map`] - enables `Mapxxx.ext` files processing.
    /// - [`FileFlags::Other`] - enables processing files other than `Map`, `System`, `Scripts` and `plugins`.
    /// - [`FileFlags::System`] - enables `System.txt` file processing.
    /// - [`FileFlags::Scripts`] - enables `Scripts.ext`/`plugins.js` file processing, based on engine type.
    ///
    /// # Arguments
    /// - `flags` - A [`FileFlags`] value indicating the file types to include.
    ///
    /// # Example
    /// ```
    /// use rvpacker_txt_rs_lib::{Writer, FileFlags};
    ///
    /// let mut writer = Writer::new();
    /// writer.set_flags(FileFlags::Map | FileFlags::Other);
    /// ```
    pub fn set_flags(&mut self, file_flags: FileFlags) {
        self.processor.file_flags = file_flags;
    }

    /// This function must have the same value that was passed to it in [`Reader`] struct.
    ///
    /// Sets the game type for custom processing.
    ///
    /// Right now, custom processing is implement for Fear & Hunger 2: Termina ([`GameType::Termina`]), and LisaRPG series games ([`GameType::LisaRPG`]).
    ///
    /// There's no single definition for "custom processing", but the current implementations filter out unnecessary text and improve the readability of output `.txt` files.
    ///
    /// For example, in LisaRPG games, `\nbt` prefix is used in dialogues to mark the tile, above which textbox should appear. When `game_type` is set to `GameType::LisaRPG`, this prefix is not included to the output `.txt` files.
    ///
    /// # Arguments
    /// - `game_type` - A [`GameType`] variant.
    ///
    /// # Example
    /// ```
    /// use rvpacker_txt_rs_lib::{Writer, GameType};
    ///
    /// let mut writer = Writer::new();
    /// writer.set_game_type(GameType::Termina);
    /// ```
    pub fn set_game_type(&mut self, game_type: GameType) {
        self.processor.base.game_type = game_type;
    }

    /// This function must have the same value that was passed to it in [`Reader`] struct.
    ///
    /// Enables or disables romanization of the parsed text.
    ///
    /// Essentially, this flag just replaces Eastern symbols in strings to their Western equivalents.
    ///
    /// For example, 「」 Eastern (Japanese) quotation marks will be replaced by `''`.
    ///
    /// # Example
    /// ```
    /// use rvpacker_txt_rs_lib::Writer;
    ///
    /// let mut writer = Writer::new();
    /// writer.set_romanize(true);
    /// ```
    pub fn set_romanize(&mut self, enabled: bool) {
        self.processor.base.romanize = enabled;
    }

    /// This function must have the same value that was passed to it in [`Reader`] struct.
    ///
    /// Sets whether to trim whitespace from strings.
    ///
    /// # Example
    /// ```
    /// use rvpacker_txt_rs_lib::Writer;
    ///
    /// let mut writer = Writer::new();
    /// writer.set_trim(true);
    /// ```
    pub fn set_trim(&mut self, enabled: bool) {
        self.processor.base.trim = enabled;
    }

    /// This function must have the same value that was passed to it in [`Reader`] struct.
    ///
    /// Sets, what to do with duplicates. Works only for map and other files.
    ///
    /// - [`DuplicateMode::Allow`]: Default and recommended. Each map/event is parsed into its own hashmap. That won't likely cause much clashes between the same lines which require different translations.
    /// - [`DuplicateMode::Remove`]: Not recommended. This mode is stable and works perfectly, but it will write the same translation into multiple places where source text is used. Recommended only when duplicates cause too much bloat.
    ///
    /// # Example
    /// ```
    /// use rvpacker_txt_rs_lib::{Writer, DuplicateMode};
    ///
    /// let mut writer = Writer::new();
    /// writer.set_duplicate_mode(DuplicateMode::Allow);
    /// ```
    pub fn set_duplicate_mode(&mut self, mode: DuplicateMode) {
        self.processor.base.duplicate_mode = mode;
    }

    /// Writes the translation from `.txt` files in `translation_path`, and outputs modified
    /// files from `source_path` to `output_path`.
    ///
    /// Make sure you've configured the writer with the same options as reader before calling it.
    ///
    /// # Arguments
    /// - `source_path` - Path to the directory containing source RPG Maker files.
    ///
    ///   For `MV/MZ` engines, parent directory of `source_path` must contain `js` directory.
    ///
    /// - `translation_path` - Path to the directory where `.txt` translation files are located.
    /// - `output_path` - Path to the directory, where output RPG Maker files will be created.
    /// - `engine_type` - Engine type of the source RPG Maker files.
    ///
    /// # Example
    /// ```no_run
    /// use rvpacker_txt_rs_lib::{Writer, EngineType};
    ///
    /// let mut writer = Writer::new();
    /// writer.write("C:/Game/Data", "C:/Game/translation", "C:/Game/output", EngineType::VXAce);
    /// ```
    pub fn write<P: AsRef<Path>>(
        &mut self,
        source_path: P,
        translation_path: P,
        output_path: P,
        engine_type: EngineType,
    ) -> Result<(), Error> {
        self.processor.process(
            Mode::Write,
            engine_type,
            source_path,
            translation_path,
            Some(output_path),
        )?;
        Ok(())
    }
}

/// A builder struct for [`Writer`].
///
/// The [`Writer`] struct, essentially, should receive the same options, as [`Reader`], to ensure proper writing.
///
/// # Fields
/// - `file_flags`: Indicates which RPG Maker files should be processed. Use [`WriterBuilder::with_flags`] to set them.
/// - `game_type`: Specifies which RPG Maker game type the data is from. Use [`WriterBuilder::game_type`] to set it.
/// - `romanize`: Enables or disables romanization of parsed text. For more info, and to set it, see [`WriterBuilder::romanize`].
/// - `trim`: Removes leading and trailing whitespace from extracted strings. Use [`WriterBuilder::trim`] to set it.
///
/// # Example
/// ```
/// use rvpacker_txt_rs_lib::{WriterBuilder, FileFlags, GameType};
///
/// let mut writer = WriterBuilder::new().with_flags(FileFlags::Map | FileFlags::Other).build();
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
    /// ```
    /// use rvpacker_txt_rs_lib::WriterBuilder;
    ///
    /// let mut writer = WriterBuilder::new().build();
    /// ```
    pub fn new() -> Self {
        Self::default()
    }

    /// Sets the file flags to determine which RPG Maker files will be parsed.
    ///
    /// There's four FileFlags variants:
    /// - [`FileFlags::Map`] - enables `Mapxxx.ext` files processing.
    /// - [`FileFlags::Other`] - enables processing files other than `Map`, `System`, `Scripts` and `plugins`.
    /// - [`FileFlags::System`] - enables `System.txt` file processing.
    /// - [`FileFlags::Scripts`] - enables `Scripts.ext`/`plugins.js` file processing, based on engine type.
    ///
    /// # Arguments
    /// - `flags` - A [`FileFlags`] value indicating the file types to include.
    ///
    /// # Example
    /// ```
    /// use rvpacker_txt_rs_lib::{WriterBuilder, FileFlags};
    ///
    /// let writer = WriterBuilder::new().with_flags(FileFlags::Map | FileFlags::Other).build();
    /// ```
    pub fn with_flags(mut self, flags: FileFlags) -> Self {
        self.writer.processor.file_flags = flags;
        self
    }

    /// This function must have the same value that was passed to it in [`Reader`] struct.
    ///
    /// Sets the game type for custom processing.
    ///
    /// Right now, custom processing is implement for Fear & Hunger 2: Termina ([`GameType::Termina`]), and LisaRPG series games ([`GameType::LisaRPG`]).
    ///
    /// There's no single definition for "custom processing", but the current implementations filter out unnecessary text and improve the readability of output `.txt` files.
    ///
    /// For example, in LisaRPG games, `\nbt` prefix is used in dialogues to mark the tile, above which textbox should appear. When `game_type` is set to [`GameType::LisaRPG`], this prefix is not included to the output `.txt` files.
    ///
    /// # Arguments
    /// - `game_type` - A [`GameType`] variant.
    ///
    /// # Example
    /// ```
    /// use rvpacker_txt_rs_lib::{WriterBuilder, GameType};
    ///
    /// let writer = WriterBuilder::new().game_type(GameType::Termina).build();
    /// ```
    pub fn game_type(mut self, game_type: GameType) -> Self {
        self.writer.processor.base.game_type = game_type;
        self
    }

    /// This function must have the same value that was passed to it in [`Reader`] struct.
    ///
    /// Enables or disables romanization of the parsed text.
    ///
    /// Essentially, this flag just replaces Eastern symbols in strings to their Western equivalents.
    ///
    /// For example, 「」 Eastern (Japanese) quotation marks will be replaced by `''`.
    ///
    /// # Example
    /// ```
    /// use rvpacker_txt_rs_lib::WriterBuilder;
    ///
    /// let writer = WriterBuilder::new().romanize(true).build();
    /// ```
    pub fn romanize(mut self, enabled: bool) -> Self {
        self.writer.processor.base.romanize = enabled;
        self
    }

    /// This function must have the same value that was passed to it in [`Reader`] struct.
    ///
    /// Sets whether to trim whitespace from strings.
    ///
    /// # Example
    /// ```
    /// use rvpacker_txt_rs_lib::WriterBuilder;
    ///
    /// let writer = WriterBuilder::new().trim(true).build();
    /// ```
    pub fn trim(mut self, enabled: bool) -> Self {
        self.writer.processor.base.trim = enabled;
        self
    }

    /// This function must have the same value that was passed to it in [`Reader`] struct.
    ///
    /// Sets, what to do with duplicates. Works only for map and other files.
    ///
    /// - [`DuplicateMode::Allow`]: Default and recommended. Each map/event is parsed into its own hashmap. That won't likely cause much clashes between the same lines which require different translations.
    /// - [`DuplicateMode::Remove`]: Not recommended. This mode is stable and works perfectly, but it will write the same translation into multiple places where source text is used. Recommended only when duplicates cause too much bloat.
    ///
    /// # Example
    /// ```
    /// use rvpacker_txt_rs_lib::{WriterBuilder, DuplicateMode};
    ///
    /// let writer = WriterBuilder::new().duplicate_mode(DuplicateMode::Remove).build();
    /// ```
    pub fn duplicate_mode(mut self, mode: DuplicateMode) -> Self {
        self.writer.processor.base.duplicate_mode = mode;
        self
    }

    /// Builds and returns the [`Writer`].
    pub fn build(self) -> Writer {
        self.writer
    }
}

/// A struct used for purging lines with no translation from `.txt` files.
///
/// The [`Purger`] struct, essentially, should receive the same options, as [`Reader`], to ensure proper purging.
///
/// # Fields
/// - `file_flags`: Indicates which RPG Maker files should be processed. Use [`Purger::set_flags`] to set them.
/// - `game_type`: Specifies which RPG Maker game type the data is from. Use [`Purger::set_game_type`] to set it.
/// - `romanize`: Enables or disables romanization of parsed text. For more info, and to set it, see [`Purger::set_romanize`].
/// - `trim`: Removes leading and trailing whitespace from extracted strings. Use [`Purger::set_trim`] to set it.
/// - `create_ignore`: If enabled, creates `.rvpacker-ignore` file from purged lines, that can be used in
///   `Reader` struct when its `ignore` option is set.
///
/// # Example
/// ```no_run
/// use rvpacker_txt_rs_lib::{Purger, FileFlags, EngineType};
///
/// let mut purger = Purger::new();
/// purger.set_flags(FileFlags::Map | FileFlags::Other);
/// let result = purger.purge("C:/Game/Data", "C:/Game/translation", EngineType::VXAce);
/// ```
#[derive(Default)]
pub struct Purger {
    processor: Processor,
}

impl Purger {
    /// Creates a new [`Purger`] instance with default values.
    ///
    /// By default, all four file flags are set (all files will be purged), duplicate mode is set to [`DuplicateMode::Allow`], and all other options are disabled.
    ///
    /// # Example
    /// ```
    /// use rvpacker_txt_rs_lib::Purger;
    ///
    /// let mut purger = Purger::new();
    /// ```
    pub fn new() -> Self {
        Self::default()
    }

    /// Sets the file flags to determine which RPG Maker files will be parsed.
    ///
    /// There's four [`FileFlags`] variants:
    /// - [`FileFlags::Map`] - enables `Mapxxx.ext` files processing.
    /// - [`FileFlags::Other`] - enables processing files other than `Map`, `System`, `Scripts` and `plugins`.
    /// - [`FileFlags::System`] - enables `System.txt` file processing.
    /// - [`FileFlags::Scripts`] - enables `Scripts.ext`/`plugins.js` file processing, based on engine type.
    ///
    /// # Arguments
    /// - `flags` - A `FileFlags` value indicating the file types to include.
    ///
    /// # Example
    /// ```
    /// use rvpacker_txt_rs_lib::{Purger, FileFlags};
    ///
    /// let mut purger = Purger::new();
    /// purger.set_flags(FileFlags::Map | FileFlags::Other);
    /// ```
    pub fn set_flags(&mut self, file_flags: FileFlags) {
        self.processor.file_flags = file_flags;
    }

    /// This function must have the same value that was passed to it in [`Reader`] struct.
    ///
    /// Sets the game type for custom processing.
    ///
    /// Right now, custom processing is implement for Fear & Hunger 2: Termina ([`GameType::Termina`]), and LisaRPG series games ([`GameType::LisaRPG`]).
    ///
    /// There's no single definition for "custom processing", but the current implementations filter out unnecessary text and improve the readability of output `.txt` files.
    ///
    /// For example, in LisaRPG games, `\nbt` prefix is used in dialogues to mark the tile, above which textbox should appear. When `game_type` is set to `GameType::LisaRPG`, this prefix is not included to the output `.txt` files.
    ///
    /// # Arguments
    /// - `game_type` - A [`GameType`] variant.
    ///
    /// # Example
    /// ```
    /// use rvpacker_txt_rs_lib::{Purger, GameType};
    ///
    /// let mut purger = Purger::new();
    /// purger.set_game_type(GameType::Termina);
    /// ```
    pub fn set_game_type(&mut self, game_type: GameType) {
        self.processor.base.game_type = game_type;
    }

    /// This function must have the same value that was passed to it in [`Reader`] struct.
    ///
    /// Enables or disables romanization of the parsed text.
    ///
    /// Essentially, this flag just replaces Eastern symbols in strings to their Western equivalents.
    ///
    /// For example, 「」 Eastern (Japanese) quotation marks will be replaced by `''`.
    ///
    /// # Example
    /// ```
    /// use rvpacker_txt_rs_lib::Purger;
    ///
    /// let mut purger = Purger::new();
    /// purger.set_romanize(true);
    /// ```
    pub fn set_romanize(&mut self, enabled: bool) {
        self.processor.base.romanize = enabled;
    }

    /// This function must have the same value that was passed to it in [`Reader`] struct.
    ///
    /// Sets whether to trim whitespace from strings.
    ///
    /// # Example
    /// ```
    /// use rvpacker_txt_rs_lib::Purger;
    ///
    /// let mut purger = Purger::new();
    /// purger.set_trim(true);
    /// ```
    pub fn set_trim(&mut self, enabled: bool) {
        self.processor.base.trim = enabled;
    }

    /// Sets whether to create `.rvpacker-ignore` file from purged lines.
    ///
    /// # Example
    /// ```
    /// use rvpacker_txt_rs_lib::Purger;
    ///
    /// let mut purger = Purger::new();
    /// purger.set_create_ignore(true);
    /// ```
    pub fn set_create_ignore(&mut self, enabled: bool) {
        self.processor.base.create_ignore = enabled;
    }

    /// This function must have the same value that was passed to it in [`Reader`] struct.
    ///
    /// Sets, what to do with duplicates. Works only for map and other files.
    ///
    /// - [`DuplicateMode::Allow`]: Default and recommended. Each map/event is parsed into its own hashmap. That won't likely cause much clashes between the same lines which require different translations.
    /// - [`DuplicateMode::Remove`]: Not recommended. This mode is stable and works perfectly, but it will write the same translation into multiple places where source text is used. Recommended only when duplicates cause too much bloat.
    ///
    /// # Example
    /// ```
    /// use rvpacker_txt_rs_lib::{Purger, DuplicateMode};
    ///
    /// let mut purger = Purger::new();
    /// purger.set_duplicate_mode(DuplicateMode::Allow);
    /// ```
    pub fn set_duplicate_mode(&mut self, mode: DuplicateMode) {
        self.processor.base.duplicate_mode = mode;
    }

    /// Purges the lines with no translation from `.txt` files in `translation_path`, using source RPG Maker files from `source_path`.
    ///
    /// Make sure you've configured the purger with the same options as reader before calling it.
    ///
    /// # Arguments
    /// - `source_path` - Path to the directory containing RPG Maker files.
    /// - `translation_path` - Path to the directory containing `.txt` translation files.
    /// - `engine_type` - Engine type of the source RPG Maker files.
    ///
    /// # Example
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
            Mode::Purge,
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
/// The `Purger` struct, essentially, should receive the same options, as [`Reader`], to ensure proper purging.
///
/// # Fields
/// - `file_flags`: Indicates which RPG Maker files should be processed. Use [`PurgerBuilder::with_flags`] to set them.
/// - `game_type`: Specifies which RPG Maker game type the data is from. Use [`PurgerBuilder::game_type`] to set it.
/// - `romanize`: Enables or disables romanization of parsed text. For more info, and to set it, see [`PurgerBuilder::romanize`].
/// - `trim`: Removes leading and trailing whitespace from extracted strings. Use [`PurgerBuilder::trim`] to set it.
/// - `create_ignore`: If enabled, creates `.rvpacker-ignore` file from purged lines, that can be used in
///   `Reader` struct when its `ignore` option is set.
///
/// # Example
/// ```
/// use rvpacker_txt_rs_lib::{PurgerBuilder, FileFlags, GameType};
///
/// let mut purger = PurgerBuilder::new().with_flags(FileFlags::Map | FileFlags::Other).build();
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
    /// ```
    /// use rvpacker_txt_rs_lib::PurgerBuilder;
    ///
    /// let mut purger = PurgerBuilder::new().build();
    /// ```
    pub fn new() -> Self {
        Self::default()
    }

    /// Sets the file flags to determine which RPG Maker files will be parsed.
    ///
    /// There's four [`FileFlags`] variants:
    /// - [`FileFlags::Map`] - enables `Mapxxx.ext` files processing.
    /// - [`FileFlags::Other`] - enables processing files other than `Map`, `System`, `Scripts` and `plugins`.
    /// - [`FileFlags::System`] - enables `System.txt` file processing.
    /// - [`FileFlags::Scripts`] - enables `Scripts.ext`/`plugins.js` file processing, based on engine type.
    ///
    /// # Arguments
    /// - `flags` - A `FileFlags` value indicating the file types to include.
    ///
    /// # Example
    /// ```
    /// use rvpacker_txt_rs_lib::{PurgerBuilder, FileFlags};
    ///
    /// let purger = PurgerBuilder::new().with_flags(FileFlags::Map | FileFlags::Other).build();
    /// ```
    pub fn with_flags(mut self, file_flags: FileFlags) -> Self {
        self.purger.processor.file_flags = file_flags;
        self
    }

    /// This function must have the same value that was passed to it in [`Reader`] struct.
    ///
    /// Sets the game type for custom processing.
    ///
    /// Right now, custom processing is implement for Fear & Hunger 2: Termina ([`GameType::Termina`]), and LisaRPG series games ([`GameType::LisaRPG`]).
    ///
    /// There's no single definition for "custom processing", but the current implementations filter out unnecessary text and improve the readability of output `.txt` files.
    ///
    /// For example, in LisaRPG games, `\nbt` prefix is used in dialogues to mark the tile, above which textbox should appear. When `game_type` is set to `GameType::LisaRPG`, this prefix is not included to the output `.txt` files.
    ///
    /// # Arguments
    /// - `game_type` - A [`GameType`] variant.
    ///
    /// # Example
    /// ```
    /// use rvpacker_txt_rs_lib::{PurgerBuilder, GameType};
    ///
    /// let purger = PurgerBuilder::new().game_type(GameType::Termina).build();
    /// ```
    pub fn game_type(mut self, game_type: GameType) -> Self {
        self.purger.processor.base.game_type = game_type;
        self
    }

    /// This function must have the same value that was passed to it in [`Reader`] struct.
    ///
    /// Enables or disables romanization of the parsed text.
    ///
    /// Essentially, this flag just replaces Eastern symbols in strings to their Western equivalents.
    ///
    /// For example, 「」 Eastern (Japanese) quotation marks will be replaced by `''`.
    ///
    /// # Example
    /// ```
    /// use rvpacker_txt_rs_lib::PurgerBuilder;
    ///
    /// let purger = PurgerBuilder::new().romanize(true).build();
    /// ```
    pub fn romanize(mut self, enabled: bool) -> Self {
        self.purger.processor.base.romanize = enabled;
        self
    }

    /// This function must have the same value that was passed to it in [`Reader`] struct.
    ///
    /// Sets whether to trim whitespace from strings.
    ///
    /// # Example
    /// ```
    /// use rvpacker_txt_rs_lib::PurgerBuilder;
    ///
    /// let purger = PurgerBuilder::new().trim(true).build();
    /// ```
    pub fn trim(mut self, enabled: bool) -> Self {
        self.purger.processor.base.trim = enabled;
        self
    }

    /// Sets whether to create `.rvpacker-ignore` file from purged lines.
    ///
    /// # Example
    /// ```
    /// use rvpacker_txt_rs_lib::PurgerBuilder;
    ///
    /// let purger = PurgerBuilder::new().create_ignore(true).build();
    /// ```
    pub fn create_ignore(mut self, enabled: bool) -> Self {
        self.purger.processor.base.create_ignore = enabled;
        self
    }

    /// This function must have the same value that was passed to it in [`Reader`] struct.
    ///
    /// Sets, what to do with duplicates. Works only for map and other files.
    ///
    /// - [`DuplicateMode::Allow`]: Default and recommended. Each map/event is parsed into its own hashmap. That won't likely cause much clashes between the same lines which require different translations.
    /// - [`DuplicateMode::Remove`]: Not recommended. This mode is stable and works perfectly, but it will write the same translation into multiple places where source text is used. Recommended only when duplicates cause too much bloat.
    ///
    /// # Example
    /// ```
    /// use rvpacker_txt_rs_lib::{PurgerBuilder, DuplicateMode};
    ///
    /// let purger = PurgerBuilder::new().duplicate_mode(DuplicateMode::Allow).build();
    /// ```
    pub fn duplicate_mode(mut self, mode: DuplicateMode) -> Self {
        self.purger.processor.base.duplicate_mode = mode;
        self
    }

    /// Builds and returns the [`Purger`].
    pub fn build(self) -> Purger {
        self.purger
    }
}
