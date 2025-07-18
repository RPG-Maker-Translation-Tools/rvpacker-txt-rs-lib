use crate::{
    constants::RVPACKER_IGNORE_FILE, core::*, functions::parse_ignore,
    get_engine_extension, types::*,
};
use std::{fs::create_dir_all, path::Path};

struct MapReader<'a> {
    base: MapBase<'a>,
}

impl<'a> MapReader<'a> {
    pub fn new(
        source_path: &'a Path,
        translation_path: &'a Path,
        engine_type: EngineType,
    ) -> Self {
        Self {
            base: MapBase::new(
                source_path,
                translation_path,
                translation_path,
                engine_type,
                ProcessingMode::Read,
            ),
        }
    }

    pub fn romanize(mut self, romanize: bool) -> Self {
        self.base.base.romanize = romanize;
        self
    }

    pub fn logging(mut self, logging: bool) -> Self {
        self.base.base.logging = logging;
        self
    }

    pub fn game_type(mut self, game_type: GameType) -> Self {
        self.base.base.game_type = game_type;
        self
    }

    pub fn read_mode(mut self, mode: ReadMode) -> Self {
        self.base.base.read_mode = mode;
        self
    }

    pub fn ignore(mut self, enabled: bool) -> Self {
        self.base.base.ignore = enabled;
        self
    }

    pub fn trim(mut self, enabled: bool) -> Self {
        self.base.base.trim = enabled;
        self
    }

    pub fn ignore_map(mut self, map: &'a mut IgnoreMap) -> Self {
        self.base.base.ignore_map = map;
        self
    }

    pub fn duplicate_mode(mut self, mode: DuplicateMode) -> Self {
        self.base.base.duplicate_mode = mode;
        self
    }

    pub fn read(self) -> ResultVec {
        self.base.process()
    }
}

struct OtherReader<'a> {
    base: OtherBase<'a>,
}

impl<'a> OtherReader<'a> {
    pub fn new(
        source_path: &'a Path,
        translation_path: &'a Path,
        engine_type: EngineType,
    ) -> Self {
        Self {
            base: OtherBase::new(
                source_path,
                translation_path,
                translation_path,
                engine_type,
                ProcessingMode::Read,
            ),
        }
    }

    pub fn romanize(mut self, enabled: bool) -> Self {
        self.base.base.romanize = enabled;
        self
    }

    pub fn logging(mut self, enabled: bool) -> Self {
        self.base.base.logging = enabled;
        self
    }

    pub fn game_type(mut self, game_type: GameType) -> Self {
        self.base.base.game_type = game_type;
        self
    }

    pub fn read_mode(mut self, mode: ReadMode) -> Self {
        self.base.base.read_mode = mode;
        self
    }

    pub fn ignore(mut self, enabled: bool) -> Self {
        self.base.base.ignore = enabled;
        self
    }

    pub fn trim(mut self, enabled: bool) -> Self {
        self.base.base.trim = enabled;
        self
    }

    pub fn ignore_map(mut self, map: &'a mut IgnoreMap) -> Self {
        self.base.base.ignore_map = map;
        self
    }

    pub fn duplicate_mode(mut self, mode: DuplicateMode) -> Self {
        self.base.base.duplicate_mode = mode;
        self
    }

    pub fn read(self) -> ResultVec {
        self.base.process()
    }
}

struct SystemReader<'a> {
    base: SystemBase<'a>,
}

impl<'a> SystemReader<'a> {
    pub fn new(
        system_file_path: &'a Path,
        translation_path: &'a Path,
        engine_type: EngineType,
    ) -> Self {
        Self {
            base: SystemBase::new(
                system_file_path,
                translation_path,
                translation_path,
                engine_type,
                ProcessingMode::Read,
            ),
        }
    }

    pub fn romanize(mut self, enabled: bool) -> Self {
        self.base.base.romanize = enabled;
        self
    }

    pub fn logging(mut self, enabled: bool) -> Self {
        self.base.base.logging = enabled;
        self
    }

    pub fn read_mode(mut self, mode: ReadMode) -> Self {
        self.base.base.read_mode = mode;
        self
    }

    pub fn ignore(mut self, enabled: bool) -> Self {
        self.base.base.ignore = enabled;
        self
    }

    pub fn trim(mut self, enabled: bool) -> Self {
        self.base.base.trim = enabled;
        self
    }

    pub fn ignore_map(mut self, map: &'a mut IgnoreMap) -> Self {
        self.base.base.ignore_map = map;
        self
    }

    pub fn read(self) -> ResultVec {
        self.base.process()
    }
}

struct ScriptReader<'a> {
    base: ScriptBase<'a>,
}

impl<'a> ScriptReader<'a> {
    pub fn new(
        scripts_file_path: &'a Path,
        translation_path: &'a Path,
    ) -> Self {
        Self {
            base: ScriptBase::new(
                scripts_file_path,
                translation_path,
                translation_path,
                ProcessingMode::Read,
            ),
        }
    }

    pub fn romanize(mut self, enabled: bool) -> Self {
        self.base.base.romanize = enabled;
        self
    }

    pub fn logging(mut self, enabled: bool) -> Self {
        self.base.base.logging = enabled;
        self
    }

    pub fn read_mode(mut self, mode: ReadMode) -> Self {
        self.base.base.read_mode = mode;
        self
    }

    pub fn ignore(mut self, enabled: bool) -> Self {
        self.base.base.ignore = enabled;
        self
    }

    pub fn ignore_map(mut self, map: &'a mut IgnoreMap) -> Self {
        self.base.base.ignore_map = map;
        self
    }

    pub fn read(self) -> ResultVec {
        self.base.process()
    }
}

struct PluginReader<'a> {
    base: PluginBase<'a>,
}

impl<'a> PluginReader<'a> {
    pub fn new(
        plugins_file_path: &'a Path,
        translation_path: &'a Path,
    ) -> Self {
        Self {
            base: PluginBase::new(
                plugins_file_path,
                translation_path,
                translation_path,
                ProcessingMode::Read,
            ),
        }
    }

    pub fn romanize(mut self, enabled: bool) -> Self {
        self.base.base.romanize = enabled;
        self
    }

    pub fn logging(mut self, enabled: bool) -> Self {
        self.base.base.logging = enabled;
        self
    }

    pub fn read_mode(mut self, mode: ReadMode) -> Self {
        self.base.base.read_mode = mode;
        self
    }

    pub fn ignore(mut self, enabled: bool) -> Self {
        self.base.base.ignore = enabled;
        self
    }

    pub fn ignore_map(mut self, map: &'a mut IgnoreMap) -> Self {
        self.base.base.ignore_map = map;
        self
    }

    pub fn read(self) -> ResultVec {
        self.base.process()
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
/// - `logging`: If enabled, logs operations and progress. Use [`Reader::set_logging`] to set it. As this crate uses `log` for logging, you should [set up logging in your program](https://docs.rs/log/latest/log/#available-logging-implementations).
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
    file_flags: FileFlags,
    read_mode: ReadMode,
    game_type: GameType,
    duplicate_mode: DuplicateMode,
    romanize: bool,
    logging: bool,
    ignore: bool,
    trim: bool,
}

impl Reader {
    /// Creates a new [`Reader`] instance with default values.
    ///
    /// By default, all four file flags are set (all files will be read), the [`ReadMode::Default`] read mode is used, duplicate mode is set to `AllowDuplicates`, and all other options are disabled.
    ///
    /// # Example
    /// ```compile_fail
    /// let mut reader = Reader::new();
    /// ```
    pub fn new() -> Self {
        Self {
            read_mode: ReadMode::Default,
            ..Default::default()
        }
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
    /// ```compile_fail
    /// reader.set_flags(FileFlags::Map | FileFlags::Other);
    /// ```
    pub fn set_flags(&mut self, flags: FileFlags) {
        self.file_flags = flags;
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
    /// ```compile_fail
    /// reader.set_read_mode(ReadMode::Default);
    /// ```
    pub fn set_read_mode(&mut self, mode: ReadMode) {
        self.read_mode = mode;
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
    /// ```compile_fail
    /// reader.set_game_type(GameType::Termina);
    /// ```
    pub fn set_game_type(&mut self, game_type: GameType) {
        self.game_type = game_type;
    }

    /// Enables or disables romanization of the parsed text.
    ///
    /// Essentially, this flag just replaces Eastern symbols in strings to their Western equivalents.
    ///
    /// For example, 「」 Eastern (Japanese) quotation marks will be replaced by `''`.
    ///
    /// # Example
    /// ```compile_fail
    /// reader.set_romanize(true);
    /// ```
    pub fn set_romanize(&mut self, enabled: bool) {
        self.romanize = enabled;
    }

    /// Sets whether to output logs.
    ///
    /// As this crate uses `log` for logging, you should [set up logging in your program](https://docs.rs/log/latest/log/#available-logging-implementations).
    ///
    /// # Example
    /// ```compile_fail
    /// reader.set_logging(true);
    /// ```
    pub fn set_logging(&mut self, enabled: bool) {
        self.logging = enabled;
    }

    /// Sets whether to ignore entries from `.rvpacker-ignore` file.
    ///
    /// # Example
    /// ```compile_fail
    /// reader.set_ignore(true);
    /// ```
    pub fn set_ignore(&mut self, enabled: bool) {
        self.ignore = enabled;
    }

    /// Sets whether to trim whitespace from strings.
    ///
    /// # Example
    /// ```compile_fail
    /// reader.set_trim(true);
    /// ```
    pub fn set_trim(&mut self, enabled: bool) {
        self.trim = enabled;
    }

    /// This function must have the same value that was passed to it in [`Reader`] struct.
    ///
    /// Sets, what to do with duplicates.
    ///
    /// - [`DuplicateMode::AllowDuplicates`]: Default and recommended. Each map/event is parsed into its own hashmap. That won't likely cause much clashes between the same lines which require different translations.
    /// - [`DuplicateMode::NoDuplicates`]: Not recommended. This mode is stable and works perfectly, but it will write the same translation into multiple places where source text is used. Recommended only when duplicates cause too much bloat.
    ///
    /// # Example
    /// ```compile_fail
    /// reader.set_duplicate_mode(DuplicateMode::AllowDuplicates);
    /// ```
    pub fn set_duplicate_mode(&mut self, mode: DuplicateMode) {
        self.duplicate_mode = mode;
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
    /// ```compile_fail
    /// reader.read("C:/Game/Data", "C:/Game/translation", EngineType::VXAce);
    /// ```
    pub fn read<P: AsRef<Path> + Sync>(
        &self,
        source_path: P,
        translation_path: P,
        engine_type: EngineType,
    ) -> Result<FileResults, Error> {
        let mut results = FileResults::default();

        if self.file_flags.is_empty() {
            return Ok(results);
        }

        let source_path = source_path.as_ref();
        let translation_path = translation_path.as_ref();

        let mut ignore_map = IgnoreMap::default();

        if self.ignore {
            ignore_map = parse_ignore(
                translation_path.join(RVPACKER_IGNORE_FILE),
                self.duplicate_mode,
                true,
            )?;
        }

        create_dir_all(translation_path).map_err(|err| {
            Error::CreateDirFailed {
                path: translation_path.to_path_buf(),
                err,
            }
        })?;

        if self.file_flags.contains(FileFlags::Map) {
            results.map =
                MapReader::new(source_path, translation_path, engine_type)
                    .ignore_map(&mut ignore_map)
                    .game_type(self.game_type)
                    .read_mode(self.read_mode)
                    .romanize(self.romanize)
                    .logging(self.logging)
                    .ignore(self.ignore)
                    .trim(self.trim)
                    .duplicate_mode(self.duplicate_mode)
                    .read();
        }

        if self.file_flags.contains(FileFlags::Other) {
            results.other =
                OtherReader::new(source_path, translation_path, engine_type)
                    .ignore_map(&mut ignore_map)
                    .game_type(self.game_type)
                    .read_mode(self.read_mode)
                    .romanize(self.romanize)
                    .logging(self.logging)
                    .ignore(self.ignore)
                    .trim(self.trim)
                    .duplicate_mode(self.duplicate_mode)
                    .read();
        }

        if self.file_flags.contains(FileFlags::System) {
            let system_file_path = source_path
                .join(format!("System.{}", get_engine_extension(engine_type)));

            results.system = SystemReader::new(
                &system_file_path,
                translation_path,
                engine_type,
            )
            .ignore_map(&mut ignore_map)
            .read_mode(self.read_mode)
            .romanize(self.romanize)
            .logging(self.logging)
            .ignore(self.ignore)
            .trim(self.trim)
            .read();
        }

        if self.file_flags.contains(FileFlags::Scripts) {
            if engine_type.is_new() {
                let plugins_file_path = unsafe {
                    source_path
                        .parent()
                        .unwrap_unchecked()
                        .join("js/plugins.js")
                };

                if !plugins_file_path.exists() {
                    return Err(Error::PluginsFileMissing);
                }

                results.scripts =
                    PluginReader::new(&plugins_file_path, translation_path)
                        .ignore_map(&mut ignore_map)
                        .read_mode(self.read_mode)
                        .romanize(self.romanize)
                        .logging(self.logging)
                        .ignore(self.ignore)
                        .read();
            } else {
                let scripts_file_path = source_path.join(format!(
                    "Scripts.{}",
                    get_engine_extension(engine_type)
                ));

                results.scripts =
                    ScriptReader::new(&scripts_file_path, translation_path)
                        .ignore_map(&mut ignore_map)
                        .read_mode(self.read_mode)
                        .romanize(self.romanize)
                        .logging(self.logging)
                        .ignore(self.ignore)
                        .read();
            }
        }

        Ok(results)
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
/// - `logging`: If enabled, logs operations and progress. Use [`ReaderBuilder::logging`]
/// - `ignore`: Ignores entries from `.rvpacker-ignore` file. Use [`ReaderBuilder::ignore`] to set it.
/// - `trim`: Removes leading and trailing whitespace from extracted strings. Use [`ReaderBuilder::trim`] to set it.
///
/// # Example
/// ```
/// use rvpacker_txt_rs_lib::{ReaderBuilder, FileFlags, GameType};
/// let mut reader = ReaderBuilder::new().with_flags(FileFlags::Map | FileFlags::Other).build();
/// ```
#[derive(Default)]
pub struct ReaderBuilder {
    reader: Reader,
}

impl ReaderBuilder {
    /// Creates a new [`ReaderBuilder`] instance with default values.
    ///
    /// By default, all four file flags are set (all files will be read), the [`ReadMode::Default`] read mode is used, duplicate mode is set to `AllowDuplicates`, and all other options are disabled.
    ///
    /// # Example
    /// ```compile_fail
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
    /// ```compile_fail
    /// let reader = ReaderBuilder::new().with_flags(FileFlags::Map | FileFlags::Other).build();
    /// ```
    pub fn with_flags(mut self, flags: FileFlags) -> Self {
        self.reader.file_flags = flags;
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
    /// ```compile_fail
    /// let reader = ReaderBuilder::new().read_mode(ReadMode::Default).build();
    /// ```
    pub fn read_mode(mut self, mode: ReadMode) -> Self {
        self.reader.read_mode = mode;
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
    /// ```compile_fail
    /// let reader = ReaderBuilder::new().game_type(GameType::Termina).build();
    /// ```
    pub fn game_type(mut self, game_type: GameType) -> Self {
        self.reader.game_type = game_type;
        self
    }

    /// Enables or disables romanization of the parsed text.
    ///
    /// Essentially, this flag just replaces Eastern symbols in strings to their Western equivalents.
    ///
    /// For example, 「」 Eastern (Japanese) quotation marks will be replaced by `''`.
    ///
    /// # Example
    /// ```compile_fail
    /// let reader = ReaderBuilder::new().romanize(true).build();
    /// ```
    pub fn romanize(mut self, enabled: bool) -> Self {
        self.reader.romanize = enabled;
        self
    }

    /// Sets whether to output logs.
    ///
    /// As this crate uses `log` for logging, you should [set up logging in your program](https://docs.rs/log/latest/log/#available-logging-implementations).
    ///
    /// # Example
    /// ```compile_fail
    /// let reader = ReaderBuilder::new().logging(true).build();
    /// ```
    pub fn logging(mut self, enabled: bool) -> Self {
        self.reader.logging = enabled;
        self
    }

    /// Sets whether to ignore entries from `.rvpacker-ignore` file.
    ///
    /// # Example
    /// ```compile_fail
    /// let reader = ReaderBuilder::new().ignore(true).build();
    /// ```
    pub fn ignore(mut self, enabled: bool) -> Self {
        self.reader.ignore = enabled;
        self
    }

    /// Sets whether to trim whitespace from strings.
    ///
    /// # Example
    /// ```compile_fail
    /// let reader = ReaderBuilder::new().trim(true).build();
    /// ```
    pub fn trim(mut self, enabled: bool) -> Self {
        self.reader.trim = enabled;
        self
    }

    /// Sets, what to do with duplicates. Works only for map and other files.
    ///
    /// - [`DuplicateMode::AllowDuplicates`]: Default and recommended. Each map/event is parsed into its own hashmap. That won't likely cause much clashes between the same lines which require different translations.
    /// - [`DuplicateMode::NoDuplicates`]: Not recommended. This mode is stable and works perfectly, but it will write the same translation into multiple places where source text is used. Recommended only when duplicates cause too much bloat.
    ///
    /// # Example
    /// ```compile_fail
    /// let reader = ReaderBuilder::new().duplicate_mode(DuplicateMode::AllowDuplicates).build();
    /// ```
    pub fn duplicate_mode(mut self, mode: DuplicateMode) -> Self {
        self.reader.duplicate_mode = mode;
        self
    }

    /// Builds and returns the [`Reader`].
    pub fn build(self) -> Reader {
        self.reader
    }
}
