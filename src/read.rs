use crate::{
    bases::*, constants::RVPACKER_IGNORE_FILE, functions::parse_ignore,
    get_engine_extension, types::*,
};
use std::path::Path;

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

    pub fn sort(mut self, enabled: bool) -> Self {
        self.base.base.sort = enabled;
        self
    }

    pub fn ignore_map(mut self, map: &'a mut IgnoreMap) -> Self {
        self.base.base.ignore_map = map;
        self
    }

    #[inline(always)]
    pub fn read(self) {
        self.base.process();
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

    pub fn sort(mut self, enabled: bool) -> Self {
        self.base.base.sort = enabled;
        self
    }

    pub fn ignore_map(mut self, map: &'a mut IgnoreMap) -> Self {
        self.base.base.ignore_map = map;
        self
    }

    #[inline(always)]
    pub fn read(self) {
        self.base.process();
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

    pub fn sort(mut self, enabled: bool) -> Self {
        self.base.base.sort = enabled;
        self
    }

    pub fn ignore_map(mut self, map: &'a mut IgnoreMap) -> Self {
        self.base.base.ignore_map = map;
        self
    }

    #[inline(always)]
    pub fn read(self) {
        self.base.process();
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

    pub fn sort(mut self, enabled: bool) -> Self {
        self.base.base.sort = enabled;
        self
    }

    pub fn ignore_map(mut self, map: &'a mut IgnoreMap) -> Self {
        self.base.base.ignore_map = map;
        self
    }

    #[inline(always)]
    pub fn read(self) {
        self.base.process();
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

    pub fn sort(mut self, enabled: bool) -> Self {
        self.base.base.sort = enabled;
        self
    }

    pub fn ignore_map(mut self, map: &'a mut IgnoreMap) -> Self {
        self.base.base.ignore_map = map;
        self
    }

    #[inline(always)]
    pub fn read(self) {
        self.base.process();
    }
}

/// A struct used for parsing and extracting text from RPG Maker files into `.txt` format.
///
/// The `Reader` provides a configurable interface to control how files are parsed,
/// which files are selected, and how text content is filtered.
///
/// # Fields
/// - `file_flags`: Indicates which RPG Maker files should be processed. Use `set_flags` to set them.
/// - `read_mode`: Defines the read strategy. Use `set_read_mode` to set it.
/// - `game_type`: Specifies which RPG Maker game type the data is from. Use `set_game_type` to set it.
/// - `romanize`: Enables or disables romanization of parsed text. For more info, and to set it, see `set_romanize`.
/// - `logging`: If enabled, logs operations and progress. Use `set_logging`
/// - `ignore`: Ignores entries from `.rvpacker-ignore` file. Use `set_ignore` to set it.
/// - `trim`: Removes leading and trailing whitespace from extracted strings. Use `set_trim` to set it.
/// - `sort`: Sorts the extracted text entries chronologically. Use `set_sort` to set it.
///
/// # Example
/// ```no_run
/// use rvpacker_txt_rs_lib::{Reader, FileFlags, EngineType};
///
/// let mut reader = Reader::new();
/// reader.set_flags(FileFlags::Map | FileFlags::Other);
/// reader.read("C:/Game/Data", "C:/Game/translation", EngineType::VXAce);
/// ```
pub struct Reader {
    file_flags: FileFlags,
    read_mode: ReadMode,
    game_type: GameType,
    romanize: bool,
    logging: bool,
    ignore: bool,
    trim: bool,
    sort: bool,
}

impl Reader {
    /// Creates a new `Reader` instance with default values.
    ///
    /// By default, all four file flags are set (all files will be read), the `ReadMode::Default` read mode is used,
    /// and all other options are disabled.
    ///
    /// # Example
    /// ```no_run
    /// let mut reader = Reader::new();
    /// ```
    pub fn new() -> Self {
        Self {
            file_flags: FileFlags::All,
            read_mode: ReadMode::Default,
            game_type: GameType::None,
            romanize: false,
            logging: false,
            ignore: false,
            trim: false,
            sort: false,
        }
    }

    /// Sets the file flags to determine which RPG Maker files will be parsed.
    ///
    /// There's four FileFlags variants:
    /// * `FileFlags::Map` - enables `Mapxxx.ext` files processing.
    /// * `FileFlags::Other` - enables processing files other than `Map`, `System`, `Scripts` and `plugins`.
    /// * `FileFlags::System` - enables `System.txt` file processing.
    /// * `FileFlags::Scripts` - enables `Scripts.ext`/`plugins.js` file processing, based on engine type.
    ///
    /// # Arguments
    /// * `flags` - A `FileFlags` value indicating the file types to include.
    ///
    /// # Example
    /// ```no_run
    /// reader.set_flags(FileFlags::Map | FileFlags::Other);
    /// ```
    pub fn set_flags(&mut self, flags: FileFlags) {
        self.file_flags = flags;
    }

    /// Sets the read mode that affects how data is parsed.
    ///
    /// There's only three read modes:
    /// * `ReadMode::Default` - parses the text from the RPG Maker files, aborts if translation files already exist.
    /// * `ReadMode::Append` - appends the new text to the translation files. That's particularly helpful if the game received content update.
    /// * `ReadMode::Force` - parses the text from the RPG Maker files, overwrites the existing translation. **DANGEROUS!**
    ///
    /// # Arguments
    /// * `mode` - A `ReadMode` variant.
    ///
    /// # Example
    /// ```no_run
    /// reader.set_read_mode(ReadMode::Default);
    /// ```
    pub fn set_read_mode(&mut self, mode: ReadMode) {
        self.read_mode = mode;
    }

    /// Sets the game type for custom processing.
    ///
    /// Right now, custom processing is implement for Fear & Hunger 2: Termina (`GameType::Termina`), and LisaRPG series games (`GameType::LisaRPG`).
    ///
    /// There's no single definition for "custom processing", but the current implementations filter out unnecessary text and improve the readability of output `.txt` files.
    ///
    /// For example, in LisaRPG games, `\nbt` prefix is used in dialogues to mark the tile, above which textbox should appear. When `game_type` is set to `GameType::LisaRPG`, this prefix is not included to the output `.txt` files.
    ///
    /// # Arguments
    /// * `game_type` - A `GameType` variant.
    ///
    /// # Example
    /// ```no_run
    /// reader.set_game_type(GameType::Termina);
    /// ```
    pub fn set_game_type(&mut self, game_type: GameType) {
        self.game_type = game_type;
    }

    /// Enables or disables romanization of the parsed text.
    ///
    /// Essentially, this flag just replaced Eastern symbols in strings to their Western equivalents.
    ///
    /// For example, 「」 Eastern (Japanese) quotation marks will be replaced by `''`.
    ///
    /// # Example
    /// ```no_run
    /// reader.set_romanize(true);
    /// ```
    pub fn set_romanize(&mut self, enabled: bool) {
        self.romanize = enabled;
    }

    /// Sets whether to log file processing.
    ///
    /// # Example
    /// ```no_run
    /// reader.set_logging(true);
    /// ```
    pub fn set_logging(&mut self, enabled: bool) {
        self.logging = enabled;
    }

    /// Sets whether to ignore entries from `.rvpacker-ignore` file.
    ///
    /// # Example
    /// ```no_run
    /// reader.set_ignore(true);
    /// ```
    pub fn set_ignore(&mut self, enabled: bool) {
        self.ignore = enabled;
    }

    /// Sets whether to trim whitespace from strings.
    ///
    /// # Example
    /// ```no_run
    /// reader.set_trim(true);
    /// ```
    pub fn set_trim(&mut self, enabled: bool) {
        self.trim = enabled;
    }

    /// Sets whether to sort extracted text chronologically.
    /// Works only with `ReadMode::Append`.
    ///
    /// # Example
    /// ```no_run
    /// reader.set_sort(true);
    /// ```
    pub fn set_sort(&mut self, enabled: bool) {
        self.sort = enabled;
    }

    /// Reads the RPG Maker files from `source_path` to `.txt` files in `translation_path`.
    ///
    /// Make sure you've configured the reader as you desire before calling it.
    ///
    /// # Arguments
    /// * `source_path` - Path to the directory containing RPG Maker files.
    /// * `translation_path` - Path to the directory where `translation` directory with `.txt` files will be created.
    /// * `engine_type` - Engine type of the source RPG Maker files.
    ///
    /// # Example
    /// ```no_run
    /// reader.read("C:/Game/Data", "C:/Game/translation", EngineType::VXAce);
    /// ```
    pub fn read<P: AsRef<Path> + Sync>(
        &self,
        source_path: P,
        translation_path: P,
        engine_type: EngineType,
    ) {
        if self.file_flags == FileFlags::None {
            return;
        }

        let mut ignore_map: IgnoreMap = IgnoreMap::default();

        if self.ignore {
            ignore_map.extend(parse_ignore(
                translation_path.as_ref().join(RVPACKER_IGNORE_FILE),
            ));
        }

        if self.file_flags.contains(FileFlags::Map) {
            let reader = MapReader::new(
                source_path.as_ref(),
                translation_path.as_ref(),
                engine_type,
            );

            reader
                .ignore_map(&mut ignore_map)
                .game_type(self.game_type)
                .read_mode(self.read_mode)
                .romanize(self.romanize)
                .logging(self.logging)
                .ignore(self.ignore)
                .trim(self.trim)
                .sort(self.sort)
                .read();
        }

        if self.file_flags.contains(FileFlags::Other) {
            let reader = OtherReader::new(
                source_path.as_ref(),
                translation_path.as_ref(),
                engine_type,
            );

            reader
                .ignore_map(&mut ignore_map)
                .game_type(self.game_type)
                .read_mode(self.read_mode)
                .romanize(self.romanize)
                .logging(self.logging)
                .ignore(self.ignore)
                .trim(self.trim)
                .sort(self.sort)
                .read();
        }

        if self.file_flags.contains(FileFlags::System) {
            let system_file_path = source_path
                .as_ref()
                .join(format!("System.{}", get_engine_extension(engine_type)));

            let reader = SystemReader::new(
                system_file_path.as_path(),
                translation_path.as_ref(),
                engine_type,
            );

            reader
                .ignore_map(&mut ignore_map)
                .read_mode(self.read_mode)
                .romanize(self.romanize)
                .logging(self.logging)
                .ignore(self.ignore)
                .trim(self.trim)
                .sort(self.sort)
                .read();
        }

        if self.file_flags.contains(FileFlags::Scripts) {
            if engine_type.is_new() {
                let plugins_file_path = source_path
                    .as_ref()
                    .parent()
                    .unwrap_log()
                    .join("js/plugins.js");

                let reader = PluginReader::new(
                    plugins_file_path.as_path(),
                    translation_path.as_ref(),
                );

                reader
                    .ignore_map(&mut ignore_map)
                    .read_mode(self.read_mode)
                    .romanize(self.romanize)
                    .logging(self.logging)
                    .ignore(self.ignore)
                    .sort(self.sort)
                    .read();
            } else {
                let scripts_file_path = source_path.as_ref().join(format!(
                    "Scripts.{}",
                    get_engine_extension(engine_type)
                ));

                let reader = ScriptReader::new(
                    scripts_file_path.as_path(),
                    translation_path.as_ref(),
                );

                reader
                    .ignore_map(&mut ignore_map)
                    .read_mode(self.read_mode)
                    .romanize(self.romanize)
                    .logging(self.logging)
                    .ignore(self.ignore)
                    .sort(self.sort)
                    .read();
            }
        }
    }
}

impl Default for Reader {
    fn default() -> Self {
        Self::new()
    }
}

/// A builder struct for `Reader`.
///
/// The `Reader` provides a configurable interface to control how files are parsed,
/// which files are selected, and how text content is filtered.
///
/// # Fields
/// - `file_flags`: Indicates which RPG Maker files should be processed. Use `with_flags` to set them.
/// - `read_mode`: Defines the read strategy. Use `read_mode` to set it.
/// - `game_type`: Specifies which RPG Maker game type the data is from. Use `game_type` to set it.
/// - `romanize`: Enables or disables romanization of parsed text. For more info, and to set it, see `romanize`.
/// - `logging`: If enabled, logs operations and progress. Use `logging`
/// - `ignore`: Ignores entries from `.rvpacker-ignore` file. Use `ignore` to set it.
/// - `trim`: Removes leading and trailing whitespace from extracted strings. Use `trim` to set it.
/// - `sort`: Sorts the extracted text entries chronologically. Use `sort` to set it.
///
/// # Example
/// ```
/// use rvpacker_txt_rs_lib::{ReaderBuilder, FileFlags, GameType};
///
/// let mut reader = ReaderBuilder::new().with_flags(FileFlags::Map | FileFlags::Other).build();
/// ```
pub struct ReaderBuilder {
    reader: Reader,
}

impl ReaderBuilder {
    /// Creates a new `ReaderBuilder` instance with default values.
    ///
    /// By default, all four file flags are set (all files will be read), the `ReadMode::Default` read mode is used,
    /// and all other options are disabled.
    ///
    /// # Example
    /// ```no_run
    /// let mut reader = ReaderBuilder::new().build();
    /// ```
    pub fn new() -> Self {
        Self {
            reader: Reader::new(),
        }
    }

    /// Sets the file flags to determine which RPG Maker files will be parsed.
    ///
    /// There's four FileFlags variants:
    /// * `FileFlags::Map` - enables `Mapxxx.ext` files processing.
    /// * `FileFlags::Other` - enables processing files other than `Map`, `System`, `Scripts` and `plugins`.
    /// * `FileFlags::System` - enables `System.txt` file processing.
    /// * `FileFlags::Scripts` - enables `Scripts.ext`/`plugins.js` file processing, based on engine type.
    ///
    /// # Arguments
    /// * `flags` - A `FileFlags` value indicating the file types to include.
    ///
    /// # Example
    /// ```no_run
    /// let reader = ReaderBuilder::new().with_flags(FileFlags::Map | FileFlags::Other).build();
    /// ```
    pub fn with_flags(mut self, flags: FileFlags) -> Self {
        self.reader.file_flags = flags;
        self
    }

    /// Sets the read mode that affects how data is parsed.
    ///
    /// There's only three read modes:
    /// * `ReadMode::Default` - parses the text from the RPG Maker files, aborts if translation files already exist.
    /// * `ReadMode::Append` - appends the new text to the translation files. That's particularly helpful if the game received content update.
    /// * `ReadMode::Force` - parses the text from the RPG Maker files, overwrites the existing translation. **DANGEROUS!**
    ///
    /// # Arguments
    /// * `mode` - A `ReadMode` variant.
    ///
    /// # Example
    /// ```no_run
    /// let reader = ReaderBuilder::new().read_mode(ReadMode::Default).build();
    /// ```
    pub fn read_mode(mut self, mode: ReadMode) -> Self {
        self.reader.read_mode = mode;
        self
    }

    /// Sets the game type for custom processing.
    ///
    /// Right now, custom processing is implement for Fear & Hunger 2: Termina (`GameType::Termina`), and LisaRPG series games (`GameType::LisaRPG`).
    ///
    /// There's no single definition for "custom processing", but the current implementations filter out unnecessary text and improve the readability of output `.txt` files.
    ///
    /// For example, in LisaRPG games, `\nbt` prefix is used in dialogues to mark the tile, above which textbox should appear. When `game_type` is set to `GameType::LisaRPG`, this prefix is not included to the output `.txt` files.
    ///
    /// # Arguments
    /// * `game_type` - A `GameType` variant.
    ///
    /// # Example
    /// ```no_run
    /// let reader = ReaderBuilder::new().game_type(GameType::Termina).build();
    /// ```
    pub fn game_type(mut self, game_type: GameType) -> Self {
        self.reader.game_type = game_type;
        self
    }

    /// Enables or disables romanization of the parsed text.
    ///
    /// Essentially, this flag just replaced Eastern symbols in strings to their Western equivalents.
    ///
    /// For example, 「」 Eastern (Japanese) quotation marks will be replaced by `''`.
    ///
    /// # Example
    /// ```no_run
    /// let reader = ReaderBuilder::new().romanize(true).build();
    /// ```
    pub fn romanize(mut self, enabled: bool) -> Self {
        self.reader.romanize = enabled;
        self
    }

    /// Sets whether to log file processing.
    ///
    /// # Example
    /// ```no_run
    /// let reader = ReaderBuilder::new().logging(true).build();
    /// ```
    pub fn logging(mut self, enabled: bool) -> Self {
        self.reader.logging = enabled;
        self
    }

    /// Sets whether to ignore entries from `.rvpacker-ignore` file.
    ///
    /// # Example
    /// ```no_run
    /// let reader = ReaderBuilder::new().ignore(true).build();
    /// ```
    pub fn ignore(mut self, enabled: bool) -> Self {
        self.reader.ignore = enabled;
        self
    }

    /// Sets whether to trim whitespace from strings.
    ///
    /// # Example
    /// ```no_run
    /// let reader = ReaderBuilder::new().trim(true).build();
    /// ```
    pub fn trim(mut self, enabled: bool) -> Self {
        self.reader.trim = enabled;
        self
    }

    /// Sets whether to sort extracted text chronologically.
    /// Works only with `ReadMode::Append`.
    ///
    /// # Example
    /// ```no_run
    /// let reader = ReaderBuilder::new().sort(true).build();
    /// ```
    pub fn sort(mut self, enabled: bool) -> Self {
        self.reader.sort = enabled;
        self
    }

    /// Builds and returns the `Reader`.
    pub fn build(self) -> Reader {
        self.reader
    }
}

impl Default for ReaderBuilder {
    fn default() -> Self {
        Self::new()
    }
}
