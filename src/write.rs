use crate::{bases::*, get_engine_extension, types::*};
use std::path::Path;

struct MapWriter<'a> {
    base: MapBase<'a>,
}

impl<'a> MapWriter<'a> {
    pub fn new(
        source_path: &'a Path,
        translation_path: &'a Path,
        output_path: &'a Path,
        engine_type: EngineType,
    ) -> Self {
        Self {
            base: MapBase::new(
                source_path,
                translation_path,
                output_path,
                engine_type,
                ProcessingMode::Write,
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

    pub fn trim(mut self, trim: bool) -> Self {
        self.base.base.trim = trim;
        self
    }

    #[inline(always)]
    pub fn write(self) {
        self.base.process();
    }
}

struct OtherWriter<'a> {
    base: OtherBase<'a>,
}

impl<'a> OtherWriter<'a> {
    pub fn new(
        source_path: &'a Path,
        translation_path: &'a Path,
        output_path: &'a Path,
        engine_type: EngineType,
    ) -> Self {
        Self {
            base: OtherBase::new(
                source_path,
                translation_path,
                output_path,
                engine_type,
                ProcessingMode::Write,
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

    pub fn trim(mut self, trim: bool) -> Self {
        self.base.base.trim = trim;
        self
    }

    #[inline(always)]
    pub fn write(self) {
        self.base.process();
    }
}

struct SystemWriter<'a> {
    base: SystemBase<'a>,
}

impl<'a> SystemWriter<'a> {
    pub fn new(
        system_file_path: &'a Path,
        translation_path: &'a Path,
        output_path: &'a Path,
        engine_type: EngineType,
    ) -> Self {
        Self {
            base: SystemBase::new(
                system_file_path,
                translation_path,
                output_path,
                engine_type,
                ProcessingMode::Write,
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

    pub fn trim(mut self, trim: bool) -> Self {
        self.base.base.trim = trim;
        self
    }

    #[inline(always)]
    pub fn write(self) {
        self.base.process();
    }
}

struct PluginWriter<'a> {
    base: PluginBase<'a>,
}

impl<'a> PluginWriter<'a> {
    pub fn new(
        plugins_file_path: &'a Path,
        translation_path: &'a Path,
        output_path: &'a Path,
    ) -> Self {
        Self {
            base: PluginBase::new(
                plugins_file_path,
                translation_path,
                output_path,
                ProcessingMode::Write,
            ),
        }
    }

    pub fn logging(mut self, logging: bool) -> Self {
        self.base.base.logging = logging;
        self
    }

    pub fn romanize(mut self, romanize: bool) -> Self {
        self.base.base.romanize = romanize;
        self
    }

    #[inline(always)]
    pub fn write(self) {
        self.base.process();
    }
}

struct ScriptWriter<'a> {
    base: ScriptBase<'a>,
}

impl<'a> ScriptWriter<'a> {
    pub fn new(
        scripts_file_path: &'a Path,
        translation_path: &'a Path,
        output_path: &'a Path,
    ) -> Self {
        Self {
            base: ScriptBase::new(
                scripts_file_path,
                translation_path,
                output_path,
                ProcessingMode::Write,
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

    #[inline(always)]
    pub fn write(self) {
        self.base.process();
    }
}

/// A struct used for writing translation from `.txt` files back to RPG Maker files.
///
/// The `Writer` struct, essentially, should receive the same options, as `Reader`, to ensure proper writing.
///
/// # Fields
/// - `file_flags`: Indicates which RPG Maker files should be processed. Use `set_flags` to set them.
/// - `game_type`: Specifies which RPG Maker game type the data is from. Use `set_game_type` to set it.
/// - `romanize`: Enables or disables romanization of parsed text. For more info, and to set it, see `set_romanize`.
/// - `logging`: If enabled, logs operations and progress. Use `set_logging`
/// - `trim`: Removes leading and trailing whitespace from extracted strings. Use `set_trim` to set it.
///
/// # Example
/// ```no_run
/// use rvpacker_txt_rs_lib::{Writer, FileFlags, EngineType};
///
/// let mut writer = Writer::new();
/// writer.set_flags(FileFlags::Map | FileFlags::Other);
/// writer.write("C:/Game/Data", "C:/Game/translation", "C:/Game/output", EngineType::VXAce);
/// ```
pub struct Writer {
    file_flags: FileFlags,
    game_type: GameType,
    romanize: bool,
    logging: bool,
    trim: bool,
}

impl Writer {
    /// Creates a new `Writer` instance with default values.
    ///
    /// By default, all four file flags are set (all files will be written),
    /// and all other options are disabled.
    ///
    /// # Example
    /// ```no_run
    /// let mut writer = Writer::new();
    /// ```
    pub fn new() -> Self {
        Self {
            file_flags: FileFlags::All,
            game_type: GameType::None,
            romanize: false,
            logging: false,
            trim: false,
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
    /// writer.set_flags(FileFlags::Map | FileFlags::Other);
    /// ```
    pub fn set_flags(&mut self, file_flags: FileFlags) {
        self.file_flags = file_flags;
    }

    /// This function must have the same value that was passed to it in `Reader` struct.
    ///
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
    /// writer.set_game_type(GameType::Termina);
    /// ```
    pub fn set_game_type(&mut self, game_type: GameType) {
        self.game_type = game_type;
    }

    /// This function must have the same value that was passed to it in `Reader` struct.
    ///
    /// Enables or disables romanization of the parsed text.
    ///
    /// Essentially, this flag just replaced Eastern symbols in strings to their Western equivalents.
    ///
    /// For example, 「」 Eastern (Japanese) quotation marks will be replaced by `''`.
    ///
    /// # Example
    /// ```no_run
    /// writer.set_romanize(true);
    /// ```
    pub fn set_romanize(&mut self, logging: bool) {
        self.romanize = logging;
    }

    /// Sets whether to log file processing.
    ///
    /// # Example
    /// ```no_run
    /// writer.set_logging(true);
    /// ```
    pub fn set_logging(&mut self, logging: bool) {
        self.logging = logging;
    }

    /// This function must have the same value that was passed to it in `Reader` struct.
    ///
    /// Sets whether to trim whitespace from strings.
    ///
    /// # Example
    /// ```no_run
    /// writer.set_trim(true);
    /// ```
    pub fn set_trim(&mut self, logging: bool) {
        self.trim = logging;
    }

    #[inline(always)]
    /// Writes the translation from `.txt` files in `translation_path`, and outputs modified
    /// files from `source_path` to `output_path`.
    ///
    /// Make sure you've configured the writer with the same options as reader before calling it.
    ///
    /// # Arguments
    /// * `source_path` - Path to the directory containing source RPG Maker files.
    /// * `translation_path` - Path to the directory where `translation` directory with `.txt` files is located.
    /// * `output_path` - Path to the directory, where output RPG Maker files will be created.
    /// * `engine_type` - Engine type of the source RPG Maker files.
    ///
    /// # Example
    /// ```no_run
    /// writer.write("C:/Game/Data", "C:/Game/translation", "C:/Game/output", EngineType::VXAce);
    /// ```
    pub fn write<P: AsRef<Path>>(
        &self,
        source_path: P,
        translation_path: P,
        output_path: P,
        engine_type: EngineType,
    ) {
        if self.file_flags.contains(FileFlags::Map) {
            let writer = MapWriter::new(
                source_path.as_ref(),
                translation_path.as_ref(),
                output_path.as_ref(),
                engine_type,
            )
            .game_type(self.game_type)
            .romanize(self.romanize)
            .logging(self.logging)
            .trim(self.trim);

            writer.write();
        }

        if self.file_flags.contains(FileFlags::Other) {
            let writer = OtherWriter::new(
                source_path.as_ref(),
                translation_path.as_ref(),
                output_path.as_ref(),
                engine_type,
            )
            .game_type(self.game_type)
            .romanize(self.romanize)
            .logging(self.logging)
            .trim(self.trim);

            writer.write();
        }

        if self.file_flags.contains(FileFlags::System) {
            let system_file_path = source_path
                .as_ref()
                .join(format!("System.{}", get_engine_extension(engine_type)));

            let writer = SystemWriter::new(
                system_file_path.as_path(),
                translation_path.as_ref(),
                output_path.as_ref(),
                engine_type,
            )
            .romanize(self.romanize)
            .logging(self.logging)
            .trim(self.trim);

            writer.write();
        }

        if self.file_flags.contains(FileFlags::Scripts) {
            if engine_type.is_new() {
                let plugins_file_path = source_path
                    .as_ref()
                    .parent()
                    .unwrap_log()
                    .join("js/plugins.js");

                let writer = PluginWriter::new(
                    plugins_file_path.as_path(),
                    translation_path.as_ref(),
                    output_path.as_ref(),
                )
                .romanize(self.romanize)
                .logging(self.logging);

                writer.write();
            } else {
                let scripts_file_path = source_path.as_ref().join(format!(
                    "Scripts.{}",
                    get_engine_extension(engine_type)
                ));

                let writer = ScriptWriter::new(
                    scripts_file_path.as_path(),
                    translation_path.as_ref(),
                    output_path.as_ref(),
                )
                .romanize(self.romanize)
                .logging(self.logging);

                writer.write();
            }
        }
    }
}

impl Default for Writer {
    fn default() -> Self {
        Self::new()
    }
}

/// A builder struct for `Writer`.
///
/// The `Writer` struct, essentially, should receive the same options, as `Reader`, to ensure proper writing.
///
/// # Fields
/// - `file_flags`: Indicates which RPG Maker files should be processed. Use `set_flags` to set them.
/// - `game_type`: Specifies which RPG Maker game type the data is from. Use `set_game_type` to set it.
/// - `romanize`: Enables or disables romanization of parsed text. For more info, and to set it, see `set_romanize`.
/// - `logging`: If enabled, logs operations and progress. Use `set_logging`
/// - `trim`: Removes leading and trailing whitespace from extracted strings. Use `set_trim` to set it.
///
/// # Example
/// ```
/// use rvpacker_txt_rs_lib::{WriterBuilder, FileFlags, GameType};
///
/// let mut writer = WriterBuilder::new().with_flags(FileFlags::Map | FileFlags::Other).build();
/// ```
pub struct WriterBuilder {
    writer: Writer,
}

impl WriterBuilder {
    /// Creates a new `WriterBuilder` instance with default values.
    ///
    /// By default, all four file flags are set (all files will be written),
    /// and all other options are disabled.
    ///
    /// # Example
    /// ```no_run
    /// let mut writer = WriterBuilder::new().build();
    /// ```
    pub fn new() -> Self {
        Self {
            writer: Writer::new(),
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
    /// let writer = WriterBuilder::new().with_flags(FileFlags::Map | FileFlags::Other).build();
    /// ```
    pub fn with_flags(mut self, flags: FileFlags) -> Self {
        self.writer.file_flags = flags;
        self
    }

    /// This function must have the same value that was passed to it in `Reader` struct.
    ///
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
    /// let writer = WriterBuilder::new().game_type(GameType::Termina).build();
    /// ```
    pub fn game_type(mut self, game_type: GameType) -> Self {
        self.writer.game_type = game_type;
        self
    }

    /// This function must have the same value that was passed to it in `Reader` struct.
    ///
    /// Enables or disables romanization of the parsed text.
    ///
    /// Essentially, this flag just replaced Eastern symbols in strings to their Western equivalents.
    ///
    /// For example, 「」 Eastern (Japanese) quotation marks will be replaced by `''`.
    ///
    /// # Example
    /// ```no_run
    /// let writer = WriterBuilder::new().romanize(true).build();
    /// ```
    pub fn romanize(mut self, enabled: bool) -> Self {
        self.writer.romanize = enabled;
        self
    }

    /// Sets whether to log file processing.
    ///
    /// # Example
    /// ```no_run
    /// let writer = WriterBuilder::new().logging(true).build();
    /// ```
    pub fn logging(mut self, enabled: bool) -> Self {
        self.writer.logging = enabled;
        self
    }

    /// This function must have the same value that was passed to it in `Reader` struct.
    ///
    /// Sets whether to trim whitespace from strings.
    ///
    /// # Example
    /// ```no_run
    /// let writer = WriterBuilder::new().trim(true).build();
    /// ```
    pub fn trim(mut self, enabled: bool) -> Self {
        self.writer.trim = enabled;
        self
    }

    /// Builds and returns the `Writer`.
    pub fn build(self) -> Writer {
        self.writer
    }
}

impl Default for WriterBuilder {
    fn default() -> Self {
        Self::new()
    }
}
