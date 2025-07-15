#[allow(unused_imports)]
use crate::Reader;
use crate::{core::*, get_engine_extension, types::*};
use std::{fs::create_dir_all, path::Path};

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

    pub fn duplicate_mode(mut self, mode: DuplicateMode) -> Self {
        self.base.base.duplicate_mode = mode;
        self
    }

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

    pub fn duplicate_mode(mut self, mode: DuplicateMode) -> Self {
        self.base.base.duplicate_mode = mode;
        self
    }

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

    pub fn write(self) {
        self.base.process();
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
/// - `logging`: If enabled, logs operations and progress. Use [`Writer::set_logging`]
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
    file_flags: FileFlags,
    game_type: GameType,
    duplicate_mode: DuplicateMode,
    romanize: bool,
    logging: bool,
    trim: bool,
}

impl Writer {
    /// Creates a new [`Writer`] instance with default values.
    ///
    /// By default, all four file flags are set (all files will be written), duplicate mode is set to `AllowDuplicates`, and all other options are disabled.
    ///
    /// # Example
    /// ```no_run
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
    /// ```no_run
    /// writer.set_flags(FileFlags::Map | FileFlags::Other);
    /// ```
    pub fn set_flags(&mut self, file_flags: FileFlags) {
        self.file_flags = file_flags;
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
    /// ```no_run
    /// writer.set_game_type(GameType::Termina);
    /// ```
    pub fn set_game_type(&mut self, game_type: GameType) {
        self.game_type = game_type;
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

    /// This function must have the same value that was passed to it in [`Reader`] struct.
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

    /// This function must have the same value that was passed to it in [`Reader`] struct.
    ///
    /// Sets, what to do with duplicates. Works only for map and other files.
    ///
    /// - [`DuplicateMode::AllowDuplicates`]: Default and recommended. Each map/event is parsed into its own hashmap. That won't likely cause much clashes between the same lines which require different translations.
    /// - [`DuplicateMode::NoDuplicates`]: Not recommended. This mode is stable and works perfectly, but it will write the same translation into multiple places where source text is used. Recommended only when duplicates cause too much bloat.
    ///
    /// # Example
    /// ```no_run
    /// writer.set_duplicate_mode(DuplicateMode::AllowDuplicates);
    /// ```
    pub fn set_duplicate_mode(&mut self, mode: DuplicateMode) {
        self.duplicate_mode = mode;
    }

    /// Writes the translation from `.txt` files in `translation_path`, and outputs modified
    /// files from `source_path` to `output_path`.
    ///
    /// Make sure you've configured the writer with the same options as reader before calling it.
    ///
    /// # Arguments
    /// - `source_path` - Path to the directory containing source RPG Maker files.
    /// - `translation_path` - Path to the directory where `translation` directory with `.txt` files is located.
    /// - `output_path` - Path to the directory, where output RPG Maker files will be created.
    /// - `engine_type` - Engine type of the source RPG Maker files.
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
        if self.file_flags.is_empty() {
            return;
        }

        let source_path = source_path.as_ref();
        let translation_path = translation_path.as_ref();
        let output_path = output_path.as_ref();

        create_dir_all(output_path).unwrap_log();

        if self.file_flags.contains(FileFlags::Map) {
            MapWriter::new(
                source_path,
                translation_path,
                output_path,
                engine_type,
            )
            .game_type(self.game_type)
            .romanize(self.romanize)
            .logging(self.logging)
            .duplicate_mode(self.duplicate_mode)
            .trim(self.trim)
            .write();
        }

        if self.file_flags.contains(FileFlags::Other) {
            OtherWriter::new(
                source_path,
                translation_path,
                output_path,
                engine_type,
            )
            .game_type(self.game_type)
            .romanize(self.romanize)
            .logging(self.logging)
            .duplicate_mode(self.duplicate_mode)
            .trim(self.trim)
            .write();
        }

        if self.file_flags.contains(FileFlags::System) {
            let system_file_path = source_path
                .join(format!("System.{}", get_engine_extension(engine_type)));

            SystemWriter::new(
                system_file_path.as_path(),
                translation_path,
                output_path,
                engine_type,
            )
            .romanize(self.romanize)
            .logging(self.logging)
            .trim(self.trim)
            .write();
        }

        if self.file_flags.contains(FileFlags::Scripts) {
            if engine_type.is_new() {
                let plugins_file_path =
                    source_path.parent().unwrap_log().join("js/plugins.js");
                let plugins_output_path =
                    output_path.parent().unwrap_log().join("js");

                create_dir_all(&plugins_output_path).unwrap_log();

                PluginWriter::new(
                    &plugins_file_path,
                    translation_path,
                    &plugins_output_path,
                )
                .romanize(self.romanize)
                .logging(self.logging)
                .write();
            } else {
                let scripts_file_path = source_path.join(format!(
                    "Scripts.{}",
                    get_engine_extension(engine_type)
                ));

                ScriptWriter::new(
                    &scripts_file_path,
                    translation_path,
                    output_path,
                )
                .romanize(self.romanize)
                .logging(self.logging)
                .write();
            }
        }
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
/// - `logging`: If enabled, logs operations and progress. Use [`WriterBuilder::logging`]
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
    /// By default, all four file flags are set (all files will be written), duplicate mode is set to `AllowDuplicates`, and all other options are disabled.
    ///
    /// # Example
    /// ```no_run
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
    /// ```no_run
    /// let writer = WriterBuilder::new().with_flags(FileFlags::Map | FileFlags::Other).build();
    /// ```
    pub fn with_flags(mut self, flags: FileFlags) -> Self {
        self.writer.file_flags = flags;
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
    /// ```no_run
    /// let writer = WriterBuilder::new().game_type(GameType::Termina).build();
    /// ```
    pub fn game_type(mut self, game_type: GameType) -> Self {
        self.writer.game_type = game_type;
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

    /// This function must have the same value that was passed to it in [`Reader`] struct.
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

    /// This function must have the same value that was passed to it in [`Reader`] struct.
    ///
    /// Sets, what to do with duplicates. Works only for map and other files.
    ///
    /// - [`DuplicateMode::AllowDuplicates`]: Default and recommended. Each map/event is parsed into its own hashmap. That won't likely cause much clashes between the same lines which require different translations.
    /// - [`DuplicateMode::NoDuplicates`]: Not recommended. This mode is stable and works perfectly, but it will write the same translation into multiple places where source text is used. Recommended only when duplicates cause too much bloat.
    ///
    /// # Example
    /// ```no_run
    /// let writer = WriterBuilder::new().duplicate_mode(DuplicateMode::NoDuplicates);.build();
    /// ```
    pub fn duplicate_mode(mut self, mode: DuplicateMode) -> Self {
        self.writer.duplicate_mode = mode;
        self
    }

    /// Builds and returns the [`Writer`].
    pub fn build(self) -> Writer {
        self.writer
    }
}
