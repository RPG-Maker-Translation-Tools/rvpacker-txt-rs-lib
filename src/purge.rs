#[allow(unused_imports)]
use crate::Reader;
use crate::{
    constants::*, core::*, functions::parse_ignore, get_engine_extension,
    types::*,
};
use std::{fs::write, path::Path};

struct MapPurger<'a> {
    base: MapBase<'a>,
}

impl<'a> MapPurger<'a> {
    fn new(
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
                ProcessingMode::Purge,
            ),
        }
    }

    fn romanize(mut self, romanize: bool) -> Self {
        self.base.base.romanize = romanize;
        self
    }

    fn logging(mut self, logging: bool) -> Self {
        self.base.base.logging = logging;
        self
    }

    fn trim(mut self, trim: bool) -> Self {
        self.base.base.trim = trim;
        self
    }

    fn game_type(mut self, game_type: GameType) -> Self {
        self.base.base.game_type = game_type;
        self
    }

    fn create_ignore(mut self, create_ignore: bool) -> Self {
        self.base.base.create_ignore = create_ignore;
        self
    }

    fn ignore_map(mut self, ignore_map: &'a mut IgnoreMap) -> Self {
        self.base.base.ignore_map = ignore_map;
        self
    }

    fn duplicate_mode(mut self, mode: DuplicateMode) -> Self {
        self.base.base.duplicate_mode = mode;
        self
    }

    fn purge(self) -> ResultVec {
        self.base.process()
    }
}

struct OtherPurger<'a> {
    base: OtherBase<'a>,
}

impl<'a> OtherPurger<'a> {
    fn new(
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
                ProcessingMode::Purge,
            ),
        }
    }

    fn romanize(mut self, romanize: bool) -> Self {
        self.base.base.romanize = romanize;
        self
    }

    fn logging(mut self, logging: bool) -> Self {
        self.base.base.logging = logging;
        self
    }

    fn game_type(mut self, game_type: GameType) -> Self {
        self.base.base.game_type = game_type;
        self
    }

    fn trim(mut self, trim: bool) -> Self {
        self.base.base.trim = trim;
        self
    }

    fn create_ignore(mut self, create_ignore: bool) -> Self {
        self.base.base.create_ignore = create_ignore;
        self
    }

    fn ignore_map(mut self, ignore_map: &'a mut IgnoreMap) -> Self {
        self.base.base.ignore_map = ignore_map;
        self
    }

    fn duplicate_mode(mut self, mode: DuplicateMode) -> Self {
        self.base.base.duplicate_mode = mode;
        self
    }

    fn purge(self) -> ResultVec {
        self.base.process()
    }
}

struct SystemPurger<'a> {
    base: SystemBase<'a>,
}

impl<'a> SystemPurger<'a> {
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
                ProcessingMode::Purge,
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

    pub fn create_ignore(mut self, create_ignore: bool) -> Self {
        self.base.base.create_ignore = create_ignore;
        self
    }

    pub fn ignore_map(mut self, ignore_map: &'a mut IgnoreMap) -> Self {
        self.base.base.ignore_map = ignore_map;
        self
    }

    pub fn purge(self) -> ResultVec {
        self.base.process()
    }
}

struct PluginPurger<'a> {
    base: PluginBase<'a>,
}

impl<'a> PluginPurger<'a> {
    pub fn new(
        plugins_file_path: &'a Path,
        translation_path: &'a Path,
    ) -> Self {
        Self {
            base: PluginBase::new(
                plugins_file_path,
                translation_path,
                translation_path,
                ProcessingMode::Purge,
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

    pub fn create_ignore(mut self, create_ignore: bool) -> Self {
        self.base.base.create_ignore = create_ignore;
        self
    }

    pub fn ignore_map(mut self, ignore_map: &'a mut IgnoreMap) -> Self {
        self.base.base.ignore_map = ignore_map;
        self
    }

    pub fn purge(self) -> ResultVec {
        self.base.process()
    }
}

struct ScriptPurger<'a> {
    base: ScriptBase<'a>,
}

impl<'a> ScriptPurger<'a> {
    pub fn new(
        scripts_file_path: &'a Path,
        translation_path: &'a Path,
    ) -> Self {
        Self {
            base: ScriptBase::new(
                scripts_file_path,
                translation_path,
                translation_path,
                ProcessingMode::Purge,
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

    pub fn create_ignore(mut self, create_ignore: bool) -> Self {
        self.base.base.create_ignore = create_ignore;
        self
    }

    pub fn ignore_map(mut self, ignore_map: &'a mut IgnoreMap) -> Self {
        self.base.base.ignore_map = ignore_map;
        self
    }

    pub fn purge(self) -> ResultVec {
        self.base.process()
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
/// - `logging`: If enabled, logs operations and progress. Use [`Purger::set_logging`] to set it. As this crate uses `log` for logging, you should [set up logging in your program](https://docs.rs/log/latest/log/#available-logging-implementations).
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
    file_flags: FileFlags,
    game_type: GameType,
    duplicate_mode: DuplicateMode,
    romanize: bool,
    logging: bool,
    trim: bool,
    create_ignore: bool,
}

impl Purger {
    /// Creates a new [`Purger`] instance with default values.
    ///
    /// By default, all four file flags are set (all files will be purged), duplicate mode is set to `AllowDuplicates`, and all other options are disabled.
    ///
    /// # Example
    /// ```compile_fail
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
    /// ```compile_fail
    /// purger.set_flags(FileFlags::Map | FileFlags::Other);
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
    /// ```compile_fail
    /// purger.set_game_type(GameType::Termina);
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
    /// ```compile_fail
    /// purger.set_romanize(true);
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
    /// purger.set_logging(true);
    /// ```
    pub fn set_logging(&mut self, enabled: bool) {
        self.logging = enabled;
    }

    /// This function must have the same value that was passed to it in [`Reader`] struct.
    ///
    /// Sets whether to trim whitespace from strings.
    ///
    /// # Example
    /// ```compile_fail
    /// purger.set_trim(true);
    /// ```
    pub fn set_trim(&mut self, enabled: bool) {
        self.trim = enabled;
    }

    /// Sets whether to create `.rvpacker-ignore` file from purged lines.
    ///
    /// # Example
    /// ```compile_fail
    /// purger.set_create_ignore(true);
    /// ```
    pub fn set_create_ignore(&mut self, enabled: bool) {
        self.create_ignore = enabled;
    }

    /// This function must have the same value that was passed to it in [`Reader`] struct.
    ///
    /// Sets, what to do with duplicates. Works only for map and other files.
    ///
    /// - [`DuplicateMode::AllowDuplicates`]: Default and recommended. Each map/event is parsed into its own hashmap. That won't likely cause much clashes between the same lines which require different translations.
    /// - [`DuplicateMode::NoDuplicates`]: Not recommended. This mode is stable and works perfectly, but it will write the same translation into multiple places where source text is used. Recommended only when duplicates cause too much bloat.
    ///
    /// # Example
    /// ```compile_fail
    /// purger.set_duplicate_mode(DuplicateMode::AllowDuplicates);
    /// ```
    pub fn set_duplicate_mode(&mut self, mode: DuplicateMode) {
        self.duplicate_mode = mode;
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
    /// ```compile_fail
    /// reader.read("C:/Game/Data", "C:/Game/translation", EngineType::VXAce);
    /// ```
    pub fn purge<P: AsRef<Path> + Sync>(
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

        if self.create_ignore {
            ignore_map = parse_ignore(
                translation_path.join(RVPACKER_IGNORE_FILE),
                self.duplicate_mode,
                false,
            )?;
        }

        if self.file_flags.contains(FileFlags::Map) {
            results.map =
                MapPurger::new(source_path, translation_path, engine_type)
                    .ignore_map(&mut ignore_map)
                    .game_type(self.game_type)
                    .romanize(self.romanize)
                    .logging(self.logging)
                    .trim(self.trim)
                    .duplicate_mode(self.duplicate_mode)
                    .create_ignore(self.create_ignore)
                    .purge();
        }

        if self.file_flags.contains(FileFlags::Other) {
            results.other =
                OtherPurger::new(source_path, translation_path, engine_type)
                    .ignore_map(&mut ignore_map)
                    .game_type(self.game_type)
                    .romanize(self.romanize)
                    .logging(self.logging)
                    .trim(self.trim)
                    .duplicate_mode(self.duplicate_mode)
                    .create_ignore(self.create_ignore)
                    .purge();
        }

        if self.file_flags.contains(FileFlags::System) {
            let system_file_path = source_path
                .join(format!("System.{}", get_engine_extension(engine_type)));

            results.system = SystemPurger::new(
                &system_file_path,
                translation_path,
                engine_type,
            )
            .ignore_map(&mut ignore_map)
            .romanize(self.romanize)
            .logging(self.logging)
            .trim(self.trim)
            .create_ignore(self.create_ignore)
            .purge();
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
                    PluginPurger::new(&plugins_file_path, translation_path)
                        .ignore_map(&mut ignore_map)
                        .romanize(self.romanize)
                        .logging(self.logging)
                        .create_ignore(self.create_ignore)
                        .purge();
            } else {
                let scripts_file_path = source_path.join(format!(
                    "Scripts.{}",
                    get_engine_extension(engine_type)
                ));

                results.scripts =
                    ScriptPurger::new(&scripts_file_path, translation_path)
                        .ignore_map(&mut ignore_map)
                        .romanize(self.romanize)
                        .logging(self.logging)
                        .create_ignore(self.create_ignore)
                        .purge();
            }
        }

        if self.create_ignore {
            use std::fmt::Write;

            let contents: String = ignore_map.into_iter().fold(
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

            let ignore_file_path = translation_path.join(RVPACKER_IGNORE_FILE);

            write(&ignore_file_path, contents).map_err(|err| {
                Error::WriteFileFailed {
                    file: ignore_file_path,
                    err,
                }
            })?;
        }

        Ok(results)
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
/// - `logging`: If enabled, logs operations and progress. Use [`PurgerBuilder::logging`]
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
    /// By default, all four file flags are set (all files will be purged), duplicate mode is set to `AllowDuplicates`, and all other options are disabled.
    ///
    /// # Example
    /// ```compile_fail
    /// let mut purger = PurgerBuilder::new().build();
    /// ```
    pub fn new() -> Self {
        Self {
            purger: Purger::new(),
        }
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
    /// ```compile_fail
    /// let purger = PurgerBuilder::new().with_flags(FileFlags::Map | FileFlags::Other).build();
    /// ```
    pub fn with_flags(mut self, file_flags: FileFlags) -> Self {
        self.purger.file_flags = file_flags;
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
    /// ```compile_fail
    /// let purger = PurgerBuilder::new().game_type(FileFlags::Map | FileFlags::Other).build();
    /// ```
    pub fn game_type(mut self, game_type: GameType) -> Self {
        self.purger.game_type = game_type;
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
    /// ```compile_fail
    /// let purger = PurgerBuilder::new().romanize(true).build();
    /// ```
    pub fn romanize(mut self, enabled: bool) -> Self {
        self.purger.romanize = enabled;
        self
    }

    /// Sets whether to output logs.
    ///
    /// As this crate uses `log` for logging, you should [set up logging in your program](https://docs.rs/log/latest/log/#available-logging-implementations).
    ///
    /// # Example
    /// ```compile_fail
    /// let purger = PurgerBuilder::new().logging(true).build();
    /// ```
    pub fn logging(mut self, enabled: bool) -> Self {
        self.purger.logging = enabled;
        self
    }

    /// This function must have the same value that was passed to it in [`Reader`] struct.
    ///
    /// Sets whether to trim whitespace from strings.
    ///
    /// # Example
    /// ```compile_fail
    /// let purger = PurgerBuilder::new().trim(true).build();
    /// ```
    pub fn trim(mut self, enabled: bool) -> Self {
        self.purger.trim = enabled;
        self
    }

    /// Sets whether to create `.rvpacker-ignore` file from purged lines.
    ///
    /// # Example
    /// ```compile_fail
    /// let purger = PurgerBuilder::new().create_ignore(true).build();
    /// ```
    pub fn create_ignore(mut self, enabled: bool) -> Self {
        self.purger.create_ignore = enabled;
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
    /// ```compile_fail
    /// let purger = PurgerBuilder::new().duplicate_mode(DuplicateMode::AllowDuplicates);.build();
    /// ```
    pub fn duplicate_mode(mut self, mode: DuplicateMode) -> Self {
        self.purger.duplicate_mode = mode;
        self
    }

    /// Builds and returns the [`Purger`].
    pub fn build(self) -> Purger {
        self.purger
    }
}
