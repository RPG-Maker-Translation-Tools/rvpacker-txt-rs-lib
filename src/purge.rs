use crate::{
    bases::*, constants::*, functions::parse_ignore, get_engine_extension,
    types::*,
};
use std::path::Path;

struct MapPurger<'a> {
    base: MapBase<'a>,
}

impl<'a> MapPurger<'a> {
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

    pub fn game_type(mut self, game_type: GameType) -> Self {
        self.base.base.game_type = game_type;
        self
    }

    pub fn stat(mut self, stat: bool) -> Self {
        self.base.base.stat = stat;
        self
    }

    pub fn leave_filled(mut self, leave_filled: bool) -> Self {
        self.base.base.leave_filled = leave_filled;
        self
    }

    pub fn create_ignore(mut self, create_ignore: bool) -> Self {
        self.base.base.create_ignore = create_ignore;
        self
    }

    pub fn purge_empty(mut self, purge_empty: bool) -> Self {
        self.base.base.purge_empty = purge_empty;
        self
    }

    pub fn stat_vec(mut self, stat_vec: &'a mut StatVec) -> Self {
        self.base.base.stat_vec = stat_vec;
        self
    }

    pub fn ignore_map(mut self, ignore_map: &'a mut IgnoreMap) -> Self {
        self.base.base.ignore_map = ignore_map;
        self
    }

    #[inline(always)]
    pub fn purge(self) {
        self.base.process();
    }
}

struct OtherPurger<'a> {
    base: OtherBase<'a>,
}

impl<'a> OtherPurger<'a> {
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

    pub fn game_type(mut self, game_type: GameType) -> Self {
        self.base.base.game_type = game_type;
        self
    }

    pub fn trim(mut self, trim: bool) -> Self {
        self.base.base.trim = trim;
        self
    }

    pub fn stat(mut self, stat: bool) -> Self {
        self.base.base.stat = stat;
        self
    }

    pub fn leave_filled(mut self, leave_filled: bool) -> Self {
        self.base.base.leave_filled = leave_filled;
        self
    }

    pub fn create_ignore(mut self, create_ignore: bool) -> Self {
        self.base.base.create_ignore = create_ignore;
        self
    }

    pub fn purge_empty(mut self, purge_empty: bool) -> Self {
        self.base.base.purge_empty = purge_empty;
        self
    }

    pub fn stat_vec(mut self, stat_vec: &'a mut StatVec) -> Self {
        self.base.base.stat_vec = stat_vec;
        self
    }

    pub fn ignore_map(mut self, ignore_map: &'a mut IgnoreMap) -> Self {
        self.base.base.ignore_map = ignore_map;
        self
    }

    #[inline(always)]
    pub fn purge(self) {
        self.base.process();
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

    pub fn stat(mut self, stat: bool) -> Self {
        self.base.base.stat = stat;
        self
    }

    pub fn leave_filled(mut self, leave_filled: bool) -> Self {
        self.base.base.leave_filled = leave_filled;
        self
    }

    pub fn create_ignore(mut self, create_ignore: bool) -> Self {
        self.base.base.create_ignore = create_ignore;
        self
    }

    pub fn purge_empty(mut self, purge_empty: bool) -> Self {
        self.base.base.purge_empty = purge_empty;
        self
    }

    pub fn stat_vec(mut self, stat_vec: &'a mut StatVec) -> Self {
        self.base.base.stat_vec = stat_vec;
        self
    }

    pub fn ignore_map(mut self, ignore_map: &'a mut IgnoreMap) -> Self {
        self.base.base.ignore_map = ignore_map;
        self
    }

    #[inline(always)]
    pub fn purge(self) {
        self.base.process();
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

    pub fn stat(mut self, stat: bool) -> Self {
        self.base.base.stat = stat;
        self
    }

    pub fn leave_filled(mut self, leave_filled: bool) -> Self {
        self.base.base.leave_filled = leave_filled;
        self
    }

    pub fn create_ignore(mut self, create_ignore: bool) -> Self {
        self.base.base.create_ignore = create_ignore;
        self
    }

    pub fn purge_empty(mut self, purge_empty: bool) -> Self {
        self.base.base.purge_empty = purge_empty;
        self
    }

    pub fn stat_vec(mut self, stat_vec: &'a mut StatVec) -> Self {
        self.base.base.stat_vec = stat_vec;
        self
    }

    pub fn ignore_map(mut self, ignore_map: &'a mut IgnoreMap) -> Self {
        self.base.base.ignore_map = ignore_map;
        self
    }

    #[inline(always)]
    pub fn purge(self) {
        self.base.process();
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

    pub fn stat(mut self, stat: bool) -> Self {
        self.base.base.stat = stat;
        self
    }

    pub fn leave_filled(mut self, leave_filled: bool) -> Self {
        self.base.base.leave_filled = leave_filled;
        self
    }

    pub fn create_ignore(mut self, create_ignore: bool) -> Self {
        self.base.base.create_ignore = create_ignore;
        self
    }

    pub fn purge_empty(mut self, purge_empty: bool) -> Self {
        self.base.base.purge_empty = purge_empty;
        self
    }

    pub fn stat_vec(mut self, stat_vec: &'a mut StatVec) -> Self {
        self.base.base.stat_vec = stat_vec;
        self
    }

    pub fn ignore_map(mut self, ignore_map: &'a mut IgnoreMap) -> Self {
        self.base.base.ignore_map = ignore_map;
        self
    }

    #[inline(always)]
    pub fn purge(self) {
        self.base.process();
    }
}

/// A struct used for purging translation lines from `.txt` files based on options.
///
/// The `Purger` struct, essentially, should receive the same options, as `Reader`, to ensure proper purging.
///
/// # Fields
/// - `file_flags`: Indicates which RPG Maker files should be processed. Use `set_flags` to set them.
/// - `game_type`: Specifies which RPG Maker game type the data is from. Use `set_game_type` to set it.
/// - `romanize`: Enables or disables romanization of parsed text. For more info, and to set it, see `set_romanize`.
/// - `logging`: If enabled, logs operations and progress. Use `set_logging`
/// - `trim`: Removes leading and trailing whitespace from extracted strings. Use `set_trim` to set it.
/// - `stat`: If enabled, doesn't actually purge any lines, and outputs `stat.txt` file with purge statistics
///   to translation directory.
/// - `leave_filled`: If enabled, leaves the lines with filled translation, even if they don't exist in game.
/// - `create_ignore`: If enabled, creates `.rvpacker-ignore` file from purged lines, that can be used in
///   `Reader` struct when its `ignore` option is set.
/// - `purge_empty`: If enabled, purges only the lines with empty translation.
///
/// # Example
/// ```no_run
/// use rvpacker_txt_rs_lib::{Purger, FileFlags, EngineType};
///
/// let mut purger = Purger::new();
/// purger.set_flags(FileFlags::Map | FileFlags::Other);
/// purger.purge("C:/Game/Data", "C:/Game/translation", EngineType::VXAce);
/// ```
pub struct Purger {
    file_flags: FileFlags,
    game_type: GameType,
    romanize: bool,
    logging: bool,
    trim: bool,
    stat: bool,
    leave_filled: bool,
    create_ignore: bool,
    purge_empty: bool,
}

impl Purger {
    /// Creates a new `Purger` instance with default values.
    ///
    /// By default, all four file flags are set (all files will be purged),
    /// and all other options are disabled.
    ///
    /// # Example
    /// ```no_run
    /// let mut purger = Purger::new();
    /// ```
    pub fn new() -> Self {
        Self {
            file_flags: FileFlags::All,
            game_type: GameType::None,
            romanize: false,
            logging: false,
            trim: false,
            stat: false,
            leave_filled: false,
            create_ignore: false,
            purge_empty: false,
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
    /// purger.set_flags(FileFlags::Map | FileFlags::Other);
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
    /// purger.set_game_type(GameType::Termina);
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
    /// purger.set_romanize(true);
    /// ```
    pub fn set_romanize(&mut self, enabled: bool) {
        self.romanize = enabled;
    }

    /// Sets whether to log file processing.
    ///
    /// # Example
    /// ```no_run
    /// purger.set_logging(true);
    /// ```
    pub fn set_logging(&mut self, enabled: bool) {
        self.logging = enabled;
    }

    /// This function must have the same value that was passed to it in `Reader` struct.
    ///
    /// Sets whether to trim whitespace from strings.
    ///
    /// # Example
    /// ```no_run
    /// purger.set_trim(true);
    /// ```
    pub fn set_trim(&mut self, enabled: bool) {
        self.trim = enabled;
    }

    /// If set, `purge` outputs purge statistics, instead of actually purging anything.
    ///
    /// # Example
    /// ```no_run
    /// purger.set_stat(true);
    /// ```
    pub fn set_stat(&mut self, enabled: bool) {
        self.stat = enabled;
    }

    /// Sets whether to leave lines with filled translation untouched.
    ///
    /// # Example
    /// ```no_run
    /// purger.set_leave_filled(true);
    /// ```
    pub fn set_leave_filled(&mut self, enabled: bool) {
        self.leave_filled = enabled;
    }

    /// Sets whether to create `.rvpacker-ignore` file from purged lines.
    ///
    /// # Example
    /// ```no_run
    /// purger.set_create_ignore(true);
    /// ```
    pub fn set_create_ignore(&mut self, enabled: bool) {
        self.create_ignore = enabled;
    }

    /// Sets whether to purge only lines with empty translation.
    ///
    /// # Example
    /// ```no_run
    /// purger.set_purge_empty(true);
    /// ```
    pub fn set_purge_empty(&mut self, enabled: bool) {
        self.purge_empty = enabled;
    }

    /// Purges the lines from `.txt` files in `translation_path`, using source RPG Maker files from `source_path`.
    ///
    /// Make sure you've configured the purger with the same options as reader before calling it.
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
    pub fn purge<P: AsRef<Path> + Sync>(
        &self,
        source_path: P,
        translation_path: P,
        engine_type: EngineType,
    ) {
        let mut ignore_map: IgnoreMap = IgnoreMap::default();

        if self.create_ignore {
            ignore_map.extend(parse_ignore(
                translation_path.as_ref().join(RVPACKER_IGNORE_FILE),
            ));
        }

        let mut stat_vec: StatVec = StatVec::default();

        if self.file_flags.contains(FileFlags::Map) {
            let purger = MapPurger::new(
                source_path.as_ref(),
                translation_path.as_ref(),
                engine_type,
            )
            .ignore_map(&mut ignore_map)
            .game_type(self.game_type)
            .stat_vec(&mut stat_vec)
            .romanize(self.romanize)
            .logging(self.logging)
            .trim(self.trim)
            .stat(self.stat)
            .leave_filled(self.leave_filled)
            .create_ignore(self.create_ignore)
            .purge_empty(self.purge_empty);

            purger.purge();
        }

        if self.file_flags.contains(FileFlags::Other) {
            let purger = OtherPurger::new(
                source_path.as_ref(),
                translation_path.as_ref(),
                engine_type,
            )
            .ignore_map(&mut ignore_map)
            .game_type(self.game_type)
            .stat_vec(&mut stat_vec)
            .romanize(self.romanize)
            .logging(self.logging)
            .trim(self.trim)
            .stat(self.stat)
            .leave_filled(self.leave_filled)
            .create_ignore(self.create_ignore)
            .purge_empty(self.purge_empty);

            purger.purge();
        }

        if self.file_flags.contains(FileFlags::System) {
            let system_file_path = source_path
                .as_ref()
                .join(format!("System.{}", get_engine_extension(engine_type)));

            let purger = SystemPurger::new(
                system_file_path.as_ref(),
                translation_path.as_ref(),
                engine_type,
            )
            .ignore_map(&mut ignore_map)
            .stat_vec(&mut stat_vec)
            .romanize(self.romanize)
            .logging(self.logging)
            .trim(self.trim)
            .stat(self.stat)
            .leave_filled(self.leave_filled)
            .create_ignore(self.create_ignore)
            .purge_empty(self.purge_empty);

            purger.purge();
        }

        if self.file_flags.contains(FileFlags::Scripts) {
            if engine_type.is_new() {
                let plugins_file_path = source_path
                    .as_ref()
                    .parent()
                    .unwrap_log()
                    .join("js/plugins.js");

                let purger = PluginPurger::new(
                    plugins_file_path.as_path(),
                    translation_path.as_ref(),
                )
                .ignore_map(&mut ignore_map)
                .stat_vec(&mut stat_vec)
                .romanize(self.romanize)
                .logging(self.logging)
                .stat(self.stat)
                .leave_filled(self.leave_filled)
                .create_ignore(self.create_ignore)
                .purge_empty(self.purge_empty);

                purger.purge();
            } else {
                let scripts_file_path = source_path.as_ref().join(format!(
                    "Scripts.{}",
                    get_engine_extension(engine_type)
                ));

                let purger = ScriptPurger::new(
                    scripts_file_path.as_path(),
                    translation_path.as_ref(),
                )
                .ignore_map(&mut ignore_map)
                .stat_vec(&mut stat_vec)
                .romanize(self.romanize)
                .logging(self.logging)
                .stat(self.stat)
                .leave_filled(self.leave_filled)
                .create_ignore(self.create_ignore)
                .purge_empty(self.purge_empty);

                purger.purge();
            }
        }

        if self.stat {
            std::fs::write(
                translation_path.as_ref().join("stat.txt"),
                String::from_iter(stat_vec.into_iter().map(
                    |(source, translation)| {
                        format!("{source}{LINES_SEPARATOR}{translation}\n")
                    },
                )),
            )
            .unwrap_log();
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

            std::fs::write(
                translation_path.as_ref().join(RVPACKER_IGNORE_FILE),
                contents,
            )
            .unwrap_log();
        }
    }
}

impl Default for Purger {
    fn default() -> Self {
        Self::new()
    }
}

/// A builder struct for `Purger`.
///
/// The `Purger` struct, essentially, should receive the same options, as `Reader`, to ensure proper purging.
///
/// # Fields
/// - `file_flags`: Indicates which RPG Maker files should be processed. Use `set_flags` to set them.
/// - `game_type`: Specifies which RPG Maker game type the data is from. Use `set_game_type` to set it.
/// - `romanize`: Enables or disables romanization of parsed text. For more info, and to set it, see `set_romanize`.
/// - `logging`: If enabled, logs operations and progress. Use `set_logging`
/// - `trim`: Removes leading and trailing whitespace from extracted strings. Use `set_trim` to set it.
/// - `stat`: If enabled, doesn't actually purge any lines, and outputs `stat.txt` file with purge statistics
///   to translation directory.
/// - `leave_filled`: If enabled, leaves the lines with filled translation, even if they don't exist in game.
/// - `create_ignore`: If enabled, creates `.rvpacker-ignore` file from purged lines, that can be used in
///   `Reader` struct when its `ignore` option is set.
/// - `purge_empty`: If enabled, purges only the lines with empty translation.
///
/// # Example
/// ```
/// use rvpacker_txt_rs_lib::{PurgerBuilder, FileFlags, GameType};
///
/// let mut purger = PurgerBuilder::new().with_flags(FileFlags::Map | FileFlags::Other).build();
/// ```
pub struct PurgerBuilder {
    purger: Purger,
}

impl PurgerBuilder {
    /// Creates a new `Purger` instance with default values.
    ///
    /// By default, all four file flags are set (all files will be purged),
    /// and all other options are disabled.
    ///
    /// # Example
    /// ```no_run
    /// let mut purger = PurgerBuilder::new().build();
    /// ```
    pub fn new() -> Self {
        Self {
            purger: Purger::new(),
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
    /// let purger = PurgerBuilder::new().with_flags(FileFlags::Map | FileFlags::Other).build();
    /// ```
    pub fn with_flags(mut self, file_flags: FileFlags) -> Self {
        self.purger.file_flags = file_flags;
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
    /// let purger = PurgerBuilder::new().game_type(FileFlags::Map | FileFlags::Other).build();
    /// ```
    pub fn game_type(mut self, game_type: GameType) -> Self {
        self.purger.game_type = game_type;
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
    /// let purger = PurgerBuilder::new().romanize(true).build();
    /// ```
    pub fn romanize(mut self, enabled: bool) -> Self {
        self.purger.romanize = enabled;
        self
    }

    /// Sets whether to log file processing.
    ///
    /// # Example
    /// ```no_run
    /// let purger = PurgerBuilder::new().logging(true).build();
    /// ```
    pub fn logging(mut self, enabled: bool) -> Self {
        self.purger.logging = enabled;
        self
    }

    /// This function must have the same value that was passed to it in `Reader` struct.
    ///
    /// Sets whether to trim whitespace from strings.
    ///
    /// # Example
    /// ```no_run
    /// let purger = PurgerBuilder::new().trim(true).build();
    /// ```
    pub fn trim(mut self, enabled: bool) -> Self {
        self.purger.trim = enabled;
        self
    }

    /// If set, `purge` outputs purge statistics, instead of actually purging anything.
    ///
    /// # Example
    /// ```no_run
    /// let purger = PurgerBuilder::new().stat(true).build();
    /// ```
    pub fn stat(mut self, enabled: bool) -> Self {
        self.purger.stat = enabled;
        self
    }

    /// Sets whether to leave lines with filled translation untouched.
    ///
    /// # Example
    /// ```no_run
    /// let purger = PurgerBuilder::new().leave_filled(true).build();
    /// ```
    pub fn leave_filled(mut self, enabled: bool) -> Self {
        self.purger.leave_filled = enabled;
        self
    }

    /// Sets whether to create `.rvpacker-ignore` file from purged lines.
    ///
    /// # Example
    /// ```no_run
    /// let purger = PurgerBuilder::new().create_ignore(true).build();
    /// ```
    pub fn create_ignore(mut self, enabled: bool) -> Self {
        self.purger.create_ignore = enabled;
        self
    }

    /// Sets whether to purge only lines with empty translation.
    ///
    /// # Example
    /// ```no_run
    /// let purger = PurgerBuilder::new().purge_empty(true).build();
    /// ```
    pub fn purge_empty(mut self, enabled: bool) -> Self {
        self.purger.purge_empty = enabled;
        self
    }

    /// Builds and returns the `Purger`.
    pub fn build(self) -> Purger {
        self.purger
    }
}

impl Default for PurgerBuilder {
    fn default() -> Self {
        Self::new()
    }
}
