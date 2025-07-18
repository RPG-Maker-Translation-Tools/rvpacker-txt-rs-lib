use const_format::formatcp;
use phf::phf_set;

pub(crate) mod localization {
    pub const PARSED_FILE_MSG: &str = "Parsed file.";
    pub const PURGED_FILE_MSG: &str = "Purged file.";
    pub const WROTE_FILE_MSG: &str = "Wrote file.";

    pub const FILE_ALREADY_EXISTS_MSG: &str = "file already exists. If you want to forcefully re-read files or append \
                                                new text, set `read_mode` to `Force` or `Append`.";

    pub const COULD_NOT_SPLIT_LINE_MSG: &str =
        "Couldn't split line to source and translation parts.";
    pub const AT_POSITION_MSG: &str = "At position";
    pub const IN_FILE_MSG: &str = "In file";
}

/// Quotes aren't included in this set because user might want to change them.
/// For example, in Europe, guillemets are used instead of default quotes.
pub(crate) const SYMBOLS: phf::Set<char> = phf_set! {
    ',', '.', '(', ')', '+', '-', ':', ';', '[', ']', '^', '~', '%', '&', '!', '№', '$', '@', '`', '*', '/', '→', '×', '？', '?', 'ｘ', '％', '▼', '|', '♥', '♪', '！', '：', '〜', '『', '』', '「', '」', '〽', '。', '…', '‥', '＝', '゠', '、', '，', '【', '】', '［', '］', '｛', '｝', '（', '）', '〔', '〕', '｟', '｠', '〘', '〙', '〈', '〉', '《', '》', '・', '\\', '#', '<', '>', '=', '_', 'ー', '※', '▶', 'Ⅰ', 'ⅰ', 'Ⅱ', 'ⅱ', 'Ⅲ', 'ⅲ', 'Ⅳ', 'ⅳ', 'Ⅴ', 'ⅴ', 'Ⅵ', 'ⅵ', 'Ⅶ', 'ⅶ', 'Ⅷ', 'ⅷ', 'Ⅸ', 'ⅸ', 'Ⅹ', 'ⅹ', 'Ⅺ', 'ⅺ', 'Ⅻ', 'ⅻ', 'Ⅼ', 'ⅼ', 'Ⅽ', 'ⅽ', 'Ⅾ', 'ⅾ', 'Ⅿ', 'ⅿ', ' ', '\t', '\r', '\n'
};

pub const NEW_LINE: &str = r"\#";
pub const SEPARATOR: &str = "<#>";

pub const COMMENT_PREFIX: &str = "<!-- ";
pub const COMMENT_SUFFIX: &str = " -->";
pub const MAP_ID_COMMENT: &str =
    formatcp!("{COMMENT_PREFIX}Map{COMMENT_SUFFIX}");
pub const MAP_ORDER_COMMENT: &str =
    formatcp!("{COMMENT_PREFIX}Order{COMMENT_SUFFIX}");
pub const MAP_NAME_COMMENT: &str =
    formatcp!("{COMMENT_PREFIX}Map Name{COMMENT_SUFFIX}");
pub const MAP_DISPLAY_NAME_COMMENT_PREFIX: &str =
    formatcp!("{COMMENT_PREFIX}In-game Displayed Name: ");
pub const EVENT_ID_COMMENT: &str =
    formatcp!("{COMMENT_PREFIX}Event ID{COMMENT_SUFFIX}");
pub const EVENT_NAME_COMMENT: &str =
    formatcp!("{COMMENT_PREFIX}Event Name{COMMENT_SUFFIX}");
pub const SYSTEM_ENTRY_COMMENT: &str =
    formatcp!("{COMMENT_PREFIX}System Entry{COMMENT_SUFFIX}");
pub const SCRIPT_ID_COMMENT: &str =
    formatcp!("{COMMENT_PREFIX}Script ID{COMMENT_SUFFIX}");
pub const SCRIPT_NAME_COMMENT: &str =
    formatcp!("{COMMENT_PREFIX}Script Name{COMMENT_SUFFIX}");
pub const PLUGIN_ID_COMMENT: &str =
    formatcp!("{COMMENT_PREFIX}Plugin ID{COMMENT_SUFFIX}");
pub const PLUGIN_NAME_COMMENT: &str =
    formatcp!("{COMMENT_PREFIX}Plugin Name{COMMENT_SUFFIX}");
pub const IGNORE_ENTRY_COMMENT: &str =
    formatcp!("{COMMENT_PREFIX}Ignore Entry{COMMENT_SUFFIX}");
pub const ADDITIONAL_HASHMAP_LABEL: &str = "Additional";

pub const RVPACKER_IGNORE_FILE: &str = ".rvpacker-ignore";
pub const RVPACKER_METADATA_FILE: &str = ".rvpacker-metadata";

pub const INSTANCE_VAR_PREFIX: Option<&str> = Some("");
