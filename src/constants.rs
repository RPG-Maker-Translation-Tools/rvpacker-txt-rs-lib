use phf::phf_set;

pub(crate) const COULD_NOT_SPLIT_LINE_MSG: &str = "Couldn't split line to source and translation parts.";
pub(crate) const AT_POSITION_MSG: &str = "At position";
pub(crate) const IN_FILE_MSG: &str = "In file";

/// Quotes aren't included in this set because user might want to change them.
/// For example, in Europe, guillemets are used instead of default quotes.
pub(crate) const SYMBOLS: phf::Set<char> = phf_set! {
    ',', '.', '(', ')', '+', '-', ':', ';', '[', ']', '^', '~', '%', '&', '!', '№', '$', '@', '`', '*', '/', '→', '×', '？', '?', 'ｘ', '％', '▼', '|', '♥', '♪', '！', '：', '〜', '『', '』', '「', '」', '〽', '。', '…', '‥', '＝', '゠', '、', '，', '【', '】', '［', '］', '｛', '｝', '（', '）', '〔', '〕', '｟', '｠', '〘', '〙', '〈', '〉', '《', '》', '・', '\\', '#', '<', '>', '=', '_', 'ー', '※', '▶', 'Ⅰ', 'ⅰ', 'Ⅱ', 'ⅱ', 'Ⅲ', 'ⅲ', 'Ⅳ', 'ⅳ', 'Ⅴ', 'ⅴ', 'Ⅵ', 'ⅵ', 'Ⅶ', 'ⅶ', 'Ⅷ', 'ⅷ', 'Ⅸ', 'ⅸ', 'Ⅹ', 'ⅹ', 'Ⅺ', 'ⅺ', 'Ⅻ', 'ⅻ', 'Ⅼ', 'ⅼ', 'Ⅽ', 'ⅽ', 'Ⅾ', 'ⅾ', 'Ⅿ', 'ⅿ', ' ', '\t', '\r', '\n'
};

pub const DEFAULT_COMMENT_PREFIX: &str = "<!>";
pub const DEFAULT_LINE_BREAK: &str = r"\#";
pub const DEFAULT_LINE_SEPARATOR: &str = "<#>";

pub const RVPACKER_IGNORE_FILE: &str = ".rvpacker-ignore";
pub const RVPACKER_METADATA_FILE: &str = ".rvpacker-metadata";
