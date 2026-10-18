use phf::phf_set;

pub(crate) const COULD_NOT_SPLIT_LINE_MSG: &str = "Couldn't split line to source and translation parts.";
pub(crate) const AT_POSITION_MSG: &str = "At position";
pub(crate) const IN_FILE_MSG: &str = "In file";

/// Quotes aren't included in this set because user might want to change them.
/// For example, in Europe, guillemets are used instead of default quotes.
pub(crate) const SYMBOLS: phf::Set<char> = phf_set! {
    ',', '.', '(', ')', '+', '-', ':', ';', '[', ']', '^', '~', '%', '&', '!', '№', '$', '@', '`', '*', '/', '→', '×', '？', '?', 'ｘ', '％', '▼', '|', '♥', '♪', '！', '：', '〜', '『', '』', '「', '」', '〽', '。', '…', '‥', '＝', '゠', '、', '，', '【', '】', '［', '］', '｛', '｝', '（', '）', '〔', '〕', '｟', '｠', '〘', '〙', '〈', '〉', '《', '》', '・', '\\', '#', '<', '>', '=', '_', 'ー', '※', '▶', 'Ⅰ', 'ⅰ', 'Ⅱ', 'ⅱ', 'Ⅲ', 'ⅲ', 'Ⅳ', 'ⅳ', 'Ⅴ', 'ⅴ', 'Ⅵ', 'ⅵ', 'Ⅶ', 'ⅶ', 'Ⅷ', 'ⅷ', 'Ⅸ', 'ⅸ', 'Ⅹ', 'ⅹ', 'Ⅺ', 'ⅺ', 'Ⅻ', 'ⅻ', 'Ⅼ', 'ⅼ', 'Ⅽ', 'ⅽ', 'Ⅾ', 'ⅾ', 'Ⅿ', 'ⅿ', ' ', '\t', '\r', '\n'
};

/// The comment prefix a `.txt` file starts its comment lines with, unless [`set_comment_prefix`](crate::set_comment_prefix) was called.
pub const DEFAULT_COMMENT_PREFIX: &str = "<!>";
/// The marker that stands in for a line break in `.txt` files, unless [`set_line_break`](crate::set_line_break) was called.
pub const DEFAULT_LINE_BREAK: &str = r"\#";
/// The separator between a source and its translation, unless [`set_line_separator`](crate::set_line_separator) was called.
pub const DEFAULT_LINE_SEPARATOR: &str = "<#>";

/// The name of the ignore file that [`BaseFlags::Ignore`](crate::BaseFlags::Ignore) reads and [`BaseFlags::CreateIgnore`](crate::BaseFlags::CreateIgnore) writes.
pub const RVPACKER_IGNORE_FILE: &str = ".rvpacker-ignore";
/// Name of a settings file that stores the separators, prefixes and break marker beside a project.
/// The library never reads or writes it.
pub const RVPACKER_METADATA_FILE: &str = ".rvpacker-metadata";
