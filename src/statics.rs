use xxhash_rust::xxh3::Xxh3DefaultBuilder;

pub mod localization {
    // read messages
    pub const FILES_ARE_NOT_PARSED_MSG: &str = "Files aren't already parsed. Skipping processing.";
    pub const PARSED_FILE_MSG: &str = "Parsed file";
    pub const FILE_ALREADY_EXISTS_MSG: &str = "file already exists. If you want to forcefully re-read files or append \
                                               new text, use --mode force or --mode append arguments.";

    // write messages
    pub const WROTE_FILE_MSG: &str = "Wrote file";
    pub const COULD_NOT_SPLIT_LINE_MSG: &str =
        "Couldn't split line to original and translated part.\nThe line won't be written to the output file.";
    pub const AT_POSITION_MSG: &str = "At position:";
    pub const IN_FILE_MSG: &str = "In file:";
}

pub mod regexes {
    use once_cell::sync::Lazy;
    use regex::Regex;

    pub static STRING_IS_ONLY_SYMBOLS_RE: Lazy<Regex> = Lazy::new(|| unsafe {
        Regex::new(r#"^[,.()+\-:;\[\]^~%&!№$@`*\/→×？?ｘ％▼|♥♪！：〜『』「」〽。…‥＝゠、，【】［］｛｝（）〔〕｟｠〘〙〈〉《》・\\#<>=_ー※▶ⅠⅰⅡⅱⅢⅲⅣⅳⅤⅴⅥⅵⅦⅶⅧⅷⅨⅸⅩⅹⅪⅺⅫⅻⅬⅼⅭⅽⅮⅾⅯⅿ\s0-9]+$"#).unwrap_unchecked()
    });
    pub static ENDS_WITH_IF_RE: Lazy<Regex> = Lazy::new(|| unsafe { Regex::new(r" if\(.*\)$").unwrap_unchecked() });
    pub static LISA_PREFIX_RE: Lazy<Regex> =
        Lazy::new(|| unsafe { Regex::new(r"^(\\et\[[0-9]+\]|\\nbt)").unwrap_unchecked() });
    pub static INVALID_MULTILINE_VARIABLE_RE: Lazy<Regex> =
        Lazy::new(|| unsafe { Regex::new(r"^#? ?<.*>.?$|^[a-z][0-9]$").unwrap_unchecked() });
    pub static INVALID_VARIABLE_RE: Lazy<Regex> =
        Lazy::new(|| unsafe { Regex::new(r"^[+-]?[0-9]+$|^///|---|restrict eval").unwrap_unchecked() });
}

/// 401 - Dialogue line.
///
/// 101 - Start of the dialogue line. (**XP ENGINE ONLY!**)
///
/// 102 - Dialogue choices array.
///
/// 402 - One of the dialogue choices from the array. (**WRITE ONLY!**)
///
/// 405 - Credits lines. (**probably NEWER ENGINES ONLY!**)
///
/// 356 - System line, special text. (that one needs clarification)
///
/// 655 - Line displayed in shop - probably from an external script. (**OLDER ENGINES ONLY!**)
///
/// 324, 320 - Some used in-game line. (**probably NEWER ENGINES ONLY!**)
pub const ALLOWED_CODES: [u16; 9] = [101, 102, 320, 324, 356, 401, 402, 405, 655];
pub const NEW_LINE: &str = r"\#";
pub const LINES_SEPARATOR: &str = "<#>";

pub static ENCODINGS: [&encoding_rs::Encoding; 5] = [
    encoding_rs::UTF_8,
    encoding_rs::WINDOWS_1252,
    encoding_rs::WINDOWS_1251,
    encoding_rs::SHIFT_JIS,
    encoding_rs::GB18030,
];

pub const HASHER: Xxh3DefaultBuilder = Xxh3DefaultBuilder::new();
