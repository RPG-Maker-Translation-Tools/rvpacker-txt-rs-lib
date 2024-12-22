pub mod localization {
    // read messages
    pub static FILES_ARE_NOT_PARSED_MSG: &str =
        "Files aren't already parsed. Continuing as if --mode append argument was omitted.";
    pub static PARSED_FILE_MSG: &str = "Parsed file";
    pub static FILE_ALREADY_EXISTS_MSG: &str = "file already exists. If you want to forcefully re-read files or append new text, use --mode force or --mode append arguments.";

    // write messages
    pub static WROTE_FILE_MSG: &str = "Wrote file";
    pub static COULD_NOT_SPLIT_LINE_MSG: &str = "Couldn't split line to original and translated part.\nThe line won't be written to the output file.";
    pub static AT_POSITION_MSG: &str = "At position:";
}

pub mod regexes {
    use once_cell::sync::Lazy;
    use regex::Regex;

    pub static STRING_IS_ONLY_SYMBOLS_RE: Lazy<Regex> = Lazy::new(|| unsafe {
        Regex::new(r#"^[.()+\-:;\[\]^~%&!№$@`*\/→×？?ｘ％▼|♥♪！：〜『』「」〽。…‥＝゠、，【】［］｛｝（）〔〕｟｠〘〙〈〉《》・\\#<>=_ー※▶ⅠⅰⅡⅱⅢⅲⅣⅳⅤⅴⅥⅵⅦⅶⅧⅷⅨⅸⅩⅹⅪⅺⅫⅻⅬⅼⅭⅽⅮⅾⅯⅿ\s0-9]+$"#).unwrap_unchecked()
    });
    pub static ENDS_WITH_IF_RE: Lazy<Regex> =
        Lazy::new(|| unsafe { Regex::new(r" if\(.*\)$").unwrap_unchecked() });
    pub static LISA_PREFIX_RE: Lazy<Regex> =
        Lazy::new(|| unsafe { Regex::new(r"^(\\et\[[0-9]+\]|\\nbt)").unwrap_unchecked() });
    pub static INVALID_MULTILINE_VARIABLE_RE: Lazy<Regex> =
        Lazy::new(|| unsafe { Regex::new(r"^#? ?<.*>.?$|^[a-z][0-9]$").unwrap_unchecked() });
    pub static INVALID_VARIABLE_RE: Lazy<Regex> = Lazy::new(|| unsafe {
        Regex::new(r"^[+-]?[0-9]+$|^///|---|restrict eval").unwrap_unchecked()
    });
}

pub static NEW_LINE: &str = r"\#";
pub static LINES_SEPARATOR: &str = "<#>";
