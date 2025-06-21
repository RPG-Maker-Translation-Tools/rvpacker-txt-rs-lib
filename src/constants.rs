use const_format::formatcp;
use phf::phf_set;

pub mod localization {
    pub const FILES_ARE_NOT_PARSED_MSG: &str =
        "Files aren't already parsed. Skipping processing.";
    pub const PARSED_FILE_MSG: &str = "Parsed file";
    pub const FILE_ALREADY_EXISTS_MSG: &str = "file already exists. If you want to forcefully re-read files or append \
                                               new text, use --mode force or --mode append arguments.";

    pub const JSON_ALREADY_EXIST: &str = "Directory containing json files already exists. If you want to forcefully regenerate .json files, use --mode force.";
    pub const CANNOT_GENERATE_JSON: &str =
        "Can't generate json for MV/MZ engines, they're already json.";

    pub const PURGED_FILE_MSG: &str = "Purged unused lines from file";
    pub const APPEND_MODE_IS_NOT_SUPPORTED: &str =
        "Append mode is not supported in `generate_json` function.";

    pub const WROTE_FILE_MSG: &str = "Wrote file";
    pub const COULD_NOT_SPLIT_LINE_MSG: &str =
        "Couldn't split line to source and translation parts.\nThe line won't be written to the output file.";
    pub const AT_POSITION_MSG: &str = "At position:";
    pub const IN_FILE_MSG: &str = "In file:";
}

pub mod regexes {
    use regex::Regex;
    use std::sync::LazyLock;

    pub static IS_INVALID_MULTILINE_VARIABLE_RE: LazyLock<Regex> =
        LazyLock::new(|| unsafe {
            Regex::new(r"^#? ?<.*>.?$|^[a-z]\d$").unwrap_unchecked()
        });
    pub static IS_INVALID_VARIABLE_RE: LazyLock<Regex> =
        LazyLock::new(|| unsafe {
            Regex::new(r"^[+-]?$|^///|---|restrict eval").unwrap_unchecked()
        });
    pub static PLUGINS_REGEXPS: LazyLock<[Regex; 11]> = LazyLock::new(
        || unsafe {
            [
                Regex::new(r"^(name|description|Window Width|Window Height|ATTENTION!!!|Shown Elements|Width|Outline Color|Command Alignment|Command Position|Command Rows|Chinese Font|Korean Font|Default Font|Text Align|Scenes To Draw|displacementImage|Turn Alignment|Buff Formula|Counter Alignment|Default Width|Face Indent|Fast Forward Key|Font Name|Font Name CH|Font Name KR|Name Box Padding|Name Box Added Text|Critical Rate Formula|Critical Multplier Formula|Flat Critical Formula|Default SE|---List---|Button Events List|Kill Switch|Ex Turn Image|Ex Turn Name Color|Non Ex Turn Name Color|Option menu entry|Add to options|Default Ambient Light|Reset Lights|Gab Font Name|Escape Ratio|Translated Format|Default Sound|Action Speed|Default System|Untranslated Format|Default Format|Victory Screen Level Sound|Warning Side Battle UI|Weapon Swap Text Hit|Weapon Swap Text Critical|Weapon Swap Command|Weapon Swap Text Evasion|alwaysDash|renderingMode|Attributes Command|Attributes Column 1|Attributes Column 2|Attributes Column 3|Warning OTB|</span> Minimum Damage</span></td>|Present Settings)$").unwrap_unchecked(),
                Regex::new(r"^Folder.*\w$").unwrap_unchecked(),
                Regex::new(r"[XY]$").unwrap_unchecked(),
                Regex::new(r"BGM").unwrap_unchecked(),
                Regex::new(r"Label").unwrap_unchecked(),
                Regex::new(r"^Custom \w").unwrap_unchecked(),
                Regex::new(r"^outlineColor").unwrap_unchecked(),
                Regex::new(r"^(Menu|Item|Skill|Equip|Status|Save|Options|End).*(Background|Motion)$").unwrap_unchecked(),
                Regex::new(r"^Menu \w").unwrap_unchecked(),
                Regex::new(r"^(MHP|MMP|ATK|DEF|MAT|MDF|AGI|LUK).*(Formula|Maximum|Minimum|Effect|Color)$").unwrap_unchecked(),
                Regex::new(r"^Damage\w*$").unwrap_unchecked(),
            ]
        },
    );

    pub static IS_ONLY_SYMBOLS_RE: LazyLock<Regex> = LazyLock::new(|| unsafe {
        Regex::new(r#"^[,.()+\-:;\[\]^~%&!№$@`*\/→×？?ｘ％▼|♥♪！：〜『』「」〽。…‥＝゠、，【】［］｛｝（）〔〕｟｠〘〙〈〉《》・\\#<>=_ー※▶ⅠⅰⅡⅱⅢⅲⅣⅳⅤⅴⅥⅵⅦⅶⅧⅷⅨⅸⅩⅹⅪⅺⅫⅻⅬⅼⅭⅽⅮⅾⅯⅿ\s\d"']+$"#).unwrap_unchecked()
    });
}

pub const NEW_LINE: &str = r"\#";
pub const LINES_SEPARATOR: &str = "<#>";

pub const SYMBOLS: phf::Set<char> = phf_set! {
    ',', '.', '(', ')', '+', '-', ':', ';', '[', ']', '^', '~', '%', '&', '!', '№', '$', '@', '`', '*', '/', '→', '×', '？', '?', 'ｘ', '％', '▼', '|', '♥', '♪', '！', '：', '〜', '『', '』', '「', '」', '〽', '。', '…', '‥', '＝', '゠', '、', '，', '【', '】', '［', '］', '｛', '｝', '（', '）', '〔', '〕', '｟', '｠', '〘', '〙', '〈', '〉', '《', '》', '・', '\\', '#', '<', '>', '=', '_', 'ー', '※', '▶', 'Ⅰ', 'ⅰ', 'Ⅱ', 'ⅱ', 'Ⅲ', 'ⅲ', 'Ⅳ', 'ⅳ', 'Ⅴ', 'ⅴ', 'Ⅵ', 'ⅵ', 'Ⅶ', 'ⅶ', 'Ⅷ', 'ⅷ', 'Ⅸ', 'ⅸ', 'Ⅹ', 'ⅹ', 'Ⅺ', 'ⅺ', 'Ⅻ', 'ⅻ', 'Ⅼ', 'ⅼ', 'Ⅽ', 'ⅽ', 'Ⅾ', 'ⅾ', 'Ⅿ', 'ⅿ', ' ', '\t', '\r', '\n'
};

pub static ENCODINGS: [&encoding_rs::Encoding; 5] = [
    encoding_rs::UTF_8,
    encoding_rs::WINDOWS_1252,
    encoding_rs::WINDOWS_1251,
    encoding_rs::SHIFT_JIS,
    encoding_rs::GB18030,
];

pub const COMMENT_PREFIX: &str = "<!-- ";
pub const COMMENT_SUFFIX: &str = " -->";
pub const MAP_NUMBER_COMMENT: &str =
    formatcp!("{COMMENT_PREFIX}Map{COMMENT_SUFFIX}");
pub const ORDER_COMMENT: &str =
    formatcp!("{COMMENT_PREFIX}Order{COMMENT_SUFFIX}");
pub const FILE_ENTRY_PREFIX: &str = formatcp!("{COMMENT_PREFIX}File: ");
pub const DISPLAY_NAME_COMMENT_PREFIX: &str =
    formatcp!("{COMMENT_PREFIX}In-game Displayed Name: ");
pub const MAP_NAME_COMMENT_PREFIX: &str =
    formatcp!("{COMMENT_PREFIX}Map Name: ");
pub const EVENT_NAME_COMMENT_PREFIX: &str =
    formatcp!("{COMMENT_PREFIX}Event Name: ");
pub const RVPACKER_IGNORE_FILE: &str = ".rvpacker-ignore";
pub const RVPACKER_METADATA_FILE: &str = ".rvpacker-metadata";
pub const PLUGINS_ENTRY_STRING: &str =
    formatcp!("{FILE_ENTRY_PREFIX}plugins{COMMENT_SUFFIX}");
pub const SCRIPTS_ENTRY_STRING: &str =
    formatcp!("{FILE_ENTRY_PREFIX}scripts{COMMENT_SUFFIX}");
pub const SYSTEM_ENTRY_STRING: &str =
    formatcp!("{FILE_ENTRY_PREFIX}system{COMMENT_SUFFIX}");
pub const SCRIPT_COMMENT_PREFIX: &str = formatcp!("{COMMENT_PREFIX}Script: ");
pub const PLUGIN_COMMENT_PREFIX: &str = formatcp!("{COMMENT_PREFIX}Plugin: ");
pub const INSTANCE_VAR_PREFIX: Option<&str> = Some("");
