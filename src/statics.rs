use phf::phf_set;
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

    pub static INVALID_MULTILINE_VARIABLE_RE: Lazy<Regex> =
        Lazy::new(|| unsafe { Regex::new(r"^#? ?<.*>.?$|^[a-z]\d$").unwrap_unchecked() });
    pub static INVALID_VARIABLE_RE: Lazy<Regex> =
        Lazy::new(|| unsafe { Regex::new(r"^[+-]?$|^///|---|restrict eval").unwrap_unchecked() });
    pub static PLUGINS_REGEXPS: Lazy<[Regex; 11]> = Lazy::new(|| unsafe {
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

pub const HASHER: Xxh3DefaultBuilder = Xxh3DefaultBuilder::new();
