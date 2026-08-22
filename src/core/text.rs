//! Text handling shared by every file kind, and the `.txt` format's contract.
//!
//! [`CustomReplace`] and [`split_translation_line`] define how a translation line
//! is written and read back, so anything else parsing these files - a GUI, a CLI -
//! can rely on the same functions rather than reimplementing them.

use crate::{
    constants::{ID_COMMENT, NEW_LINE, SEPARATOR, SYMBOLS},
    types::TranslationEntry,
};
use regex::Regex;
use std::{borrow::Cow, cell::LazyCell};

thread_local! {
    static LINE_BREAKS_RE: LazyCell<Regex> = LazyCell::new(|| unsafe {
        Regex::new(r"\r|\n|\r\n").unwrap_unchecked()
    });
    static NEW_LINE_RE: LazyCell<Regex> = LazyCell::new(|| unsafe {
        Regex::new(r"\\#").unwrap_unchecked()
    });
}

/// Converts between RPG Maker's line breaks and the library's.
///
/// Implemented for [`str`], so a consumer parsing the generated `.txt` files can
/// round-trip a line the same way this crate does.
pub trait CustomReplace {
    /// Normalizes RPG Maker line break symbols (`\n`, `\r`, `\r\n`) to the format that the library uses (`\#`).
    fn normalize(&self) -> Cow<'_, str>;

    /// Denormalizes library line break symbols to the format that RPG Maker uses (`\n`).
    fn denormalize(&self) -> Cow<'_, str>;
}

impl CustomReplace for str {
    fn normalize(&self) -> Cow<'_, str> {
        LINE_BREAKS_RE.with(|re| re.replace_all(self, NEW_LINE))
    }

    fn denormalize(&self) -> Cow<'_, str> {
        NEW_LINE_RE.with(|re| re.replace_all(self, "\n"))
    }
}

/// Latinize CJK/Unicode (mainly punctuation) characters to their Latin equivalents.
///
/// # Parameters
///
/// - `string` - String to latinize.
///
/// # Returns
///
/// - [`Cow<str>`] - as owned if replacements occurred, as borrowed otherwise.
///
pub fn latinize_string(string: &str) -> Cow<'_, str> {
    let mut result: Option<String> = None;

    for (i, char) in string.chars().enumerate() {
        let replacement = match char {
            '。' => ".",
            '、' | '，' => ",",
            '・' | '※' => "·",
            '゠' => "–",
            '＝' | 'ー' => "—",
            '「' | '」' | '〈' | '〉' => "'",
            '『' | '』' | '《' | '》' => "\"",
            '（' | '〔' | '｟' | '〘' => "(",
            '）' | '〕' | '｠' | '〙' => ")",
            '｛' => "{",
            '｝' => "}",
            '［' | '【' | '〖' | '〚' => "[",
            '］' | '】' | '〗' | '〛' => "]",
            '〜' => "~",
            '？' => "?",
            '！' => "!",
            '：' => ":",
            '…' | '‥' => "...",
            '　' => " ",
            'Ⅰ' => "I",
            'ⅰ' => "i",
            'Ⅱ' => "II",
            'ⅱ' => "ii",
            'Ⅲ' => "III",
            'ⅲ' => "iii",
            'Ⅳ' => "IV",
            'ⅳ' => "iv",
            'Ⅴ' => "V",
            'ⅴ' => "v",
            'Ⅵ' => "VI",
            'ⅵ' => "vi",
            'Ⅶ' => "VII",
            'ⅶ' => "vii",
            'Ⅷ' => "VIII",
            'ⅷ' => "viii",
            'Ⅸ' => "IX",
            'ⅸ' => "ix",
            'Ⅹ' => "X",
            'ⅹ' => "x",
            'Ⅺ' => "XI",
            'ⅺ' => "xi",
            'Ⅻ' => "XII",
            'ⅻ' => "xii",
            'Ⅼ' => "L",
            'ⅼ' => "l",
            'Ⅽ' => "C",
            'ⅽ' => "c",
            'Ⅾ' => "D",
            'ⅾ' => "d",
            'Ⅿ' => "M",
            'ⅿ' => "m",
            _ => {
                if let Some(s) = &mut result {
                    s.push(char);
                }
                continue;
            }
        };

        if result.is_none() {
            let mut s = String::with_capacity(string.len());
            s.push_str(&string[..string.char_indices().nth(i).unwrap().0]);
            result = Some(s);
        }

        result.as_mut().unwrap().push_str(replacement);
    }

    match result {
        Some(s) => Cow::Owned(s),
        None => Cow::Borrowed(string),
    }
}

/// Outcome of splitting one `source<#>translation` line from a translation file.
pub enum TranslationLine<'a> {
    /// No separator present; the caller should warn and skip the line.
    Malformed,
    /// Empty translation on a write pass - the entry is unused, so skip it.
    Untranslated,
    Split {
        source: Cow<'a, str>,
        translation: Cow<'a, str>,
    },
}

/// Splits one translation-file line into its source and translation halves.
///
/// The translation is the last non-empty field after the first separator, so files
/// carrying several translation columns still resolve to the rightmost filled one.
///
/// Borrows throughout; allocates only where `write` forces denormalization.
pub fn split_translation_line(
    line: &str,
    trim: bool,
    write: bool,
) -> TranslationLine<'_> {
    let Some((source, rest)) = line.split_once(SEPARATOR) else {
        return TranslationLine::Malformed;
    };

    let translation = rest
        .rsplit(SEPARATOR)
        .find(|field| !field.is_empty())
        .unwrap_or_default();

    let (source, translation) = if trim {
        (source.trim(), translation.trim())
    } else {
        (source, translation)
    };

    if write {
        // Lines with no translation are unused on write.
        if translation.is_empty() {
            return TranslationLine::Untranslated;
        }

        TranslationLine::Split {
            source: source.denormalize(),
            translation: translation.denormalize(),
        }
    } else {
        TranslationLine::Split {
            source: Cow::Borrowed(source),
            translation: Cow::Borrowed(translation),
        }
    }
}

pub(crate) fn push_metadata(
    output: &mut Vec<u8>,
    id: u16,
    metadata: &[String],
) {
    output.extend_from_slice(ID_COMMENT.as_bytes());
    output.extend_from_slice(SEPARATOR.as_bytes());
    output.extend_from_slice(id.to_string().as_bytes());
    output.push(b'\n');

    for comment in metadata.iter().filter(|c| !c.is_empty()) {
        output.extend_from_slice(comment.as_bytes());
        output.push(b'\n');
    }
}

pub(crate) fn push_entries(
    output: &mut Vec<u8>,
    source: &str,
    translation: &TranslationEntry,
) {
    for comment in translation.comments.iter().filter(|c| !c.is_empty()) {
        output.extend_from_slice(comment.as_bytes());
        output.push(b'\n');
    }

    if !source.is_empty() {
        output.extend_from_slice(source.as_bytes());
        output.extend_from_slice(SEPARATOR.as_bytes());
    }

    if !translation.is_empty() {
        output.extend_from_slice(translation.as_bytes());
    }

    if !source.is_empty() || !translation.is_empty() {
        output.push(b'\n');
    }
}

/// Whether every character is punctuation, whitespace or a numeral form, which
/// makes the string untranslatable.
///
/// Quotes and digits are *not* counted as symbols: a translator may want to swap
/// quotes for the target locale's, and a bare number can still be meaningful.
#[must_use]
pub fn string_is_only_symbols(string: &str) -> bool {
    !string.chars().any(|c| !SYMBOLS.contains(&c))
}

/// Byte index of a trailing `if(...)` condition, or [`None`] if there is none.
///
/// Older engines append a condition to some event parameters; it is code rather
/// than text, so it is cut before translation and restored on write.
// TODO(v15): Check when starts with if
//* This is breaking
#[must_use]
pub fn ends_with_if_index(string: &str) -> Option<usize> {
    if !string.ends_with(')') {
        return None;
    }

    let mut stage: u8 = 0;
    let char_indices = string.char_indices().rev().skip(1);

    for (i, char) in char_indices {
        match stage {
            0 => {
                if char == '(' {
                    stage = 1;
                }
            }
            1 => {
                if char == 'f' {
                    stage = 2;
                } else {
                    return None;
                }
            }
            2 => {
                if char == 'i' {
                    return Some(i);
                }
            }
            _ => unreachable!(),
        }
    }

    None
}
