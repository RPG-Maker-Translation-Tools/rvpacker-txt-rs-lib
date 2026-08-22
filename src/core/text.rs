use crate::{
    constants::{ID_COMMENT, NEW_LINE, SEPARATOR, SYMBOLS},
    types::TranslationEntry,
};
use std::borrow::Cow;

pub(crate) trait CustomReplace {
    /// Normalizes RPG Maker line break symbols (`\n`, `\r`, `\r\n`) to the format that the library uses (`\#`).
    fn normalize(&self) -> Cow<'_, str>;

    /// Denormalizes library line break symbols to the format that RPG Maker uses (`\n`).
    fn denormalize(&self) -> Cow<'_, str>;
}

impl CustomReplace for str {
    fn normalize(&self) -> Cow<'_, str> {
        let bytes = self.as_bytes();

        let Some(first) = bytes.iter().position(|&b| b == b'\r' || b == b'\n')
        else {
            return Cow::Borrowed(self);
        };

        // `\r` and `\n` are replaced independently, so a CRLF pair yields two
        // separators - matching the leftmost-first `\r|\n|\r\n` regex this replaced.
        let mut out = Vec::with_capacity(self.len() + 8);
        out.extend_from_slice(&bytes[..first]);
        out.extend_from_slice(NEW_LINE.as_bytes());

        let mut chunk_start = first + 1;

        for i in chunk_start..bytes.len() {
            if bytes[i] == b'\r' || bytes[i] == b'\n' {
                out.extend_from_slice(&bytes[chunk_start..i]);
                out.extend_from_slice(NEW_LINE.as_bytes());
                chunk_start = i + 1;
            }
        }

        out.extend_from_slice(&bytes[chunk_start..]);

        // SAFETY: `self` is valid UTF-8, and the only bytes rewritten are ASCII
        // `\r`/`\n`, which cannot occur inside a multi-byte sequence. The
        // replacement is ASCII too, so the result is still valid UTF-8.
        Cow::Owned(unsafe { String::from_utf8_unchecked(out) })
    }

    fn denormalize(&self) -> Cow<'_, str> {
        // The first `find` doubles as the no-match check, so a string without
        // separators is scanned once and never copied.
        let Some(first) = self.find(NEW_LINE) else {
            return Cow::Borrowed(self);
        };

        let mut out = String::with_capacity(self.len());
        out.push_str(&self[..first]);
        out.push('\n');

        let mut rest = &self[first + NEW_LINE.len()..];

        while let Some(i) = rest.find(NEW_LINE) {
            out.push_str(&rest[..i]);
            out.push('\n');
            rest = &rest[i + NEW_LINE.len()..];
        }

        out.push_str(rest);
        Cow::Owned(out)
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
pub(crate) enum TranslationLine<'a> {
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
pub(crate) fn split_translation_line(
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

pub(crate) fn string_is_only_symbols(string: &str) -> bool {
    !string.chars().any(|c| !SYMBOLS.contains(&c))
}

// TODO(v15): Check when starts with if
//* This is breaking

pub(crate) fn ends_with_if_index(string: &str) -> Option<usize> {
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
