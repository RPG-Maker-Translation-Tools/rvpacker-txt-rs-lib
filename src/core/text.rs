//! Text handling shared by every file kind, and the `.txt` format's contract.
//!
//! [`CustomReplace`] and [`split_translation_line`] define how a translation line
//! is written and read back, so anything else parsing these files - a GUI, a CLI -
//! can rely on the same functions rather than reimplementing them.

use crate::{constants::SYMBOLS, get_id_comment, get_line_break, get_line_separator, types::TranslationEntry};
use memchr::{memchr2, memmem};
use std::{borrow::Cow, io::Write as _};

/// Converts between RPG Maker's line breaks and the library's.
///
/// Implemented for [`str`], so a consumer parsing the generated `.txt` files can
/// round-trip a line the same way this crate does.
pub trait CustomReplace {
    /// Normalizes RPG Maker line break symbols (`\n`, `\r`, `\r\n`) to the custom internal format that the library uses.
    fn normalize(&self) -> Cow<'_, str>;

    /// Denormalizes library line break symbols to the format that RPG Maker uses (`\n`).
    fn denormalize(&self) -> Cow<'_, str>;
}

impl CustomReplace for str {
    fn normalize(&self) -> Cow<'_, str> {
        let bytes = self.as_bytes();

        let Some(mut at) = memchr2(b'\r', b'\n', bytes) else {
            return Cow::Borrowed(self);
        };

        let line_break = get_line_break();
        let mut output = String::with_capacity(self.len() + line_break.len());
        let mut copied = 0;

        // `\r` and `\n` are ASCII, so every index here is on a character boundary.
        loop {
            output.push_str(&self[copied..at]);
            output.push_str(line_break);

            // `\r\n` is one break.
            copied = if bytes[at] == b'\r' && bytes.get(at + 1) == Some(&b'\n') {
                at + 2
            } else {
                at + 1
            };

            match memchr2(b'\r', b'\n', &bytes[copied..]) {
                Some(next) => at = copied + next,
                None => break,
            }
        }

        output.push_str(&self[copied..]);
        Cow::Owned(output)
    }

    fn denormalize(&self) -> Cow<'_, str> {
        // Reads the line break on every call, so `set_line_break` takes effect after first use.
        let line_break = get_line_break();

        // Most strings have no marker, so skip building a finder for them.
        if memmem::find(self.as_bytes(), line_break.as_bytes()).is_none() {
            return Cow::Borrowed(self);
        }

        replace_all(self, &memmem::Finder::new(line_break), "\n")
    }
}

/// One SIMD search pass
fn replace_all<'a>(text: &'a str, from: &memmem::Finder<'_>, to: &str) -> Cow<'a, str> {
    let mut matches = from.find_iter(text.as_bytes());

    let Some(first) = matches.next() else {
        return Cow::Borrowed(text);
    };

    let mut output = String::with_capacity(text.len());
    let mut copied = 0;

    // Matches of a `str` needle start and end on character boundaries.
    for start in std::iter::once(first).chain(matches) {
        output.push_str(&text[copied..start]);
        output.push_str(to);
        copied = start + from.needle().len();
    }

    output.push_str(&text[copied..]);
    Cow::Owned(output)
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
#[must_use]
pub fn latinize_string(string: &str) -> Cow<'_, str> {
    let mut result: Option<String> = None;

    for (offset, char) in string.char_indices() {
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

        let s = result.get_or_insert_with(|| {
            let mut s = String::with_capacity(string.len());
            s.push_str(&string[..offset]);
            s
        });
        s.push_str(replacement);
    }

    match result {
        Some(s) => Cow::Owned(s),
        None => Cow::Borrowed(string),
    }
}

/// Outcome of splitting one `source{SEP}translation` line from a translation file.
pub enum TranslationLine<'a> {
    /// No separator present; the caller should warn and skip the line.
    Malformed,
    /// Empty translation on a write pass - the entry is unused, so skip it.
    Untranslated,
    /// A source and a non-empty translation.
    Split {
        /// The source text. Line-break markers become real line breaks when the split is for writing.
        source: Cow<'a, str>,
        /// The translation text. Line-break markers become real line breaks when the split is for writing.
        translation: Cow<'a, str>,
    },
}

/// Splits one translation-file line into its source and translation halves.
///
/// The translation is the last non-empty field after the first separator, so files
/// carrying several translation columns still resolve to the rightmost filled one.
///
/// Borrows throughout; allocates only where `write` forces denormalization.
#[must_use]
pub fn split_translation_line(line: &str, write: bool) -> TranslationLine<'_> {
    LineSplitter::new().split(line, write)
}

/// [`split_translation_line`] with its searches set up once, for a whole file of lines.
pub(crate) struct LineSplitter {
    separator: memmem::Finder<'static>,
    separator_rev: memmem::FinderRev<'static>,
    line_break: memmem::Finder<'static>,
}

impl LineSplitter {
    pub(crate) fn new() -> Self {
        Self {
            separator: memmem::Finder::new(get_line_separator()),
            separator_rev: memmem::FinderRev::new(get_line_separator()),
            line_break: memmem::Finder::new(get_line_break()),
        }
    }

    pub(crate) fn split<'a>(&self, line: &'a str, write: bool) -> TranslationLine<'a> {
        let separator_len = self.separator.needle().len();

        let Some(at) = self.separator.find(line.as_bytes()) else {
            return TranslationLine::Malformed;
        };

        let (source, rest) = (&line[..at], &line[at + separator_len..]);

        // The last non-empty field. Usually there is just one, so there is nothing to search past.
        let mut end = rest.len();
        let mut translation = "";

        for start in self.separator_rev.rfind_iter(rest.as_bytes()) {
            translation = &rest[start + separator_len..end];

            if !translation.is_empty() {
                break;
            }

            end = start;
        }

        if translation.is_empty() {
            translation = &rest[..end];
        }

        if write {
            // Lines with no translation are unused on write.
            if translation.is_empty() {
                return TranslationLine::Untranslated;
            }

            TranslationLine::Split {
                source: replace_all(source, &self.line_break, "\n"),
                translation: replace_all(translation, &self.line_break, "\n"),
            }
        } else {
            TranslationLine::Split {
                source: Cow::Borrowed(source),
                translation: Cow::Borrowed(translation),
            }
        }
    }
}

/// `comments` are the section's own comment lines, which must follow its id line to be parsed back under it.
pub(crate) fn push_metadata(output: &mut Vec<u8>, id: u32, metadata: &[String], comments: &[String]) {
    output.extend_from_slice(get_id_comment().as_bytes());
    output.extend_from_slice(get_line_separator().as_bytes());
    let _ = write!(output, "{id}");
    output.push(b'\n');

    for comment in metadata.iter().chain(comments).filter(|c| !c.is_empty()) {
        output.extend_from_slice(comment.as_bytes());
        output.push(b'\n');
    }
}

pub(crate) fn push_entries(output: &mut Vec<u8>, source: &str, translation: &TranslationEntry) {
    for comment in translation.comments.iter().filter(|c| !c.is_empty()) {
        output.extend_from_slice(comment.as_bytes());
        output.push(b'\n');
    }

    if !source.is_empty() {
        output.extend_from_slice(source.as_bytes());
        output.extend_from_slice(get_line_separator().as_bytes());
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
