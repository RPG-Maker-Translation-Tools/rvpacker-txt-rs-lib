use crate::types::{IndexSetExt, IndexSetGx};
use crate::{get_glob_entry_comment, get_line_separator};

/// A shell-style pattern from a `.rvpacker-ignore` file.
///
/// Supports `*` (any run of characters, including none) and `?` (exactly one
/// character); everything else matches literally. Deliberately not a regex - the
/// entries are game text, and a full regex dialect would mean escaping most
/// punctuation.
pub struct Glob {
    pattern: String,
}

impl Glob {
    /// Wraps `pattern`, as written after the `Glob` marker in a `.rvpacker-ignore` file.
    #[must_use]
    pub const fn new(pattern: String) -> Self {
        Self { pattern }
    }

    /// The pattern this glob was built from.
    #[must_use]
    pub fn pattern(&self) -> &str {
        &self.pattern
    }

    /// Whether `text` matches.
    ///
    /// Iterative with backtracking rather than recursive: patterns come from a
    /// user-supplied file, and a recursive matcher can be driven to blow the stack
    /// by a pattern full of `*`.
    #[must_use]
    pub fn matches(&self, text: &str) -> bool {
        let char_len = |t: usize| text[t..].chars().next().map_or(1, char::len_utf8);
        let pattern = self.pattern.as_bytes();
        let bytes = text.as_bytes();

        let (mut p, mut t) = (0, 0);
        // Where to resume if the current `*` turns out to have consumed too little.
        let (mut star, mut retry) = (usize::MAX, 0);

        while t < bytes.len() {
            // `*` and `?` before the literal compare: a literal `*` in the text must not consume a wildcard.
            if p < pattern.len() && pattern[p] == b'*' {
                star = p;
                retry = t;
                p += 1;
            } else if p < pattern.len() && pattern[p] == b'?' {
                p += 1;
                t += char_len(t);
            } else if p < pattern.len() && pattern[p] == bytes[t] {
                p += 1;
                t += 1;
            } else if star != usize::MAX {
                // Backtrack: let the last `*` swallow one more character.
                p = star + 1;
                retry += char_len(retry);
                t = retry;
            } else {
                return false;
            }
        }

        while p < pattern.len() && pattern[p] == b'*' {
            p += 1;
        }

        p == pattern.len()
    }
}

/// Lines to skip for one file or section.
///
/// Plain lines are matched exactly, which is the common case and stays a hash
/// lookup. Lines written as `{COMMENT_PREFIX}Glob{SEP}pattern` are matched as [`Glob`]s,
/// for text that can only be recognised by shape - a shared prefix or suffix
/// rather than a fixed string.
#[derive(Default)]
pub struct IgnoreEntry {
    // Ordered, so the ignore file comes out the same on every run.
    literals: IndexSetGx<String>,
    globs: Vec<Glob>,
}

impl IgnoreEntry {
    /// An empty entry with room for `capacity` exact lines.
    #[must_use]
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            literals: IndexSetGx::with_capacity(capacity),
            globs: Vec::new(),
        }
    }

    /// Adds a line read from a `.rvpacker-ignore` file, which may be a glob.
    pub fn insert_line(&mut self, line: &str) {
        if let Some(pattern) = line
            .strip_prefix(get_glob_entry_comment())
            .and_then(|rest| rest.strip_prefix(get_line_separator()))
        {
            self.globs.push(Glob::new(pattern.to_owned()));
        } else {
            self.literals.insert(line.to_owned());
        }
    }

    /// Adds an exact line, as purging does when it collects an untranslated entry.
    pub fn insert(&mut self, line: String) {
        self.literals.insert(line);
    }

    /// Whether `text` is ignored, by exact match or by any glob.
    #[must_use]
    pub fn contains(&self, text: &str) -> bool {
        self.literals.contains(text) || self.globs.iter().any(|glob| glob.matches(text))
    }

    /// Whether the entry holds neither exact lines nor globs.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.literals.is_empty() && self.globs.is_empty()
    }

    /// The entry's lines, ready to be written back out.
    ///
    /// Globs keep their marker so the file round-trips.
    pub fn lines(&self) -> impl Iterator<Item = std::borrow::Cow<'_, str>> {
        self.literals
            .iter()
            .map(|literal| std::borrow::Cow::Borrowed(literal.as_str()))
            .chain(self.globs.iter().map(|glob| {
                std::borrow::Cow::Owned(format!(
                    "{comment}{sep}{pattern}",
                    comment = get_glob_entry_comment(),
                    sep = get_line_separator(),
                    pattern = glob.pattern()
                ))
            }))
    }
}

#[cfg(test)]
mod tests {
    use super::{Glob, IgnoreEntry};
    use crate::{DuplicateMode, get_glob_entry_comment, get_ignore_entry_comment, get_line_separator, parse_ignore};

    #[test]
    fn globs_match_by_shape() {
        assert!(Glob::new("*.png".to_owned()).matches("hero.png"));
        assert!(Glob::new("a*b*c".to_owned()).matches("aXXbYYbZc"));
        assert!(Glob::new("???".to_owned()).matches("勇者の"));
        assert!(!Glob::new("???".to_owned()).matches("勇者"));
        assert!(!Glob::new("*.png".to_owned()).matches("hero.jpg"));
        assert!(Glob::new("*".to_owned()).matches(""));
    }

    #[test]
    fn a_glob_line_is_matched_by_shape() {
        let mut entry = IgnoreEntry::default();
        entry.insert_line(&format!("{}{}debug_*", get_glob_entry_comment(), get_line_separator()));
        entry.insert_line("exact");

        assert!(entry.contains("debug_menu"));
        assert!(entry.contains("exact"));
        assert!(!entry.contains("other"));
    }

    #[test]
    fn lines_belong_to_the_section_above_them() {
        let header = format!("{}{}Actors: 1", get_ignore_entry_comment(), get_line_separator());
        let content = format!("before the first header\n{header}\nexact line\n");

        let map = parse_ignore(&content, DuplicateMode::Allow, true);
        let entry = map.get(header.as_str()).expect("the section is keyed by its header");

        assert!(entry.contains("exact line"));
        assert!(!entry.contains("before the first header"));
    }
}
