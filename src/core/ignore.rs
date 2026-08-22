use crate::constants::{GLOB_ENTRY_COMMENT, SEPARATOR};
use gxhash::{HashSet, HashSetExt};

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
    #[must_use]
    pub fn new(pattern: String) -> Self {
        Self { pattern }
    }

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
        let pattern = self.pattern.as_bytes();
        let text = text.as_bytes();

        let (mut p, mut t) = (0, 0);
        // Where to resume if the current `*` turns out to have consumed too little.
        let (mut star, mut retry) = (usize::MAX, 0);

        while t < text.len() {
            if p < pattern.len()
                && (pattern[p] == b'?' || pattern[p] == text[t])
            {
                p += 1;
                t += 1;
            } else if p < pattern.len() && pattern[p] == b'*' {
                star = p;
                retry = t;
                p += 1;
            } else if star != usize::MAX {
                // Backtrack: let the last `*` swallow one more byte.
                p = star + 1;
                retry += 1;
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
/// lookup. Lines written as `<!-- Glob --><#>pattern` are matched as [`Glob`]s,
/// for text that can only be recognised by shape - a shared prefix or suffix
/// rather than a fixed string.
#[derive(Default)]
pub struct IgnoreEntry {
    literals: HashSet<String>,
    globs: Vec<Glob>,
}

impl IgnoreEntry {
    #[must_use]
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            literals: HashSet::with_capacity(capacity),
            globs: Vec::new(),
        }
    }

    /// Adds a line read from a `.rvpacker-ignore` file, which may be a glob.
    pub fn insert_line(&mut self, line: &str) {
        if let Some(pattern) = line
            .strip_prefix(GLOB_ENTRY_COMMENT)
            .and_then(|rest| rest.strip_prefix(SEPARATOR))
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
        self.literals.contains(text)
            || self.globs.iter().any(|glob| glob.matches(text))
    }

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
                    "{GLOB_ENTRY_COMMENT}{SEPARATOR}{pattern}",
                    pattern = glob.pattern()
                ))
            }))
    }
}

#[cfg(test)]
mod tests {
    use super::Glob;

    #[test]
    fn glob_matches() {
        let cases: &[(&str, &str, bool)] = &[
            ("abc", "abc", true),
            ("abc", "abd", false),
            ("*", "", true),
            ("*", "anything", true),
            ("a*", "abc", true),
            ("a*", "bbc", false),
            ("*c", "abc", true),
            ("*c", "abd", false),
            ("a*c", "abc", true),
            ("a*c", "ac", true),
            ("a*c", "abbbbc", true),
            ("a*c", "abcd", false),
            ("*soul", "corrupted soul", true),
            ("*soul", "soulless", false),
            ("The Fellowship*", "The Fellowship of the Ring", true),
            ("The Fellowship*", "A Fellowship", false),
            ("*---*", "a --- b", true),
            ("*---*", "a - b", false),
            ("?bc", "abc", true),
            ("?bc", "bc", false),
            ("a?c", "abc", true),
            ("**a", "ba", true),
            ("*a*a*a*", "aaa", true),
            ("*a*a*a*", "aa", false),
        ];

        for &(pattern, text, expected) in cases {
            let glob = Glob::new(pattern.to_owned());
            assert_eq!(
                glob.matches(text),
                expected,
                "glob {pattern:?} against {text:?}"
            );
        }
    }
}
