//! The `.txt` format's building blocks: line breaks, line splitting, the symbol
//! predicate, and the glob matcher used by `.rvpacker-ignore`.

use rvpacker_txt_rs_lib::core::{
    Glob, IgnoreEntry,
    text::{
        CustomReplace, TranslationLine, ends_with_if_index, latinize_string, split_translation_line,
        string_is_only_symbols,
    },
};
use std::borrow::Cow;

fn split(line: &str, write: bool) -> (String, String) {
    match split_translation_line(line, write) {
        TranslationLine::Split { source, translation } => (source.into_owned(), translation.into_owned()),
        TranslationLine::Malformed => panic!("malformed: {line}"),
        TranslationLine::Untranslated => panic!("untranslated: {line}"),
    }
}

mod line_breaks {
    use super::*;

    #[test]
    fn every_break_becomes_one_marker() {
        assert_eq!("a\nb".normalize(), r"a\#b");
        assert_eq!("a\rb".normalize(), r"a\#b");
        // The alternation puts `\r\n` first, so a CRLF pair collapses into a
        // single marker instead of two.
        assert_eq!("a\r\nb".normalize(), r"a\#b");
    }

    #[test]
    fn text_without_breaks_is_borrowed() {
        assert!(matches!("plain text".normalize(), Cow::Borrowed(_)));
        assert!(matches!("plain text".denormalize(), Cow::Borrowed(_)));
    }

    #[test]
    fn denormalize_restores_line_feeds() {
        assert_eq!(r"a\#b\#c".denormalize(), "a\nb\nc");
    }

    #[test]
    fn round_trips_through_line_feeds() {
        let original = "first\nsecond\nthird";
        assert_eq!(original.normalize().denormalize(), original);
    }

    #[test]
    fn a_lone_backslash_is_left_alone() {
        // Escapes RPG Maker itself interprets must survive; only the library's
        // own `\#` marker is a break.
        assert_eq!(r"damage \c[2]x2".denormalize(), r"damage \c[2]x2");
    }
}

mod splitting {
    use super::*;

    #[test]
    fn splits_on_the_first_separator() {
        assert_eq!(
            split("source<#>translation", false),
            ("source".to_owned(), "translation".to_owned())
        );
    }

    #[test]
    fn takes_the_rightmost_filled_column() {
        assert_eq!(
            split("source<#>first<#>second", false),
            ("source".to_owned(), "second".to_owned())
        );
        assert_eq!(
            split("source<#>filled<#>", false),
            ("source".to_owned(), "filled".to_owned())
        );
    }

    #[test]
    fn a_line_without_a_separator_is_malformed() {
        assert!(matches!(
            split_translation_line("no separator here", false),
            TranslationLine::Malformed
        ));
    }

    #[test]
    fn an_empty_translation_reads_but_does_not_write() {
        assert_eq!(split("source<#>", false), ("source".to_owned(), String::new()));
        assert!(matches!(
            split_translation_line("source<#>", true),
            TranslationLine::Untranslated
        ));
    }

    #[test]
    fn writing_denormalizes_both_halves() {
        assert_eq!(split(r"a\#b<#>c\#d", true), ("a\nb".to_owned(), "c\nd".to_owned()));
    }
}

mod symbols {
    use super::*;

    #[test]
    fn punctuation_only_text_is_symbols() {
        assert!(string_is_only_symbols("..."));
        assert!(string_is_only_symbols("---"));
        assert!(string_is_only_symbols("  \t\n"));
        // Vacuously true: there is no character that is not a symbol.
        assert!(string_is_only_symbols(""));
    }

    #[test]
    fn letters_digits_and_quotes_are_not_symbols() {
        assert!(!string_is_only_symbols("a"));
        assert!(!string_is_only_symbols("...text..."));
        // Digits are meaningful on their own, and quotes may need swapping for
        // the target locale's, so neither counts as a symbol.
        assert!(!string_is_only_symbols("123"));
        assert!(!string_is_only_symbols("\"\""));
    }
}

mod trailing_if {
    use super::*;

    #[test]
    fn finds_the_condition() {
        let text = "Show text if(v[1] > 0)";
        let index = ends_with_if_index(text).expect("condition not found");
        assert_eq!(&text[index..], "if(v[1] > 0)");
    }

    #[test]
    fn rejects_parentheses_that_are_not_conditions() {
        assert!(ends_with_if_index("plain text").is_none());
        assert!(ends_with_if_index("a parenthetical (aside)").is_none());
        assert!(ends_with_if_index("trailing if(unclosed").is_none());
    }
}

mod latinize {
    use super::*;

    #[test]
    fn replaces_cjk_punctuation() {
        assert_eq!(latinize_string("Ⅷ"), "VIII");
        assert_eq!(latinize_string("…"), "...");
        assert_eq!(latinize_string("（）"), "()");
    }

    #[test]
    fn borrows_when_nothing_changes() {
        assert!(matches!(latinize_string("plain"), Cow::Borrowed(_)));
    }

    #[test]
    fn keeps_the_prefix_before_the_first_replacement() {
        assert_eq!(latinize_string("Hello、world"), "Hello,world");
    }
}

mod globs {
    use super::*;

    fn matches(pattern: &str, text: &str) -> bool {
        Glob::new(pattern.to_owned()).matches(text)
    }

    #[test]
    fn star_spans_any_run_including_none() {
        assert!(matches("The Fellowship*", "The Fellowship"));
        assert!(matches("The Fellowship*", "The Fellowship of the Ring"));
        assert!(!matches("The Fellowship*", "the fellowship"));

        assert!(matches("*soul", "black soul"));
        assert!(!matches("*soul", "soulless"));

        assert!(matches("*", ""));
        assert!(matches("*", "anything"));
        assert!(matches("**", "anything"));
    }

    #[test]
    fn question_mark_spans_exactly_one() {
        assert!(matches("test_armor?", "test_armor1"));
        assert!(!matches("test_armor?", "test_armor"));
        assert!(!matches("test_armor?", "test_armor12"));
    }

    #[test]
    fn backtracks_past_a_false_start() {
        // The first place the star could stop is not the matching one.
        assert!(matches("*abc", "abxabc"));
        assert!(matches("a*b*c", "axxbxxc"));
        assert!(!matches("a*b*c", "axxbxx"));
    }

    #[test]
    fn a_pattern_without_wildcards_matches_exactly() {
        assert!(matches("Torch", "Torch"));
        assert!(!matches("Torch", "Torches"));
        assert!(!matches("Torch", "Torc"));
        assert!(matches("", ""));
        assert!(!matches("", "x"));
    }
}

mod ignore_entries {
    use super::*;

    #[test]
    fn plain_lines_match_exactly() {
        let mut entry = IgnoreEntry::default();
        assert!(entry.is_empty());

        entry.insert_line("Torch");
        assert!(entry.contains("Torch"));
        assert!(!entry.contains("Torches"));
        assert!(!entry.is_empty());
    }

    #[test]
    fn a_marked_line_becomes_a_glob() {
        let mut entry = IgnoreEntry::default();
        entry.insert_line("<!>Glob<#>test_*");

        assert!(entry.contains("test_armor"));
        // The marked line is stored as a pattern, not as a literal.
        assert!(!entry.contains("<!>Glob<#>test_*"));
    }

    #[test]
    fn lines_round_trip_with_their_markers() {
        let mut entry = IgnoreEntry::default();
        entry.insert_line("Torch");
        entry.insert_line("<!>Glob<#>*soul");

        let mut lines: Vec<String> = entry.lines().map(Cow::into_owned).collect();
        lines.sort_unstable();

        assert_eq!(lines, ["<!>Glob<#>*soul", "Torch"]);
    }
}
