//! The separator and comment-prefix setters. Kept in its own file so the global config is changed in a process of its own.

use rvpacker_txt_rs_lib::{
    DEFAULT_COMMENT_PREFIX, DEFAULT_LINE_BREAK, DEFAULT_LINE_SEPARATOR, get_comment_prefix, get_line_break,
    get_line_separator, set_comment_prefix, set_line_break, set_line_separator,
};

#[test]
fn setters_update_getters_and_keep_earlier_results_valid() {
    assert_eq!(get_comment_prefix(), DEFAULT_COMMENT_PREFIX);
    assert_eq!(get_line_separator(), DEFAULT_LINE_SEPARATOR);
    assert_eq!(get_line_break(), DEFAULT_LINE_BREAK);

    // A `&'static str` taken before a setter runs must stay readable afterwards.
    let old_prefix = get_comment_prefix();

    set_comment_prefix("##");
    set_line_separator("<SEP>");
    set_line_break("<BR>");

    assert_eq!(get_comment_prefix(), "##");
    assert_eq!(get_line_separator(), "<SEP>");
    assert_eq!(get_line_break(), "<BR>");
    assert_eq!(old_prefix, DEFAULT_COMMENT_PREFIX);

    // Each setter changes only its own field.
    set_comment_prefix("//");
    assert_eq!(get_line_separator(), "<SEP>");
    assert_eq!(get_line_break(), "<BR>");
}
