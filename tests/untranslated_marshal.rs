//! Writing a partial translation into a Marshal engine's file must leave every untranslated string as it was.

use rvpacker_txt_rs_lib::{EngineType, Mode, core::Base, get_line_separator, json::generate_file};
use std::fs::read;

#[test]
fn untranslated_event_text_survives_a_write() {
    for name in ["CommonEvents.rvdata2", "Troops.rvdata2"] {
        let content = read(format!("tests/RMVXACE/Data/{name}")).unwrap();

        let mut read_base = Base::new(Mode::read(), EngineType::VXAce);
        let text = read_base.process_other(name, &content, None).unwrap().unwrap();
        let text = String::from_utf8(text.as_ref().to_vec()).unwrap();

        // Only the first entry gets a translation.
        let separator = get_line_separator();
        let mut translated = false;
        let filled: Vec<String> = text
            .lines()
            .map(|line| {
                if !translated
                    && line.ends_with(separator)
                    && !line.starts_with(rvpacker_txt_rs_lib::DEFAULT_COMMENT_PREFIX)
                {
                    translated = true;
                    format!("{line}ZZZ")
                } else {
                    line.to_owned()
                }
            })
            .collect();
        assert!(translated);

        let mut write_base = Base::new(Mode::Write, EngineType::VXAce);
        let written = write_base
            .process_other(name, &content, Some(&filled.join("\n")))
            .unwrap()
            .unwrap();

        let before = generate_file(&content, name, None).unwrap();
        let after = generate_file(written.as_ref(), name, None).unwrap();

        let changed: Vec<_> = before.lines().zip(after.lines()).filter(|(a, b)| a != b).collect();
        assert!(!changed.is_empty());
        assert!(
            changed.iter().all(|(_, after)| after.contains("ZZZ")),
            "{name}: {changed:?}"
        );
    }
}
