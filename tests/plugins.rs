//! `js/plugins.js` processing: which strings are extracted, which are skipped,
//! what a malformed file is rejected for, and that a translation is written back.

use rvpacker_txt_rs_lib::{EngineType, Error, Mode, ProcessedData, core::Base};
use serde_json::{Value, json};

/// A `plugins.js` whose one plugin has the given parameters.
fn plugin(parameters: Value) -> String {
    format!("plugins = [{}]", json!({"name": "Demo", "parameters": parameters}))
}

/// The translation file a read of `content` produces.
fn extract(content: &str) -> String {
    let mut base = Base::new(Mode::read(), EngineType::MVMZ);
    let data = base
        .process_plugins(content.as_bytes(), None)
        .expect("plugins should parse")
        .expect("a read always yields a translation file");
    let bytes: &[u8] = data.as_ref();

    String::from_utf8(bytes.to_vec()).expect("translation output is UTF-8")
}

mod extraction {
    use super::*;

    #[test]
    fn plugin_text_is_extracted() {
        assert!(extract(&plugin(json!({"text": "Hello world."}))).contains("Hello world."));
    }

    #[test]
    fn strings_nested_in_objects_and_arrays_are_extracted() {
        let text = extract(&plugin(json!({"list": [{"inner": "Nested line."}]})));

        assert!(text.contains("Nested line."));
    }

    #[test]
    fn engine_keys_are_never_extracted() {
        // `description` and `Menu Text` match the key filter, so their values are format, not text.
        let baseline = extract(&plugin(json!({})));
        let text = extract(&plugin(
            json!({"description": "Adds a menu.", "Menu Text": "Fancy Menu Entry"}),
        ));

        assert_eq!(text, baseline);
    }
}

mod skipped_values {
    use super::*;

    #[test]
    fn values_that_are_not_text_are_skipped() {
        let baseline = extract(&plugin(json!({})));

        for value in [
            "",
            "   ",
            "true",
            "false",
            "none",
            "time",
            "off",
            "...",
            "!!!",
            "→",
            "rgba(0, 0, 0, 0.5)",
            "this.isOpen()",
        ] {
            assert_eq!(
                extract(&plugin(json!({"text": value}))),
                baseline,
                "{value:?} should not be extracted"
            );
        }
    }

    #[test]
    fn a_this_expression_without_a_call_is_text() {
        // Only `this.name()` is code; `this.label` is a word.
        let baseline = extract(&plugin(json!({})));

        assert_ne!(extract(&plugin(json!({"text": "this.label"}))), baseline);
    }

    #[test]
    fn a_latin_key_keeps_a_value_the_filters_would_drop() {
        let baseline = extract(&plugin(json!({})));

        assert_ne!(extract(&plugin(json!({"LATIN1": "true"}))), baseline);
    }
}

mod rejection {
    use super::*;

    #[test]
    fn content_without_an_assignment_is_rejected() {
        let mut base = Base::new(Mode::read(), EngineType::MVMZ);

        assert!(matches!(
            base.process_plugins(b"[]", None),
            Err(Error::InvalidPlugins(_))
        ));
    }

    #[test]
    fn a_non_array_is_rejected() {
        let mut base = Base::new(Mode::read(), EngineType::MVMZ);

        assert!(matches!(
            base.process_plugins(b"plugins = {}", None),
            Err(Error::InvalidPlugins(_))
        ));
    }

    #[test]
    fn a_plugin_without_a_name_is_rejected() {
        let mut base = Base::new(Mode::read(), EngineType::MVMZ);

        assert!(matches!(
            base.process_plugins(b"plugins = [{\"status\":true}]", None),
            Err(Error::InvalidPlugins(_))
        ));
    }

    #[test]
    fn invalid_json_is_a_parse_error() {
        let mut base = Base::new(Mode::read(), EngineType::MVMZ);

        assert!(matches!(
            base.process_plugins(b"plugins = [}", None),
            Err(Error::JsonParse(_))
        ));
    }

    #[test]
    fn non_utf8_content_is_rejected() {
        let mut base = Base::new(Mode::read(), EngineType::MVMZ);

        assert!(matches!(
            base.process_plugins(b"plugins = [\xff]", None),
            Err(Error::InvalidPlugins(_))
        ));
    }

    #[test]
    fn writing_without_a_translation_is_rejected() {
        let mut base = Base::new(Mode::Write, EngineType::MVMZ);

        assert!(matches!(
            base.process_plugins(plugin(json!({"text": "Hello world."})).as_bytes(), None),
            Err(Error::NoTranslation)
        ));
    }

    #[test]
    fn writing_an_empty_translation_yields_nothing() {
        // No plugin has a section in an empty translation, so there is nothing to write.
        let mut base = Base::new(Mode::Write, EngineType::MVMZ);
        let result = base.process_plugins(plugin(json!({"text": "Hello world."})).as_bytes(), Some(""));

        assert!(matches!(result, Ok(None)));
    }
}

mod round_trip {
    use super::*;

    #[test]
    fn a_translated_plugin_string_is_written_back() {
        let source = plugin(json!({"text": "Hello world."}));
        let extracted = extract(&source);

        assert!(
            extracted.contains("Hello world.<#>"),
            "the read did not extract the plugin text in the expected form"
        );

        let translated = extracted.replace("Hello world.<#>", "Hello world.<#>Hola mundo.");
        let mut base = Base::new(Mode::Write, EngineType::MVMZ);
        let data = base
            .process_plugins(source.as_bytes(), Some(translated.as_str()))
            .expect("writing should succeed")
            .expect("a translated plugin should produce output");
        let ProcessedData::RPGMData(bytes) = data else {
            panic!("writing should produce game data");
        };
        let output = String::from_utf8(bytes).expect("plugins output is UTF-8");

        assert!(output.starts_with("plugins = "), "the original assignment is kept");
        assert!(output.contains("Hola mundo."));
        assert!(!output.contains("Hello world."));
    }
}

#[test]
fn plugins_nested_too_deeply_are_rejected() {
    let depth = 1000;
    let text = format!(
        "plugins = [{{\"name\":\"Demo\",\"parameters\":{}{}}}]",
        "[".repeat(depth),
        "]".repeat(depth)
    );
    let mut base = Base::new(Mode::read(), EngineType::MVMZ);

    assert!(matches!(
        base.process_plugins(text.as_bytes(), None),
        Err(Error::InvalidPlugins(_))
    ));
}

#[test]
fn an_equals_sign_in_a_comment_above_an_unnamed_plugins_list_is_skipped() {
    let text = extract(&format!(
        "/* Version = 1 */\n{}",
        plugin(json!({"text": "Hello world."}))
    ));

    assert!(text.contains("Hello world."));
}
