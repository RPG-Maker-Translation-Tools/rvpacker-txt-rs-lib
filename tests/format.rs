//! Parsing of the things around the translation files: `.rvpacker-ignore`
//! sections, filenames, titles, and the enums that survive a round trip through
//! their serialized forms.

use rvpacker_txt_rs_lib::{
    DuplicateMode, EngineType, Error, FileFlags, Mode, RPGMFileType,
    core::Base, get_ini_title, get_system_title, parse_ignore,
};
use std::str::FromStr;

mod ignore_file {
    use super::*;

    const TWO_ITEM_SECTIONS: &str = "\
<!>Ignore Entry<#>Items: 1
Torch
<!>Ignore Entry<#>Items: 2
Flashlight
";

    #[test]
    fn removing_duplicates_collapses_a_file_into_one_section() {
        let map = parse_ignore(TWO_ITEM_SECTIONS, DuplicateMode::Remove, true);

        assert_eq!(map.len(), 1);

        let entry = map
            .get("<!>Ignore Entry<#>Items")
            .expect("collapsed section missing");
        assert!(entry.contains("Torch"));
        assert!(entry.contains("Flashlight"));
    }

    #[test]
    fn allowing_duplicates_keys_each_section_on_its_id() {
        let map = parse_ignore(TWO_ITEM_SECTIONS, DuplicateMode::Allow, true);

        assert_eq!(map.len(), 2);
        assert!(
            map.get("<!>Ignore Entry<#>Items: 1")
                .expect("first section missing")
                .contains("Torch")
        );
        assert!(
            map.get("<!>Ignore Entry<#>Items: 2")
                .expect("second section missing")
                .contains("Flashlight")
        );
    }

    #[test]
    fn purging_never_collapses() {
        // Purge writes entries back per id, so the key has to stay whole.
        let map = parse_ignore(TWO_ITEM_SECTIONS, DuplicateMode::Remove, false);
        assert_eq!(map.len(), 2);
    }

    #[test]
    fn single_section_files_keep_their_ids() {
        // System, Scripts and Plugins have one section each; there is nothing to
        // collapse, so their keys are left alone.
        let content = "\
<!>Ignore Entry<#>System: 1
Fight
<!>Ignore Entry<#>Scripts: 1
Yanfly
<!>Ignore Entry<#>Plugins: 1
plugin text
";
        let map = parse_ignore(content, DuplicateMode::Remove, true);

        assert_eq!(map.len(), 3);
        assert!(map.contains_key("<!>Ignore Entry<#>System: 1"));
        assert!(map.contains_key("<!>Ignore Entry<#>Scripts: 1"));
        assert!(map.contains_key("<!>Ignore Entry<#>Plugins: 1"));
    }

    #[test]
    fn a_header_without_an_id_is_accepted() {
        let map = parse_ignore(
            "<!>Ignore Entry<#>Items\nTorch\n",
            DuplicateMode::Remove,
            true,
        );

        assert_eq!(map.len(), 1);
        assert!(
            map.get("<!>Ignore Entry<#>Items")
                .expect("section missing")
                .contains("Torch")
        );
    }

    #[test]
    fn lines_before_the_first_header_are_dropped() {
        let map = parse_ignore(
            "orphan\n<!>Ignore Entry<#>Items: 1\nTorch\n",
            DuplicateMode::Allow,
            true,
        );

        assert_eq!(map.len(), 1);
        assert!(map.values().all(|entry| !entry.contains("orphan")));
    }

    #[test]
    fn globs_survive_parsing() {
        let map = parse_ignore(
            "<!>Ignore Entry<#>Items: 1\n<!>Glob<#>*soul\n",
            DuplicateMode::Allow,
            true,
        );

        let entry = map
            .get("<!>Ignore Entry<#>Items: 1")
            .expect("section missing");
        assert!(entry.contains("Rotten soul"));
        assert!(!entry.contains("Torch"));
    }

    #[test]
    fn the_shipped_example_parses() {
        let content = include_str!("../examples/.rvpacker-ignore");
        let map = parse_ignore(content, DuplicateMode::Allow, true);

        assert_eq!(map.len(), 5);

        let items = map
            .get("<!>Ignore Entry<#>Items: 1")
            .expect("Items missing");
        assert!(items.contains("Torch"));
        assert!(items.contains("The Fellowship of the Dark"));
        assert!(items.contains("Rotten soul"));
        assert!(!items.contains("Bandage"));

        let armors = map
            .get("<!>Ignore Entry<#>Armors: 1")
            .expect("Armors missing");
        assert!(armors.contains("test_armor2"));
    }
}

mod filenames {
    use super::*;

    #[test]
    fn file_types_come_from_the_first_three_bytes() {
        assert_eq!(RPGMFileType::from_filename("Actors"), RPGMFileType::Actors);
        assert_eq!(
            RPGMFileType::from_filename("CommonEvents"),
            RPGMFileType::Events
        );
        assert_eq!(RPGMFileType::from_filename("Map001"), RPGMFileType::Map);
        assert_eq!(
            RPGMFileType::from_filename("plugins"),
            RPGMFileType::Plugins
        );
        assert_eq!(
            RPGMFileType::from_filename("Unknown"),
            RPGMFileType::Invalid
        );
        // Too short to have a prefix, and a multi-byte character at byte 3 must
        // not panic the way slicing did.
        assert_eq!(RPGMFileType::from_filename("ab"), RPGMFileType::Invalid);
        assert_eq!(
            RPGMFileType::from_filename("マップ"),
            RPGMFileType::Invalid
        );
    }

    #[test]
    fn file_type_groups() {
        assert!(RPGMFileType::Items.is_other());
        assert!(RPGMFileType::Items.is_main());
        assert!(RPGMFileType::Map.is_main());
        assert!(!RPGMFileType::Map.is_other());
        assert!(RPGMFileType::System.is_misc());
        assert!(RPGMFileType::Scripts.is_misc());
    }

    #[test]
    fn map_ids_are_parsed_past_three_digits() {
        assert_eq!(Base::parse_map_id("Map001.rvdata2"), 1);
        assert_eq!(Base::parse_map_id("Map042.json"), 42);
        // A game may have more than 999 maps.
        assert_eq!(Base::parse_map_id("Map1024.rvdata2"), 1024);
    }

    #[test]
    fn engine_types_map_to_extensions() {
        for engine in [
            EngineType::New,
            EngineType::VXAce,
            EngineType::VX,
            EngineType::XP,
        ] {
            assert_eq!(
                EngineType::from_extension(engine.extension()),
                Some(engine)
            );
        }

        assert_eq!(EngineType::from_extension("txt"), None);
    }
}

mod titles {
    use super::*;

    #[test]
    fn ini_titles_are_read_as_raw_bytes() {
        // Not necessarily UTF-8, so the title comes back as bytes.
        let ini = b"[Game]\r\nRTP=RPGVXAce\r\nTitle=Fear & Hunger 2\r\nScripts=Data\r\n";
        assert_eq!(get_ini_title(ini).unwrap(), b"Fear & Hunger 2");
    }

    #[test]
    fn the_ini_key_is_case_insensitive_and_the_value_trimmed() {
        assert_eq!(get_ini_title(b"title =  Spaced  \n").unwrap(), b"Spaced");
    }

    #[test]
    fn an_ini_without_a_title_errors() {
        assert!(matches!(
            get_ini_title(b"[Game]\nRTP=RPGVXAce\n"),
            Err(Error::NoTitle)
        ));
    }

    #[test]
    fn system_titles_are_read_past_the_byte_order_mark() {
        assert_eq!(
            get_system_title(r#"{"gameTitle":"Some Game"}"#).unwrap(),
            "Some Game"
        );
        assert_eq!(
            get_system_title("\u{feff}{\"gameTitle\":\"Some Game\"}").unwrap(),
            "Some Game"
        );
    }

    #[test]
    fn a_system_file_without_a_title_errors() {
        assert!(matches!(get_system_title("{}"), Err(Error::NoTitle)));
        assert!(matches!(
            get_system_title("not json"),
            Err(Error::JsonParse(_))
        ));
    }
}

mod modes {
    use super::*;

    #[test]
    fn every_mode_survives_the_byte_encoding() {
        let modes = [
            Mode::Read {
                append: false,
                force: false,
            },
            Mode::Read {
                append: false,
                force: true,
            },
            Mode::Read {
                append: true,
                force: false,
            },
            // This one used to collide with `Write`.
            Mode::Read {
                append: true,
                force: true,
            },
            Mode::Write,
            Mode::Purge,
        ];

        for mode in modes {
            let byte = u8::from(mode);
            let back = Mode::try_from(byte).expect("valid byte rejected");
            assert_eq!(u8::from(back), byte, "{mode:?} did not round trip");
        }

        // All six encodings are distinct.
        let mut bytes: Vec<u8> = modes.into_iter().map(u8::from).collect();
        bytes.sort_unstable();
        bytes.dedup();
        assert_eq!(bytes.len(), 6);
    }

    #[test]
    fn out_of_range_bytes_are_rejected() {
        assert!(Mode::try_from(6).is_err());
        assert!(Mode::try_from(255).is_err());
    }

    #[test]
    fn mode_names_parse() {
        assert!(Mode::from_str("default").unwrap().is_default_default());
        assert!(Mode::from_str("append").unwrap().is_append_default());
        assert!(Mode::from_str("force").unwrap().is_default());
        assert!(Mode::from_str("force-append").unwrap().is_append());
        assert!(Mode::from_str("write").unwrap().is_write());
        assert!(Mode::from_str("purge").unwrap().is_purge());
        assert!(Mode::from_str("nonsense").is_err());
    }
}

mod file_flags {
    use super::*;

    #[test]
    fn other_is_everything_but_maps_system_and_scripts() {
        let rest =
            FileFlags::map() | FileFlags::system() | FileFlags::scripts();
        assert_eq!(FileFlags::other() | rest, FileFlags::all());
        assert!(!FileFlags::other().intersects(rest));
    }

    #[test]
    fn aliases_name_the_same_bits_as_the_constants() {
        assert_eq!(FileFlags::map(), FileFlags::Map);
        assert_eq!(FileFlags::common_events(), FileFlags::CommonEvents);
        assert_eq!(FileFlags::weapons(), FileFlags::Weapons);
        assert_eq!(FileFlags::scripts(), FileFlags::Scripts);
    }

    #[test]
    fn flags_parse_from_filenames() {
        assert_eq!(FileFlags::from_str("Actors").unwrap(), FileFlags::Actors);
        assert_eq!(
            FileFlags::from_str("CommonEvents").unwrap(),
            FileFlags::CommonEvents
        );
        // Both script kinds share one flag.
        assert_eq!(FileFlags::from_str("Scripts").unwrap(), FileFlags::Scripts);
        assert_eq!(FileFlags::from_str("plugins").unwrap(), FileFlags::Scripts);
        assert!(FileFlags::from_str("Unknown").is_err());
    }

    #[test]
    fn flags_survive_the_bit_encoding() {
        let flags = FileFlags::Map | FileFlags::other();
        assert_eq!(FileFlags::try_from(u16::from(flags)).unwrap(), flags);
    }
}
