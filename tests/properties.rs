//! Properties of the public byte and text APIs: the save codec round-trips text, and the decoders never panic on input
//! they didn't write.
use proptest::{collection::vec, prelude::*, sample::select, test_runner::Config as ProptestConfig};
use rvpacker_txt_rs_lib::{json, save};

const FILE_NAMES: [&str; 3] = ["Actors.json", "Map001.rvdata2", "Scripts.rxdata"];

proptest! {
    #![proptest_config(ProptestConfig::with_cases(if cfg!(miri) { 4 } else { 256 }))]

    // Compressing and then decompressing gives back the same text, for any text with at least one character. Empty text
    // is left out, since `load` makes no promise about decoding an empty stream.
    #[test]
    fn save_dump_then_load_restores_any_text(text in "(?s).{1,512}") {
        prop_assert_eq!(save::load(&save::dump(&text)), Some(text));
    }

    // The save decoder rejects what it can't read, and never panics.
    #[test]
    fn save_load_never_panics(input in any::<String>()) {
        let _ = save::load(&input);
    }

    // The game-file reader rejects arbitrary bytes, whichever file they are given as, and never panics.
    #[test]
    fn generate_file_never_panics(bytes in vec(any::<u8>(), 0..512), name in select(FILE_NAMES.to_vec())) {
        let _ = json::generate_file(&bytes, name, None);
    }

    // The JSON writer rejects arbitrary text, and never panics.
    #[test]
    fn write_file_never_panics(text in any::<String>()) {
        let _ = json::write_file(&text);
    }
}
