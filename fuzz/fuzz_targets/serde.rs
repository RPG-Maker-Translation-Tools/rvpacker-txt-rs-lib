#![no_main]
use libfuzzer_sys::fuzz_target;
use rvpacker_txt_rs_lib::serde::{
    export_csv, export_json, export_xlsx, export_xml, export_yaml, import_csv, import_json, import_xlsx, import_xml,
    import_yaml,
};
use std::error::Error;

/// `export(x)` must import, and export -> import -> export must settle after one step.
fn check<T>(
    name: &str,
    text: &str,
    export: fn(&str) -> Result<T, Box<dyn Error>>,
    import: fn(&T) -> Result<String, Box<dyn Error>>,
) {
    let Ok(first) = export(text) else {
        return;
    };
    let imported = import(&first).unwrap_or_else(|e| panic!("{name}: own export must import: {e}"));
    let second = export(&imported).unwrap_or_else(|e| panic!("{name}: own import must export: {e}"));
    // Compared as text: xlsx embeds a creation timestamp, so equal exports differ in bytes.
    let imported_again = import(&second).unwrap_or_else(|e| panic!("{name}: second export must import: {e}"));
    assert!(imported == imported_again, "{name}: round trip is not stable for {text:?}");
}

fuzz_target!(|input: &[u8]| {
    let _ = import_xlsx(input);

    let Ok(text) = std::str::from_utf8(input) else {
        return;
    };

    let _ = (import_csv(text), import_xml(text), import_json(text), import_yaml(text));

    check("csv", text, export_csv, |s: &String| import_csv(s));
    check("xml", text, export_xml, |s: &String| import_xml(s));
    check("json", text, export_json, |s: &String| import_json(s));
    check("yaml", text, export_yaml, |s: &String| import_yaml(s));
    check("xlsx", text, export_xlsx, |b: &Vec<u8>| import_xlsx(b));
});
