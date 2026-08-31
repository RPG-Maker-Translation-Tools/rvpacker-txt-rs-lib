//! VX Ace's Ruby 1.9+ Marshal format tags a `Str` with the `E`/`encoding`
//! ivar it actually declared - `Base::extract_string` is supposed to trust
//! that ivar on read. On write, translated text is always encoded per
//! `Base::set_write_encoding` (UTF-8 by default) rather than back into
//! whatever the source declared - preserving the source encoding would
//! silently corrupt any translation into a script that encoding can't
//! represent (Windows-1251 can't hold Japanese, Shift_JIS can't hold
//! Cyrillic, etc.). These build a fixture by hand (via
//! `marshal_rs::arena::Arena`, since no bundled fixture project uses a
//! non-UTF-8 declared encoding) and check both directions.

use marshal_rs::arena::Arena;
use rvpacker_txt_rs_lib::{EngineType, Mode, core::Base};

/// One `Actors.rvdata2`-shaped array:
/// `[nil, {"id" => 1, "name" => "Actor1", "nickname" => <nickname>}]`, with
/// `nickname` a `Str` tagged `encoding_name`.
///
/// `name` stays plain ASCII on purpose: it doubles as this entry's `<!>NAME`
/// comment header, read through a raw, non-encoding-aware accessor
/// (`other.rs`'s `update_metadata` call) that is out of scope here - this
/// fixture isolates the probing behavior to `nickname`, which only ever goes
/// through `Base::extract_string`/`Base::write_translated`.
fn actors_with_encoded_nickname(nickname_bytes: &[u8], encoding_name: &[u8]) -> Vec<u8> {
    let mut arena = Arena::builder();

    let name_id = arena.push_string("Actor1".to_owned());
    let nickname_id = arena.push_str_with_encoding_name(nickname_bytes.to_vec(), encoding_name);
    let id_id = arena.push_fixnum(1);
    let actor_id = arena.push_object(
        b"Object".to_vec(),
        &[
            (b"id".to_vec(), id_id),
            (b"name".to_vec(), name_id),
            (b"nickname".to_vec(), nickname_id),
        ],
    );

    let nil_id = arena.push_nil();
    let array_id = arena.push_array(&[nil_id, actor_id]);
    arena.set_root(array_id);

    marshal_rs::dump(&arena)
}

/// Reads `content`, translates the given `nickname` source line to
/// `translation`, and writes it back - returning the rebuilt bytes.
fn translate_nickname(content: &[u8], nickname: &str, translation: &str) -> Vec<u8> {
    let mut read_base = Base::new(Mode::read(), EngineType::VXAce);
    let data = read_base
        .process_other("Actors.rvdata2", content, None)
        .expect("read should not fail")
        .expect("actors list is non-empty, so there should be output");
    let text = String::from_utf8(data.as_ref().to_vec()).unwrap();

    let source_line = format!("{nickname}<#>");
    assert!(
        text.contains(&source_line),
        "expected an untranslated {source_line:?} line in:\n{text}"
    );
    let translation_line = format!("{nickname}<#>{translation}");
    let translation_text = text.replace(&source_line, &translation_line);

    let mut write_base = Base::new(Mode::Write, EngineType::VXAce);
    let rewritten = write_base
        .process_other("Actors.rvdata2", content, Some(&translation_text))
        .expect("write should not fail")
        .expect("a translation was applied, so there should be output bytes");

    rewritten.as_ref().to_vec()
}

#[test]
fn declared_encoding_is_trusted_over_a_guess_or_override_on_read() {
    let (windows_1251_bytes, _, _) = encoding_rs::WINDOWS_1251.encode("Иван");
    let content = actors_with_encoded_nickname(&windows_1251_bytes, b"Windows-1251");

    // Force a codepage that would mis-decode these bytes, to prove the
    // declared ivar wins over it rather than being ignored.
    let mut base = Base::new(Mode::read(), EngineType::VXAce);
    base.set_read_encoding(Some(encoding_rs::SHIFT_JIS));

    let data = base
        .process_other("Actors.rvdata2", &content, None)
        .expect("read should not fail")
        .expect("actors list is non-empty, so there should be output");

    let text = String::from_utf8(data.as_ref().to_vec()).unwrap();
    assert!(
        text.contains("Иван"),
        "expected the Windows-1251-decoded name in output:\n{text}"
    );
}

#[test]
fn write_defaults_to_utf8_regardless_of_the_source_declared_encoding() {
    let (windows_1251_bytes, _, _) = encoding_rs::WINDOWS_1251.encode("Иван");
    let content = actors_with_encoded_nickname(&windows_1251_bytes, b"Windows-1251");

    // Translated into Japanese - a script Windows-1251 cannot represent at
    // all, so a "preserve the source encoding" write would have corrupted
    // this into numeric character references instead of failing loudly.
    let rewritten_bytes = translate_nickname(&content, "Иван", "山田");

    let reloaded = marshal_rs::load(&rewritten_bytes).unwrap();
    let root = marshal_rs::value::ValueRef::root(&reloaded);
    let nickname = root
        .at(1)
        .expect("actor entry")
        .get("nickname")
        .expect("nickname field");

    // Tagged UTF-8 - not left declaring Windows-1251 (impossible for this
    // text), and not left untagged/implicitly ASCII-8BIT either, which would
    // risk `Encoding::CompatibilityError` when VX Ace's own scripts
    // concatenate it against a real UTF-8 string at runtime.
    assert_eq!(nickname.encoding_name(), Some(b"UTF-8".as_slice()));
    assert_eq!(nickname.as_str(), Some("山田"));
}

#[test]
fn write_encoding_is_independent_of_read_encoding() {
    let (windows_1251_bytes, _, _) = encoding_rs::WINDOWS_1251.encode("Иван");
    let content = actors_with_encoded_nickname(&windows_1251_bytes, b"Windows-1251");

    let mut read_base = Base::new(Mode::read(), EngineType::VXAce);
    let data = read_base
        .process_other("Actors.rvdata2", &content, None)
        .unwrap()
        .unwrap();
    let text = String::from_utf8(data.as_ref().to_vec()).unwrap();
    let translation_text = text.replace("Иван<#>", "Иван<#>Jean");

    // Forcing a *read* codepage that would mis-decode Windows-1251 bytes,
    // alongside a *write* codepage the translation actually fits in, proves
    // the two settings don't leak into each other.
    let mut write_base = Base::new(Mode::Write, EngineType::VXAce);
    write_base.set_read_encoding(Some(encoding_rs::SHIFT_JIS));
    write_base.set_write_encoding(Some(encoding_rs::WINDOWS_1252));

    let rewritten = write_base
        .process_other("Actors.rvdata2", &content, Some(&translation_text))
        .unwrap()
        .unwrap();

    let rewritten_bytes = rewritten.as_ref().to_vec();
    let reloaded = marshal_rs::load(&rewritten_bytes).unwrap();
    let root = marshal_rs::value::ValueRef::root(&reloaded);
    let nickname = root.at(1).unwrap().get("nickname").unwrap();

    assert_eq!(nickname.encoding_name(), Some(b"windows-1252".as_slice()));

    let (decoded, _, had_errors) = encoding_rs::WINDOWS_1252.decode(nickname.as_bytes().unwrap());
    assert!(!had_errors);
    assert_eq!(decoded, "Jean");
}
