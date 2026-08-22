# rvpacker-txt-rs-lib v15.0.0

A large release. The API surface is smaller, the comment syntax in the
translation files has changed, per-game special handling is gone, and the crate
has an integration test suite for the first time — which turned up a dozen more
bugs, listed below.

**This release is not source-compatible with v14, and its translation files are
not compatible either.** Read [Migrating](#migrating) before upgrading.

---

## Highlights

- **One `Processor` instead of `Reader`, `Writer`, `Purger` and their builders.**
  A plain struct with public fields; `mode` decides what it does.
- **`<!>` replaces `<!-- ... -->`** as the metadata marker in `.txt` files,
  `.rvpacker-ignore` files and the `json` module's Ruby output.
- **Game-specific handling is gone.** What used to be hardcoded for Fear &
  Hunger 2: Termina now lives in a `.rvpacker-ignore` file, which understands
  glob patterns. What used to be attributed to a game is now attributed to the
  plugin it actually comes from.
- **`core::text` is public**, so a GUI or CLI can parse the generated files with
  the same functions that wrote them.
- **168 integration tests** over bundled default projects, one per engine.

---

## Breaking changes

### `Processor` replaces `Reader`, `Writer` and `Purger`

The three structs, their builders and their duplicated option setters are one
struct with public fields and one method.

```rust
// v14
let mut reader = ReaderBuilder::new()
    .with_files(FileFlags::Map | FileFlags::other())
    .read_mode(ReadMode::Default { force: true })
    .build();
reader.read("C:/Game/Data", "C:/Game/translation", EngineType::VXAce)?;

// v15
let mut processor = Processor {
    mode: Mode::Read { append: false, force: true },
    file_flags: FileFlags::Map | FileFlags::other(),
    ..Default::default()
};
processor.process(EngineType::VXAce, "C:/Game/Data", "C:/Game/translation", None)?;
```

`process` takes the output directory as a fourth argument; it is only used by
`Mode::Write` and may be `None` otherwise.

### `Mode` absorbed `ReadMode`

```rust
pub enum Mode {
    Read { append: bool, force: bool },
    Write,
    Purge,
}
```

`Mode::read()` is a plain read. `is_default`, `is_append`, `is_default_default`
and `is_append_default` cover the combinations the old enum had names for.

**The `u8` encoding changed**: reads occupy `0..=3` (bit 0 `force`, bit 1
`append`), `Write` is `4`, `Purge` is `5`. The old encoding gave `Write` the
value `3`, which a forced append read also produced — that combination could not
survive a round trip. Anything that persisted a mode as a byte has to be
re-encoded.

### The comment marker is `<!>`

Metadata lines used to be wrapped in `<!-- ... -->`. The closing marker was
almost never checked, and RPG Maker plugin markup does not use HTML-like tags,
so the opening marker alone now separates metadata from text. There is no
trailing space; the label starts immediately.

```text
<!--  ID  --><#>1              →  <!>ID<#>1
<!-- NAME --><#>Potion         →  <!>NAME<#>Potion
<!-- Ignore Entry --><#>Items: 1  →  <!>Ignore Entry<#>Items: 1
```

The `json` module's Ruby output changed with it, and its script header now uses
the ordinary separator rather than a comma:

```text
<!-- SCRIPT: 75819864, Game_Temp -->   →   <!>SCRIPT<#>75819864<#>Game_Temp
```

Existing `.txt` and `.rb` files need converting. A search and replace over the
five markers is enough for `.txt`; regenerate `.rb` files from the game data.

### Game-specific processing is gone

`GameType` and every `game_type` parameter are removed. Two things replace
them:

- **Plugin markup is recognised as plugin markup.** Yanfly's
  `<Menu Category: …>` notetags and the VX Ace Advanced Text System's `\et[n]`,
  `\nbt` and `\nblt` dialogue prefixes were attributed to the games they were
  found in. They are handled unconditionally now, keyed off the markup and the
  engine that can run the plugin.
- **Everything else belongs in `.rvpacker-ignore`.** Lines you do not want
  extracted go in that file, which now understands glob patterns:

  ```text
  <!>Ignore Entry<#>Items: 1
  Torch
  <!>Glob<#>The Fellowship*
  <!>Glob<#>*soul
  ```

  `*` matches any run of characters, `?` exactly one; everything else is
  literal. `examples/termina.rvpacker-ignore` is a worked example covering the
  unused items, classes, enemies, armors and weapons the crate used to filter in
  code.

  One rule cannot be expressed: Termina's actor filter was an allowlist
  (translate these eighteen names, drop every other actor), and an ignore file
  can only deny. Leaving it out costs a few extra entries to skip past.

### `core` is a module tree, and `Base` does the work directly

`core.rs` is now `core/`, and the per-file-kind wrapper structs are gone. `Base`
exposes one method per kind:

```rust
let mut base = Base::new(Mode::read(), EngineType::VXAce);

base.process_other("Actors.rvdata2", &content, None)?;
base.process_system(&content, None)?;
base.process_scripts(&content, None)?;
base.process_plugins(&content, None)?;

// Maps share one translation file, so they run as a batch.
base.begin_maps();
base.process_map("Map001.rvdata2", &content, &mapinfos, None)?;
let translation = base.finish_maps();
```

The `generic` module is removed.

### `core::text` is public

`CustomReplace` (`normalize` / `denormalize`), `split_translation_line`,
`TranslationLine`, `string_is_only_symbols`, `ends_with_if_index` and
`latinize_string` are public, so anything else reading the generated files can
round-trip a line exactly the way this crate does.

`core::latinize_string` moved to `core::text::latinize_string`.

### Smaller changes

- `FileFlags` gained a function per flag — `map()`, `actors()`, `system()` and
  so on — so a single kind reads the same at a call site as the `other()` group.
  `other()` is now a `const fn`.
- `EngineType`, `FileFlags` and `BaseFlags` implement `PartialEq` and `Eq`. None
  of them could be compared before.
- `Labels::currency_unit` is engine-varying (see the fixes below).
- `Base::set_game_title` is no longer a no-op outside `Mode::Read`.

---

## Bug fixes

### Data loss and corruption

- **Purged entries never matched on the next read.** Purging collects its ignore
  entries at the end of the run, by which point the selected entry was whichever
  id happened to be processed last — so every purged line landed in the final
  section of `.rvpacker-ignore` and nothing matched afterwards. The file was
  effectively inert.
- **Appending a translated file emitted a translation with no source.** Event
  headers travel through the pipeline as an entry with an empty source; the code
  that wrote them out did not check for that, so it fired for whatever entry
  came first and wrote its translation on a line of its own — while the real
  entry lost it.
- **Appending a map read with `map_events` lost every translation past the first
  event.** A map is flushed once per event, and only the first of those blocks
  carried the translations parsed for the map.
- **Fields with trailing whitespace could not be written back.** The write path
  trimmed every line of a source before looking it up; the read path only trims
  messages, or everything under `BaseFlags::Trim`. The keys never matched. VX
  Ace's default actor biographies are affected.
- **Plugin strings containing a line break could not be written back.** The
  plugin writer looked its translation up by the normalized form (`\#`) while
  the parsed keys had been denormalized (`\n`). The default MV plugin set has
  several.
- **The currency unit was never extracted on MV/MZ.** `Labels::currency_unit`
  was in the invariant set as `currency_unit`, but MV and MZ spell it
  `currencyUnit`; and the extraction skipped those engines outright even so.
- **A game title passed for a write pass was dropped.** `set_game_title` stored
  the title only in read mode, which left the write branch unreachable even
  though `Processor` passes the title on every mode.
- **Comments never round-tripped through the `generic` module**, which
  reassigned its accumulator at the top of every iteration. That module is gone.
- **`.rvpacker-ignore` only ever matched its first section.** The first header in
  a file kept its marker in the key while every later one lost it. A header
  without an `: id` suffix also read past the end of the string.
- **`Mode::Write` and a forced append read encoded to the same byte**, so a
  forced append round-tripped back as a write.

### `json` module

- **`write_file` dumped without the instance-variable prefix it loaded with**, so
  every `@name` came back as `name` — a game handed such a file finds none of the
  fields it expects, and loading it again strips a further character.
- **A script's name grew one leading space per round trip**, because the reader
  kept the space that the writer puts after the header's separator. The header
  format changed, so the ambiguity is gone.

### `serde` module

- **Every format still spoke the old comment syntax.** `parse_entries` scanned
  for `<!-- ... -->`, so metadata came through as translatable rows, and any game
  text containing `<!--` swallowed everything up to the next `-->` — turning one
  line into two on the way back.
- **The XML importer dropped text around entity references.** The reader splits a
  run of text at every `&…;`, and the importer replaced its buffer on each piece
  instead of appending, so only the text after the last `&lt;` survived. Any `<`
  in game text — a Yanfly notetag, for instance — was mangled. Text is also no
  longer trimmed, which was eating meaningful leading spaces.

### Memory safety

Every unsound construct the crate carried is gone:

- Three fields were initialized as `&mut *(16 as *mut T)` and dereferenced
  before assignment on some paths. They are indices now.
- One `Box::leak` leaked for the process's lifetime; a matching
  `Box::from_raw` freed its buffer at the end of the function that owned it,
  leaving a dangling field behind.
- All seventeen uses of a `mutable!` macro that cast `&T` to `&mut T` are
  removed, and the macro with them.

### Performance

- **Writing with `DuplicateMode::Remove` was quadratic.** Every key lookup
  linearly scanned every parsed map. The maps are flattened once into a write
  lookup — entries moved, not cloned — making it a single hash lookup.
- Filename parsing, ignore-key building and the per-entry comment vector no
  longer allocate per call.

---

## Tests

The crate ships five default projects — `tests/RMXP`, `tests/RMVX`,
`tests/RMVXACE`, `tests/RMMV`, `tests/RMMZ` — and 168 tests over them:

- `tests/lifecycle.rs` — fourteen scenarios per engine, driving `Processor`
  through the filesystem: plain read, reproducibility, append after deletion,
  purge into an ignore file and back, event headers, write-then-reread, duplicate
  removal, plugins, `Game.ini` titles, `Trim`, `SkipObsolete`, `skip_events`,
  `skip_maps`, and the content-hash skip.
- `tests/json.rs`, `tests/serde.rs` — round trips through both modules.
- `tests/roundtrip.rs` — the `Base` lifecycle over synthetic MV/MZ files.
- `tests/text.rs`, `tests/format.rs` — the format's primitives.

Run them with `cargo test`, or `cargo test --all-features` to include the CSV,
XLSX, XML and YAML exporters.

---

## Requirements

- **marshal-rs 2.0.2 or newer.** 2.0.1 wrote a positive length byte for negative
  integers in `-256..=-124`, so they loaded back 256 too high — RPG Maker XP's
  and VX's default skills carry `-150` and `-250` for healing power. Writing a
  project with 2.0.1 corrupts them.
- Rust 1.87.

---

## Migrating

1. Replace `Reader`/`Writer`/`Purger` and their builders with `Processor`, and
   set `mode`.
2. Re-encode any persisted `Mode` byte.
3. Convert existing `.txt` files: `<!-- ` → `<!>` and drop ` -->` on the five
   metadata markers. Regenerate any `.rb` files produced by the `json` module.
4. Drop `GameType` arguments. If you relied on the Termina filtering, copy
   `examples/termina.rvpacker-ignore` into the translation directory and set
   `BaseFlags::Ignore`.
5. Rename `core::latinize_string` to `core::text::latinize_string`.
6. Bump marshal-rs to 2.0.2.
