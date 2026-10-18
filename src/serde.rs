//! This module provides functions for serializing/deserializing (exporting/importing)
//! the translation text files into other structured formats.

#[cfg(feature = "serde-xlsx")]
use calamine::{Reader as CalamineReader, Xlsx, open_workbook_from_rs};
#[cfg(feature = "serde-xml")]
use quick_xml::{
    escape::unescape,
    events::{BytesCData, BytesEnd, BytesStart, BytesText, Event},
    reader::Reader as XmlReader,
    writer::Writer as XmlWriter,
};
#[cfg(feature = "serde-xlsx")]
use rust_xlsxwriter::{Format, Workbook};

use crate::{get_comment_prefix, get_line_separator};
use serde::{Deserialize, Serialize};
use std::error::Error;

#[cfg(any(feature = "serde-xlsx", feature = "serde-xml"))]
use std::io::Cursor;

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "type", rename_all = "lowercase")]
enum Entry {
    Comment { text: String },
    Translation { source: String, translations: Vec<String> },
}

/// Splits a translation file into entries.
///
/// A line carrying the comment marker is metadata - an id, a name, a map's
/// displayed name - and is kept whole; every other line is a
/// `source{SEP}translation` row.
fn parse_entries(content: &str) -> Vec<Entry> {
    let mut entries = Vec::new();

    let mut lines = content.split('\n').peekable();
    while let Some(raw_line) = lines.next() {
        // Only a `\r` followed by `\n` ends a line. The last line has no `\n`, so its `\r` is text, and
        // `entries_to_content` writes it back unchanged.
        let line = if lines.peek().is_some() {
            raw_line.strip_suffix('\r').unwrap_or(raw_line)
        } else {
            raw_line
        };

        if line.trim().is_empty() {
            continue;
        }

        if line.starts_with(get_comment_prefix()) {
            entries.push(Entry::Comment { text: line.to_string() });
            continue;
        }

        let mut parts = line.split(get_line_separator());
        let source = parts.next().unwrap_or("").to_string();
        let translations: Vec<String> = parts.map(std::string::ToString::to_string).collect();

        entries.push(Entry::Translation { source, translations });
    }

    entries
}

fn entries_to_content(entries: &[Entry]) -> String {
    entries
        .iter()
        .map(|e| match e {
            Entry::Comment { text } => text.clone(),
            Entry::Translation { source, translations } => {
                let mut parts = vec![source.clone()];
                parts.extend(translations.iter().cloned());
                parts.join(get_line_separator())
            }
        })
        .collect::<Vec<_>>()
        .join("\n")
}

#[cfg(any(feature = "serde-xlsx", feature = "serde-csv"))]
fn max_translations(entries: &[Entry]) -> usize {
    entries
        .iter()
        .filter_map(|e| match e {
            Entry::Translation { translations, .. } => Some(translations.len()),
            Entry::Comment { .. } => None,
        })
        .max()
        .unwrap_or(0)
}

/// The header row shared by CSV and XLSX: the three fixed columns, then one `translation_N` per translation.
#[cfg(any(feature = "serde-csv", feature = "serde-xlsx"))]
fn header_names(max_t: usize) -> Vec<String> {
    let mut header = vec![
        "type".to_string(),
        "source".to_string(),
        "translation_count".to_string(),
    ];
    header.extend((1..=max_t).map(|i| format!("translation_{i}")));
    header
}

/// Builds one entry from a spreadsheet row. `available` is how many translation cells the row has, and `translation(i)`
/// reads the `i`-th of them. A count that isn't a number is an error, since reading it as zero would drop the row's
/// translations.
#[cfg(any(feature = "serde-csv", feature = "serde-xlsx"))]
fn row_entry(
    kind: &str,
    source: String,
    count: &str,
    available: usize,
    translation: impl Fn(usize) -> String,
) -> Result<Entry, Box<dyn Error>> {
    if kind == "comment" {
        return Ok(Entry::Comment { text: source });
    }

    let count: usize = count
        .parse()
        .map_err(|_| format!("the translation_count of `{source}` is not a number: `{count}`"))?;

    // Capped by the cells the row has: a mistyped count must not allocate that many strings.
    let translations = (0..count.min(available)).map(translation).collect();
    Ok(Entry::Translation { source, translations })
}

/// Whether a spreadsheet would read a cell holding `text` as a formula: it starts with `=`, `+`, `-`, `@`, a tab or a
/// carriage return, after any number of `'`.
#[cfg(feature = "serde-csv")]
fn is_formula_like(text: &str) -> bool {
    text.trim_start_matches('\'')
        .starts_with(['=', '+', '-', '@', '\t', '\r'])
}

/// `text` as a CSV cell: a formula-like one gets a `'` in front, which spreadsheets show as text instead of running.
#[cfg(feature = "serde-csv")]
fn guard_cell(text: &str) -> String {
    if is_formula_like(text) {
        format!("'{text}")
    } else {
        text.to_owned()
    }
}

/// [`guard_cell`] undone. Only a `'` that [`guard_cell`] would have added is removed.
#[cfg(feature = "serde-csv")]
fn unguard_cell(cell: &str) -> String {
    match cell.strip_prefix('\'') {
        Some(rest) if is_formula_like(rest) => rest.to_owned(),
        _ => cell.to_owned(),
    }
}
/// CSV schema:
///
/// ```text
/// Header row: type,source,translation_count,translation_1..translation_N
/// - type: "comment" | "translation"
/// - For type="comment": source column holds the comment text; translation_count=0; translation_* empty
/// - For type="translation": source column holds the source string; translation_count=len(translations);
///   translation_1..translation_N hold each translation in order
/// - Text cells that a spreadsheet would read as a formula (starting with `=`, `+`, `-`, `@`, a tab or a carriage
///   return, after any number of `'`) get a `'` in front, which `import_csv` removes again.
/// ```
///
/// # Errors
///
/// Returns an error if the CSV writer fails.
#[cfg(feature = "serde-csv")]
pub fn export_csv(content: &str) -> Result<String, Box<dyn Error>> {
    let entries = parse_entries(content);
    let max_t = max_translations(&entries);

    let mut wtr = csv::WriterBuilder::new().from_writer(vec![]);

    let header = header_names(max_t);
    wtr.write_record(&header)?;

    for entry in &entries {
        let mut record = Vec::with_capacity(header.len());
        match entry {
            Entry::Comment { text } => {
                record.push("comment".to_string());
                record.push(guard_cell(text));
                record.push("0".to_string());
            }
            Entry::Translation { source, translations } => {
                record.push("translation".to_string());
                record.push(guard_cell(source));
                record.push(translations.len().to_string());
                record.extend(translations.iter().map(|translation| guard_cell(translation)));
            }
        }
        record.resize(header.len(), String::new());
        wtr.write_record(&record)?;
    }

    Ok(String::from_utf8(wtr.into_inner()?)?)
}

/// Converts CSV in the [`export_csv`] layout back into translation text.
///
/// # Errors
///
/// Returns an error if a row can't be read as CSV.
#[cfg(feature = "serde-csv")]
pub fn import_csv(csv_content: &str) -> Result<String, Box<dyn Error>> {
    let mut rdr = csv::ReaderBuilder::new().from_reader(csv_content.as_bytes());
    let mut entries = Vec::new();

    for result in rdr.records() {
        let record = result?;
        let kind = record.get(0).unwrap_or("");
        let source = unguard_cell(record.get(1).unwrap_or(""));
        let count = record.get(2).unwrap_or("");
        let available = record.len().saturating_sub(3);

        entries.push(row_entry(kind, source, count, available, |i| {
            unguard_cell(record.get(3 + i).unwrap_or(""))
        })?);
    }

    Ok(entries_to_content(&entries))
}

/// XLSX schema: identical layout to CSV (see `export_csv`), written to sheet 1,
/// row 0 = header, bold. Row order corresponds to entry order.
///
/// # Errors
///
/// Returns an error if the workbook can't be built or saved.
#[cfg(feature = "serde-xlsx")]
pub fn export_xlsx(content: &str) -> Result<Vec<u8>, Box<dyn Error>> {
    let entries = parse_entries(content);
    let max_t = max_translations(&entries);

    let mut workbook = Workbook::new();
    let worksheet = workbook.add_worksheet();
    let bold = Format::new().set_bold();

    for (column, name) in header_names(max_t).iter().enumerate() {
        worksheet.write_string_with_format(0, column as u16, name, &bold)?;
    }

    for (row_idx, entry) in entries.iter().enumerate() {
        let row = (row_idx + 1) as u32;
        match entry {
            Entry::Comment { text } => {
                worksheet.write_string(row, 0, "comment")?;
                worksheet.write_string(row, 1, text)?;
                worksheet.write_string(row, 2, "0")?;
            }
            Entry::Translation { source, translations } => {
                worksheet.write_string(row, 0, "translation")?;
                worksheet.write_string(row, 1, source)?;
                worksheet.write_string(row, 2, translations.len().to_string())?;
                for (i, t) in translations.iter().enumerate() {
                    worksheet.write_string(row, (3 + i) as u16, t)?;
                }
            }
        }
    }

    Ok(workbook.save_to_buffer()?)
}

/// Converts an XLSX workbook in the [`export_xlsx`] layout back into translation text.
///
/// # Errors
///
/// Returns an error if `xlsx_bytes` isn't a readable workbook, or it has no sheets.
#[cfg(feature = "serde-xlsx")]
pub fn import_xlsx(xlsx_bytes: &[u8]) -> Result<String, Box<dyn Error>> {
    let cursor = Cursor::new(xlsx_bytes.to_vec());
    let mut workbook: Xlsx<_> = open_workbook_from_rs(cursor)?;

    let sheet_name = workbook.sheet_names().first().cloned().ok_or("no worksheet found")?;
    let range = workbook.worksheet_range(&sheet_name)?;

    let mut entries = Vec::new();
    for row in range.rows().skip(1) {
        if row.is_empty() {
            continue;
        }
        let kind = row[0].to_string();
        let source = row.get(1).map(ToString::to_string).unwrap_or_default();
        let count = row.get(2).map(ToString::to_string).unwrap_or_default();
        let available = row.len().saturating_sub(3);

        entries.push(row_entry(&kind, source, &count, available, |i| {
            row.get(3 + i).map(ToString::to_string).unwrap_or_default()
        })?);
    }

    Ok(entries_to_content(&entries))
}

/// XML schema:
/// <entries>
///   <comment><![CDATA[ ... ]]></comment>
///   <entry>
///     <source>...</source>
///     <translations>
///       <translation>...</translation>*
///     </translations>
///   </entry>
///   ...
/// </entries>
/// Element order corresponds to entry order.
///
/// # Errors
///
/// Returns an error if the XML can't be written.
#[cfg(feature = "serde-xml")]
pub fn export_xml(content: &str) -> Result<String, Box<dyn Error>> {
    let entries = parse_entries(content);
    let mut writer = XmlWriter::new_with_indent(Cursor::new(Vec::new()), b' ', 2);

    writer.write_event(Event::Start(BytesStart::new("entries")))?;

    for entry in &entries {
        match entry {
            Entry::Comment { text } => {
                writer.write_event(Event::Start(BytesStart::new("comment")))?;
                // A CDATA section ends at the first `]]>`, so one inside the text is split across two sections.
                let mut rest = text.as_str();

                while let Some(at) = rest.find("]]>") {
                    writer.write_event(Event::CData(BytesCData::new(&rest[..at + 2])))?;
                    rest = &rest[at + 2..];
                }

                writer.write_event(Event::CData(BytesCData::new(rest)))?;
                writer.write_event(Event::End(BytesEnd::new("comment")))?;
            }
            Entry::Translation { source, translations } => {
                writer.write_event(Event::Start(BytesStart::new("entry")))?;

                writer.write_event(Event::Start(BytesStart::new("source")))?;
                writer.write_event(Event::Text(BytesText::new(source)))?;
                writer.write_event(Event::End(BytesEnd::new("source")))?;

                writer.write_event(Event::Start(BytesStart::new("translations")))?;
                for t in translations {
                    writer.write_event(Event::Start(BytesStart::new("translation")))?;
                    writer.write_event(Event::Text(BytesText::new(t)))?;
                    writer.write_event(Event::End(BytesEnd::new("translation")))?;
                }
                writer.write_event(Event::End(BytesEnd::new("translations")))?;

                writer.write_event(Event::End(BytesEnd::new("entry")))?;
            }
        }
    }

    writer.write_event(Event::End(BytesEnd::new("entries")))?;

    let bytes = writer.into_inner().into_inner();
    Ok(format!(
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n{}",
        String::from_utf8(bytes)?
    ))
}

/// Appends a piece of text to whichever element is open, or to the comment.
#[cfg(feature = "serde-xml")]
fn push_text(
    text: &str,
    in_comment: bool,
    comment_text: &mut String,
    in_source: bool,
    in_translation: bool,
    source: &mut Option<String>,
    translations: &mut [String],
) {
    if in_comment {
        comment_text.push_str(text);
    } else if in_source {
        source.get_or_insert_default().push_str(text);
    } else if in_translation && let Some(last) = translations.last_mut() {
        last.push_str(text);
    }
}

/// Converts XML in the [`export_xml`] layout back into translation text.
///
/// # Errors
///
/// Returns an error if `xml_content` is malformed.
#[cfg(feature = "serde-xml")]
pub fn import_xml(xml_content: &str) -> Result<String, Box<dyn Error>> {
    let mut reader = XmlReader::from_str(xml_content);

    let mut entries: Vec<Entry> = Vec::new();
    let mut buf = Vec::new();

    let mut in_comment = false;
    let mut comment_text = String::new();

    let mut current_source: Option<String> = None;
    let mut current_translations: Vec<String> = Vec::new();
    let mut in_source = false;
    let mut in_translation = false;

    loop {
        match reader.read_event_into(&mut buf)? {
            Event::Start(e) => match e.name().as_ref() {
                "comment" => {
                    in_comment = true;
                    comment_text.clear();
                }
                "entry" => {
                    current_source = None;
                    current_translations = Vec::new();
                }
                "source" => {
                    in_source = true;
                    current_source = Some(String::new());
                }
                "translation" => {
                    in_translation = true;
                    current_translations.push(String::new());
                }
                _ => {}
            },
            Event::Empty(e) => match e.name().as_ref() {
                "source" => current_source = Some(String::new()),
                "translation" => current_translations.push(String::new()),
                "comment" => entries.push(Entry::Comment { text: String::new() }),
                _ => {}
            },
            // The reader splits a run of text at every entity reference, so
            // the pieces are appended rather than assigned - assigning kept
            // only the piece after the last `&lt;`, which quietly ate the
            // comment marker and any game text holding a `<`.
            Event::Text(t) => {
                push_text(
                    &unescape(&t)?,
                    in_comment,
                    &mut comment_text,
                    in_source,
                    in_translation,
                    &mut current_source,
                    &mut current_translations,
                );
            }
            Event::GeneralRef(reference) => {
                let resolved = match reference.resolve_char_ref()? {
                    Some(char) => char.to_string(),
                    None => unescape(&format!("&{name};", name = &*reference))?.into_owned(),
                };

                push_text(
                    &resolved,
                    in_comment,
                    &mut comment_text,
                    in_source,
                    in_translation,
                    &mut current_source,
                    &mut current_translations,
                );
            }
            Event::CData(t) => {
                if in_comment {
                    comment_text.push_str(&t.into_inner());
                }
            }
            Event::End(e) => match e.name().as_ref() {
                "comment" => {
                    in_comment = false;
                    entries.push(Entry::Comment {
                        text: std::mem::take(&mut comment_text),
                    });
                }
                "source" => in_source = false,
                "translation" => in_translation = false,
                "entry" => {
                    let source = current_source.take().ok_or("an <entry> has no <source>")?;
                    entries.push(Entry::Translation {
                        source,
                        translations: std::mem::take(&mut current_translations),
                    });
                }
                _ => {}
            },
            Event::Eof => break,
            _ => {}
        }
        buf.clear();
    }

    Ok(entries_to_content(&entries))
}

/// JSON schema: array of entries, tagged by "type":
///
/// ```text
/// [
///   {"type":"comment","text":"..."},
///   {"type":"translation","source":"...","translations":["...","..."]}
/// ]
/// ```
///
/// Array order corresponds to entry order. Matches `Entry` serde representation.
///
/// # Errors
///
/// Returns an error if the entries can't be serialized.
pub fn export_json(content: &str) -> Result<String, Box<dyn Error>> {
    let entries = parse_entries(content);
    Ok(serde_json::to_string_pretty(&entries)?)
}

/// Parses the JSON that [`export_json`] writes back into translation text.
///
/// # Errors
///
/// Returns an error if `json_content` doesn't match the entry schema.
pub fn import_json(json_content: &str) -> Result<String, Box<dyn Error>> {
    let entries: Vec<Entry> = serde_json::from_str(json_content)?;
    Ok(entries_to_content(&entries))
}

/// YAML schema: same structure as JSON (see `export_json`), serialized as YAML:
/// - type: comment
///   text: "..."
/// - type: translation
///   source: "..."
///   translations:
///     - "..."
///
/// # Errors
///
/// Returns an error if the entries can't be serialized.
#[cfg(feature = "serde-yaml")]
pub fn export_yaml(content: &str) -> Result<String, Box<dyn Error>> {
    let entries = parse_entries(content);
    Ok(yaml_serde::to_string(&entries)?)
}

/// Converts YAML in the [`export_yaml`] layout back into translation text.
///
/// # Errors
///
/// Returns an error if `yaml_content` doesn't match the entry schema.
#[cfg(feature = "serde-yaml")]
pub fn import_yaml(yaml_content: &str) -> Result<String, Box<dyn Error>> {
    let entries: Vec<Entry> = yaml_serde::from_str(yaml_content)?;
    Ok(entries_to_content(&entries))
}
