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

use serde::{Deserialize, Serialize};
use std::error::Error;

#[cfg(any(feature = "serde-xlsx", feature = "serde-xml"))]
use std::io::Cursor;

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "lowercase")]
enum Entry {
    Comment {
        text: String,
    },
    Translation {
        source: String,
        translations: Vec<String>,
    },
}

fn parse_entries(content: &str) -> Vec<Entry> {
    let mut entries = Vec::new();
    let mut rest = content;

    loop {
        match rest.find("<!--") {
            Some(start) => {
                process_text_segment(&rest[..start], &mut entries);
                match rest[start..].find("-->") {
                    Some(end_rel) => {
                        let end = start + end_rel + 3;
                        entries.push(Entry::Comment {
                            text: rest[start..end].to_string(),
                        });
                        rest = &rest[end..];
                    }
                    None => {
                        entries.push(Entry::Comment {
                            text: rest[start..].to_string(),
                        });
                        break;
                    }
                }
            }
            None => {
                process_text_segment(rest, &mut entries);
                break;
            }
        }
    }

    entries
}

fn process_text_segment(segment: &str, entries: &mut Vec<Entry>) {
    for raw_line in segment.split('\n') {
        let line = raw_line.strip_suffix('\r').unwrap_or(raw_line);
        if line.trim().is_empty() {
            continue;
        }
        let mut parts = line.split("<#>");
        let source = parts.next().unwrap_or("").to_string();
        let translations: Vec<String> = parts.map(|s| s.to_string()).collect();
        entries.push(Entry::Translation {
            source,
            translations,
        });
    }
}

fn entries_to_content(entries: &[Entry]) -> String {
    entries
        .iter()
        .map(|e| match e {
            Entry::Comment { text } => text.clone(),
            Entry::Translation {
                source,
                translations,
            } => {
                let mut parts = vec![source.clone()];
                parts.extend(translations.iter().cloned());
                parts.join("<#>")
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

/// CSV schema:
/// Header row: type,source,translation_count,translation_1..translation_N
/// - type: "comment" | "translation"
/// - For type="comment": source column holds the comment text; translation_count=0; translation_* empty
/// - For type="translation": source column holds the source string; translation_count=len(translations);
///   translation_1..translation_N hold each translation in order
#[cfg(feature = "serde-csv")]
pub fn export_csv(content: &str) -> Result<String, Box<dyn Error>> {
    let entries = parse_entries(content);
    let max_t = max_translations(&entries);

    let mut wtr = csv::WriterBuilder::new().from_writer(vec![]);

    let mut header = vec![
        "type".to_string(),
        "source".to_string(),
        "translation_count".to_string(),
    ];
    header.extend((1..=max_t).map(|i| format!("translation_{i}")));
    wtr.write_record(&header)?;

    for entry in &entries {
        let mut record = Vec::with_capacity(header.len());
        match entry {
            Entry::Comment { text } => {
                record.push("comment".to_string());
                record.push(text.clone());
                record.push("0".to_string());
            }
            Entry::Translation {
                source,
                translations,
            } => {
                record.push("translation".to_string());
                record.push(source.clone());
                record.push(translations.len().to_string());
                record.extend(translations.iter().cloned());
            }
        }
        record.resize(header.len(), String::new());
        wtr.write_record(&record)?;
    }

    Ok(String::from_utf8(wtr.into_inner()?)?)
}

#[cfg(feature = "serde-csv")]
pub fn import_csv(csv_content: &str) -> Result<String, Box<dyn Error>> {
    let mut rdr = csv::ReaderBuilder::new().from_reader(csv_content.as_bytes());
    let mut entries = Vec::new();

    for result in rdr.records() {
        let record = result?;
        let type_field = record.get(0).unwrap_or("");
        let source_or_text = record.get(1).unwrap_or("").to_string();
        let count: usize = record.get(2).unwrap_or("0").parse().unwrap_or(0);

        if type_field == "comment" {
            entries.push(Entry::Comment {
                text: source_or_text,
            });
        } else {
            let translations = (0..count)
                .map(|i| record.get(3 + i).unwrap_or("").to_string())
                .collect();
            entries.push(Entry::Translation {
                source: source_or_text,
                translations,
            });
        }
    }

    Ok(entries_to_content(&entries))
}

/// XLSX schema: identical layout to CSV (see `export_csv`), written to sheet 1,
/// row 0 = header, bold. Row order corresponds to entry order.
#[cfg(feature = "serde-xlsx")]
pub fn export_xlsx(content: &str) -> Result<Vec<u8>, Box<dyn Error>> {
    let entries = parse_entries(content);
    let max_t = max_translations(&entries);

    let mut workbook = Workbook::new();
    let worksheet = workbook.add_worksheet();
    let bold = Format::new().set_bold();

    worksheet.write_string_with_format(0, 0, "type", &bold)?;
    worksheet.write_string_with_format(0, 1, "source", &bold)?;
    worksheet.write_string_with_format(0, 2, "translation_count", &bold)?;
    for i in 0..max_t {
        worksheet.write_string_with_format(
            0,
            (3 + i) as u16,
            format!("translation_{}", i + 1),
            &bold,
        )?;
    }

    for (row_idx, entry) in entries.iter().enumerate() {
        let row = (row_idx + 1) as u32;
        match entry {
            Entry::Comment { text } => {
                worksheet.write_string(row, 0, "comment")?;
                worksheet.write_string(row, 1, text)?;
                worksheet.write_string(row, 2, "0")?;
            }
            Entry::Translation {
                source,
                translations,
            } => {
                worksheet.write_string(row, 0, "translation")?;
                worksheet.write_string(row, 1, source)?;
                worksheet.write_string(
                    row,
                    2,
                    translations.len().to_string(),
                )?;
                for (i, t) in translations.iter().enumerate() {
                    worksheet.write_string(row, (3 + i) as u16, t)?;
                }
            }
        }
    }

    Ok(workbook.save_to_buffer()?)
}

#[cfg(feature = "serde-xlsx")]
pub fn import_xlsx(xlsx_bytes: &[u8]) -> Result<String, Box<dyn Error>> {
    let cursor = Cursor::new(xlsx_bytes.to_vec());
    let mut workbook: Xlsx<_> = open_workbook_from_rs(cursor)?;

    let sheet_name = workbook
        .sheet_names()
        .get(0)
        .cloned()
        .ok_or("no worksheet found")?;
    let range = workbook.worksheet_range(&sheet_name)?;

    let mut entries = Vec::new();
    for row in range.rows().skip(1) {
        if row.is_empty() {
            continue;
        }
        let type_field = row[0].to_string();
        let source_or_text =
            row.get(1).map(|c| c.to_string()).unwrap_or_default();
        let count: usize = row
            .get(2)
            .map(|c| c.to_string())
            .unwrap_or_default()
            .parse()
            .unwrap_or(0);

        if type_field == "comment" {
            entries.push(Entry::Comment {
                text: source_or_text,
            });
        } else {
            let translations = (0..count)
                .map(|i| {
                    row.get(3 + i).map(|c| c.to_string()).unwrap_or_default()
                })
                .collect();
            entries.push(Entry::Translation {
                source: source_or_text,
                translations,
            });
        }
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
#[cfg(feature = "serde-xml")]
pub fn export_xml(content: &str) -> Result<String, Box<dyn Error>> {
    let entries = parse_entries(content);
    let mut writer =
        XmlWriter::new_with_indent(Cursor::new(Vec::new()), b' ', 2);

    writer.write_event(Event::Start(BytesStart::new("entries")))?;

    for entry in &entries {
        match entry {
            Entry::Comment { text } => {
                writer.write_event(Event::Start(BytesStart::new("comment")))?;
                writer.write_event(Event::CData(BytesCData::new(
                    text.as_str(),
                )))?;
                writer.write_event(Event::End(BytesEnd::new("comment")))?;
            }
            Entry::Translation {
                source,
                translations,
            } => {
                writer.write_event(Event::Start(BytesStart::new("entry")))?;

                writer.write_event(Event::Start(BytesStart::new("source")))?;
                writer.write_event(Event::Text(BytesText::new(source)))?;
                writer.write_event(Event::End(BytesEnd::new("source")))?;

                writer.write_event(Event::Start(BytesStart::new(
                    "translations",
                )))?;
                for t in translations {
                    writer.write_event(Event::Start(BytesStart::new(
                        "translation",
                    )))?;
                    writer.write_event(Event::Text(BytesText::new(t)))?;
                    writer.write_event(Event::End(BytesEnd::new(
                        "translation",
                    )))?;
                }
                writer
                    .write_event(Event::End(BytesEnd::new("translations")))?;

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

#[cfg(feature = "serde-xml")]
pub fn import_xml(xml_content: &str) -> Result<String, Box<dyn Error>> {
    let mut reader = XmlReader::from_str(xml_content);
    reader.config_mut().trim_text(true);

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
                b"comment" => {
                    in_comment = true;
                    comment_text.clear();
                }
                b"entry" => {
                    current_source = None;
                    current_translations = Vec::new();
                }
                b"source" => {
                    in_source = true;
                    current_source = Some(String::new());
                }
                b"translation" => {
                    in_translation = true;
                    current_translations.push(String::new());
                }
                _ => {}
            },
            Event::Empty(e) => match e.name().as_ref() {
                b"source" => current_source = Some(String::new()),
                b"translation" => current_translations.push(String::new()),
                b"comment" => entries.push(Entry::Comment {
                    text: String::new(),
                }),
                _ => {}
            },
            Event::Text(t) => {
                let text = unescape(&t.decode()?)?.into_owned();
                if in_source {
                    current_source = Some(text);
                } else if in_translation {
                    if let Some(last) = current_translations.last_mut() {
                        *last = text;
                    }
                }
            }
            Event::CData(t) => {
                if in_comment {
                    comment_text =
                        String::from_utf8(t.into_inner().into_owned())?;
                }
            }
            Event::End(e) => match e.name().as_ref() {
                b"comment" => {
                    in_comment = false;
                    entries.push(Entry::Comment {
                        text: std::mem::take(&mut comment_text),
                    });
                }
                b"source" => in_source = false,
                b"translation" => in_translation = false,
                b"entry" => {
                    if let Some(source) = current_source.take() {
                        entries.push(Entry::Translation {
                            source,
                            translations: std::mem::take(
                                &mut current_translations,
                            ),
                        });
                    }
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
/// [
///   {"type":"comment","text":"..."},
///   {"type":"translation","source":"...","translations":["...","..."]}
/// ]
/// Array order corresponds to entry order. Matches `Entry` serde representation.
pub fn export_json(content: &str) -> Result<String, Box<dyn Error>> {
    let entries = parse_entries(content);
    Ok(serde_json::to_string_pretty(&entries)?)
}

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
#[cfg(feature = "serde-yaml")]
pub fn export_yaml(content: &str) -> Result<String, Box<dyn Error>> {
    let entries = parse_entries(content);
    Ok(yaml_serde::to_string(&entries)?)
}

#[cfg(feature = "serde-yaml")]
pub fn import_yaml(yaml_content: &str) -> Result<String, Box<dyn Error>> {
    let entries: Vec<Entry> = yaml_serde::from_str(yaml_content)?;
    Ok(entries_to_content(&entries))
}
