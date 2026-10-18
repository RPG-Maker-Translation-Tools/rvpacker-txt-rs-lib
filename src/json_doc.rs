//! A JSON document for MV/MZ data files, built for how this crate uses them: read most of a file once, replace a
//! few strings, write it back.
//!
//! Nodes live in one flat list, the same arena shape as `marshal-rs`. A string is a range of the source bytes
//! unless it holds an escape, and numbers stay unparsed until asked for, so parsing a file allocates a handful of
//! vectors rather than one heap object per value, and dropping it frees only those. Writing copies the source and
//! splices the replaced values in, so every byte nothing touched stays as it was.

use serde::de::Error as _;
use std::fmt::Write as _;

pub type NodeId = u32;

/// MZ writes its data files with one.
const UTF8_BOM: &[u8] = &[0xEF, 0xBB, 0xBF];

#[derive(Clone, Copy, Eq, PartialEq)]
enum Kind {
    Null,
    True,
    False,
    Number,
    /// Text is the source between the quotes.
    Str,
    /// Text held an escape; index into `JsonDoc::unescaped`.
    Escaped(u32),
    /// Overwritten; index into `JsonDoc::replacements`.
    Replaced(u32),
    Array,
    Object,
}

/// For a scalar, `start..end` is its whole span in the source, quotes included. For a container, `start` is where
/// its children begin in `JsonDoc::children` and `end` how many there are - key/value pairs count twice.
#[derive(Clone, Copy)]
struct Node {
    start: u32,
    end: u32,
    kind: Kind,
}

pub struct JsonDoc {
    /// Validated as UTF-8 by [`JsonDoc::parse`].
    source: Vec<u8>,
    nodes: Vec<Node>,
    children: Vec<NodeId>,
    unescaped: Vec<String>,
    replacements: Vec<(NodeId, String)>,
    root: NodeId,
}

struct Frame {
    node: NodeId,
    /// Where this container's children start on the scratch stack.
    mark: usize,
    is_object: bool,
}

struct Parser<'a> {
    bytes: &'a [u8],
    pos: usize,
    nodes: Vec<Node>,
    children: Vec<NodeId>,
    unescaped: Vec<String>,
    /// Children of the open containers, innermost last.
    scratch: Vec<NodeId>,
    stack: Vec<Frame>,
}

fn error(message: &str, pos: usize) -> serde_json::Error {
    serde_json::Error::custom(format_args!("{message} at byte {pos}"))
}

impl Parser<'_> {
    fn skip_whitespace(&mut self) {
        while let Some(b' ' | b'\n' | b'\r' | b'\t') = self.bytes.get(self.pos) {
            self.pos += 1;
        }
    }

    fn peek(&self) -> Option<u8> {
        self.bytes.get(self.pos).copied()
    }

    fn push(&mut self, kind: Kind, start: usize, end: usize) -> NodeId {
        let id = self.nodes.len() as NodeId;
        // Both fit: `JsonDoc::parse` refuses sources longer than `u32::MAX`.
        self.nodes.push(Node {
            start: start as u32,
            end: end as u32,
            kind,
        });
        id
    }

    fn expect(&mut self, byte: u8, what: &str) -> Result<(), serde_json::Error> {
        self.skip_whitespace();

        if self.peek() != Some(byte) {
            return Err(error(what, self.pos));
        }

        self.pos += 1;
        Ok(())
    }

    fn literal(&mut self, word: &[u8], kind: Kind) -> Result<NodeId, serde_json::Error> {
        let start = self.pos;

        if !self.bytes[start..].starts_with(word) {
            return Err(error("invalid literal", start));
        }

        self.pos += word.len();
        Ok(self.push(kind, start, self.pos))
    }

    fn number(&mut self) -> Result<NodeId, serde_json::Error> {
        let start = self.pos;
        let digits = |parser: &mut Self| {
            let from = parser.pos;

            while parser.peek().is_some_and(|b| b.is_ascii_digit()) {
                parser.pos += 1;
            }

            parser.pos > from
        };

        if self.peek() == Some(b'-') {
            self.pos += 1;
        }

        if self.peek() == Some(b'0') {
            self.pos += 1;
        } else if !digits(self) {
            return Err(error("invalid number", start));
        }

        if self.peek() == Some(b'.') {
            self.pos += 1;

            if !digits(self) {
                return Err(error("invalid number", start));
            }
        }

        if let Some(b'e' | b'E') = self.peek() {
            self.pos += 1;

            if let Some(b'+' | b'-') = self.peek() {
                self.pos += 1;
            }

            if !digits(self) {
                return Err(error("invalid number", start));
            }
        }

        Ok(self.push(Kind::Number, start, self.pos))
    }

    fn hex4(&mut self) -> Result<u32, serde_json::Error> {
        let hex = self
            .bytes
            .get(self.pos..self.pos + 4)
            .ok_or_else(|| error("truncated escape", self.pos))?;
        // ASCII hex digits are valid UTF-8; anything else fails the parse below.
        let value = std::str::from_utf8(hex)
            .ok()
            .and_then(|hex| u32::from_str_radix(hex, 16).ok())
            .filter(|_| hex.iter().all(u8::is_ascii_hexdigit))
            .ok_or_else(|| error("invalid escape", self.pos))?;

        self.pos += 4;
        Ok(value)
    }

    fn string(&mut self) -> Result<NodeId, serde_json::Error> {
        let start = self.pos;
        self.pos += 1;

        // The common case: no escape, so the text is the source itself.
        loop {
            match self.peek() {
                Some(b'"') => {
                    self.pos += 1;
                    return Ok(self.push(Kind::Str, start, self.pos));
                }
                Some(b'\\') => break,
                Some(0..0x20) => return Err(error("control character in string", self.pos)),
                Some(_) => self.pos += 1,
                None => return Err(error("unterminated string", start)),
            }
        }

        // SAFETY: `JsonDoc::parse` validated the whole source as UTF-8, and `start + 1..self.pos` begins after an
        // ASCII quote and ends before an ASCII backslash, so both ends are character boundaries.
        let mut text = unsafe { std::str::from_utf8_unchecked(&self.bytes[start + 1..self.pos]) }.to_owned();

        loop {
            match self.peek() {
                Some(b'"') => break,
                Some(b'\\') => {
                    self.pos += 1;
                    let escaped = self.peek().ok_or_else(|| error("truncated escape", self.pos))?;
                    self.pos += 1;

                    let char = match escaped {
                        b'"' => '"',
                        b'\\' => '\\',
                        b'/' => '/',
                        b'b' => '\u{8}',
                        b'f' => '\u{c}',
                        b'n' => '\n',
                        b'r' => '\r',
                        b't' => '\t',
                        b'u' => {
                            let high = self.hex4()?;

                            let code = if (0xD800..0xDC00).contains(&high) {
                                if !self.bytes[self.pos..].starts_with(b"\\u") {
                                    return Err(error("lone surrogate", self.pos));
                                }

                                self.pos += 2;
                                let low = self.hex4()?;

                                if !(0xDC00..0xE000).contains(&low) {
                                    return Err(error("invalid surrogate pair", self.pos));
                                }

                                0x10000 + ((high - 0xD800) << 10) + (low - 0xDC00)
                            } else {
                                high
                            };

                            char::from_u32(code).ok_or_else(|| error("invalid escape", self.pos))?
                        }
                        _ => return Err(error("invalid escape", self.pos)),
                    };

                    text.push(char);
                }
                Some(0..0x20) => return Err(error("control character in string", self.pos)),
                Some(_) => {
                    // Copy the run up to the next quote or escape in one go.
                    let run_start = self.pos;

                    while self.peek().is_some_and(|b| b != b'"' && b != b'\\' && b >= 0x20) {
                        self.pos += 1;
                    }

                    // SAFETY: as above - the run sits between ASCII bytes of a validated UTF-8 source.
                    text.push_str(unsafe { std::str::from_utf8_unchecked(&self.bytes[run_start..self.pos]) });
                }
                None => return Err(error("unterminated string", start)),
            }
        }

        self.pos += 1;
        let index = self.unescaped.len() as u32;
        self.unescaped.push(text);
        Ok(self.push(Kind::Escaped(index), start, self.pos))
    }

    /// Reads an object's next key and the colon after it, leaving the parser before the value.
    fn key(&mut self) -> Result<(), serde_json::Error> {
        self.skip_whitespace();

        if self.peek() != Some(b'"') {
            return Err(error("expected a key", self.pos));
        }

        let key = self.string()?;
        self.scratch.push(key);
        self.expect(b':', "expected ':'")
    }

    fn close(&mut self) -> NodeId {
        // A container is only closed while one is open.
        let frame = self.stack.pop().expect("an open container");
        let start = self.children.len();
        self.children.extend(self.scratch.drain(frame.mark..));

        let node = &mut self.nodes[frame.node as usize];
        node.start = start as u32;
        node.end = (self.children.len() - start) as u32;
        frame.node
    }

    /// Iterative rather than recursive, so deeply nested input can't overflow the stack.
    #[allow(clippy::type_complexity)]
    fn parse(mut self) -> Result<(Vec<Node>, Vec<NodeId>, Vec<String>, NodeId), serde_json::Error> {
        let mut finished: Option<NodeId> = None;

        let root = loop {
            let id = if let Some(id) = finished.take() {
                id
            } else {
                self.skip_whitespace();

                match self.peek() {
                    Some(open @ (b'{' | b'[')) => {
                        let is_object = open == b'{';
                        let node = self.push(if is_object { Kind::Object } else { Kind::Array }, 0, 0);
                        self.pos += 1;
                        self.stack.push(Frame {
                            node,
                            mark: self.scratch.len(),
                            is_object,
                        });
                        self.skip_whitespace();

                        if self.peek() == Some(if is_object { b'}' } else { b']' }) {
                            self.pos += 1;
                            finished = Some(self.close());
                        } else if is_object {
                            self.key()?;
                        }

                        continue;
                    }
                    Some(b'"') => self.string()?,
                    Some(b't') => self.literal(b"true", Kind::True)?,
                    Some(b'f') => self.literal(b"false", Kind::False)?,
                    Some(b'n') => self.literal(b"null", Kind::Null)?,
                    Some(b'-' | b'0'..=b'9') => self.number()?,
                    _ => return Err(error("expected a value", self.pos)),
                }
            };

            let Some(frame) = self.stack.last() else {
                break id;
            };

            let is_object = frame.is_object;
            self.scratch.push(id);
            self.skip_whitespace();

            match self.peek() {
                Some(b',') => {
                    self.pos += 1;

                    if is_object {
                        self.key()?;
                    }
                }
                Some(b'}') if is_object => {
                    self.pos += 1;
                    finished = Some(self.close());
                }
                Some(b']') if !is_object => {
                    self.pos += 1;
                    finished = Some(self.close());
                }
                _ => return Err(error("expected ',' or a closing bracket", self.pos)),
            }
        };

        self.skip_whitespace();

        if self.pos != self.bytes.len() {
            return Err(error("trailing characters", self.pos));
        }

        Ok((self.nodes, self.children, self.unescaped, root))
    }
}

/// Writes `text` as a JSON string, escaping what JSON requires and nothing else, like `JSON.stringify`.
fn write_string(output: &mut Vec<u8>, text: &str) {
    output.push(b'"');
    let mut run_start = 0;

    for (index, byte) in text.bytes().enumerate() {
        let escape: &[u8] = match byte {
            b'"' => b"\\\"",
            b'\\' => b"\\\\",
            b'\n' => b"\\n",
            b'\r' => b"\\r",
            b'\t' => b"\\t",
            0x08 => b"\\b",
            0x0C => b"\\f",
            0..0x20 => b"",
            _ => continue,
        };

        output.extend_from_slice(&text.as_bytes()[run_start..index]);

        if escape.is_empty() {
            let mut code = String::with_capacity(6);
            let _ = write!(code, "\\u{byte:04x}");
            output.extend_from_slice(code.as_bytes());
        } else {
            output.extend_from_slice(escape);
        }

        run_start = index + 1;
    }

    output.extend_from_slice(&text.as_bytes()[run_start..]);
    output.push(b'"');
}

impl JsonDoc {
    /// # Errors
    ///
    /// - [`serde_json::Error`] - if `source` is not UTF-8 or not valid JSON.
    pub(crate) fn parse(source: Vec<u8>) -> Result<Self, serde_json::Error> {
        if source.len() > u32::MAX as usize {
            return Err(error("file too large", 0));
        }

        if let Err(utf8_error) = std::str::from_utf8(&source) {
            return Err(error("invalid UTF-8", utf8_error.valid_up_to()));
        }

        // Sized for the usual file; a bigger one grows from here. Capped, so a huge input can't ask for gigabytes
        // before a single node exists.
        let capacity = (source.len() / 8).min(1 << 22);

        let parser = Parser {
            bytes: &source,
            // A byte order mark stays in the source, so the file is written back with it.
            pos: if source.starts_with(UTF8_BOM) {
                UTF8_BOM.len()
            } else {
                0
            },
            nodes: Vec::with_capacity(capacity),
            children: Vec::with_capacity(capacity),
            unescaped: Vec::new(),
            scratch: Vec::new(),
            stack: Vec::new(),
        };

        let (nodes, children, unescaped, root) = parser.parse()?;

        Ok(Self {
            source,
            nodes,
            children,
            unescaped,
            replacements: Vec::new(),
            root,
        })
    }

    pub(crate) const fn root(&self) -> NodeId {
        self.root
    }

    fn node(&self, id: NodeId) -> Node {
        self.nodes[id as usize]
    }

    fn span(&self, node: Node) -> &[u8] {
        &self.source[node.start as usize..node.end as usize]
    }

    pub(crate) fn is_null(&self, id: NodeId) -> bool {
        self.node(id).kind == Kind::Null
    }

    pub(crate) fn as_bool(&self, id: NodeId) -> Option<bool> {
        match self.node(id).kind {
            Kind::True => Some(true),
            Kind::False => Some(false),
            _ => None,
        }
    }

    /// An integer number, like `serde_json::Value::as_i64`: [`None`] for fractions and exponents.
    pub(crate) fn as_i64(&self, id: NodeId) -> Option<i64> {
        let node = self.node(id);

        if node.kind != Kind::Number {
            return None;
        }

        // SAFETY: a number's span is ASCII.
        unsafe { std::str::from_utf8_unchecked(self.span(node)) }.parse().ok()
    }

    pub(crate) fn as_str(&self, id: NodeId) -> Option<&str> {
        let node = self.node(id);

        match node.kind {
            // SAFETY: the source is validated UTF-8, and a string's span starts and ends with ASCII quotes.
            Kind::Str => Some(unsafe {
                std::str::from_utf8_unchecked(&self.source[node.start as usize + 1..node.end as usize - 1])
            }),
            Kind::Escaped(index) => Some(&self.unescaped[index as usize]),
            Kind::Replaced(index) => Some(&self.replacements[index as usize].1),
            _ => None,
        }
    }

    pub(crate) fn is_array(&self, id: NodeId) -> bool {
        self.node(id).kind == Kind::Array
    }

    /// An array's length; 0 for anything else.
    pub(crate) fn len(&self, id: NodeId) -> usize {
        let node = self.node(id);

        if node.kind == Kind::Array { node.end as usize } else { 0 }
    }

    pub(crate) fn at(&self, id: NodeId, index: usize) -> Option<NodeId> {
        let node = self.node(id);

        (node.kind == Kind::Array && index < node.end as usize).then(|| self.children[node.start as usize + index])
    }

    /// An object's members as `(key, value)`; nothing for anything else.
    pub(crate) fn members(&self, id: NodeId) -> impl Iterator<Item = (&str, NodeId)> {
        let node = self.node(id);
        let pairs = if node.kind == Kind::Object {
            &self.children[node.start as usize..(node.start + node.end) as usize]
        } else {
            &[]
        };

        pairs
            .as_chunks::<2>()
            .0
            .iter()
            .map(|pair| (self.as_str(pair[0]).unwrap_or_default(), pair[1]))
    }

    pub(crate) fn member(&self, id: NodeId, name: &str) -> Option<NodeId> {
        self.members(id).find(|(key, _)| *key == name).map(|(_, value)| value)
    }

    /// Replaces a scalar with a string, spliced into the source on [`JsonDoc::into_bytes`].
    ///
    /// # Panics
    ///
    /// Panics if `id` is an array or object: this crate only ever replaces text.
    pub(crate) fn set_string(&mut self, id: NodeId, text: String) {
        let node = &mut self.nodes[id as usize];

        match node.kind {
            Kind::Array | Kind::Object => panic!("cannot replace a container with a string"),
            Kind::Replaced(index) => self.replacements[index as usize].1 = text,
            _ => {
                node.kind = Kind::Replaced(self.replacements.len() as u32);
                self.replacements.push((id, text));
            }
        }
    }

    /// The source with every replacement spliced in. Without replacements, the source itself.
    pub(crate) fn into_bytes(mut self) -> Vec<u8> {
        if self.replacements.is_empty() {
            return self.source;
        }

        // Spans start in document order, and replaced scalars never overlap.
        self.replacements
            .sort_unstable_by_key(|(id, _)| self.nodes[*id as usize].start);

        let added: usize = self.replacements.iter().map(|(_, text)| text.len() + 2).sum();
        let mut output = Vec::with_capacity(self.source.len() + added);
        let mut copied = 0;

        for (id, text) in &self.replacements {
            let node = self.nodes[*id as usize];
            output.extend_from_slice(&self.source[copied..node.start as usize]);
            write_string(&mut output, text);
            copied = node.end as usize;
        }

        output.extend_from_slice(&self.source[copied..]);
        output
    }
}

#[cfg(test)]
mod tests {
    use super::{JsonDoc, NodeId};
    use proptest::{collection::btree_map, prelude::*, test_runner::Config as ProptestConfig};
    use serde_json::Value;

    // Scalars, arrays and objects. Floats are left out, since `JsonDoc` has no accessor for them.
    fn json_value() -> impl Strategy<Value = Value> {
        let leaf = prop_oneof![
            Just(Value::Null),
            any::<bool>().prop_map(Value::Bool),
            any::<i64>().prop_map(Value::from),
            any::<String>().prop_map(Value::String),
        ];

        leaf.prop_recursive(4, 48, 4, |inner| {
            prop_oneof![
                proptest::collection::vec(inner.clone(), 0..4).prop_map(Value::Array),
                btree_map(any::<String>(), inner, 0..4).prop_map(|map| Value::Object(map.into_iter().collect())),
            ]
        })
    }

    // `JsonDoc` gives no kind for an object, and `members` is empty for every node that isn't one, so an object is any
    // node that is neither a scalar nor an array.
    fn is_object(doc: &JsonDoc, id: NodeId) -> bool {
        !doc.is_null(id)
            && !doc.is_array(id)
            && doc.as_bool(id).is_none()
            && doc.as_i64(id).is_none()
            && doc.as_str(id).is_none()
    }

    // Whether the node at `id` reads back as `value`, through the same accessors the rest of the crate uses.
    fn holds(doc: &JsonDoc, id: NodeId, value: &Value) -> bool {
        match value {
            Value::Null => doc.is_null(id),
            Value::Bool(expected) => doc.as_bool(id) == Some(*expected),
            Value::Number(number) => doc.as_i64(id) == number.as_i64(),
            Value::String(expected) => doc.as_str(id) == Some(expected.as_str()),
            Value::Array(items) => {
                doc.is_array(id)
                    && doc.len(id) == items.len()
                    && items
                        .iter()
                        .enumerate()
                        .all(|(index, item)| doc.at(id, index).is_some_and(|child| holds(doc, child, item)))
            }
            Value::Object(map) => {
                if !is_object(doc, id) {
                    return false;
                }

                let mut members = doc.members(id);
                let matches = map.iter().all(|(key, item)| {
                    members
                        .next()
                        .is_some_and(|(name, child)| name == key.as_str() && holds(doc, child, item))
                });
                matches && members.next().is_none()
            }
        }
    }

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(if cfg!(miri) { 4 } else { 256 }))]

        // Every document `serde_json` writes reads back as the value it was written from.
        #[test]
        fn parser_reads_what_serde_json_writes(value in json_value()) {
            let text = serde_json::to_vec(&value).unwrap();
            let doc = JsonDoc::parse(text).unwrap();
            prop_assert!(holds(&doc, doc.root(), &value));
        }
    }

    #[test]
    fn reads_and_splices_without_touching_the_rest() {
        let source = br#"{ "a": [1, -2.5e3, true, null], "b" : "x\"y\u00e9\ud83d\ude00", "c": "plain" }"#;
        let mut doc = JsonDoc::parse(source.to_vec()).unwrap();
        let root = doc.root();

        let a = doc.member(root, "a").unwrap();
        assert_eq!(doc.len(a), 4);
        assert_eq!(doc.as_i64(doc.at(a, 0).unwrap()), Some(1));
        assert_eq!(doc.as_i64(doc.at(a, 1).unwrap()), None);
        assert_eq!(doc.as_bool(doc.at(a, 2).unwrap()), Some(true));
        assert!(doc.is_null(doc.at(a, 3).unwrap()));
        assert_eq!(doc.as_str(doc.member(root, "b").unwrap()), Some("x\"yé😀"));

        let c = doc.member(root, "c").unwrap();
        doc.set_string(c, "new \"line\"\n".to_owned());
        assert_eq!(doc.as_str(c), Some("new \"line\"\n"));

        let written = String::from_utf8(doc.into_bytes()).unwrap();
        assert_eq!(
            written,
            r#"{ "a": [1, -2.5e3, true, null], "b" : "x\"y\u00e9\ud83d\ude00", "c": "new \"line\"\n" }"#
        );
    }

    #[test]
    fn rejects_malformed_input() {
        for bad in [
            &b"[1,]"[..],
            b"{\"a\" 1}",
            b"[\"\x01\"]",
            b"tru",
            b"[1] 2",
            b"\"\\ud800\"",
            b"01",
            b"[",
            b"{",
        ] {
            assert!(
                JsonDoc::parse(bad.to_vec()).is_err(),
                "{:?} parsed",
                String::from_utf8_lossy(bad)
            );
        }
    }

    #[test]
    fn deep_nesting_does_not_overflow() {
        let source = [vec![b'['; 1_000_000], vec![b']'; 1_000_000]].concat();
        assert!(JsonDoc::parse(source).is_ok());
    }
}
