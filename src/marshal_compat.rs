//! A thin cursor that lets the rest of this crate read and mutate RPG Maker
//! data - JSON for MV/MZ, Marshal for everything older - through one shared
//! set of operations, without copying the whole document.
//!
//! Marshal is handled through marshal-rs v3's `Arena` directly: `load`
//! returns an `Arena` borrowing from the input buffer; [`RpgmData::from_marshal`]
//! promotes it to `Arena<'static>` (cheap - it only detaches string/blob
//! payloads from the input buffer, not a rebuild of the node graph) so it can
//! live in [`RpgmData`] across a whole `process_*` call. From there,
//! [`Value::Marshal`] mutates that *same* arena in place - replacing a
//! `String`/`Bytes` node at an existing array/object slot - via the
//! `set_array_*`/`set_member_*` methods. Nothing is copied except the handful of
//! strings actually translated.
//!
//! Every mutation this crate performs is a scalar leaf replacement.

use crate::types::EngineType;
use marshal_rs::{
    Kind, ReadError,
    arena::{Arena, ValueId},
    load as marshal_load,
    value::ValueRef,
};
use serde_json::Value as JsonValue;

/// Owns a whole parsed RPG Maker document - either a native JSON tree (MV/MZ)
/// or a Marshal `Arena` (everything older). Cheap to build (see the module
/// docs), and dumped back with [`RpgmData::into_bytes`].
pub(crate) enum RpgmData {
    Json(JsonValue),
    Marshal(Arena<'static>),
}

impl RpgmData {
    pub(crate) fn from_json(json: JsonValue) -> Self {
        Self::Json(json)
    }

    pub(crate) fn from_marshal(bytes: &[u8]) -> Result<Self, ReadError> {
        Ok(Self::Marshal(marshal_load(bytes)?.into_owned()))
    }

    /// A cursor over the document root, for reading and/or mutating.
    pub(crate) fn root(&mut self) -> Value<'_> {
        match self {
            Self::Json(json) => Value::Json(json),
            Self::Marshal(arena) => {
                let id = arena.root();
                Value::Marshal {
                    arena,
                    id,
                    slot: Slot::Root,
                }
            }
        }
    }

    #[must_use]
    pub(crate) fn is_json(&self) -> bool {
        matches!(self, Self::Json(_))
    }

    /// Serializes the document back to its wire format - `serde_json::to_vec`
    /// for JSON, `marshal_rs::dump` for Marshal.
    pub(crate) fn into_bytes(self) -> Vec<u8> {
        match self {
            // SAFETY: every `JsonValue` this crate builds or loads is valid
            // UTF-8 JSON - `serde_json::to_vec` only fails on a writer error
            // or a non-string map key, neither possible for `Vec<u8>`/`Value`.
            Self::Json(json) => unsafe { serde_json::to_vec(&json).unwrap_unchecked() },
            Self::Marshal(arena) => marshal_rs::dump(&arena),
        }
    }
}

/// How to overwrite a [`Value::Marshal`] cursor's own slot, if it's ever
/// assigned to - i.e. which parent array/object it came from. Not needed for
/// [`Value::Json`]: a real `&mut serde_json::Value` can already be assigned
/// through directly.
#[derive(Clone)]
pub(crate) enum Slot {
    Array {
        parent: ValueId,
        index: usize,
    },
    Member {
        parent: ValueId,
        name: String,
    },
    /// The document root, or a value read in a context that never writes
    /// back (e.g. an event pulled out of a Marshal `Hash` for its metadata
    /// only) - assigning to one of these is a logic error, so it panics
    /// rather than silently doing nothing.
    Root,
}

/// A cursor onto one value inside a loaded [`RpgmData`] - either a live
/// `&mut serde_json::Value` node, or an `(arena, id)` pair into a Marshal
/// `Arena` plus enough provenance ([`Slot`]) to overwrite its own slot.
///
/// Navigation (`member`/`at`/`for_each_*`) takes `&mut self` uniformly, even
/// when only reading - the underlying access is trivial either way, and one
/// method shape means callers don't need to know which mode they're in.
pub(crate) enum Value<'r> {
    Json(&'r mut JsonValue),
    Marshal {
        arena: &'r mut Arena<'static>,
        id: ValueId,
        slot: Slot,
    },
}

/// Classifies a Marshal `Str`/`Bytes` node the same way as the rest of this
/// crate's text handling: text if the raw bytes happen to validate as UTF-8
/// (regardless of whether an encoding ivar was declared), opaque bytes
/// otherwise. `Other` covers every non-text `Kind`.
enum TextKind<'r> {
    String(&'r str),
    Bytes(&'r [u8]),
    Other,
}

/// Reads a Marshal `Str`/`Bytes` value as text, the same way [`Value`] does:
/// by UTF-8 validity of the raw bytes, not by whether an encoding ivar was
/// declared. Exposed for code that reads `Arena` data directly (mapinfos,
/// `Scripts.*`) without going through the [`Value`] cursor.
#[must_use]
pub(crate) fn marshal_as_text<'r>(v: ValueRef<'r, 'static>) -> Option<&'r str> {
    match v.kind() {
        Kind::Str | Kind::Bytes => core::str::from_utf8(v.as_bytes().unwrap_or_default()).ok(),
        _ => None,
    }
}

fn marshal_text_kind<'r>(arena: &'r Arena<'static>, id: ValueId) -> TextKind<'r> {
    let v = ValueRef::new(arena, id);
    match v.kind() {
        Kind::Str | Kind::Bytes => {
            let bytes = v.as_bytes().unwrap_or_default();
            match core::str::from_utf8(bytes) {
                Ok(s) => TextKind::String(s),
                Err(_) => TextKind::Bytes(bytes),
            }
        }
        _ => TextKind::Other,
    }
}

impl Value<'_> {
    #[must_use]
    pub(crate) fn is_null(&self) -> bool {
        match self {
            Self::Json(v) => v.is_null(),
            Self::Marshal { arena, id, .. } => ValueRef::new(arena, *id).is_nil(),
        }
    }

    #[must_use]
    pub(crate) fn as_int(&self) -> Option<i32> {
        match self {
            Self::Json(v) => v.as_i64().map(|n| n as i32),
            Self::Marshal { arena, id, .. } => ValueRef::new(arena, *id).as_i64().map(|n| n as i32),
        }
    }

    #[must_use]
    pub(crate) fn as_bool(&self) -> Option<bool> {
        match self {
            Self::Json(v) => v.as_bool(),
            Self::Marshal { arena, id, .. } => ValueRef::new(arena, *id).as_bool(),
        }
    }

    #[must_use]
    pub(crate) fn as_str(&self) -> Option<&str> {
        match self {
            Self::Json(v) => v.as_str(),
            Self::Marshal { arena, id, .. } => match marshal_text_kind(arena, *id) {
                TextKind::String(s) => Some(s),
                _ => None,
            },
        }
    }

    /// Takes ownership of this value's text content without cloning it,
    /// leaving `null`/an empty string behind - sound only when nothing reads
    /// this node again afterward (read-mode extraction, which never
    /// re-serializes the source tree). Mirrors [`Value::as_str`]'s
    /// UTF-8-validity classification; returns [`None`] for anything else
    /// (non-string, or Marshal bytes that aren't valid UTF-8 - those still
    /// go through [`Base::extract_string`](crate::core::Base) and its
    /// decode-with-fallback path, which already produces an owned `String`
    /// with no extra copy to avoid).
    pub(crate) fn take_str(&mut self) -> Option<String> {
        match self {
            Self::Json(v) => {
                if !v.is_string() {
                    return None;
                }

                match core::mem::take::<JsonValue>(v) {
                    JsonValue::String(s) => Some(s),
                    _ => unreachable!(),
                }
            }
            Self::Marshal { arena, id, .. } => {
                if !matches!(marshal_text_kind(arena, *id), TextKind::String(_)) {
                    return None;
                }

                let bytes = arena.take_bytes_content(*id);
                // SAFETY: `marshal_text_kind` just validated these exact bytes as UTF-8.
                Some(unsafe { String::from_utf8_unchecked(bytes) })
            }
        }
    }

    /// The raw bytes of a value classified as opaque (non-UTF-8) text. Only
    /// meaningful for Marshal - JSON has no such concept.
    #[must_use]
    pub(crate) fn as_byte_vec(&self) -> Option<&[u8]> {
        match self {
            Self::Json(_) => None,
            Self::Marshal { arena, id, .. } => match marshal_text_kind(arena, *id) {
                TextKind::Bytes(b) => Some(b),
                _ => None,
            },
        }
    }

    /// The Ruby encoding name this value's `E`/`encoding` ivar declared, if
    /// one was present at load time - `Kind::Str` per `marshal-rs`. `None`
    /// for JSON (no such concept), and for `Kind::Bytes` - Ruby 1.8-era
    /// Marshal (XP/VX) never wrote the ivar at all, so there is nothing to
    /// probe and the caller should fall back to guessing/[`Base::set_read_encoding`]
    /// instead. This is deliberately independent of whether the bytes
    /// happen to validate as UTF-8 - [`Value::as_str`]/[`Value::as_byte_vec`]
    /// classify by that; this classifies by what the file actually declared.
    #[must_use]
    pub(crate) fn declared_encoding(&self) -> Option<&[u8]> {
        match self {
            Self::Json(_) => None,
            Self::Marshal { arena, id, .. } => {
                let v = ValueRef::new(arena, *id);
                (v.kind() == Kind::Str).then(|| v.encoding_name()).flatten()
            }
        }
    }

    #[must_use]
    pub(crate) fn is_string(&self) -> bool {
        match self {
            Self::Json(v) => v.is_string(),
            Self::Marshal { arena, id, .. } => matches!(marshal_text_kind(arena, *id), TextKind::String(_)),
        }
    }

    #[must_use]
    pub(crate) fn is_bytes(&self) -> bool {
        match self {
            Self::Json(_) => false,
            Self::Marshal { arena, id, .. } => matches!(marshal_text_kind(arena, *id), TextKind::Bytes(_)),
        }
    }

    /// Whether this value is array- or hash-shaped - used only to bail out
    /// early when a field that's supposed to hold a list turns out not to
    /// (e.g. a map's `events` field being `false` instead of an array).
    #[must_use]
    pub(crate) fn is_container(&self) -> bool {
        match self {
            Self::Json(v) => v.is_array(),
            Self::Marshal { arena, id, .. } => matches!(ValueRef::new(arena, *id).kind(), Kind::Array | Kind::Hash),
        }
    }

    /// Looks up an object field/instance variable by name - with or without
    /// a leading `@` on the Marshal side, matching how RPG Maker's own ivar
    /// names are declared.
    #[must_use]
    pub(crate) fn member(&mut self, name: &str) -> Option<Value<'_>> {
        match self {
            Self::Json(v) => v.get_mut(name).map(Value::Json),
            Self::Marshal { arena, id, .. } => {
                let child = ValueRef::new(arena, *id).get(name)?.id();
                Some(Value::Marshal {
                    arena,
                    id: child,
                    slot: Slot::Member {
                        parent: *id,
                        name: name.to_owned(),
                    },
                })
            }
        }
    }

    /// Array element by index.
    #[must_use]
    pub(crate) fn at(&mut self, index: usize) -> Option<Value<'_>> {
        match self {
            Self::Json(v) => v.get_mut(index).map(Value::Json),
            Self::Marshal { arena, id, .. } => {
                let child = ValueRef::new(arena, *id).at(index)?.id();
                Some(Value::Marshal {
                    arena,
                    id: child,
                    slot: Slot::Array { parent: *id, index },
                })
            }
        }
    }

    #[must_use]
    pub(crate) fn len(&self) -> usize {
        match self {
            Self::Json(v) => v.as_array().map_or(0, Vec::len),
            Self::Marshal { arena, id, .. } => ValueRef::new(arena, *id).len(),
        }
    }

    /// Replaces this value's own slot with a UTF-8 string.
    pub(crate) fn set_string(&mut self, text: String) {
        match self {
            Self::Json(v) => **v = JsonValue::String(text),
            Self::Marshal { arena, id, slot } => {
                let new_id = match slot {
                    Slot::Array { parent, index } => arena.set_array_string(*parent, *index, text),
                    Slot::Member { parent, name } => arena
                        .set_member_string(*parent, name.as_bytes(), text)
                        .expect("field existed when this cursor was created"),
                    Slot::Root => panic!("cannot replace the document root"),
                };
                *id = new_id;
            }
        }
    }

    /// Replaces this value's own slot with raw (non-UTF-8-tagged) bytes.
    /// JSON has no byte-string concept, so this writes a plain JSON string
    /// instead - only ever called on JSON when the source text is already
    /// known to be valid UTF-8 (a translation), so nothing is lost.
    pub(crate) fn set_bytes(&mut self, bytes: Vec<u8>) {
        match self {
            Self::Json(v) => **v = JsonValue::String(String::from_utf8_lossy(&bytes).into_owned()),
            Self::Marshal { arena, id, slot } => {
                let new_id = match slot {
                    Slot::Array { parent, index } => arena.set_array_bytes(*parent, *index, bytes),
                    Slot::Member { parent, name } => arena
                        .set_member_bytes(*parent, name.as_bytes(), bytes)
                        .expect("field existed when this cursor was created"),
                    Slot::Root => panic!("cannot replace the document root"),
                };
                *id = new_id;
            }
        }
    }

    /// Replaces this value's own slot with bytes tagged as `Str` under
    /// `encoding_name` (the write-side counterpart of [`Value::declared_encoding`]
    /// on read) - re-encoding a translation back into the same encoding the
    /// source field declared, instead of downgrading it to an untagged
    /// [`Value::set_bytes`] blob. JSON has no such concept, so this falls back
    /// to the same lossy-UTF-8 behavior [`Value::set_bytes`] uses there.
    pub(crate) fn set_bytes_with_encoding(&mut self, bytes: Vec<u8>, encoding_name: &[u8]) {
        match self {
            Self::Json(v) => **v = JsonValue::String(String::from_utf8_lossy(&bytes).into_owned()),
            Self::Marshal { arena, id, slot } => {
                let new_id = arena.push_str_with_encoding_name(bytes, encoding_name);

                let repointed = match slot {
                    Slot::Array { parent, index } => Some(arena.set_array_value(*parent, *index, new_id)),
                    Slot::Member { parent, name } => arena.set_member_value(*parent, name.as_bytes(), new_id),
                    Slot::Root => panic!("cannot replace the document root"),
                };

                *id = repointed.expect("field existed when this cursor was created");
            }
        }
    }

    /// Visits every array element from `skip` onward.
    pub(crate) fn for_each_element_mut(&mut self, skip: usize, mut f: impl FnMut(&mut Value<'_>)) {
        match self {
            Self::Json(v) => {
                let Some(arr) = v.as_array_mut() else { return };
                for item in arr.iter_mut().skip(skip) {
                    f(&mut Value::Json(item));
                }
            }
            Self::Marshal { arena, id, .. } => {
                let len = ValueRef::new(arena, *id).len();
                let parent = *id;
                for index in skip..len {
                    // Re-reads the child id fresh each iteration, so a
                    // previous iteration replacing its own slot (mutating
                    // `arena.children`) can't leave this holding a stale id.
                    let Some(child) = ValueRef::new(arena, parent).at(index) else {
                        continue;
                    };
                    let child_id = child.id();
                    let mut cursor = Value::Marshal {
                        arena,
                        id: child_id,
                        slot: Slot::Array { parent, index },
                    };
                    f(&mut cursor);
                }
            }
        }
    }

    /// Visits every event in a map's `events` field - a JSON array (MV/MZ,
    /// `null` at index 0) or a Marshal `Hash` keyed by event id (everything
    /// older). Returns `false` without visiting anything if this value is
    /// neither shape.
    pub(crate) fn for_each_event_mut(&mut self, mut f: impl FnMut(&mut Value<'_>)) -> bool {
        match self {
            Self::Json(_) => {
                if !self.is_container() {
                    return false;
                }
                self.for_each_element_mut(1, f);
                true
            }
            Self::Marshal { arena, id, .. } => {
                let node_kind = ValueRef::new(arena, *id).kind();
                let parent = *id;
                match node_kind {
                    Kind::Array => {
                        self.for_each_element_mut(1, f);
                        true
                    }
                    Kind::Hash => {
                        let len = ValueRef::new(arena, parent).len();
                        for index in 0..len {
                            let Some((_, value)) = ValueRef::new(arena, parent).entries().nth(index) else {
                                continue;
                            };
                            let child_id = value.id();
                            // Hash values are never assigned back to in this
                            // crate - `Slot::Root` makes that a hard error if
                            // that ever changes, rather than a silent no-op.
                            let mut cursor = Value::Marshal {
                                arena,
                                id: child_id,
                                slot: Slot::Root,
                            };
                            f(&mut cursor);
                        }
                        true
                    }
                    _ => false,
                }
            }
        }
    }

    /// Visits every `(name, value)` member of an object.
    pub(crate) fn for_each_member_mut(&mut self, mut f: impl FnMut(&str, &mut Value<'_>)) {
        match self {
            Self::Json(v) => {
                let Some(obj) = v.as_object_mut() else { return };
                for (name, value) in obj.iter_mut() {
                    f(name, &mut Value::Json(value));
                }
            }
            Self::Marshal { arena, id, .. } => {
                let parent = *id;
                let names: Vec<String> = ValueRef::new(arena, parent)
                    .members()
                    .map(|(name, _)| String::from_utf8_lossy(name.strip_prefix(b"@").unwrap_or(name)).into_owned())
                    .collect();

                for name in names {
                    let Some(child) = ValueRef::new(arena, parent).get(&name) else {
                        continue;
                    };
                    let child_id = child.id();
                    let mut cursor = Value::Marshal {
                        arena,
                        id: child_id,
                        slot: Slot::Member {
                            parent,
                            name: name.clone(),
                        },
                    };
                    f(&name, &mut cursor);
                }
            }
        }
    }
}

/// Parses one RPG Maker data file's content into [`RpgmData`] - JSON for
/// `EngineType::MVMZ`, Marshal otherwise. Not used for `Scripts.*`, which
/// needs raw (unclassified) bytes regardless of text validity - see
/// `core::script`.
///
/// # Errors
///
/// - [`crate::types::Error::MarshalLoad`] - if unable to load the Marshal data.
/// - [`crate::types::Error::JsonParse`] - if unable to parse the JSON data.
pub(crate) fn parse_rpgm_file(mut content: &[u8], engine_type: EngineType) -> Result<RpgmData, crate::types::Error> {
    const BOM: &[u8] = &[0xEF, 0xBB, 0xBF];

    if engine_type.is_mvmz() {
        // MZ includes Byte Order Mark in files.
        if content.starts_with(BOM) {
            content = &content[3..];
        }

        // SAFETY: JSON is always valid UTF-8.
        let parsed = serde_json::from_str::<JsonValue>(unsafe { std::str::from_utf8_unchecked(content) })?;

        Ok(RpgmData::from_json(parsed))
    } else {
        Ok(RpgmData::from_marshal(content)?)
    }
}
