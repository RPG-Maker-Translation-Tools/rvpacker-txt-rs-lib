//! Resolving one scalar value out of a raw RPG Maker data file by a dotted
//! key/index path - for a consumer (a GUI tooltip, say) that wants to show
//! one field's current value without processing the whole file.
//!
//! [`RpgmData`]/[`Value`] themselves stay `pub(crate)` - this is the one
//! narrow capability built on top of them that's actually meant for callers
//! outside this crate.

use crate::{
    marshal_compat::{Value, parse_rpgm_file},
    types::{EngineType, Error},
};
use std::fmt;

/// One step of a path into a parsed RPG Maker data file: an object
/// field/instance variable name, or an array index.
#[derive(Clone, Copy, Debug)]
pub enum PathSegment<'a> {
    Key(&'a str),
    Index(usize),
}

impl fmt::Display for PathSegment<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Key(key) => f.write_str(key),
            Self::Index(index) => write!(f, "{index}"),
        }
    }
}

/// Navigates `content` (an RPG Maker data file, parsed per `engine_type`) by
/// `path` - one step per nesting level - then reads the scalar value (a
/// boolean, integer or string) at each of `leaves`, siblings under the object
/// `path` resolved to.
///
/// # Errors
///
/// - [`Error::MarshalLoad`]/[`Error::JsonParse`] - if `content` fails to parse.
/// - [`Error::InvalidPath`] - if any step of `path`, or any of `leaves`,
///   doesn't resolve, or a resolved leaf isn't a scalar.
pub fn get_entity_values(
    content: &[u8],
    engine_type: EngineType,
    path: &[PathSegment<'_>],
    leaves: &[PathSegment<'_>],
) -> Result<Vec<String>, Error> {
    let mut data = parse_rpgm_file(content, engine_type)?;
    walk(data.root(), path, leaves)
}

fn step<'v>(cursor: &'v mut Value<'_>, segment: PathSegment<'_>) -> Result<Value<'v>, Error> {
    match segment {
        PathSegment::Key(key) => cursor.member(key),
        PathSegment::Index(index) => cursor.at(index),
    }
    .ok_or_else(|| Error::InvalidPath(segment.to_string()))
}

fn leaf_to_string(leaf: &Value<'_>, segment: PathSegment<'_>) -> Result<String, Error> {
    if let Some(b) = leaf.as_bool() {
        Ok(b.to_string())
    } else if let Some(n) = leaf.as_int() {
        Ok(n.to_string())
    } else if let Some(s) = leaf.as_str() {
        Ok(s.to_owned())
    } else {
        Err(Error::InvalidPath(segment.to_string()))
    }
}

/// Recurses one level per `path` entry - each level's `cursor` local outlives
/// the recursive call it feeds, so this needs no unsafe lifetime games despite
/// [`Value::member`]/[`Value::at`] borrowing `&mut self`.
fn walk(mut cursor: Value<'_>, path: &[PathSegment<'_>], leaves: &[PathSegment<'_>]) -> Result<Vec<String>, Error> {
    let Some((&segment, rest)) = path.split_first() else {
        return leaves
            .iter()
            .map(|&segment| {
                let leaf = step(&mut cursor, segment)?;
                leaf_to_string(&leaf, segment)
            })
            .collect();
    };

    let child = step(&mut cursor, segment)?;
    walk(child, rest, leaves)
}
