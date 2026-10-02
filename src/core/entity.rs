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

/// The name-bearing lists of `RPG_RT.ldb` that [`get_rm2k_entity_name`] can look an entry up in.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum Rm2kEntity {
    Actors,
    Variables,
}

impl fmt::Display for Rm2kEntity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Actors => "actors",
            Self::Variables => "variables",
        })
    }
}

/// The RPG Maker 2000/2003 counterpart of [`get_entity_values`], for the one thing a message code needs: the name
/// of entry `id` of `entity` in the database (`content` is `RPG_RT.ldb`).
///
/// RM2K has no per-entity files and no dynamic object tree to walk by a path - the database is one typed struct -
/// so it gets its own lookup rather than a [`PathSegment`] path. `read_encoding` decodes the name the same way
/// processing the database does; `None` guesses between the common codepages.
///
/// # Errors
///
/// - [`Error::Rm2kLoad`] - if `content` is not a database.
/// - [`Error::InvalidPath`] - if the list has no entry with this `id`.
pub fn get_rm2k_entity_name(
    content: &[u8],
    entity: Rm2kEntity,
    id: i32,
    read_encoding: Option<&'static encoding_rs::Encoding>,
) -> Result<String, Error> {
    let database = rm2k::file::load_database(content)?;

    let name = match entity {
        Rm2kEntity::Actors => database.actors.iter().find(|actor| actor.id == id).map(|actor| actor.name.as_bytes()),
        Rm2kEntity::Variables => database
            .variables
            .iter()
            .find(|variable| variable.id == id)
            .map(|variable| variable.name.as_bytes()),
    };

    name.map(|bytes| super::Base::decode_bytes_with(bytes, read_encoding))
        .ok_or_else(|| Error::InvalidPath(format!("{entity}[{id}]")))
}

/// The name of the system graphic (`System/<name>.png`) the RPG Maker 2000/2003 database selects - where the text
/// colors of `\C[n]` live. `content` is `RPG_RT.ldb`; `read_encoding` is as in [`get_rm2k_entity_name`].
///
/// An empty name (a database that selects no graphic) is returned as is.
///
/// # Errors
///
/// - [`Error::Rm2kLoad`] - if `content` is not a database.
pub fn get_rm2k_system_graphic_name(
    content: &[u8],
    read_encoding: Option<&'static encoding_rs::Encoding>,
) -> Result<String, Error> {
    let database = rm2k::file::load_database(content)?;

    Ok(super::Base::decode_bytes_with(database.system.system_name.as_bytes(), read_encoding))
}
