//! Resolving one scalar value out of a raw RPG Maker data file by a
//! key/index path - for a consumer (a GUI tooltip, say) that wants to show
//! one field's current value without processing the whole file.
//!
//! `RpgmData`/`Value` themselves stay `pub(crate)` - this is the one
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
    /// An object's field or instance variable, by name.
    Key(&'a str),
    /// An array element, by zero-based index.
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

/// Reads the scalar values at `leaves` from an RPG Maker data file.
///
/// `content` is parsed per `engine_type`, then navigated by `path` - one step per nesting level. Each leaf is a sibling
/// under the object `path` resolved to: a boolean, integer or string. Marshal strings with no encoding of their own are
/// decoded with `read_encoding`, UTF-8 if [`None`].
///
/// # Errors
///
/// - [`Error::MarshalLoad`]/[`Error::JsonParse`] - if `content` fails to parse.
/// - [`Error::InvalidPath`] - if any step of `path`, or any of `leaves`,
///   doesn't resolve, or a resolved leaf isn't a scalar.
pub fn get_entity_values(
    content: &[u8],
    engine_type: EngineType,
    read_encoding: Option<&'static encoding_rs::Encoding>,
    path: &[PathSegment<'_>],
    leaves: &[PathSegment<'_>],
) -> Result<Vec<String>, Error> {
    let mut data = parse_rpgm_file(content, engine_type)?;
    walk(data.root(), read_encoding, path, leaves)
}

fn step<'v>(cursor: &'v mut Value<'_>, segment: PathSegment<'_>) -> Result<Value<'v>, Error> {
    match segment {
        PathSegment::Key(key) => cursor.member(key),
        PathSegment::Index(index) => cursor.at(index),
    }
    .ok_or_else(|| Error::InvalidPath(segment.to_string()))
}

fn leaf_to_string(
    leaf: &Value<'_>,
    read_encoding: Option<&'static encoding_rs::Encoding>,
    segment: PathSegment<'_>,
) -> Result<String, Error> {
    if let Some(b) = leaf.as_bool() {
        Ok(b.to_string())
    } else if let Some(n) = leaf.as_int() {
        Ok(n.to_string())
    } else if let Some(s) = leaf.utf8_str()
        && (read_encoding.is_none() || leaf.as_raw_bytes().is_none() || leaf.declared_encoding().is_some())
    {
        // Untagged Marshal text is in the read encoding even when its bytes happen to be valid UTF-8.
        Ok(s.to_owned())
    } else if let Some(bytes) = leaf.as_raw_bytes() {
        let encoding = leaf
            .declared_encoding()
            .and_then(encoding_rs::Encoding::for_label)
            .or(read_encoding);

        super::Base::decode_bytes_with(bytes, encoding)
    } else {
        Err(Error::InvalidPath(segment.to_string()))
    }
}

/// Recurses one level per `path` entry - each level's `cursor` local outlives
/// the recursive call it feeds, so this needs no unsafe lifetime games despite
/// [`Value::member`]/[`Value::at`] borrowing `&mut self`.
fn walk(
    mut cursor: Value<'_>,
    read_encoding: Option<&'static encoding_rs::Encoding>,
    path: &[PathSegment<'_>],
    leaves: &[PathSegment<'_>],
) -> Result<Vec<String>, Error> {
    let Some((&segment, rest)) = path.split_first() else {
        return leaves
            .iter()
            .map(|&segment| {
                let leaf = step(&mut cursor, segment)?;
                leaf_to_string(&leaf, read_encoding, segment)
            })
            .collect();
    };

    let child = step(&mut cursor, segment)?;
    walk(child, read_encoding, rest, leaves)
}

/// The name-bearing lists of `RPG_RT.ldb` that [`get_rm2k_entity_name`] can look an entry up in.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(u8)]
pub enum Rm2kEntity {
    /// The actors list.
    Actors,
    /// The variables list. Id `3` is the variable `\V[3]` shows.
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

/// An RPG Maker 2000/2003 `RPG_RT.ldb`, loaded once for any number of lookups.
///
/// The counterpart of [`get_entity_values`] for the things a message code needs. RM2K has no per-entity files and no
/// dynamic object tree to walk by a path - the database is one typed struct - so it gets its own lookups rather than
/// a [`PathSegment`] path. Loading parses the file once here rather than per lookup.
pub struct Rm2kDatabase<'a> {
    database: rm2k::file::Loaded<'a, rm2k::rpg::Database<'a>>,
    read_encoding: Option<&'static encoding_rs::Encoding>,
}

impl<'a> Rm2kDatabase<'a> {
    /// Loads `content`, which is `RPG_RT.ldb`. `read_encoding` decodes names the same way processing the database
    /// does; [`None`] decodes as UTF-8.
    ///
    /// # Errors
    ///
    /// - [`Error::Rm2kLoad`] - if `content` is not a database.
    pub fn load(content: &'a [u8], read_encoding: Option<&'static encoding_rs::Encoding>) -> Result<Self, Error> {
        let database = rm2k::file::load_database(content)?;
        Ok(Self {
            database,
            read_encoding,
        })
    }

    /// The name of entry `id` of `entity`.
    ///
    /// # Errors
    ///
    /// - [`Error::InvalidPath`] - if the list has no entry with this `id`.
    /// - [`Error::InvalidText`] - if the name isn't valid in the codepage.
    pub fn entity_name(&self, entity: Rm2kEntity, id: i32) -> Result<String, Error> {
        let name = match entity {
            Rm2kEntity::Actors => self
                .database
                .actors
                .iter()
                .find(|actor| actor.id == id)
                .map(|actor| actor.name.as_bytes()),
            Rm2kEntity::Variables => self
                .database
                .variables
                .iter()
                .find(|variable| variable.id == id)
                .map(|variable| variable.name.as_bytes()),
        };

        let bytes = name.ok_or_else(|| Error::InvalidPath(format!("{entity}[{id}]")))?;
        super::Base::decode_bytes_with(bytes, self.read_encoding)
    }

    /// The name of the system graphic (`System/<name>.png`) the database selects - where the text colors of `\C[n]`
    /// live. An empty name (a database that selects no graphic) is returned as is.
    ///
    /// # Errors
    ///
    /// - [`Error::InvalidText`] - if the name isn't valid in the codepage.
    pub fn system_graphic_name(&self) -> Result<String, Error> {
        super::Base::decode_bytes_with(self.database.system.system_name.as_bytes(), self.read_encoding)
    }
}

/// The name of entry `id` of `entity` in the RPG Maker 2000/2003 database (`content` is `RPG_RT.ldb`).
///
/// Loads the database on every call; use [`Rm2kDatabase`] for more than one lookup.
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
    Rm2kDatabase::load(content, read_encoding)?.entity_name(entity, id)
}

/// The name of the system graphic (`System/<name>.png`) the RPG Maker 2000/2003 database selects. `content` is
/// `RPG_RT.ldb`; `read_encoding` is as in [`get_rm2k_entity_name`].
///
/// Loads the database on every call; use [`Rm2kDatabase`] for more than one lookup.
///
/// # Errors
///
/// - [`Error::Rm2kLoad`] - if `content` is not a database.
pub fn get_rm2k_system_graphic_name(
    content: &[u8],
    read_encoding: Option<&'static encoding_rs::Encoding>,
) -> Result<String, Error> {
    Rm2kDatabase::load(content, read_encoding)?.system_graphic_name()
}
