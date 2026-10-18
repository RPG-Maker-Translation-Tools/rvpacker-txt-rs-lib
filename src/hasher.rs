//! The hash maps and sets this crate uses.
//!
//! With the `gxhash` feature, every hasher is a [`GuardedHasher`]. gxhash 3.5.0 reads a whole 16-byte vector for an input
//! shorter than 16 bytes, so for an input near the end of its allocation the read runs past the end of the input. The
//! guard copies such a write into a 16-byte buffer first, so the read stays inside it. gxhash masks the bytes past the
//! input, so the hash values don't change.

#[cfg(feature = "gxhash")]
use gxhash::{GxBuildHasher, GxHasher};
#[cfg(feature = "gxhash")]
use std::hash::{BuildHasher, Hasher};

/// A [`GxHasher`] whose short writes can't be over-read. See the [module docs](self).
#[cfg(feature = "gxhash")]
#[derive(Clone, Debug)]
pub struct GuardedHasher(GxHasher);

#[cfg(feature = "gxhash")]
impl Hasher for GuardedHasher {
    #[inline]
    fn finish(&self) -> u64 {
        self.0.finish()
    }

    #[inline]
    fn write(&mut self, bytes: &[u8]) {
        if bytes.len() < 16 {
            let mut buffer = [0u8; 16];
            buffer[..bytes.len()].copy_from_slice(bytes);
            // gxhash reads all 16 bytes of `buffer`, which is in bounds, and masks the bytes past `bytes`.
            self.0.write(&buffer[..bytes.len()]);
        } else {
            self.0.write(bytes);
        }
    }
}

/// The [`BuildHasher`] for `GuardedHasher`s, fixed-seeded (the `deterministic` feature of `gxhash`), so hashes are the same on every run.
#[cfg(feature = "gxhash")]
#[derive(Clone, Debug, Default)]
pub struct FastBuildHasher(GxBuildHasher);

#[cfg(feature = "gxhash")]
impl BuildHasher for FastBuildHasher {
    type Hasher = GuardedHasher;

    #[inline]
    fn build_hasher(&self) -> GuardedHasher {
        GuardedHasher(self.0.build_hasher())
    }
}

/// A [`std::collections::HashMap`] hashed with [`FastBuildHasher`].
#[cfg(feature = "gxhash")]
pub type HashMap<K, V> = std::collections::HashMap<K, V, FastBuildHasher>;

/// A [`std::collections::HashSet`] hashed with [`FastBuildHasher`].
#[cfg(feature = "gxhash")]
pub type HashSet<K> = std::collections::HashSet<K, FastBuildHasher>;

/// Constructors for [`HashMap`], matching the ones `rapidhash` provides.
#[cfg(feature = "gxhash")]
pub trait HashMapExt {
    /// An empty map.
    fn new() -> Self;
    /// An empty map with room for `capacity` entries.
    fn with_capacity(capacity: usize) -> Self;
}

#[cfg(feature = "gxhash")]
// Only the default-hasher map has these constructors, so the impl stays on it.
#[allow(clippy::implicit_hasher)]
impl<K, V> HashMapExt for HashMap<K, V> {
    fn new() -> Self {
        Self::default()
    }

    fn with_capacity(capacity: usize) -> Self {
        Self::with_capacity_and_hasher(capacity, FastBuildHasher::default())
    }
}

/// Constructors for [`HashSet`], matching the ones `rapidhash` provides.
#[cfg(feature = "gxhash")]
pub trait HashSetExt {
    /// An empty set.
    fn new() -> Self;
    /// An empty set with room for `capacity` entries.
    fn with_capacity(capacity: usize) -> Self;
}

#[cfg(feature = "gxhash")]
// Only the default-hasher set has these constructors, so the impl stays on it.
#[allow(clippy::implicit_hasher)]
impl<K> HashSetExt for HashSet<K> {
    fn new() -> Self {
        Self::default()
    }

    fn with_capacity(capacity: usize) -> Self {
        Self::with_capacity_and_hasher(capacity, FastBuildHasher::default())
    }
}

#[cfg(not(feature = "gxhash"))]
pub use rapidhash::{
    HashMapExt, HashSetExt, RapidHashMap as HashMap, RapidHashSet as HashSet, fast::RandomState as FastBuildHasher,
};

#[cfg(all(test, feature = "gxhash"))]
mod tests {
    use super::*;

    /// The guard must hash every short write exactly as gxhash does. The reference reads past each input too, but
    /// only inside its own buffer, which is longer than the input.
    #[test]
    fn short_writes_hash_as_gxhash_does() {
        for len in 0..=40 {
            let buffer: Vec<u8> = (0..len + 16).map(|byte| byte as u8).collect();
            let mut guarded = GuardedHasher(GxHasher::default());
            let mut reference = GxHasher::default();

            guarded.write(&buffer[..len]);
            reference.write(&buffer[..len]);

            assert_eq!(guarded.finish(), reference.finish(), "write of {len} bytes");
        }
    }
}
