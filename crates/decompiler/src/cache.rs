//! Deterministic in-memory cache for interprocedural function analysis.

use std::collections::BTreeMap;

use serde::Serialize;

use crate::interprocedural::FunctionSummary;

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub struct Fingerprint(pub u64);

impl Fingerprint {
    pub fn bytes(chunks: impl IntoIterator<Item = impl AsRef<[u8]>>) -> Self {
        let mut state = Fnv1a::new();
        for chunk in chunks {
            let bytes = chunk.as_ref();
            state.write(&(bytes.len() as u64).to_le_bytes());
            state.write(bytes);
        }
        Self(state.finish())
    }

    pub fn serializable(value: &impl Serialize) -> Self {
        match serde_json::to_vec(value) {
            Ok(bytes) => Self::bytes([bytes]),
            Err(_) => Self::default(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub struct FunctionCacheKey {
    pub function_index: usize,
    pub bytecode_hash: Fingerprint,
    pub configuration: Fingerprint,
    pub dependencies: Fingerprint,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Serialize)]
pub struct CacheStats {
    pub hits: usize,
    pub misses: usize,
    pub invalidations: usize,
    pub stores: usize,
}

#[derive(Debug, Clone)]
struct CacheEntry {
    key: FunctionCacheKey,
    value: FunctionSummary,
}

/// Cache entries are indexed by HashLink function index and validated against
/// their full key on every lookup.
#[derive(Debug, Default)]
pub struct AnalysisCache {
    entries: BTreeMap<usize, CacheEntry>,
    stats: CacheStats,
}

impl AnalysisCache {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get(&mut self, key: FunctionCacheKey) -> Option<FunctionSummary> {
        match self.entries.get(&key.function_index) {
            Some(entry) if entry.key == key => {
                self.stats.hits += 1;
                Some(entry.value.clone())
            }
            Some(_) => {
                self.stats.misses += 1;
                self.stats.invalidations += 1;
                self.entries.remove(&key.function_index);
                None
            }
            None => {
                self.stats.misses += 1;
                None
            }
        }
    }

    pub fn insert(&mut self, key: FunctionCacheKey, value: FunctionSummary) {
        self.entries
            .insert(key.function_index, CacheEntry { key, value });
        self.stats.stores += 1;
    }

    pub fn invalidate_function(&mut self, function_index: usize) -> bool {
        let removed = self.entries.remove(&function_index).is_some();
        self.stats.invalidations += usize::from(removed);
        removed
    }

    pub fn clear(&mut self) {
        self.stats.invalidations += self.entries.len();
        self.entries.clear();
    }

    pub fn len(&self) -> usize {
        self.entries.len()
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    pub fn stats(&self) -> CacheStats {
        self.stats
    }

    pub fn reset_stats(&mut self) {
        self.stats = CacheStats::default();
    }
}

struct Fnv1a(u64);

impl Fnv1a {
    const OFFSET: u64 = 0xcbf2_9ce4_8422_2325;
    const PRIME: u64 = 0x0000_0100_0000_01b3;

    const fn new() -> Self {
        Self(Self::OFFSET)
    }

    fn write(&mut self, bytes: &[u8]) {
        for byte in bytes {
            self.0 ^= u64::from(*byte);
            self.0 = self.0.wrapping_mul(Self::PRIME);
        }
    }

    const fn finish(self) -> u64 {
        self.0
    }
}

#[cfg(test)]
mod tests {
    use super::{Fingerprint, Fnv1a};

    #[test]
    fn fnv_fingerprint_is_stable_and_chunk_sensitive() {
        assert_eq!(
            Fingerprint::bytes([b"abc".as_slice()]),
            Fingerprint::bytes([b"abc".as_slice()])
        );
        assert_ne!(
            Fingerprint::bytes([b"a".as_slice(), b"bc".as_slice()]),
            Fingerprint::bytes([b"ab".as_slice(), b"c".as_slice()])
        );
        let mut hash = Fnv1a::new();
        hash.write(b"hello");
        assert_eq!(hash.finish(), 0xa430_d846_80aa_bd0b);
    }
}
