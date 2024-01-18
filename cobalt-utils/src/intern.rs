use hashbrown::HashTable;
use once_cell::sync::Lazy;
use std::hash::{Hash, Hasher};
use std::sync::RwLock;
#[inline]
fn hash(val: &impl Hash) -> u64 {
    let mut state = std::collections::hash_map::DefaultHasher::default();
    val.hash(&mut state);
    state.finish()
}
#[derive(Debug)]
pub struct Interner<'a, T: PartialEq + Eq + Hash> {
    vec: Lazy<boxcar::Vec<T>>,
    map: RwLock<HashTable<(&'a T, usize)>>,
}
impl<'a, K: PartialEq + Eq + Hash> Interner<'a, K> {
    pub const fn new() -> Self {
        Self {
            vec: Lazy::new(boxcar::Vec::new),
            map: RwLock::new(HashTable::new()),
        }
    }
    pub fn intern(&'a self, key: K) -> &K {
        let hashed = hash(&key);
        let lock = self.map.read().unwrap();
        if let Some(k) = lock.find(hashed, |v| v.0 == &key).map(|x| x.1) {
            eprintln!("reusing old element @ {k}");
            &self.vec[k]
        } else {
            eprintln!("creating new element @ {}", self.vec.count());
            std::mem::drop(lock);
            let mut lock = self.map.write().unwrap();
            let idx = self.vec.push(key);
            let val = &self.vec[idx];
            lock.insert_unique(hashed, (val, idx), |v| hash(&v.0));
            val
        }
    }
    pub fn intern_ref<R: PartialEq + ?Sized, Q: Hash + Eq + AsRef<R> + ?Sized + 'a>(
        &'a self,
        key: &Q,
    ) -> &K
    where
        K: std::borrow::Borrow<Q> + AsRef<R>,
        for<'b> &'b Q: Into<K>,
    {
        let hashed = hash(&key);
        let lock = self.map.read().unwrap();
        if let Some(k) = lock
            .find(hashed, |v| key.as_ref() == v.0.as_ref())
            .map(|x| x.1)
        {
            &self.vec[k]
        } else {
            std::mem::drop(lock);
            let mut lock = self.map.write().unwrap();
            let idx = self.vec.push(key.into());
            let val = &self.vec[idx];
            lock.insert_unique(hashed, (val, idx), |v| hash(&v.0));
            val
        }
    }
}

impl<T: Clone + PartialEq + Eq + Hash> Default for Interner<'_, T> {
    fn default() -> Self {
        Self::new()
    }
}
