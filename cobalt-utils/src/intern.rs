use std::collections::HashMap;
use std::hash::Hash;
use std::sync::RwLock;
pub struct Interner<'a, T: Clone + PartialEq + Eq + Hash> {
    vec: aovec::Aovec<T>,
    map: RwLock<HashMap<&'a T, usize>>,
}
impl<'a, T: Clone + PartialEq + Eq + Hash> Interner<'a, T> {
    pub fn new() -> Self {
        Self {
            vec: aovec::Aovec::new(16),
            map: RwLock::default(),
        }
    }
    pub fn intern(&'a self, key: T) -> &T {
        if let Some(&k) = self.map.read().unwrap().get(&key) {
            &self.vec[k]
        } else {
            let mut lock = self.map.write().unwrap();
            let idx = self.vec.push(key);
            let val = &self.vec[idx];
            lock.insert(val, idx);
            val
        }
    }
    pub fn intern_ref<K: Hash + Eq + ToOwned<Owned = T>>(&'a self, key: K) -> &T
    where
        &'a T: std::borrow::Borrow<K>,
    {
        if let Some(&k) = self.map.read().unwrap().get(&key) {
            &self.vec[k]
        } else {
            let mut lock = self.map.write().unwrap();
            let idx = self.vec.push(key.to_owned());
            let val = &self.vec[idx];
            lock.insert(val, idx);
            val
        }
    }
}

impl<T: Clone + PartialEq + Eq + Hash> Default for Interner<'_, T> {
    fn default() -> Self {
        Self::new()
    }
}
