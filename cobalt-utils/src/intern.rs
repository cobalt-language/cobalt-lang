use bumpalo_herd::*;
use hashbrown::HashTable;
use once_cell::sync::Lazy;
use parking_lot::RwLock;
use std::borrow::*;
use std::collections::hash_map::RandomState;
use std::fmt;
use std::hash::*;
use thread_local::ThreadLocal;

enum MaybeLazy<T> {
    Eager(T),
    Lazy(Lazy<T>),
}
impl<T> std::ops::Deref for MaybeLazy<T> {
    type Target = T;
    fn deref(&self) -> &T {
        match self {
            Self::Eager(val) => val,
            Self::Lazy(val) => val,
        }
    }
}

#[ouroboros::self_referencing]
struct InternerInner<T: Eq + Hash + Send + Sync + 'static> {
    owner: Herd,
    #[borrows(owner)]
    #[not_covariant]
    member: ThreadLocal<Member<'this>>,
    #[borrows(owner)]
    #[not_covariant]
    set: RwLock<HashTable<&'this T>>,
}

pub struct Interner<T: Eq + Hash + Send + Sync + 'static, S = RandomState>(
    Lazy<InternerInner<T>>,
    MaybeLazy<S>,
);

impl<T: fmt::Debug + Eq + Hash + Send + Sync + 'static, S> fmt::Debug for Interner<T, S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.with_set(|set| fmt::Debug::fmt(set, f))
    }
}
impl<T: Eq + Hash + Send + Sync + 'static, S> Interner<T, S> {
    pub const fn with_hasher(state: S) -> Self {
        Self(
            Lazy::new(|| {
                InternerInner::new(Herd::new(), |_| ThreadLocal::new(), |_| RwLock::default())
            }),
            MaybeLazy::Eager(state),
        )
    }
}
impl<T: Eq + Hash + Send + Sync + 'static, S: Default> Interner<T, S> {
    pub const fn new() -> Self {
        Self(
            Lazy::new(|| {
                InternerInner::new(Herd::new(), |_| ThreadLocal::new(), |_| RwLock::default())
            }),
            MaybeLazy::Lazy(Lazy::new(S::default)),
        )
    }
}
impl<T: Eq + Hash + Send + Sync + 'static, S: BuildHasher> Interner<T, S> {
    pub fn intern<Q: Eq + Hash + Into<T>>(&self, key: Q) -> & T
    where
        T: Borrow<Q>,
    {
        let builder = &*self.1;
        let hash = builder.hash_one(&key);
        self.0.with_set(|set| {
            let lock = set.read();
            if let Some(r) = lock.find(hash, |m| <T as Borrow<Q>>::borrow(m) == &key) {
                // safety: lifetime can't be invalidated as long as self is not moved
                unsafe { crate::new_lifetime(r) }
            } else {
                std::mem::drop(lock);
                let ret = self.0.with_member(|m| unsafe {
                    crate::new_lifetime(
                        m.get_or(|| crate::new_lifetime(self.0.borrow_owner()).get())
                            .alloc(key.into()),
                    )
                });
                set.write()
                    .insert_unique(hash, ret, |v| builder.hash_one(v));
                ret
            }
        })
    }
    pub fn intern_ref<'a, Q: Eq + Hash + ToOwned + ?Sized>(&'a self, key: &Q) -> &'a T
    where
        T: Borrow<Q>,
        Q::Owned: Into<T>,
    {
        let builder = &*self.1;
        let hash = builder.hash_one(key);
        self.0.with_set(|set| {
            let lock = set.read();
            if let Some(r) = lock.find(hash, |m| <T as Borrow<Q>>::borrow(m) == key) {
                // safety: lifetime can't be invalidated as long as self is not moved
                unsafe { crate::new_lifetime(r) }
            } else {
                std::mem::drop(lock);
                let ret = self.0.with_member(|m| unsafe {
                    crate::new_lifetime(
                        m.get_or(|| crate::new_lifetime(self.0.borrow_owner()).get())
                            .alloc(key.to_owned().into()),
                    )
                });
                set.write()
                    .insert_unique(hash, ret, |v| builder.hash_one(v));
                ret
            }
        })
    }
}
impl<T: Eq + Hash + Send + Sync + 'static, S> Drop for Interner<T, S> {
    fn drop(&mut self) {
        self.0.with_set_mut(|set| {
            set.get_mut()
                .iter_mut()
                .for_each(|p| unsafe { std::ptr::drop_in_place::<T>(*p as *const T as *mut T) })
        })
    }
}

impl<T: Eq + Hash + Send + Sync + 'static, S: Default> Default for Interner<T, S> {
    fn default() -> Self {
        Self::new()
    }
}
