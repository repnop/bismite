pub struct Arena<T> {
    inner: Vec<T>,
}

impl<T> Arena<T> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, item: T) -> Key<T> {
        let key = Key { idx: self.inner.len(), _t: core::marker::PhantomData };
        self.inner.push(item);

        key
    }
}

impl<T> core::ops::Index<Key<T>> for Arena<T> {
    type Output = T;

    fn index(&self, index: Key<T>) -> &Self::Output {
        &self.inner[index.idx]
    }
}

impl<T> core::ops::IndexMut<Key<T>> for Arena<T> {
    fn index_mut(&mut self, index: Key<T>) -> &mut Self::Output {
        &mut self.inner[index.idx]
    }
}

impl<T> Default for Arena<T> {
    fn default() -> Self {
        Self { inner: Vec::with_capacity(8) }
    }
}

pub struct Key<T> {
    idx: usize,
    _t: core::marker::PhantomData<fn() -> T>,
}

impl<T> Clone for Key<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for Key<T> {}

impl<T> core::fmt::Debug for Key<T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let ty_name = format!("Key<{}>", core::any::type_name::<T>());
        f.debug_struct(&ty_name).field("idx", &self.idx).finish()
    }
}

impl<T> std::hash::Hash for Key<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.idx.hash(state)
    }
}

impl<T> core::cmp::PartialEq for Key<T> {
    fn eq(&self, other: &Self) -> bool {
        self.idx.eq(&other.idx)
    }
}

impl<T> core::cmp::Eq for Key<T> {}
