use std::{
    cell::UnsafeCell,
    sync::atomic::{AtomicBool, AtomicUsize},
};

#[derive(Debug, Clone, Copy)]
pub struct Index {
    index: usize,
}

#[derive(Debug)]
pub struct StrCache {
    backing: UnsafeCell<Vec<&'static str>>,
    readers: AtomicUsize,
    write_locked: AtomicBool,
}

impl StrCache {
    pub const fn new() -> Self {
        Self {
            backing: UnsafeCell::new(Vec::new()),
            readers: AtomicUsize::new(0),
            write_locked: AtomicBool::new(false),
        }
    }
}

unsafe impl Sync for StrCache {}
