
use std::{fmt::Display, sync::RwLock};

use lazy_static::lazy_static;

lazy_static! {
    static ref STRING_INTERNER: RwLock<Vec<&'static str>> = RwLock::new(Vec::new());
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub(crate) struct UniqueString(u32);

impl Display for UniqueString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = self.as_str();
        write!(f, "{s}")
    }
}

impl UniqueString {
    pub(crate) fn new(s: String) -> Self {
        let index = Self::find_index(s.as_ref());
        if index >= 0 {
            return Self(index as u32)
        }

        let mut writer = STRING_INTERNER.write().unwrap();
        let id = writer.len();
        writer.push(Box::leak(s.into_boxed_str()));
        Self(id as u32)
    }

    fn find_index(s: &str) -> isize {
        let reader = STRING_INTERNER.write().unwrap();
        for i in 0..reader.len() {
            if reader[i] == s {
                return i as isize;
            }
        }

        -1
    }

    pub(crate) fn as_str(&self) -> &'static str {
        let reader = STRING_INTERNER.read().unwrap();
        reader[self.0 as usize]
    }
}
