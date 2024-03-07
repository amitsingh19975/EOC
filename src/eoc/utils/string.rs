
use std::{fmt::Display, sync::RwLock};

use lazy_static::lazy_static;

lazy_static! {
    static ref STRING_INTERNER: RwLock<Vec<&'static str>> = RwLock::new(Vec::new());
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub(crate) struct UniqueString(u32, &'static str);

impl Display for UniqueString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl UniqueString {
    pub(crate) fn new<S: AsRef<str>>(s: S) -> Self {
        let index = Self::find_index(s.as_ref());
        if index >= 0 {
            let reader = STRING_INTERNER.write().unwrap();
            return Self(index as u32, reader[index as usize])
        }

        let mut writer = STRING_INTERNER.write().unwrap();
        let id = writer.len();
        let s = Box::leak(s.as_ref().to_owned().into_boxed_str());
        writer.push(s);
        Self(id as u32, s)
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
        self.1
    }
}
