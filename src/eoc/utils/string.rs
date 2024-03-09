
use std::{collections::HashSet, fmt::Display, sync::RwLock};

use lazy_static::lazy_static;

lazy_static! {
    static ref STRING_INTERNER: RwLock<HashSet<&'static str>> = RwLock::new(HashSet::new());
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub(crate) struct UniqueString(&'static str);

impl Display for UniqueString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl UniqueString {
    pub(crate) fn new<S: AsRef<str>>(s: S) -> Self {
        let c_str = Self::get(s.as_ref());
        if let Some(c_str) = c_str {
            return Self(c_str)
        }

        let mut writer = STRING_INTERNER.write().unwrap();
        let s = Box::leak(s.as_ref().to_owned().into_boxed_str());
        writer.insert(s);
        Self(s)
    }

    fn get(s: &str) -> Option<&'static str> {
        let reader = STRING_INTERNER.write().unwrap();
        reader.get(s).copied()
    }

    pub(crate) fn as_str(&self) -> &'static str {
        self.0
    }
}
