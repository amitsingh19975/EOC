
use std::{collections::HashSet, fmt::Display, hash::Hash, sync::RwLock};

use lazy_static::lazy_static;

lazy_static! {
    static ref STRING_INTERNER: RwLock<HashSet<&'static str>> = RwLock::new(HashSet::new());
}

#[derive(Debug, Eq, Clone, Copy)]
pub(crate) struct UniqueString(&'static str);

impl Display for UniqueString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl UniqueString {
    pub(crate) fn try_new<S: AsRef<str>>(s: S) -> Option<Self> {
        let c_str = Self::get(s.as_ref());
        if let Some(c_str) = c_str {
            Some(Self(c_str))
        } else {
            None
        }
    }

    pub(crate) fn new<S: AsRef<str>>(s: S) -> Self {
        if let Some(n) = Self::try_new(s.as_ref()) {
            return n;
        }

        let mut writer = STRING_INTERNER.write().unwrap();
        let s = Box::leak(s.as_ref().to_owned().into_boxed_str());
        writer.insert(s);
        Self(s)
    }

    pub(crate) fn from_static(s: &'static str) -> Self {
        let mut writer = STRING_INTERNER.write().unwrap();
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


impl PartialEq for UniqueString {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.0, other.0)
    }
}

impl PartialEq<UniqueString> for str {
    fn eq(&self, other: &UniqueString) -> bool {
        self == other.0
    }
}

impl PartialEq<&str> for UniqueString {
    fn eq(&self, other: &&str) -> bool {
        self.0 == *other
    }
}

impl PartialEq<UniqueString> for &str {
    fn eq(&self, other: &UniqueString) -> bool {
        *self == other.0
    }
}

impl PartialEq<String> for UniqueString {
    fn eq(&self, other: &String) -> bool {
        self.0 == other
    }
}

impl PartialEq<UniqueString> for String {
    fn eq(&self, other: &UniqueString) -> bool {
        self == other.0
    }
}

impl AsRef<str> for UniqueString {
    fn as_ref(&self) -> &str {
        self.0
    }
}

impl Hash for UniqueString {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.as_ptr().hash(state);
    }
}
