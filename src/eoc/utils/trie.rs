#![allow(dead_code)]

use std::{collections::HashMap, hash::Hash};

#[derive(Debug)]
pub(crate) struct Trie<T: Eq + Hash + Copy, U> {
    children: HashMap<T, Trie<T, U>>,
    is_terminal: bool,
    value: Option<U>,
}

impl<T: Eq + Hash + Copy, U> Default for Trie<T, U> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Eq + Hash + Copy, U> Trie<T, U> {
    pub(crate) fn new() -> Self {
        Trie {
            children: HashMap::new(),
            is_terminal: false,
            value: None,
        }
    }

    pub(crate) fn insert(&mut self, word: &[T], value: U) {
        let mut node = self;
        for &c in word {
            node = node.children.entry(c).or_insert(Trie::new());
        }
        node.value = Some(value);
        node.is_terminal = true;
    }

    pub(crate) fn contains(&self, word: &[T]) -> bool {
        let mut node = self;
        for &c in word {
            if let Some(next) = node.children.get(&c) {
                node = next;
            } else {
                return false;
            }
        }
        node.is_terminal
    }

    pub(crate) fn try_match(&self, word: &[T]) -> Option<&Trie<T, U>> {
        let mut node = self;
        for &c in word {
            if let Some(next) = node.children.get(&c) {
                node = next;
            } else {
                return None;
            }
        }
        Some(node)
    }

    pub(crate) fn is_terminal(&self) -> bool {
        self.is_terminal
    }

    pub(crate) fn get_value(&self) -> Option<&U> {
        self.value.as_ref()
    }
}