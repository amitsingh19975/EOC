#![allow(dead_code)]

use super::{filesystem::MappedFile, source_manager::SourceManager};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub(crate) struct Span {
    pub(crate) start: u32,
    pub(crate) end: u32,
}

impl Span {
    pub(crate) fn new(start: u32, end: u32) -> Self {
        Self { start, end }
    }

    pub(crate) fn from_usize(start: usize, end: usize) -> Self {
        Self::new(start as u32, end as u32)
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.start == self.end
    }

    pub(crate) fn len(&self) -> usize {
        if (self.end as i64 - self.start as i64) < 0 {
            return 0;
        }
        (self.end - self.start) as usize
    }

    pub(crate) fn is_intersecting(&self, other: &Self) -> bool {
        self.start < other.end && other.start < self.end
    }

    pub(crate) fn union(&self, other: &Self) -> Self {
        let start = self.start.min(other.start);
        let end = self.end.max(other.end);
        Self::new(start, end)
    }

    pub(crate) fn split_if_intersect(&self, other: &Self) -> (Self, Self) {
        if !self.is_intersecting(other) {
            return (*self, *other);
        }

        let start = self.start;
        let end = self.end;

        if self.start < other.start {
            let first = Self::new(start, other.start);
            let second = Self::new(other.start, end);
            (first, second)
        } else {
            let first = Self::new(other.start, start);
            let second = Self::new(start, end);
            (first, second)
        }
    }

    pub(crate) fn trim(&self, source_manager: &SourceManager) -> Span {
        if self.is_empty() {
            return *self;
        }

        let mut start = self.start as usize;
        let mut end = self.end as usize - 1;

        {
            let mut c = source_manager[start];
            while c.is_ascii_whitespace() {
                start += 1;
                c = source_manager[start];
            }
        }
        {
            let mut c = source_manager[end];
            while c.is_ascii_whitespace() {
                end -= 1;
                c = source_manager[end];
            }
        }

        if start > end {
            return Span::default();
        }

        Span::from_usize(start, end + 1)
    }

    pub(crate) fn relative(&self, offset: u32) -> Span {
        Span::new(self.start + offset, self.end + offset)
    }
}

impl MappedFile {
    pub(crate) fn get_subspan(&self, span: Span) -> &[u8] {
        let start = span.start as usize;
        let end = span.end as usize;
        if (end as i64 - start as i64) <= 0 {
            return &[];
        }
        assert!(end <= self.len(), "Span out of range");
        &self.as_slice()[start..end]
    }
}