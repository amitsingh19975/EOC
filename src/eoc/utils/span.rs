use super::filesystem::MappedFile;

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
        (self.end - self.start) as usize
    }

    pub(crate) fn is_intersecting(&self, other: &Self) -> bool {
        self.start < other.end && other.start < self.end
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
}

impl MappedFile {
    pub(crate) fn get_subspan(&self, span: Span) -> &[u8] {
        let start = span.start as usize;
        let end = span.end as usize;
        assert!(start <= end, "Invalid span");
        assert!(end <= self.len(), "Span out of range");
        &self.as_slice()[start..end]
    }
}