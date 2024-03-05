use super::utils::byte_to_char;

pub(crate) struct ByteToCharIter<'a>(&'a [u8]);

impl<'a> ByteToCharIter<'a> {
    pub fn new(bytes: &'a [u8]) -> Self {
        ByteToCharIter(bytes)
    }

    pub(crate) fn utf8_len_after_skip(&self, mut n: usize) -> usize {
        let mut size = 0usize;
        let mut iter = ByteToCharIter::new(self.0);
        while let Some(c) = iter.next() {
            if n == 0 {
                break;
            }
            size += c.len_utf8();
            n -= 1;
        }
        size
    }
}

impl<'a> Iterator for ByteToCharIter<'a> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        if self.0.is_empty() {
            return None;
        }

        let (ch, size) = byte_to_char(self.0);
        self.0 = &self.0[size..];
        ch
    }
}
