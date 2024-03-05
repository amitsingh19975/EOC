use super::utils::byte_to_char;

pub(crate) struct ByteToCharIter<'a>(&'a [u8]);

impl<'a> ByteToCharIter<'a> {
    pub fn new(bytes: &'a [u8]) -> Self {
        ByteToCharIter(bytes)
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
