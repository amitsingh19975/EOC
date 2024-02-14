use std::{fs::File, io::Error, ops::Deref, path::Path};

#[derive(Debug)]
pub(crate) enum MappedFile {
    Mmap(memmap2::Mmap),
    Memory(Vec<u8>),
}

impl MappedFile {
    pub(crate) fn new<P: AsRef<Path>>(path: P) -> Result<Self, Error> {
        let path = path.as_ref();
        let file = File::open(path)?;
        let metadata = file.metadata()?;
        let len = metadata.len() as usize;
        let mmap = unsafe { memmap2::MmapOptions::new().map(&file)? };
        Ok(if len < 1024 * 1024 {
            MappedFile::Memory(mmap.to_vec())
        } else {
            MappedFile::Mmap(mmap)
        })
    }

    pub(crate) fn as_slice(&self) -> &[u8] {
        match self {
            MappedFile::Mmap(mmap) => &mmap,
            MappedFile::Memory(mem) => &mem,
        }
    }

    pub(crate) fn as_str(&self) -> &str {
        std::str::from_utf8(self.as_slice()).unwrap()
    }
}

impl AsRef<[u8]> for MappedFile {
    fn as_ref(&self) -> &[u8] {
        self.as_slice()
    }
}

impl AsRef<str> for MappedFile {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl Deref for MappedFile {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        self.as_slice()
    }
}
