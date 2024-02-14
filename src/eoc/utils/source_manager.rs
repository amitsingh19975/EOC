use std::{ops::{Index, RangeBounds, RangeFrom}, path::{Path, PathBuf}};
use crate::eoc::lexer::utils::valid_utf8_character_with_char_len;

use super::{filesystem::MappedFile, span::Span};

pub(crate) struct SourceManager {
    source: MappedFile,
    cached_line: Vec<usize>,
    filepath: PathBuf,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Default)]
pub(crate) struct SourceManagerDiagnosticInfo {
    pub(crate) source: String,
    pub(crate) line: usize,
    pub(crate) column: usize,
}

impl SourceManager {
    pub(crate) fn new<P: AsRef<Path>>(path: P) -> Result<Self, std::io::Error> {
        let path = path.as_ref().to_path_buf();
        let source = MappedFile::new(&path)?;
        let cached_line = Self::calculate_line(source.as_ref());
        Ok(Self {
            source,
            cached_line,
            filepath: path,
        })
    }

    fn calculate_line(source: &[u8]) -> Vec<usize> {
        let mut lines: Vec<usize> = source.iter().enumerate().filter_map(|(i, &c)| {
            if c == b'\n' {
                Some(i + 1)
            } else {
                None
            }
        }).collect();
        lines.insert(0, 0);
        lines.push(source.len());
        return lines;
    }

    pub(crate) fn get_line(&self, line: usize) -> &str {
        let src = self.source.get_subspan(self.get_line_position(line));
        return std::str::from_utf8(src).expect("invalid utf-8 string slice"); 
    }

    pub(crate) fn get_line_position(&self, line: usize) -> Span {
        assert!(line > 0 && line <= self.cached_line.len() - 1, "Line out of range");
        let start = self.cached_line[line - 1] as u32;
        let end = self.cached_line[line] as u32;
        Span::new(start, end)
    }

    pub(crate) fn get_line_from_cursor(&self, cursor: usize) -> usize {
        let mut left = 0;
        let mut right = self.cached_line.len() - 1;
        while left < right {
            let mid = (left + right) / 2;
            if self.cached_line[mid] <= cursor {
                left = mid + 1;
            } else {
                right = mid;
            }
        }
        return left;
    }

    pub(crate) fn get_filepath(&self) -> &PathBuf {
        &self.filepath
    }

    pub(crate) fn fix_span(&self, span: Span) -> Span {
        let line = self.get_line_from_cursor(span.start as usize);
        let column = span.start - self.cached_line[line - 1] as u32;
        Span::new(column + 1, column + 1 + span.end - span.start)
    }

    pub(crate) fn get_source_info(&self, span: Span) -> SourceManagerDiagnosticInfo {
        let line = self.get_line_from_cursor(span.start as usize);
        let column = span.start - self.cached_line[line - 1] as u32;
        let line_span = self.get_line_position(line);
        let source = std::str::from_utf8(self.source.get_subspan(line_span)).expect("invalid utf-8 string slice").to_string();
        SourceManagerDiagnosticInfo {
            source ,
            line,
            column: column as usize,
        }
    }

    pub(crate) fn get_source_info_from_position(&self, position: usize) -> SourceManagerDiagnosticInfo {
        let line = self.get_line_from_cursor(position);
        let column = position - self.cached_line[line - 1];
        let line_span = self.get_line_position(line);
        SourceManagerDiagnosticInfo {
            source: std::str::from_utf8(self.source.get_subspan(line_span)).expect("invalid utf-8 string slice").to_string(),
            line,
            column,
        }
    }

    pub(crate) fn get_byte(&self, index: usize) -> Option<u8> {
        let source = self.source.as_slice();
        if source.len() <= index {
            return None;
        }
        return Some(source[index]);
    }

    pub(crate) fn get_char(&self, index: usize) -> (Option<char>, usize) {
        let source = self.source.as_slice();
        if source.len() <= index {
            return (None, 0);
        }
        if source[index] < 128 {
            if source[index] == b'\r' {
                if index + 1 < source.len() && source[index + 1] == b'\n' {
                    return (Some('\n'), 2);
                } else {
                    return (Some('\n'), 1);
                }
            }
            return (Some(source[index] as char), 1);
        }
        return valid_utf8_character_with_char_len(&source[index..]);
    }

    pub(crate) fn len(&self) -> usize {
        self.source.len()
    }

    pub(crate) fn get_source(&self) -> &MappedFile {
        &self.source
    }
}

impl Index<Span> for SourceManager {
    type Output = [u8];

    fn index(&self, index: Span) -> &Self::Output {
        self.source.get_subspan(index)
    }
}

impl Index<RangeFrom<usize>> for SourceManager {
    type Output = [u8];

    fn index(&self, index: RangeFrom<usize>) -> &Self::Output {
        &self.source[index]
    }
}

impl Index<usize> for SourceManager {
    type Output = u8;

    fn index(&self, index: usize) -> &Self::Output {
        &self.source[index]
    }
}
