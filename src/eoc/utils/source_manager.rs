#![allow(dead_code)]

use std::{ops::{Index, RangeFrom}, path::{Path, PathBuf}};
use crate::eoc::lexer::str_utils::byte_to_char;

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

impl SourceManagerDiagnosticInfo {
    pub(crate) fn is_valid(&self) -> bool {
        if self.source.is_empty() {
            return false;
        }
        true
    }
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
        byte_to_char(&source[index..])
    }

    pub(crate) fn str_from_span(&self, span: Span) -> &str {
        std::str::from_utf8(self[span].as_ref()).expect("invalid utf-8 string slice")
    }

    pub(crate) fn len(&self) -> usize {
        self.source.len()
    }

    pub(crate) fn get_source(&self) -> &MappedFile {
        &self.source
    }

    pub(crate) fn make_span(&self, start: usize, end: usize) -> Span {
        Span::from_usize(start, end)
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

#[derive(Clone, Copy)]
pub(crate) struct RelativeSourceManager<'a>(pub(crate) &'a SourceManager, u32);

impl<'a> RelativeSourceManager<'a> {
    pub(crate) fn new(source_manager: &'a SourceManager, base_pos: u32) -> Self {
        Self(source_manager, base_pos)
    }

    pub(crate) fn get_source_info(&self, span: Span) -> SourceManagerDiagnosticInfo {
        self.0.get_source_info(self.abs_span(span))
    }

    pub(crate) fn fix_span(&self, span: Span) -> Span {
        self.0.fix_span(self.abs_span(span))
    }

    pub(crate) fn abs_span(&self, span: Span) -> Span {
        span.relative(self.1)
    }

    pub(crate) fn shift_relative_pos_by(&self, pos: u32) -> Self {
        Self(self.0, self.1 + pos)
    }

    pub(crate) fn make_span(&self, start: usize, end: usize) -> Span {
        self.abs_span(self.0.make_span(start, end))
    }
}

impl Index<Span> for RelativeSourceManager<'_> {
    type Output = [u8];

    fn index(&self, index: Span) -> &Self::Output {
        &self.0[self.abs_span(index)]
    }
}

impl Index<RangeFrom<usize>> for RelativeSourceManager<'_> {
    type Output = [u8];

    fn index(&self, index: RangeFrom<usize>) -> &Self::Output {
        &self.0[index]
    }
}

impl Index<usize> for RelativeSourceManager<'_> {
    type Output = u8;

    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

impl<'a> From<&'a SourceManager> for RelativeSourceManager<'a> {
    fn from(value: &'a SourceManager) -> Self {
        RelativeSourceManager(&value, 0)
    }
}

impl<'a> From<RelativeSourceManager<'a>> for &'a SourceManager {
    fn from(value: RelativeSourceManager<'a>) -> Self {
        &value.0
    }
}