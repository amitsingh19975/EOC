#![allow(dead_code)]

use std::fmt::Display;

use crate::eoc::utils::{source_manager::{self, SourceManager}, span::Span};

const OPERATOR_CHARS: &'static str = "/=-+*%<>!&|^~.?";

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum DeclRefKind {
    Ordinary, // 'foo'
    BinaryOperator, // 'a+b'
    PostfixOperator, // 'a++'
    PrefixOperator // '++a'
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum CustomOperatorKind {
    InfixOperator, // 'a+b'
    PostfixOperator, // 'a++'
    PrefixOperator, // '++a'
    ContextDep, // +a, a + b
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct Identifier {
    pub(crate) span: Span,
}

impl Identifier {
    pub(crate) fn new(span: Span) -> Identifier {
        Identifier {
            span,
        }
    }

    pub(crate) fn as_bytes<'a>(&self, source_manager: &'a SourceManager) -> &'a [u8] {
        &source_manager[self.span]
    }

    pub(crate) fn len(&self) -> usize {
        self.span.len()
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.span.is_empty()
    }

    pub(crate) fn is(&self, other: Span) -> bool {
        self.span == other
    }

    pub(crate) fn is_operator_start_code_point(ch: char) -> bool {
        let c = ch as u32;

        if c < 0x80 {
            return OPERATOR_CHARS.contains(ch);
        }

        // Unicode math, symbol, arrow, dingbat, and line/box drawing chars.
        (c >= 0x00A1 && c <= 0x00A7)
            || c == 0x00A9 || c == 0x00AB || c == 0x00Ac || c == 0x00AE
            || c == 0x00B0 || c == 0x00B1 || c == 0x00B6 || c == 0x00BB
            || c == 0x00BF || c == 0x00D7 || c == 0x00F7
            || c == 0x2016 || c == 0x2017 || (c >= 0x2020 && c <= 0x2027)
            || (c >= 0x2030 && c <= 0x203E) || (c >= 0x2041 && c <= 0x2053)
            || (c >= 0x2055 && c <= 0x205E) || (c >= 0x2190 && c <= 0x23FF)
            || (c >= 0x2500 && c <= 0x2775) || (c >= 0x2794 && c <= 0x2BFF)
            || (c >= 0x2E00 && c <= 0x2E7F) || (c >= 0x3001 && c <= 0x3003)
            || (c >= 0x3008 && c <= 0x3030)
    }

    pub(crate) fn is_operator_continuation_code_point(ch: char) -> bool {
        if Self::is_operator_start_code_point(ch) {
            return true;
        }

        let c = ch as u32;

        // Unicode combining characters and variation selectors.
        (c >= 0x0300 && c <= 0x036F)
            || (c >= 0x1DC0 && c <= 0x1DFF)
            || (c >= 0x20D0 && c <= 0x20FF)
            || (c >= 0xFE00 && c <= 0xFE0F)
            || (c >= 0xFE20 && c <= 0xFE2F)
            || (c >= 0xE0100 && c <= 0xE01EF)
    }

    pub(crate) fn to_str<'a>(&self, source_manager: &'a SourceManager) -> &'a str {
        let slice = &source_manager[self.span];
        std::str::from_utf8(slice).expect("Invalid UTF-8")
    }
}
