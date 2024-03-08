use std::fmt::{Debug, Display};

use crate::eoc::{
    ast::identifier::Identifier,
    lexer::{
        str_utils::ByteToCharIter,
        utils::{
            is_valid_identifier_continuation_code_point, is_valid_identifier_start_code_point,
        },
    },
    utils::diagnostic::Diagnostic,
};

use super::{ast::RelativeSourceManager, matcher::CustomEbnfParserMatcher};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum NativeCallKind {
    StartIdentifier,
    ContIdentifier,
    Whitespace,
    NewLine,
    Tab,
    Digit,
    Letter,
    HexDigit,
    OctDigit,
    BinDigit,
    AlphaNumeric,
    StartOperator,
    ContOperator,
    Integer,
    FloatingPoint,
}

impl NativeCallKind {
    pub(super) fn as_str(&self) -> &'static str {
        match self {
            Self::StartIdentifier => "start_identifier",
            Self::ContIdentifier => "cont_identifier",
            Self::Whitespace => "whitespace",
            Self::NewLine => "new_line",
            Self::Tab => "tab",
            Self::Digit => "digit",
            Self::Letter => "letter",
            Self::HexDigit => "hex_digit",
            Self::OctDigit => "oct_digit",
            Self::BinDigit => "bin_digit",
            Self::AlphaNumeric => "alpha_numeric",
            Self::StartOperator => "start_operator",
            Self::ContOperator => "cont_operator",
            Self::Integer => "integer",
            Self::FloatingPoint => "floating_point",
        }
    }

    pub(super) fn call<'b>(
        &self,
        matcher: &CustomEbnfParserMatcher,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &mut Diagnostic,
    ) -> Option<&'b [u8]> {
        let c = ByteToCharIter::new(s).next();
        if c.is_none() {
            return None;
        }

        let c = c.unwrap();

        match self {
            Self::StartIdentifier => {
                if is_valid_identifier_start_code_point(c) {
                    Some(&s[..c.len_utf8()])
                } else {
                    None
                }
            }
            Self::ContIdentifier => {
                if is_valid_identifier_continuation_code_point(c) {
                    Some(&s[..c.len_utf8()])
                } else {
                    None
                }
            }
            Self::Whitespace => {
                if c.is_whitespace() {
                    Some(&s[..c.len_utf8()])
                } else {
                    None
                }
            }
            Self::NewLine => {
                if c == '\n' {
                    Some(&s[..c.len_utf8()])
                } else {
                    None
                }
            }
            Self::Tab => {
                if c == '\t' {
                    Some(&s[..c.len_utf8()])
                } else {
                    None
                }
            }
            Self::Digit => {
                if c.is_digit(10) {
                    Some(&s[..c.len_utf8()])
                } else {
                    None
                }
            }
            Self::Letter => {
                if c.is_alphabetic() {
                    Some(&s[..c.len_utf8()])
                } else {
                    None
                }
            }
            Self::HexDigit => {
                if c.is_digit(16) {
                    Some(&s[..c.len_utf8()])
                } else {
                    None
                }
            }
            Self::OctDigit => {
                if c.is_digit(8) {
                    Some(&s[..c.len_utf8()])
                } else {
                    None
                }
            }
            Self::BinDigit => {
                if c.is_digit(2) {
                    Some(&s[..c.len_utf8()])
                } else {
                    None
                }
            }
            Self::AlphaNumeric => {
                if c.is_alphanumeric() {
                    Some(&s[..c.len_utf8()])
                } else {
                    None
                }
            }
            Self::StartOperator => {
                if Identifier::is_operator_start_code_point(c) {
                    Some(&s[..c.len_utf8()])
                } else {
                    None
                }
            }
            Self::ContOperator => {
                if Identifier::is_operator_continuation_code_point(c) {
                    Some(&s[..c.len_utf8()])
                } else {
                    None
                }
            }
            Self::Integer => matcher.match_native_integer(s, source_manager, diagnostic),
            Self::FloatingPoint => {
                matcher.match_native_floating_point(s, source_manager, diagnostic)
            }
        }
    }

    pub(super) fn is_valid_name(name: &str) -> bool {
        match name {
            "start_identifier" | "cont_identifier" | "whitespace" | "new_line" | "tab"
            | "digit" | "letter" | "hex_digit" | "oct_digit" | "bin_digit" | "alpha_numeric"
            | "start_operator" | "floating_point" | "integer" | "cont_operator" => true,
            _ => false,
        }
    }
}

impl Debug for NativeCallKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<Native Call='{}'>", self.as_str())
    }
}

impl Display for NativeCallKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl From<&str> for NativeCallKind {
    fn from(s: &str) -> Self {
        match s {
            "start_identifier" => Self::StartIdentifier,
            "cont_identifier" => Self::ContIdentifier,
            "whitespace" => Self::Whitespace,
            "new_line" => Self::NewLine,
            "tab" => Self::Tab,
            "digit" => Self::Digit,
            "letter" => Self::Letter,
            "hex_digit" => Self::HexDigit,
            "oct_digit" => Self::OctDigit,
            "bin_digit" => Self::BinDigit,
            "alpha_numeric" => Self::AlphaNumeric,
            "start_operator" => Self::StartOperator,
            "cont_operator" => Self::ContOperator,
            "integer" => Self::Integer,
            "floating_point" => Self::FloatingPoint,
            _ => unreachable!("Unknown native call kind '{}'", s),
        }
    }
}

impl From<String> for NativeCallKind {
    fn from(s: String) -> Self {
        NativeCallKind::from(s.as_str())
    }
}

impl PartialEq<str> for NativeCallKind {
    fn eq(&self, other: &str) -> bool {
        self.as_str() == other
    }
}
