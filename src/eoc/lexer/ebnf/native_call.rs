use lazy_static::lazy_static;
use std::fmt::{Debug, Display};

use crate::eoc::{
    lexer::{
        str_utils::{get_utf8_char_len, ByteToCharIter},
        token::TokenKind,
    },
    utils::{diagnostic::Diagnostic, string::UniqueString},
};

use super::{
    ast::RelativeSourceManager, basic::LexerEbnfMatcher,
    default_matcher::DefaultLexerEbnfParserMatcher, ir_matcher::IRLexerEbnfParserMatcher, vm_state::LexerVmState,
};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum LexerNativeCallKind {
    StartIdentifier,
    ContIdentifier,
    Identifier,
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
    Operator,
    Integer,
    FloatingPoint,
    Number,
    Unknown
}

impl LexerNativeCallKind {
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
            Self::Identifier => "identifier",
            Self::Operator => "operator",
            Self::Number => "number",
            Self::Unknown => "unknown",
        }
    }

    pub(crate) fn as_unique_str(&self) -> UniqueString {
        match self {
            Self::StartIdentifier => NATIVE_CALL_KIND_ID.start_identifier_sym.clone(),
            Self::ContIdentifier => NATIVE_CALL_KIND_ID.cont_identifier_sym.clone(),
            Self::Whitespace => NATIVE_CALL_KIND_ID.whitespace_sym.clone(),
            Self::NewLine => NATIVE_CALL_KIND_ID.new_line_sym.clone(),
            Self::Tab => NATIVE_CALL_KIND_ID.tab_sym.clone(),
            Self::Digit => NATIVE_CALL_KIND_ID.is_digit.clone(),
            Self::Letter => NATIVE_CALL_KIND_ID.letter_sym.clone(),
            Self::HexDigit => NATIVE_CALL_KIND_ID.is_hex_digit.clone(),
            Self::OctDigit => NATIVE_CALL_KIND_ID.is_oct_digit.clone(),
            Self::BinDigit => NATIVE_CALL_KIND_ID.is_binary_digit.clone(),
            Self::AlphaNumeric => NATIVE_CALL_KIND_ID.alpha_numeric_sym.clone(),
            Self::StartOperator => NATIVE_CALL_KIND_ID.start_operator_sym.clone(),
            Self::ContOperator => NATIVE_CALL_KIND_ID.cont_operator_sym.clone(),
            Self::Integer => NATIVE_CALL_KIND_ID.integer_sym.clone(),
            Self::FloatingPoint => NATIVE_CALL_KIND_ID.fp_sym.clone(),
            Self::Identifier => NATIVE_CALL_KIND_ID.identifier_sym.clone(),
            Self::Operator => NATIVE_CALL_KIND_ID.operator_sym.clone(),
            Self::Number => NATIVE_CALL_KIND_ID.number_sym.clone(),
            _ => panic!("Unknown native call kind '{:?}'", self)
        }
    }

    pub(crate) fn to_token_kind(&self) -> TokenKind {
        match self {
            LexerNativeCallKind::StartIdentifier
            | LexerNativeCallKind::ContIdentifier
            | LexerNativeCallKind::StartOperator
            | LexerNativeCallKind::ContOperator
            | LexerNativeCallKind::AlphaNumeric => TokenKind::CustomToken(self.as_unique_str()),
            LexerNativeCallKind::Identifier => TokenKind::Identifier,
            LexerNativeCallKind::Whitespace => TokenKind::Whitespace,
            LexerNativeCallKind::NewLine => TokenKind::Newline,
            LexerNativeCallKind::Tab => TokenKind::TabSpace,
            LexerNativeCallKind::Digit => TokenKind::Integer,
            LexerNativeCallKind::Letter => TokenKind::Identifier,
            LexerNativeCallKind::HexDigit => TokenKind::Integer,
            LexerNativeCallKind::OctDigit => TokenKind::Integer,
            LexerNativeCallKind::BinDigit => TokenKind::Integer,
            LexerNativeCallKind::Operator => TokenKind::Operator,
            LexerNativeCallKind::Integer => TokenKind::Integer,
            LexerNativeCallKind::FloatingPoint => TokenKind::FloatingPoint,
            LexerNativeCallKind::Number => TokenKind::CustomToken(self.as_unique_str()),
            LexerNativeCallKind::Unknown => TokenKind::Unknown,
        }
    }

    pub(super) fn call<'b, T: LexerEbnfMatcher>(
        &self,
        matcher: &T,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
        state: Option<&LexerVmState>
    ) -> Option<(&'b [u8], TokenKind)> {
        let Some(c) = ByteToCharIter::new(s).next() else {
            return None;
        };

        match self {
            Self::StartIdentifier => {
                let is_valid = if matcher.is_ir() {
                    IRLexerEbnfParserMatcher::is_valid_identifier_start_code_point(c)
                } else {
                    DefaultLexerEbnfParserMatcher::is_valid_identifier_start_code_point(c)
                };

                if is_valid {
                    Some((&s[..c.len_utf8()], self.to_token_kind()))
                } else {
                    None
                }
            }
            Self::ContIdentifier => {
                let is_valid = if matcher.is_ir() {
                    IRLexerEbnfParserMatcher::is_valid_identifier_continuation_code_point(c)
                } else {
                    DefaultLexerEbnfParserMatcher::is_valid_identifier_continuation_code_point(c)
                };

                if is_valid {
                    Some((&s[..c.len_utf8()], self.to_token_kind()))
                } else {
                    None
                }
            }
            Self::Whitespace => {
                if c.is_whitespace() {
                    Some((&s[..c.len_utf8()], self.to_token_kind()))
                } else {
                    None
                }
            }
            Self::NewLine => {
                if c == '\n' {
                    Some((&s[..c.len_utf8()], self.to_token_kind()))
                } else {
                    None
                }
            }
            Self::Tab => {
                if c == '\t' {
                    Some((&s[..c.len_utf8()], self.to_token_kind()))
                } else {
                    None
                }
            }
            Self::Digit => {
                if c.is_digit(10) {
                    Some((&s[..c.len_utf8()], self.to_token_kind()))
                } else {
                    None
                }
            }
            Self::Letter => {
                if c.is_alphabetic() {
                    Some((&s[..c.len_utf8()], self.to_token_kind()))
                } else {
                    None
                }
            }
            Self::HexDigit => {
                if c.is_digit(16) {
                    Some((&s[..c.len_utf8()], self.to_token_kind()))
                } else {
                    None
                }
            }
            Self::OctDigit => {
                if c.is_digit(8) {
                    Some((&s[..c.len_utf8()], self.to_token_kind()))
                } else {
                    None
                }
            }
            Self::BinDigit => {
                if c.is_digit(2) {
                    Some((&s[..c.len_utf8()], self.to_token_kind()))
                } else {
                    None
                }
            }
            Self::AlphaNumeric => {
                if c.is_alphanumeric() {
                    Some((&s[..c.len_utf8()], self.to_token_kind()))
                } else {
                    None
                }
            }
            Self::StartOperator => {
                if !matcher.is_ir()
                    && DefaultLexerEbnfParserMatcher::is_operator_start_code_point(c)
                {
                    Some((&s[..c.len_utf8()], self.to_token_kind()))
                } else {
                    None
                }
            }
            Self::ContOperator => {
                if !matcher.is_ir()
                    && DefaultLexerEbnfParserMatcher::is_operator_continuation_code_point(c)
                {
                    Some((&s[..c.len_utf8()], self.to_token_kind()))
                } else {
                    None
                }
            }
            Self::Integer => matcher
                .match_native_integer(s, source_manager, diagnostic, state)
                .map(|s| (s, self.to_token_kind())),
            Self::FloatingPoint => matcher
                .match_native_floating_point(s, source_manager, diagnostic, state)
                .map(|s| (s, self.to_token_kind())),
            Self::Identifier => {
                if s.is_empty() {
                    return None;
                }

                if matcher
                    .match_native(
                        LexerNativeCallKind::StartIdentifier,
                        s,
                        source_manager,
                        diagnostic,
                        state.clone()
                    )
                    .is_none()
                {
                    return None;
                }

                let mut i = get_utf8_char_len(s[0]);
                while i < s.len() {
                    let end = get_utf8_char_len(s[i]);
                    let temp_source = &s[i..(i + end).min(s.len() - 1)];
                    if matcher
                        .match_native(
                            LexerNativeCallKind::ContIdentifier,
                            temp_source,
                            source_manager,
                            diagnostic,
                            state
                        )
                        .is_none()
                    {
                        break;
                    }
                    i += end;
                }

                Some((&s[..i], TokenKind::Identifier))
            }
            Self::Operator => {
                if s.is_empty() {
                    return None;
                }
                if matcher
                    .match_native(
                        LexerNativeCallKind::StartOperator,
                        s,
                        source_manager,
                        diagnostic,
                        state
                    )
                    .is_none()
                {
                    return None;
                }

                let mut i = get_utf8_char_len(s[0]);
                while i < s.len() {
                    let end = get_utf8_char_len(s[i]);
                    let temp_source = &s[i..];
                    if matcher
                        .match_native(
                            LexerNativeCallKind::ContOperator,
                            temp_source,
                            source_manager,
                            diagnostic,
                            state
                        )
                        .is_none()
                    {
                        break;
                    }

                    i += end;
                }

                Some((&s[..i], TokenKind::Operator))
            }
            Self::Number => {
                if s.is_empty() {
                    return None;
                }

                if let Some(int) = matcher.match_native(
                    LexerNativeCallKind::Integer,
                    s,
                    source_manager,
                    diagnostic,
                    state
                ) {
                    return Some(int);
                }

                matcher
                    .match_native(
                        LexerNativeCallKind::FloatingPoint,
                        s,
                        source_manager,
                        diagnostic,
                        state
                    )
            }
            Self::Unknown => None,
        }
    }

    // pub(super) fn is_valid_name(name: &str) -> bool {
    //     match name {
    //         "start_identifier" | "cont_identifier" | "whitespace" | "new_line" | "tab"
    //         | "digit" | "letter" | "hex_digit" | "oct_digit" | "bin_digit" | "alpha_numeric"
    //         | "start_operator" | "floating_point" | "integer" | "cont_operator" | "identifier"
    //         | "operator" | "number" => true,
    //         _ => false,
    //     }
    // }
}

impl Debug for LexerNativeCallKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<Native Call='{}'>", self.as_str())
    }
}

impl Display for LexerNativeCallKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl From<&str> for LexerNativeCallKind {
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

impl From<String> for LexerNativeCallKind {
    fn from(s: String) -> Self {
        LexerNativeCallKind::from(s.as_str())
    }
}

impl PartialEq<str> for LexerNativeCallKind {
    fn eq(&self, other: &str) -> bool {
        self.as_str() == other
    }
}

pub(crate) struct NativeCallKindId {
    pub(crate) is_digit: UniqueString,
    pub(crate) is_hex_digit: UniqueString,
    pub(crate) is_oct_digit: UniqueString,
    pub(crate) is_binary_digit: UniqueString,
    pub(crate) identifier_sym: UniqueString,
    pub(crate) operator_sym: UniqueString,
    pub(crate) integer_sym: UniqueString,
    pub(crate) fp_sym: UniqueString,
    pub(crate) string_sym: UniqueString,
    pub(crate) char_sym: UniqueString,
    pub(crate) start_identifier_sym: UniqueString,
    pub(crate) cont_identifier_sym: UniqueString,
    pub(crate) start_operator_sym: UniqueString,
    pub(crate) cont_operator_sym: UniqueString,
    pub(crate) whitespace_sym: UniqueString,
    pub(crate) new_line_sym: UniqueString,
    pub(crate) tab_sym: UniqueString,
    pub(crate) letter_sym: UniqueString,
    pub(crate) alpha_numeric_sym: UniqueString,
    pub(crate) number_sym: UniqueString,
}

impl NativeCallKindId {
    pub(crate) fn new() -> Self {
        Self {
            is_digit: UniqueString::new(LexerNativeCallKind::Digit.as_str()),
            is_hex_digit: UniqueString::new(LexerNativeCallKind::HexDigit.as_str()),
            is_oct_digit: UniqueString::new(LexerNativeCallKind::OctDigit.as_str()),
            is_binary_digit: UniqueString::new(LexerNativeCallKind::BinDigit.as_str()),
            identifier_sym: UniqueString::new("identifier"),
            operator_sym: UniqueString::new("operator"),
            integer_sym: UniqueString::new("integer"),
            fp_sym: UniqueString::new(LexerNativeCallKind::FloatingPoint.as_str()),
            string_sym: UniqueString::new("string_literal"),
            char_sym: UniqueString::new("char_literal"),
            start_identifier_sym: UniqueString::new(LexerNativeCallKind::StartIdentifier.as_str()),
            cont_identifier_sym: UniqueString::new(LexerNativeCallKind::ContIdentifier.as_str()),
            start_operator_sym: UniqueString::new(LexerNativeCallKind::StartOperator.as_str()),
            cont_operator_sym: UniqueString::new(LexerNativeCallKind::ContOperator.as_str()),
            whitespace_sym: UniqueString::new(LexerNativeCallKind::Whitespace.as_str()),
            new_line_sym: UniqueString::new(LexerNativeCallKind::NewLine.as_str()),
            tab_sym: UniqueString::new(LexerNativeCallKind::Tab.as_str()),
            letter_sym: UniqueString::new(LexerNativeCallKind::Letter.as_str()),
            alpha_numeric_sym: UniqueString::new(LexerNativeCallKind::AlphaNumeric.as_str()),
            number_sym: UniqueString::new("number"),
        }
    }

    pub(crate) fn get_id(&self, name: UniqueString) -> Option<usize> {
        match name {
            _ if self.identifier_sym == name => Some(0),
            _ if self.operator_sym == name => Some(1),
            _ if self.fp_sym == name => Some(2),
            _ if self.integer_sym == name => Some(3),
            _ if self.number_sym == name => Some(4),
            _ if self.string_sym == name => Some(5),
            _ if self.char_sym == name => Some(6),
            _ if self.start_identifier_sym == name => Some(7),
            _ if self.cont_identifier_sym == name => Some(8),
            _ if self.start_operator_sym == name => Some(9),
            _ if self.cont_operator_sym == name => Some(10),
            _ if self.whitespace_sym == name => Some(11),
            _ if self.new_line_sym == name => Some(12),
            _ if self.tab_sym == name => Some(13),
            _ if self.letter_sym == name => Some(14),
            _ if self.alpha_numeric_sym == name => Some(15),
            _ if self.is_binary_digit == name => Some(16),
            _ if self.is_oct_digit == name => Some(17),
            _ if self.is_hex_digit == name => Some(18),
            _ if self.is_digit == name => Some(19),
            _ => None,
        }
    }

    pub(crate) fn id_to_string(&self, id: usize) -> Option<UniqueString> {
        match id {
            0 => Some(self.identifier_sym.clone()),
            1 => Some(self.operator_sym.clone()),
            2 => Some(self.fp_sym.clone()),
            3 => Some(self.integer_sym.clone()),
            4 => Some(self.number_sym.clone()),
            5 => Some(self.string_sym.clone()),
            6 => Some(self.char_sym.clone()),
            7 => Some(self.start_identifier_sym.clone()),
            8 => Some(self.cont_identifier_sym.clone()),
            9 => Some(self.start_operator_sym.clone()),
            10 => Some(self.cont_operator_sym.clone()),
            11 => Some(self.whitespace_sym.clone()),
            12 => Some(self.new_line_sym.clone()),
            13 => Some(self.tab_sym.clone()),
            14 => Some(self.letter_sym.clone()),
            15 => Some(self.alpha_numeric_sym.clone()),
            16 => Some(self.is_binary_digit.clone()),
            17 => Some(self.is_oct_digit.clone()),
            18 => Some(self.is_hex_digit.clone()),
            19 => Some(self.is_digit.clone()),
            _ => None,
        }
    }

    pub(crate) fn to_native_call_kind(&self, name: UniqueString) -> Option<LexerNativeCallKind> {
        match name {
            _ if self.start_identifier_sym == name => Some(LexerNativeCallKind::StartIdentifier),
            _ if self.cont_identifier_sym == name => Some(LexerNativeCallKind::ContIdentifier),
            _ if self.whitespace_sym == name => Some(LexerNativeCallKind::Whitespace),
            _ if self.new_line_sym == name => Some(LexerNativeCallKind::NewLine),
            _ if self.tab_sym == name => Some(LexerNativeCallKind::Tab),
            _ if self.letter_sym == name => Some(LexerNativeCallKind::Letter),
            _ if self.alpha_numeric_sym == name => Some(LexerNativeCallKind::AlphaNumeric),
            _ if self.start_operator_sym == name => Some(LexerNativeCallKind::StartOperator),
            _ if self.cont_operator_sym == name => Some(LexerNativeCallKind::ContOperator),
            _ if self.integer_sym == name => Some(LexerNativeCallKind::Integer),
            _ if self.fp_sym == name => Some(LexerNativeCallKind::FloatingPoint),
            _ if self.identifier_sym == name => Some(LexerNativeCallKind::Identifier),
            _ if self.operator_sym == name => Some(LexerNativeCallKind::Operator),
            _ if self.number_sym == name => Some(LexerNativeCallKind::Number),
            _ if self.is_binary_digit == name => Some(LexerNativeCallKind::BinDigit),
            _ if self.is_oct_digit == name => Some(LexerNativeCallKind::OctDigit),
            _ if self.is_hex_digit == name => Some(LexerNativeCallKind::HexDigit),
            _ if self.is_digit == name => Some(LexerNativeCallKind::Digit),
            _ => None,
        }
    }

    pub(crate) fn init(&self) {
        let _ = NATIVE_CALL_KIND_ID.is_digit;
        let _ = NATIVE_CALL_KIND_ID.is_hex_digit;
        let _ = NATIVE_CALL_KIND_ID.is_oct_digit;
        let _ = NATIVE_CALL_KIND_ID.is_binary_digit;
        let _ = NATIVE_CALL_KIND_ID.identifier_sym;
        let _ = NATIVE_CALL_KIND_ID.operator_sym;
        let _ = NATIVE_CALL_KIND_ID.integer_sym;
        let _ = NATIVE_CALL_KIND_ID.fp_sym;
        let _ = NATIVE_CALL_KIND_ID.string_sym;
        let _ = NATIVE_CALL_KIND_ID.char_sym;
        let _ = NATIVE_CALL_KIND_ID.start_identifier_sym;
        let _ = NATIVE_CALL_KIND_ID.cont_identifier_sym;
        let _ = NATIVE_CALL_KIND_ID.start_operator_sym;
        let _ = NATIVE_CALL_KIND_ID.cont_operator_sym;
        let _ = NATIVE_CALL_KIND_ID.whitespace_sym;
        let _ = NATIVE_CALL_KIND_ID.new_line_sym;
        let _ = NATIVE_CALL_KIND_ID.tab_sym;
        let _ = NATIVE_CALL_KIND_ID.letter_sym;
        let _ = NATIVE_CALL_KIND_ID.alpha_numeric_sym;
        let _ = NATIVE_CALL_KIND_ID.number_sym;
    }
}

lazy_static! {
    pub(crate) static ref NATIVE_CALL_KIND_ID: NativeCallKindId = NativeCallKindId::new();
}
