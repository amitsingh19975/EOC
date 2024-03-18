use crate::eoc::{
    ast::identifier::Identifier,
    lexer::{
        number::{parse_floating_point, parse_integer},
        str_utils::ByteToCharIter,
        token::TokenKind,
        utils::{
            is_valid_identifier_continuation_code_point, is_valid_identifier_start_code_point,
        },
    },
    utils::{diagnostic::Diagnostic, span::Span},
};

use super::{
    ast::RelativeSourceManager,
    expr::EbnfExpr,
    native_call::{NativeCallKind, NATIVE_CALL_KIND_ID},
    vm::{Vm, VmBuilder},
};

pub(crate) trait EbnfMatcher {
    fn is_digit<'b>(
        &self,
        s: &'b [u8],
        _source_manager: RelativeSourceManager<'b>,
        _diagnostic: &Diagnostic,
    ) -> Option<char> {
        ByteToCharIter::new(s).next().filter(|c| c.is_ascii_digit())
    }

    fn is_hex_digit<'b>(
        &self,
        s: &'b [u8],
        _source_manager: RelativeSourceManager<'b>,
        _diagnostic: &Diagnostic,
    ) -> Option<char> {
        ByteToCharIter::new(s)
            .next()
            .filter(|c| c.is_ascii_hexdigit())
    }

    fn is_oct_digit<'b>(
        &self,
        s: &'b [u8],
        _source_manager: RelativeSourceManager<'b>,
        _diagnostic: &Diagnostic,
    ) -> Option<char> {
        ByteToCharIter::new(s)
            .next()
            .filter(|c| c.is_ascii_digit() && *c < '8')
    }

    fn is_binary_digit<'b>(
        &self,
        s: &'b [u8],
        _source_manager: RelativeSourceManager<'b>,
        _diagnostic: &Diagnostic,
    ) -> Option<char> {
        ByteToCharIter::new(s)
            .next()
            .filter(|c| *c == '0' || *c == '1')
    }

    fn contains_def(&self, _name: &str) -> bool {
        false
    }

    fn match_native_integer<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
    ) -> Option<&'b [u8]> {
        parse_integer(
            s,
            |s, sm, d| self.is_digit(s, sm, d),
            |s, sm, d| self.is_hex_digit(s, sm, d),
            |s, sm, d| self.is_oct_digit(s, sm, d),
            |s, sm, d| self.is_binary_digit(s, sm, d),
            source_manager,
            diagnostic,
        )
    }

    fn match_native_floating_point<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
    ) -> Option<&'b [u8]> {
        parse_floating_point(
            s,
            |s, sm, d| self.is_digit(s, sm, d),
            |s, sm, d| self.is_hex_digit(s, sm, d),
            source_manager,
            diagnostic,
        )
    }

    fn match_operator<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
    ) -> Option<&'b [u8]>;

    fn match_identifier<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
    ) -> Option<&'b [u8]>;

    fn match_native<'b>(
        &self,
        kind: NativeCallKind,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
    ) -> Option<&'b [u8]>;

    fn match_expr_for<'a>(
        &self,
        var: &str,
        s: &'a [u8],
        source_manager: RelativeSourceManager<'a>,
        diagnostic: &Diagnostic,
    ) -> Option<&'a [u8]>;

    fn try_match_expr<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
    ) -> Option<(&'b [u8], TokenKind)>;

    fn has_custom_digit_lexing(&self) -> bool {
        self.contains_def(NATIVE_CALL_KIND_ID.is_hex_digit.as_str())
            || self.contains_def(NATIVE_CALL_KIND_ID.is_oct_digit.as_str())
            || self.contains_def(NATIVE_CALL_KIND_ID.is_oct_digit.as_str())
            || self.contains_def(NATIVE_CALL_KIND_ID.is_binary_digit.as_str())
    }

    fn has_custom_integer_lexing(&self) -> bool {
        self.contains_def(NATIVE_CALL_KIND_ID.integer_sym.as_str())
            || self.has_custom_digit_lexing()
    }

    fn has_custom_floating_point_lexing(&self) -> bool {
        self.contains_def(NATIVE_CALL_KIND_ID.fp_sym.as_str()) || self.has_custom_integer_lexing()
    }

    fn has_custom_identifier_lexing(&self) -> bool {
        self.contains_def(NATIVE_CALL_KIND_ID.identifier_sym.as_str())
    }

    fn init<'b>(
        &mut self,
        expr: Option<EbnfExpr>,
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
    );
}

#[derive(Clone)]
pub(crate) struct DefaultEbnfParserMatcher;

impl DefaultEbnfParserMatcher {
    pub(crate) fn new() -> Self {
        Self
    }

    pub(crate) fn is_valid_identifier_start_code_point(c: char) -> bool {
        is_valid_identifier_start_code_point(c)
    }

    pub(crate) fn is_valid_identifier_continuation_code_point(c: char) -> bool {
        is_valid_identifier_continuation_code_point(c)
    }

    pub(crate) fn is_operator_start_code_point(c: char) -> bool {
        Identifier::is_operator_start_code_point(c)
    }

    pub(crate) fn is_operator_continuation_code_point(c: char) -> bool {
        Identifier::is_operator_continuation_code_point(c)
    }

    pub(crate) fn match_native_identifier<'b>(
        &self,
        s: &'b [u8],
        _source_manager: RelativeSourceManager<'b>,
        _diagnostic: &Diagnostic,
    ) -> Option<&'b [u8]> {
        if s.is_empty() {
            return None;
        }

        let mut iter = ByteToCharIter::new(s);

        let Some(c) = iter.next() else {
            return None;
        };

        if !Self::is_valid_identifier_start_code_point(c) {
            return None;
        }

        let mut i = c.len_utf8();

        while i < s.len() {
            if let Some(c) = iter.next() {
                if !Self::is_valid_identifier_continuation_code_point(c) {
                    return Some(&s[..i]);
                }
                i += c.len_utf8();
            } else {
                break;
            }
        }

        Some(&s[..i])
    }

    fn match_native_operator<'b>(
        &self,
        s: &'b [u8],
        _source_manager: RelativeSourceManager<'b>,
        _diagnostic: &Diagnostic,
    ) -> Option<&'b [u8]> {
        if s.is_empty() {
            return None;
        }

        let mut iter = ByteToCharIter::new(s);

        let Some(c) = iter.next() else {
            return None;
        };

        if !Self::is_operator_start_code_point(c) {
            return None;
        }

        let mut i = c.len_utf8();
        while i < s.len() {
            if let Some(c) = iter.next() {
                if !Self::is_operator_continuation_code_point(c) {
                    return Some(&s[..i]);
                }
                i += c.len_utf8();
            } else {
                break;
            }
        }

        Some(&s[..i])
    }

    pub(crate) fn match_identifier<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
    ) -> Option<&'b [u8]> {
        self.match_native_identifier(s, source_manager, diagnostic)
    }
}

impl EbnfMatcher for DefaultEbnfParserMatcher {
    fn init<'b>(
        &mut self,
        _expr: Option<EbnfExpr>,
        _source_manager: RelativeSourceManager<'b>,
        _diagnostic: &Diagnostic,
    ) {
    }

    fn match_operator<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
    ) -> Option<&'b [u8]> {
        self.match_native_operator(s, source_manager, diagnostic)
    }

    fn match_identifier<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
    ) -> Option<&'b [u8]> {
        self.match_native_identifier(s, source_manager, diagnostic)
    }

    fn match_native<'b>(
        &self,
        kind: NativeCallKind,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
    ) -> Option<&'b [u8]> {
        kind.call(
            &DefaultEbnfParserMatcher::new(),
            s,
            source_manager,
            diagnostic,
        )
    }

    fn match_expr_for<'a>(
        &self,
        var: &str,
        s: &'a [u8],
        source_manager: RelativeSourceManager<'a>,
        diagnostic: &Diagnostic,
    ) -> Option<&'a [u8]> {
        match var {
            _ if var == NATIVE_CALL_KIND_ID.identifier_sym => {
                self.match_native_identifier(s, source_manager, diagnostic)
            }
            _ if var == NATIVE_CALL_KIND_ID.operator_sym => {
                self.match_native_operator(s, source_manager, diagnostic)
            }
            _ if var == NATIVE_CALL_KIND_ID.fp_sym => {
                self.match_native_floating_point(s, source_manager, diagnostic)
            }
            _ if var == NATIVE_CALL_KIND_ID.integer_sym => {
                self.match_native_integer(s, source_manager, diagnostic)
            }
            _ => None,
        }
    }

    fn try_match_expr<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
    ) -> Option<(&'b [u8], TokenKind)> {
        for k in &[
            NATIVE_CALL_KIND_ID.identifier_sym,
            NATIVE_CALL_KIND_ID.operator_sym,
            NATIVE_CALL_KIND_ID.fp_sym,
            NATIVE_CALL_KIND_ID.integer_sym,
        ] {
            if let Some(s) = self.match_expr_for(k.as_str(), s, source_manager, diagnostic) {
                return Some((s, TokenKind::CustomToken(*k)));
            }
        }
        None
    }
}

#[derive(Clone)]
pub(crate) enum EbnfParserMatcher {
    Custom(Vm),
    Default(DefaultEbnfParserMatcher),
}

impl EbnfParserMatcher {
    pub(crate) fn new() -> Self {
        Self::Default(DefaultEbnfParserMatcher::new())
    }
}

impl EbnfMatcher for EbnfParserMatcher {
    fn init<'b>(
        &mut self,
        expr: Option<EbnfExpr>,
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
    ) {
        if let Some(expr) = expr {
            if expr.is_empty() {
                return;
            }
            match self {
                Self::Custom(m) => m.init(Some(expr), source_manager, diagnostic),
                Self::Default(_) => {
                    let mut m = VmBuilder::new();
                    m.from(expr, diagnostic);
                    let temp = m.build();
                    *self = Self::Custom(temp);
                }
            }
        }
    }

    fn contains_def(&self, name: &str) -> bool {
        match self {
            Self::Custom(m) => m.contains_def(name),
            Self::Default(_) => false,
        }
    }

    fn try_match_expr<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
    ) -> Option<(&'b [u8], TokenKind)> {
        match self {
            Self::Custom(m) => m.try_match_expr(s, source_manager, diagnostic),
            Self::Default(_) => None,
        }
    }

    fn match_expr_for<'b>(
        &self,
        var: &str,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
    ) -> Option<&'b [u8]> {
        match self {
            Self::Custom(m) => m.match_expr_for(var, s, source_manager, diagnostic),
            Self::Default(m) => m.match_expr_for(var, s, source_manager, diagnostic),
        }
    }

    fn match_identifier<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
    ) -> Option<&'b [u8]> {
        match self {
            Self::Custom(m) => m.match_identifier(s, source_manager, diagnostic),
            Self::Default(m) => m.match_identifier(s, source_manager, diagnostic),
        }
    }

    fn match_operator<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
    ) -> Option<&'b [u8]> {
        match self {
            Self::Custom(m) => m.match_operator(s, source_manager, diagnostic),
            Self::Default(m) => m.match_operator(s, source_manager, diagnostic),
        }
    }

    fn match_native<'b>(
        &self,
        kind: NativeCallKind,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
    ) -> Option<&'b [u8]> {
        match self {
            Self::Custom(m) => m.match_native(kind, s, source_manager, diagnostic),
            Self::Default(m) => m.match_native(kind, s, source_manager, diagnostic),
        }
    }

    fn has_custom_digit_lexing(&self) -> bool {
        match self {
            Self::Custom(m) => m.has_custom_digit_lexing(),
            Self::Default(m) => m.has_custom_digit_lexing(),
        }
    }

    fn has_custom_integer_lexing(&self) -> bool {
        match self {
            Self::Custom(m) => m.has_custom_integer_lexing(),
            Self::Default(m) => m.has_custom_integer_lexing(),
        }
    }

    fn has_custom_floating_point_lexing(&self) -> bool {
        match self {
            Self::Custom(m) => m.has_custom_floating_point_lexing(),
            Self::Default(m) => m.has_custom_floating_point_lexing(),
        }
    }

    fn has_custom_identifier_lexing(&self) -> bool {
        match self {
            Self::Custom(m) => m.has_custom_identifier_lexing(),
            Self::Default(m) => m.has_custom_identifier_lexing(),
        }
    }
}

impl From<Vm> for EbnfParserMatcher {
    fn from(m: Vm) -> Self {
        Self::Custom(m)
    }
}

impl From<DefaultEbnfParserMatcher> for EbnfParserMatcher {
    fn from(m: DefaultEbnfParserMatcher) -> Self {
        Self::Default(m)
    }
}

pub(crate) struct IREbnfParserMatcher;

impl IREbnfParserMatcher {
    pub(crate) fn new() -> Self {
        Self
    }

    pub(crate) fn is_valid_identifier_start_code_point(c: char) -> bool {
        match c {
            'a'..='z' | 'A'..='Z' | '_' | '%' | '@' => true,
            _ => false,
        }
    }

    pub(crate) fn is_valid_identifier_continuation_code_point(c: char) -> bool {
        match c {
            'a'..='z' | 'A'..='Z' | '_' | '$' | '0'..='9' | '-' => true,
            _ => false,
        }
    }

    pub(crate) fn match_number<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
    ) -> Option<(&'b [u8], TokenKind)> {
        let def = DefaultEbnfParserMatcher::new();
        if let Some(s) = def.match_native_integer(s, source_manager, diagnostic) {
            return Some((s, TokenKind::Integer));
        }

        if let Some(s) = def.match_native_floating_point(s, source_manager, diagnostic) {
            return Some((s, TokenKind::FloatingPoint));
        }

        None
    }

    pub(crate) fn is_valid_number_start_code_point(c: char) -> bool {
        c.is_ascii_digit()
    }

    pub(crate) fn match_string_literal<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
    ) -> Option<&'b [u8]> {
        if s.is_empty() {
            return None;
        }

        let mut iter = ByteToCharIter::new(s);

        let Some(c) = iter.next() else {
            return None;
        };

        if c != '"' {
            return None;
        }

        let mut i = c.len_utf8();

        let mut is_escaping = false;

        while i < s.len() {
            let Some(c) = iter.next() else {
                break;
            };

            if c == '\\' {
                is_escaping = !is_escaping;
            } else {
                if c == '"' && !is_escaping {
                    return Some(&s[..i]);
                }

                if is_escaping {
                    match c {
                        'a' | 'b' | 'f' | 'n' | 'r' | 't' | 'v' | '\\' | '\'' | '"' => {}
                        _ => {
                            let span = Span::from_usize(i, i + c.len_utf8());
                            let info = source_manager.get_source_info(span);
                            diagnostic
                                .builder()
                                .report(
                                    crate::eoc::utils::diagnostic::DiagnosticLevel::Error,
                                    "Invalid escape sequence",
                                    info,
                                    None,
                                )
                                .add_error("Invalid escape sequence", Some(span))
                                .commit();
                            return None;
                        }
                    }
                    is_escaping = false;
                }
            }

            i += c.len_utf8();
        }

        let info = source_manager.get_source_info(Span::new(0, 1));
        diagnostic
            .builder()
            .report(
                crate::eoc::utils::diagnostic::DiagnosticLevel::Error,
                "Unterminated string literal",
                info,
                None,
            )
            .add_error(
                "Unterminated string literal",
                Some(Span::from_usize(1, s.len())),
            )
            .commit();
        None
    }

    pub(crate) fn match_character_literal<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
    ) -> Option<&'b [u8]> {
        if s.is_empty() {
            return None;
        }

        let mut iter = ByteToCharIter::new(s);

        let Some(c) = iter.next() else {
            return None;
        };

        if c != '\'' {
            return None;
        }

        let mut i = c.len_utf8();

        let mut is_escaping = false;

        let mut count = 0;

        while i < s.len() {
            let Some(c) = iter.next() else {
                break;
            };

            count += 1;

            if c == '\\' {
                is_escaping = !is_escaping;
                count -= 1;
            } else {
                if c == '\'' && !is_escaping {
                    if count != 1 {
                        let span = Span::from_usize(i, i + c.len_utf8());
                        let info = source_manager.get_source_info(span);
                        diagnostic
                            .builder()
                            .report(
                                crate::eoc::utils::diagnostic::DiagnosticLevel::Error,
                                "Invalid character literal",
                                info,
                                None,
                            )
                            .add_error("Invalid character literal", Some(span))
                            .commit();
                        return None;
                    }
                    return Some(&s[..i + c.len_utf8()]);
                }

                if is_escaping {
                    match c {
                        'a' | 'b' | 'f' | 'n' | 'r' | 't' | 'v' | '\\' | '\'' => {}
                        _ => {
                            let span = Span::from_usize(i, i + c.len_utf8());
                            let info = source_manager.get_source_info(span);
                            diagnostic
                                .builder()
                                .report(
                                    crate::eoc::utils::diagnostic::DiagnosticLevel::Error,
                                    "Invalid escape sequence",
                                    info,
                                    None,
                                )
                                .add_error("Invalid escape sequence", Some(span))
                                .commit();
                            return None;
                        }
                    }
                    is_escaping = false;
                }
            }

            i += c.len_utf8();
        }

        let info = source_manager.get_source_info(Span::new(0, 1));
        diagnostic
            .builder()
            .report(
                crate::eoc::utils::diagnostic::DiagnosticLevel::Error,
                "Unterminated character literal",
                info,
                None,
            )
            .add_error(
                "Unterminated character literal",
                Some(Span::from_usize(1, s.len())),
            )
            .commit();
        None
    }
}

impl EbnfMatcher for IREbnfParserMatcher {
    fn match_operator<'b>(
        &self,
        _s: &'b [u8],
        _source_manager: RelativeSourceManager<'b>,
        _diagnostic: &Diagnostic,
    ) -> Option<&'b [u8]> {
        None
    }

    fn match_identifier<'b>(
        &self,
        s: &'b [u8],
        _source_manager: RelativeSourceManager<'b>,
        _diagnostic: &Diagnostic,
    ) -> Option<&'b [u8]> {
        if s.is_empty() {
            return None;
        }

        let mut iter = ByteToCharIter::new(s);

        let Some(c) = iter.next() else {
            return None;
        };

        if !IREbnfParserMatcher::is_valid_identifier_start_code_point(c) {
            return None;
        }

        let mut i = c.len_utf8();

        while i < s.len() {
            if let Some(c) = iter.next() {
                if !IREbnfParserMatcher::is_valid_identifier_continuation_code_point(c) {
                    return Some(&s[..i]);
                }
                i += c.len_utf8();
            } else {
                break;
            }
        }

        Some(&s[..i])
    }

    fn match_native<'b>(
        &self,
        kind: NativeCallKind,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
    ) -> Option<&'b [u8]> {
        kind.call(
            &IREbnfParserMatcher::new(),
            s,
            source_manager,
            diagnostic,
        )
    }

    fn match_expr_for<'a>(
        &self,
        var: &str,
        s: &'a [u8],
        source_manager: RelativeSourceManager<'a>,
        diagnostic: &Diagnostic,
    ) -> Option<&'a [u8]> {
        match var {
            _ if var == NATIVE_CALL_KIND_ID.identifier_sym => {
                self.match_identifier(s, source_manager, diagnostic)
            }
            _ if var == NATIVE_CALL_KIND_ID.operator_sym => {
                self.match_operator(s, source_manager, diagnostic)
            }
            _ if var == NATIVE_CALL_KIND_ID.fp_sym => {
                self.match_number(s, source_manager, diagnostic).map(|(s, _)| s)
            }
            _ if var == NATIVE_CALL_KIND_ID.integer_sym => {
                self.match_number(s, source_manager, diagnostic).map(|(s, _)| s)
            }
            _ if var == NATIVE_CALL_KIND_ID.string_sym => {
                self.match_string_literal(s, source_manager, diagnostic)
            }
            _ if var == NATIVE_CALL_KIND_ID.char_sym => {
                self.match_character_literal(s, source_manager, diagnostic)
            }
            _ => None,
        }
    }

    fn try_match_expr<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
    ) -> Option<(&'b [u8], TokenKind)> {
        for k in &[
            NATIVE_CALL_KIND_ID.identifier_sym,
            NATIVE_CALL_KIND_ID.operator_sym,
            NATIVE_CALL_KIND_ID.fp_sym,
            NATIVE_CALL_KIND_ID.integer_sym,
            NATIVE_CALL_KIND_ID.string_sym,
            NATIVE_CALL_KIND_ID.char_sym,
        ] {
            if let Some(s) = self.match_expr_for(k.as_str(), s, source_manager, diagnostic) {
                return Some((s, TokenKind::CustomToken(*k)));
            }
        }
        None
    }

    fn init<'b>(
        &mut self,
        _expr: Option<EbnfExpr>,
        _source_manager: RelativeSourceManager<'b>,
        _diagnostic: &Diagnostic,
    ) {}
}
