use crate::eoc::{
    ast::identifier::Identifier,
    lexer::{
        number::{parse_floating_point, parse_integer},
        str_utils::{get_utf8_char_len, ByteToCharIter},
        token::TokenKind,
        utils::{
            is_valid_identifier_continuation_code_point, is_valid_identifier_start_code_point,
        },
    },
    utils::{
        diagnostic::{Diagnostic, DiagnosticReporter},
        span::Span,
        string::UniqueString,
    },
};

use super::{
    ast::{EbnfParserMatcherDef, EbnfParserMatcherEnv, RelativeSourceManager},
    expr::{EbnfExpr, EbnfParserEnvVariable},
    native_call::{NativeCallKind, NATIVE_CALL_KIND_ID},
};

pub(crate) trait EbnfMatcher {
    fn is_digit<'b>(
        &self,
        s: &'b [u8],
        _source_manager: RelativeSourceManager<'b>,
        _diagnostic: &mut Diagnostic,
    ) -> Option<char> {
        ByteToCharIter::new(s).next().filter(|c| c.is_ascii_digit())
    }

    fn is_hex_digit<'b>(
        &self,
        s: &'b [u8],
        _source_manager: RelativeSourceManager<'b>,
        _diagnostic: &mut Diagnostic,
    ) -> Option<char> {
        ByteToCharIter::new(s)
            .next()
            .filter(|c| c.is_ascii_hexdigit())
    }

    fn is_oct_digit<'b>(
        &self,
        s: &'b [u8],
        _source_manager: RelativeSourceManager<'b>,
        _diagnostic: &mut Diagnostic,
    ) -> Option<char> {
        ByteToCharIter::new(s)
            .next()
            .filter(|c| c.is_ascii_digit() && *c < '8')
    }

    fn is_binary_digit<'b>(
        &self,
        s: &'b [u8],
        _source_manager: RelativeSourceManager<'b>,
        _diagnostic: &mut Diagnostic,
    ) -> Option<char> {
        ByteToCharIter::new(s)
            .next()
            .filter(|c| *c == '0' || *c == '1')
    }

    fn contains_def(&self, name: &str) -> bool {
        false
    }

    fn match_native_integer<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &mut Diagnostic,
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
        diagnostic: &mut Diagnostic,
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
        diagnostic: &mut Diagnostic,
    ) -> Option<&'b [u8]>;

    fn match_native<'b>(
        &self,
        kind: NativeCallKind,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &mut Diagnostic,
    ) -> Option<&'b [u8]>;

    fn match_expr_for<'a>(
        &self,
        var: &str,
        s: &'a [u8],
        source_manager: RelativeSourceManager<'a>,
        diagnostic: &mut Diagnostic,
    ) -> Option<&'a [u8]>;

    fn try_match_expr<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &mut Diagnostic,
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
}

pub(crate) struct CustomEbnfParserMatcher {
    env: EbnfParserMatcherEnv,
    def: EbnfParserMatcherDef,
}

impl CustomEbnfParserMatcher {
    pub(crate) fn new() -> Self {
        Self {
            env: EbnfParserMatcherEnv::new(),
            def: EbnfParserMatcherDef::new(),
        }
    }

    fn add_identifier_env(&mut self, diagnostic: &mut Diagnostic) {
        self.add_native_call(NativeCallKind::StartIdentifier);
        self.add_native_call(NativeCallKind::ContIdentifier);
        let rep_expr = EbnfExpr::Identifier(NativeCallKind::ContIdentifier.to_string(), None);
        let expr = EbnfExpr::Concat(
            vec![
                EbnfExpr::Identifier(NativeCallKind::StartIdentifier.to_string(), None),
                EbnfExpr::Repetition(Box::new(rep_expr), 1),
            ],
            1,
        );

        let statement = EbnfExpr::Variable {
            name: "identifier".to_string(),
            expr: Box::new(expr),
            is_def: false,
        };
        let identifier = EbnfExpr::Statements(vec![statement], 1);
        identifier.init_env(&mut self.env, &mut self.def, diagnostic);
    }

    fn add_operator_env(&mut self, diagnostic: &mut Diagnostic) {
        self.add_native_call(NativeCallKind::StartOperator);
        self.add_native_call(NativeCallKind::ContOperator);
        let rep_expr = EbnfExpr::Identifier(NativeCallKind::ContOperator.to_string(), None);
        let expr = EbnfExpr::Concat(
            vec![
                EbnfExpr::Identifier(NativeCallKind::StartOperator.to_string(), None),
                EbnfExpr::Repetition(Box::new(rep_expr), 1),
            ],
            1,
        );

        let statement = EbnfExpr::Variable {
            name: "operator".to_string(),
            expr: Box::new(expr),
            is_def: false,
        };
        let operator = EbnfExpr::Statements(vec![statement], 1);
        operator.init_env(&mut self.env, &mut self.def, diagnostic);
    }

    pub(crate) fn init<'b>(
        &mut self,
        expr: EbnfExpr,
        _source_manager: RelativeSourceManager<'b>,
        diagnostic: &mut Diagnostic,
    ) {
        self.add_identifier_env(diagnostic);
        self.add_operator_env(diagnostic);
        self.add_native_call(NativeCallKind::Integer);
        self.add_native_call(NativeCallKind::FloatingPoint);
        self.add_native_call(NativeCallKind::Whitespace);
        self.add_native_call(NativeCallKind::NewLine);
        self.add_native_call(NativeCallKind::Tab);
        self.add_native_call(NativeCallKind::Digit);
        self.add_native_call(NativeCallKind::Letter);
        self.add_native_call(NativeCallKind::HexDigit);
        self.add_native_call(NativeCallKind::OctDigit);
        self.add_native_call(NativeCallKind::BinDigit);
        self.add_native_call(NativeCallKind::AlphaNumeric);
        self.add_native_call(NativeCallKind::StartIdentifier);
        self.add_native_call(NativeCallKind::ContIdentifier);
        self.add_native_call(NativeCallKind::StartOperator);
        self.add_native_call(NativeCallKind::ContOperator);

        expr.init_env(&mut self.env, &mut self.def, diagnostic);
        let keys = self.env.keys();
        for key in keys {
            if let Some(e) = self.env.remove(&key) {
                let expr = e.recalculate_max_byte_len(&mut self.env);
                self.env.insert(key, expr);
            }
        }
        // for (name, value) in self.env.iter() {
        //     println!("{}: {}", name, value);
        // }
        // println!(
        //     "\ndef: {}",
        //     self.def
        //         .keys()
        //         .map(|s| s.clone())
        //         .collect::<Vec<_>>()
        //         .join(",")
        // );
    }

    fn add_native_call(&mut self, kind: NativeCallKind) {
        self.env
            .insert(kind.to_string(), EbnfParserEnvVariable::NativeCall(kind));
    }

    pub(crate) fn match_native_identifier<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &mut Diagnostic,
    ) -> Option<&'b [u8]> {
        if s.is_empty() {
            return None;
        }

        if self
            .match_native(
                NativeCallKind::StartIdentifier,
                s,
                source_manager,
                diagnostic,
            )
            .is_none()
        {
            return None;
        }

        let mut i = get_utf8_char_len(s[0]);
        while i < s.len() {
            let end = get_utf8_char_len(s[i]);
            let temp_source = &s[i..(i + end).min(s.len() - 1)];
            if self
                .match_native(
                    NativeCallKind::ContIdentifier,
                    temp_source,
                    source_manager,
                    diagnostic,
                )
                .is_none()
            {
                return Some(&s[..i]);
            }
            i += end;
        }

        Some(&s[..i])
    }

    pub(crate) fn match_native_operator<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &mut Diagnostic,
    ) -> Option<&'b [u8]> {
        if s.is_empty() {
            return None;
        }
        if self
            .match_native(NativeCallKind::StartOperator, s, source_manager, diagnostic)
            .is_none()
        {
            return None;
        }

        let mut i = get_utf8_char_len(s[0]);
        while i < s.len() {
            let end = get_utf8_char_len(s[i]);
            let temp_source = &s[i..(i + end).min(s.len() - 1)];
            if self
                .match_native(
                    NativeCallKind::ContOperator,
                    temp_source,
                    source_manager,
                    diagnostic,
                )
                .is_none()
            {
                return Some(&s[..i]);
            }
            i += end;
        }

        Some(&s[..i])
    }

    fn match_expr_helper<'b>(
        &self,
        symbol: UniqueString,
        expr: &EbnfParserEnvVariable,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &mut Diagnostic,
    ) -> Option<(&'b [u8], TokenKind)> {
        let key = symbol.as_str();
        match symbol {
            u_str if u_str == NATIVE_CALL_KIND_ID.identifier_sym => {
                let temp = if !self.contains_def(key) {
                    self.match_native_identifier(s, source_manager, diagnostic)
                        .map(|s| (s, TokenKind::Identifier))
                } else {
                    expr.match_expr(self, s, &self.env, source_manager, diagnostic)
                        .map(|s| (s, TokenKind::Identifier))
                };
                temp
            }
            u_str if u_str == NATIVE_CALL_KIND_ID.operator_sym => {
                let temp = if !self.contains_def(key) {
                    self.match_native_operator(s, source_manager, diagnostic)
                        .map(|s| (s, TokenKind::Operator))
                } else {
                    expr.match_expr(self, s, &self.env, source_manager, diagnostic)
                        .map(|s| (s, TokenKind::Operator))
                };
                temp
            }
            u_str if u_str == NATIVE_CALL_KIND_ID.fp_sym => {
                let temp = if !self.contains_def(key) {
                    self.match_native_floating_point(s, source_manager, diagnostic)
                        .map(|s| (s, TokenKind::FloatingPoint))
                } else {
                    expr.match_expr(self, s, &self.env, source_manager, diagnostic)
                        .map(|s| (s, TokenKind::FloatingPoint))
                };
                temp
            }
            u_str if u_str == NATIVE_CALL_KIND_ID.integer_sym => {
                let temp = if !self.contains_def(key) {
                    self.match_native_integer(s, source_manager, diagnostic)
                        .map(|s| (s, TokenKind::Integer))
                } else {
                    expr.match_expr(self, s, &self.env, source_manager, diagnostic)
                        .map(|s| (s, TokenKind::Integer))
                };
                temp
            }
            _ => expr
                .match_expr(self, s, &self.env, source_manager, diagnostic)
                .map(|s| (s, TokenKind::CustomToken(symbol))),
        }
    }

    pub(crate) fn match_identifier<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &mut Diagnostic,
    ) -> Option<&'b [u8]> {
        if let Some(expr) = self.env.get(NATIVE_CALL_KIND_ID.identifier_sym.as_str()) {
            self.match_expr_helper(
                NATIVE_CALL_KIND_ID.identifier_sym,
                expr,
                s,
                source_manager,
                diagnostic,
            )
            .map(|(s, _)| s)
        } else {
            self.match_native_identifier(s, source_manager, diagnostic)
        }
    }

    pub(crate) fn try_match_native_if_exists<'b>(
        &self,
        kind: NativeCallKind,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &mut Diagnostic,
    ) -> Option<&'b [u8]> {
        self.match_expr_for(kind.as_str(), s, source_manager, diagnostic)
    }
}

impl EbnfMatcher for CustomEbnfParserMatcher {
    fn match_operator<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &mut Diagnostic,
    ) -> Option<&'b [u8]> {
        if let Some(expr) = self.env.get(NATIVE_CALL_KIND_ID.operator_sym.as_str()) {
            self.match_expr_helper(
                NATIVE_CALL_KIND_ID.operator_sym,
                expr,
                s,
                source_manager,
                diagnostic,
            )
            .map(|(s, _)| s)
        } else {
            self.match_native_operator(s, source_manager, diagnostic)
        }
    }

    fn match_native<'b>(
        &self,
        kind: NativeCallKind,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &mut Diagnostic,
    ) -> Option<&'b [u8]> {
        if let Some(expr) = self.env.get(kind.as_str()) {
            expr.match_expr(self, s, &self.env, source_manager, diagnostic)
        } else {
            kind.call(self, s, source_manager, diagnostic)
        }
    }

    fn match_expr_for<'a>(
        &self,
        var: &str,
        s: &'a [u8],
        source_manager: RelativeSourceManager<'a>,
        diagnostic: &mut Diagnostic,
    ) -> Option<&'a [u8]> {
        if let Some(expr) = self.env.get(var) {
            let temp = expr.match_expr(self, s, &self.env, source_manager, diagnostic);
            temp
        } else {
            None
        }
    }

    fn try_match_expr<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &mut Diagnostic,
    ) -> Option<(&'b [u8], TokenKind)> {
        for d in self.def.ordered_iter().rev() {
            if let Some(expr) = self.env.get(d.as_str()) {
                let temp = self.match_expr_helper(*d, expr, s, source_manager, diagnostic);

                if temp.is_some() {
                    return temp;
                }
            }
        }
        None
    }

    fn is_digit<'b>(
        &self,
        s: &[u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &mut Diagnostic,
    ) -> Option<char> {
        self.match_native(NativeCallKind::Digit, s, source_manager, diagnostic)
            .map(|s| ByteToCharIter::new(s).next())
            .flatten()
    }

    fn is_hex_digit<'b>(
        &self,
        s: &[u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &mut Diagnostic,
    ) -> Option<char> {
        self.match_native(NativeCallKind::HexDigit, s, source_manager, diagnostic)
            .map(|s| ByteToCharIter::new(s).next())
            .flatten()
    }

    fn is_oct_digit<'b>(
        &self,
        s: &[u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &mut Diagnostic,
    ) -> Option<char> {
        self.match_native(NativeCallKind::OctDigit, s, source_manager, diagnostic)
            .map(|s| ByteToCharIter::new(s).next())
            .flatten()
    }

    fn is_binary_digit<'b>(
        &self,
        s: &[u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &mut Diagnostic,
    ) -> Option<char> {
        self.match_native(NativeCallKind::BinDigit, s, source_manager, diagnostic)
            .map(|s| ByteToCharIter::new(s).next())
            .flatten()
    }

    fn contains_def(&self, name: &str) -> bool {
        self.def.contains(name)
    }
}

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
        _diagnostic: &mut Diagnostic,
    ) -> Option<&'b [u8]> {
        if s.is_empty() {
            return None;
        }

        let mut iter = ByteToCharIter::new(s);

        let c = iter.next();
        if c.is_none() {
            return None;
        }

        let c = c.unwrap();

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
        _diagnostic: &mut Diagnostic,
    ) -> Option<&'b [u8]> {
        if s.is_empty() {
            return None;
        }

        let mut iter = ByteToCharIter::new(s);

        let c = iter.next();
        if c.is_none() {
            return None;
        }

        let c = c.unwrap();

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
        diagnostic: &mut Diagnostic,
    ) -> Option<&'b [u8]> {
        self.match_native_identifier(s, source_manager, diagnostic)
    }
}

impl EbnfMatcher for DefaultEbnfParserMatcher {
    fn match_operator<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &mut Diagnostic,
    ) -> Option<&'b [u8]> {
        self.match_native_operator(s, source_manager, diagnostic)
    }

    fn match_native<'b>(
        &self,
        kind: NativeCallKind,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &mut Diagnostic,
    ) -> Option<&'b [u8]> {
        kind.call(
            &CustomEbnfParserMatcher::new(),
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
        diagnostic: &mut Diagnostic,
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
        diagnostic: &mut Diagnostic,
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

pub(crate) enum EbnfParserMatcher {
    Custom(CustomEbnfParserMatcher),
    Default(DefaultEbnfParserMatcher),
}

impl EbnfParserMatcher {
    pub(crate) fn init<'b>(
        &mut self,
        expr: Option<EbnfExpr>,
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &mut Diagnostic,
    ) {
        if let Some(expr) = expr {
            if expr.is_empty() {
                return;
            }
            match self {
                Self::Custom(m) => m.init(expr, source_manager, diagnostic),
                Self::Default(_) => {
                    let mut m = CustomEbnfParserMatcher::new();
                    m.init(expr, source_manager, diagnostic);
                    *self = Self::Custom(m);
                }
            }
        }
    }

    pub(crate) fn new() -> Self {
        Self::Default(DefaultEbnfParserMatcher::new())
    }

    pub(crate) fn contains_def(&self, name: &str) -> bool {
        match self {
            Self::Custom(m) => m.contains_def(name),
            Self::Default(_) => false,
        }
    }

    pub(crate) fn try_match_expr<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &mut Diagnostic,
    ) -> Option<(&'b [u8], TokenKind)> {
        match self {
            Self::Custom(m) => m.try_match_expr(s, source_manager, diagnostic),
            Self::Default(_) => None,
        }
    }

    pub(crate) fn match_expr_for<'b>(
        &self,
        var: &str,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &mut Diagnostic,
    ) -> Option<&'b [u8]> {
        match self {
            Self::Custom(m) => m.match_expr_for(var, s, source_manager, diagnostic),
            Self::Default(m) => m.match_expr_for(var, s, source_manager, diagnostic),
        }
    }

    pub(crate) fn match_identifier<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &mut Diagnostic,
    ) -> Option<&'b [u8]> {
        match self {
            Self::Custom(m) => m.match_identifier(s, source_manager, diagnostic),
            Self::Default(m) => m.match_identifier(s, source_manager, diagnostic),
        }
    }

    pub(crate) fn match_operator<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &mut Diagnostic,
    ) -> Option<&'b [u8]> {
        match self {
            Self::Custom(m) => m.match_operator(s, source_manager, diagnostic),
            Self::Default(m) => m.match_operator(s, source_manager, diagnostic),
        }
    }

    pub(crate) fn match_native<'b>(
        &self,
        kind: NativeCallKind,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &mut Diagnostic,
    ) -> Option<&'b [u8]> {
        match self {
            Self::Custom(m) => m.match_native(kind, s, source_manager, diagnostic),
            Self::Default(m) => m.match_native(kind, s, source_manager, diagnostic),
        }
    }

    pub(crate) fn has_custom_digit_lexing(&self) -> bool {
        match self {
            Self::Custom(m) => m.has_custom_digit_lexing(),
            Self::Default(m) => m.has_custom_digit_lexing(),
        }
    }

    pub(crate) fn has_custom_integer_lexing(&self) -> bool {
        match self {
            Self::Custom(m) => m.has_custom_integer_lexing(),
            Self::Default(m) => m.has_custom_integer_lexing(),
        }
    }

    pub(crate) fn has_custom_floating_point_lexing(&self) -> bool {
        match self {
            Self::Custom(m) => m.has_custom_floating_point_lexing(),
            Self::Default(m) => m.has_custom_floating_point_lexing(),
        }
    }

    pub(crate) fn has_custom_identifier_lexing(&self) -> bool {
        match self {
            Self::Custom(m) => m.has_custom_identifier_lexing(),
            Self::Default(m) => m.has_custom_identifier_lexing(),
        }
    }
}

impl From<CustomEbnfParserMatcher> for EbnfParserMatcher {
    fn from(m: CustomEbnfParserMatcher) -> Self {
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
        // Identifiers
        // bare-id ::= (letter|[_]) (letter|digit|[_$.])*
        // suffix-id ::= (digit+ | ((letter|id-punct) (letter|id-punct|digit)*))
        // value-id ::= `%` suffix-id
        // id-punct  ::= [$._-]

        match c {
            'a'..='z' | 'A'..='Z' | '_' | '%' | '@' => true,
            _ => false,
        }
    }

    pub(crate) fn is_valid_identifier_continuation_code_point(c: char) -> bool {
        match c {
            'a'..='z' | 'A'..='Z' | '_' | '$' | '.' | '0'..='9' | '-' => true,
            _ => false,
        }
    }

    pub(crate) fn match_identifier<'b>(&self, s: &'b [u8]) -> Option<&'b [u8]> {
        if s.is_empty() {
            return None;
        }

        let mut iter = ByteToCharIter::new(s);

        let c = iter.next();
        if c.is_none() {
            return None;
        }

        let c = c.unwrap();

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

    pub(crate) fn match_number<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &mut Diagnostic,
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
        diagnostic: &mut Diagnostic,
    ) -> Option<&'b [u8]> {
        if s.is_empty() {
            return None;
        }

        let mut iter = ByteToCharIter::new(s);

        let c = iter.next();
        if c.is_none() {
            return None;
        }

        let c = c.unwrap();
        if c != '"' {
            return None;
        }

        let mut i = c.len_utf8();

        let mut is_escaping = false;

        while i < s.len() {
            let c = iter.next();
            if c.is_none() {
                break;
            }
            let c = c.unwrap();

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
        diagnostic: &mut Diagnostic,
    ) -> Option<&'b [u8]> {
        if s.is_empty() {
            return None;
        }

        let mut iter = ByteToCharIter::new(s);

        let c = iter.next();
        if c.is_none() {
            return None;
        }

        let c = c.unwrap();

        if c != '\'' {
            return None;
        }

        let mut i = c.len_utf8();

        let mut is_escaping = false;

        let mut count = 0;

        while i < s.len() {
            let c = iter.next();
            if c.is_none() {
                break;
            }
            let c = c.unwrap();

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
