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
    utils::{diagnostic::Diagnostic, string::UniqueString},
};

use super::{
    ast::{EbnfParserMatcherDef, EbnfParserMatcherEnv, RelativeSourceManager},
    expr::{EbnfExpr, EbnfParserEnvVariable},
    native_call::NativeCallKind,
};

pub(crate) struct CustomEbnfParserMatcher {
    env: EbnfParserMatcherEnv,
    def: EbnfParserMatcherDef,
    identifier_sym: UniqueString,
    operator_sym: UniqueString,
    integer_sym: UniqueString,
    fp_sym: UniqueString,
}

impl CustomEbnfParserMatcher {
    pub(crate) fn new() -> Self {
        Self {
            env: EbnfParserMatcherEnv::new(),
            def: EbnfParserMatcherDef::new(),
            identifier_sym: UniqueString::new("identifier"),
            operator_sym: UniqueString::new("operator"),
            integer_sym: UniqueString::new("integer"),
            fp_sym: UniqueString::new("floating_point"),
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

    pub(crate) fn init(&mut self, expr: Option<EbnfExpr>, diagnostic: &mut Diagnostic) {
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

        if let Some(expr) = expr {
            expr.init_env(&mut self.env, &mut self.def, diagnostic);
        }
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

    pub(crate) fn contains_def(&self, name: &str) -> bool {
        self.def.contains(name)
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

    fn get_digit<'b>(
        &self,
        s: &[u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &mut Diagnostic,
    ) -> Option<char> {
        self.match_native(NativeCallKind::Digit, s, source_manager, diagnostic)
            .map(|s| ByteToCharIter::new(s).next())
            .flatten()
    }

    fn get_hex_digit<'b>(
        &self,
        s: &[u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &mut Diagnostic,
    ) -> Option<char> {
        self.match_native(NativeCallKind::HexDigit, s, source_manager, diagnostic)
            .map(|s| ByteToCharIter::new(s).next())
            .flatten()
    }

    fn get_oct_digit<'b>(
        &self,
        s: &[u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &mut Diagnostic,
    ) -> Option<char> {
        self.match_native(NativeCallKind::OctDigit, s, source_manager, diagnostic)
            .map(|s| ByteToCharIter::new(s).next())
            .flatten()
    }

    fn get_binary_digit<'b>(
        &self,
        s: &[u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &mut Diagnostic,
    ) -> Option<char> {
        self.match_native(NativeCallKind::BinDigit, s, source_manager, diagnostic)
            .map(|s| ByteToCharIter::new(s).next())
            .flatten()
    }

    pub(crate) fn match_native_integer<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &mut Diagnostic,
    ) -> Option<&'b [u8]> {
        parse_integer(
            s,
            |s, sm, d| self.get_digit(s, sm, d),
            |s, sm, d| self.get_hex_digit(s, sm, d),
            |s, sm, d| self.get_oct_digit(s, sm, d),
            |s, sm, d| self.get_binary_digit(s, sm, d),
            source_manager,
            diagnostic,
        )
    }

    pub(crate) fn match_native_floating_point<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &mut Diagnostic,
    ) -> Option<&'b [u8]> {
        parse_floating_point(
            s,
            |s, sm, d| self.get_digit(s, sm, d),
            |s, sm, d| self.get_hex_digit(s, sm, d),
            source_manager,
            diagnostic,
        )
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
        match key {
            _ if key == self.identifier_sym.as_str() => {
                let temp = if !self.contains_def(key) {
                    self.match_native_identifier(s, source_manager, diagnostic)
                        .map(|s| (s, TokenKind::Identifier))
                } else {
                    expr.match_expr(self, s, &self.env, source_manager, diagnostic)
                        .map(|s| (s, TokenKind::Identifier))
                };
                temp
            }
            _ if key == self.operator_sym.as_str() => {
                let temp = if !self.contains_def(key) {
                    self.match_native_operator(s, source_manager, diagnostic)
                        .map(|s| (s, TokenKind::Operator))
                } else {
                    expr.match_expr(self, s, &self.env, source_manager, diagnostic)
                        .map(|s| (s, TokenKind::Operator))
                };
                temp
            }
            _ if key == self.fp_sym.as_str() => {
                let temp = if !self.contains_def(key) {
                    self.match_native_floating_point(s, source_manager, diagnostic)
                        .map(|s| (s, TokenKind::FloatingPoint))
                } else {
                    expr.match_expr(self, s, &self.env, source_manager, diagnostic)
                        .map(|s| (s, TokenKind::FloatingPoint))
                };
                temp
            }
            _ if key == self.integer_sym.as_str() => {
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

    pub(crate) fn try_match_expr<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &mut Diagnostic,
    ) -> Option<(&'b [u8], TokenKind)> {
        for d in self.def.ordered_iter() {
            if let Some(expr) = self.env.get(d.as_str()) {
                let temp = self.match_expr_helper(*d, expr, s, source_manager, diagnostic);

                if temp.is_some() {
                    return temp;
                }
            }
        }
        None
    }

    pub(crate) fn match_expr_for<'b>(
        &self,
        var: &str,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &mut Diagnostic,
    ) -> Option<&'b [u8]> {
        if let Some(expr) = self.env.get(var) {
            let temp = expr.match_expr(self, s, &self.env, source_manager, diagnostic);
            temp
        } else {
            None
        }
    }

    pub(crate) fn match_identifier<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &mut Diagnostic,
    ) -> Option<&'b [u8]> {
        if let Some(expr) = self.env.get(self.identifier_sym.as_str()) {
            self.match_expr_helper(self.identifier_sym, expr, s, source_manager, diagnostic)
                .map(|(s, _)| s)
        } else {
            self.match_native_identifier(s, source_manager, diagnostic)
        }
    }

    pub(crate) fn match_operator<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &mut Diagnostic,
    ) -> Option<&'b [u8]> {
        if let Some(expr) = self.env.get(self.operator_sym.as_str()) {
            self.match_expr_helper(self.operator_sym, expr, s, source_manager, diagnostic)
                .map(|(s, _)| s)
        } else {
            self.match_native_operator(s, source_manager, diagnostic)
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

    pub(crate) fn match_native<'b>(
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

    pub(crate) fn has_custom_digit_lexing(&self) -> bool {
        self.contains_def("hex_digit")
            || self.contains_def("oct_digit")
            || self.contains_def("bin_digit")
    }

    pub(crate) fn has_custom_integer_lexing(&self) -> bool {
        self.contains_def(self.integer_sym.as_str()) || self.has_custom_digit_lexing()
    }

    pub(crate) fn has_custom_floating_point_lexing(&self) -> bool {
        self.contains_def(self.fp_sym.as_str()) || self.has_custom_integer_lexing()
    }

    pub(crate) fn has_custom_identifier_lexing(&self) -> bool {
        self.contains_def(self.identifier_sym.as_str())
    }
}

pub(crate) struct EbnfParserMatcher(Option<CustomEbnfParserMatcher>);

impl EbnfParserMatcher {
    pub(crate) fn init(&mut self, expr: Option<EbnfExpr>, diagnostic: &mut Diagnostic) {
        if expr.is_some() {
            self.0
                .get_or_insert_with(CustomEbnfParserMatcher::new)
                .init(expr, diagnostic);
        }
    }

    pub(crate) fn new() -> Self {
        Self(None)
    }

    pub(crate) fn contains_def(&self, name: &str) -> bool {
        self.0.as_ref().map_or(false, |m| m.contains_def(name))
    }

    pub(crate) fn match_native_identifier<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &mut Diagnostic,
    ) -> Option<&'b [u8]> {
        if let Some(m) = self.0.as_ref() {
            return m.match_native_identifier(s, source_manager, diagnostic);
        }

        if s.is_empty() {
            return None;
        }

        let mut iter = ByteToCharIter::new(s);

        let c = iter.next();
        if c.is_none() {
            return None;
        }

        let c = c.unwrap();

        if !is_valid_identifier_start_code_point(c) {
            return None;
        }

        let mut i = c.len_utf8();

        while i < s.len() {
            if let Some(c) = iter.next() {
                if !is_valid_identifier_continuation_code_point(c) {
                    return Some(&s[..i]);
                }
                i += c.len_utf8();
            } else {
                break;
            }
        }

        Some(&s[..i])
    }

    fn get_digit<'b>(
        &self,
        s: &[u8],
        _source_manager: RelativeSourceManager<'b>,
        _diagnostic: &mut Diagnostic,
    ) -> Option<char> {
        if let Some((is_valid, c)) = ByteToCharIter::new(s)
            .next()
            .map(|c| (c.is_ascii_digit(), c))
        {
            if is_valid {
                Some(c)
            } else {
                None
            }
        } else {
            None
        }
    }

    fn get_hex_digit<'b>(
        &self,
        s: &[u8],
        _source_manager: RelativeSourceManager<'b>,
        _diagnostic: &mut Diagnostic,
    ) -> Option<char> {
        if let Some((is_valid, c)) = ByteToCharIter::new(s)
            .next()
            .map(|c| (c.is_ascii_hexdigit(), c))
        {
            if is_valid {
                Some(c)
            } else {
                None
            }
        } else {
            None
        }
    }

    fn get_oct_digit<'b>(
        &self,
        s: &[u8],
        _source_manager: RelativeSourceManager<'b>,
        _diagnostic: &mut Diagnostic,
    ) -> Option<char> {
        if let Some((is_valid, c)) = ByteToCharIter::new(s)
            .next()
            .map(|c| (c.is_ascii_digit() && c < '8', c))
        {
            if is_valid {
                Some(c)
            } else {
                None
            }
        } else {
            None
        }
    }

    fn get_binary_digit<'b>(
        &self,
        s: &[u8],
        _source_manager: RelativeSourceManager<'b>,
        _diagnostic: &mut Diagnostic,
    ) -> Option<char> {
        if let Some((is_valid, c)) = ByteToCharIter::new(s)
            .next()
            .map(|c| (c == '0' || c == '1', c))
        {
            if is_valid {
                Some(c)
            } else {
                None
            }
        } else {
            None
        }
    }

    pub(crate) fn match_native_integer<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &mut Diagnostic,
    ) -> Option<&'b [u8]> {
        if let Some(m) = self.0.as_ref() {
            m.match_native_integer(s, source_manager, diagnostic)
        } else {
            parse_integer(
                s,
                |s, sm, d| self.get_digit(s, sm, d),
                |s, sm, d| self.get_hex_digit(s, sm, d),
                |s, sm, d| self.get_oct_digit(s, sm, d),
                |s, sm, d| self.get_binary_digit(s, sm, d),
                source_manager,
                diagnostic,
            )
        }
    }

    pub(crate) fn match_native_floating_point<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &mut Diagnostic,
    ) -> Option<&'b [u8]> {
        if let Some(m) = self.0.as_ref() {
            m.match_native_floating_point(s, source_manager, diagnostic)
        } else {
            parse_floating_point(
                s,
                |s, sm, d| self.get_digit(s, sm, d),
                |s, sm, d| self.get_hex_digit(s, sm, d),
                source_manager,
                diagnostic,
            )
        }
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

        if Identifier::is_operator_start_code_point(c) {
            return None;
        }

        let mut i = c.len_utf8();
        while i < s.len() {
            if let Some(c) = iter.next() {
                if !Identifier::is_operator_continuation_code_point(c) {
                    return Some(&s[..i]);
                }
                i += c.len_utf8();
            } else {
                break;
            }
        }

        Some(&s[..i])
    }

    pub(crate) fn try_match_expr<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &mut Diagnostic,
    ) -> Option<(&'b [u8], TokenKind)> {
        if let Some(m) = self.0.as_ref() {
            return m.try_match_expr(s, source_manager, diagnostic);
        }
        None
    }

    pub(crate) fn match_expr_for<'b>(
        &self,
        var: &str,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &mut Diagnostic,
    ) -> Option<&'b [u8]> {
        if let Some(m) = self.0.as_ref() {
            m.match_expr_for(var, s, source_manager, diagnostic)
        } else {
            match var {
                "identifier" => self.match_native_identifier(s, source_manager, diagnostic),
                "operator" => self.match_native_operator(s, source_manager, diagnostic),
                "floating_point" => self.match_native_floating_point(s, source_manager, diagnostic),
                "integer" => self.match_native_integer(s, source_manager, diagnostic),
                _ => None,
            }
        }
    }

    pub(crate) fn match_identifier<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &mut Diagnostic,
    ) -> Option<&'b [u8]> {
        if let Some(m) = self.0.as_ref() {
            m.match_identifier(s, source_manager, diagnostic)
        } else {
            self.match_native_identifier(s, source_manager, diagnostic)
        }
    }

    pub(crate) fn match_operator<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &mut Diagnostic,
    ) -> Option<&'b [u8]> {
        if let Some(m) = self.0.as_ref() {
            m.match_operator(s, source_manager, diagnostic)
        } else {
            self.match_native_operator(s, source_manager, diagnostic)
        }
    }

    pub(crate) fn match_native<'b>(
        &self,
        kind: NativeCallKind,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &mut Diagnostic,
    ) -> Option<&'b [u8]> {
        if let Some(m) = self.0.as_ref() {
            m.match_native(kind, s, source_manager, diagnostic)
        } else {
            kind.call(
                &CustomEbnfParserMatcher::new(),
                s,
                source_manager,
                diagnostic,
            )
        }
    }

    pub(crate) fn has_custom_digit_lexing(&self) -> bool {
        self.0
            .as_ref()
            .map_or(false, |m| m.has_custom_digit_lexing())
    }

    pub(crate) fn has_custom_integer_lexing(&self) -> bool {
        self.0
            .as_ref()
            .map_or(false, |m| m.has_custom_integer_lexing())
    }

    pub(crate) fn has_custom_floating_point_lexing(&self) -> bool {
        self.0
            .as_ref()
            .map_or(false, |m| m.has_custom_floating_point_lexing())
    }

    pub(crate) fn has_custom_identifier_lexing(&self) -> bool {
        self.0
            .as_ref()
            .map_or(false, |m| m.has_custom_identifier_lexing())
    }
}

impl From<CustomEbnfParserMatcher> for EbnfParserMatcher {
    fn from(m: CustomEbnfParserMatcher) -> Self {
        Self(Some(m))
    }
}
