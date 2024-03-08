#![allow(dead_code)]

use std::{
    collections::{HashMap, HashSet},
    fmt::{Debug, Display},
    hash::Hash, slice::Iter,
};

use crate::eoc::{
    ast::identifier::Identifier,
    lexer::{
        str_utils::{ decode_unicode_escape_sequence, get_utf8_char_len, ByteToCharIter },
        token::{Token, TokenKind},
        utils::{
            is_valid_identifier_continuation_code_point, is_valid_identifier_start_code_point,
            ParenMatching,
        },
    },
    utils::{
        diagnostic::{Diagnostic, DiagnosticLevel, DiagnosticReporter},
        source_manager::{SourceManager, SourceManagerDiagnosticInfo},
        span::Span, string::UniqueString,
    },
};

#[derive(Clone, Copy)]
pub(crate) struct RelativeSourceManager<'a>(&'a SourceManager, u32);

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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum BinaryOperator {
    Alternative, // "|"
    Concat,      // ","
    Exception,   // "-"
    Extend,      // "+"
    Range,       // ".."
    RangeEqual,  // "..."
}

impl From<TokenKind> for BinaryOperator {
    fn from(kind: TokenKind) -> Self {
        match kind {
            TokenKind::Pipe => Self::Alternative,
            TokenKind::Comma => Self::Concat,
            TokenKind::Exception => Self::Exception,
            TokenKind::Plus => Self::Extend,
            TokenKind::Range => Self::Range,
            TokenKind::RangeEqual => Self::RangeEqual,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum UnaryOperator {
    Optional,
}

impl From<TokenKind> for UnaryOperator {
    fn from(kind: TokenKind) -> Self {
        match kind {
            TokenKind::QuestionMark => Self::Optional,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum TerminalValue {
    String(String),
    Char(char),
}

impl TerminalValue {
    pub(crate) fn is_empty(&self) -> bool {
        match self {
            TerminalValue::String(s) => s.is_empty(),
            TerminalValue::Char(_) => false,
        }
    }

    pub(crate) fn len(&self) -> usize {
        match self {
            TerminalValue::String(s) => s.chars().count(),
            TerminalValue::Char(_) => 1,
        }
    }

    pub(crate) fn len_utf8(&self) -> usize {
        match self {
            TerminalValue::String(s) => s.as_bytes().len(),
            TerminalValue::Char(c) => c.len_utf8(),
        }
    }

    pub(crate) fn is_char(&self) -> bool {
        match self {
            TerminalValue::String(_) => false,
            TerminalValue::Char(_) => true,
        }
    }

    pub(crate) fn as_char(&self) -> Option<char> {
        match self {
            TerminalValue::String(s) => s.chars().next(),
            TerminalValue::Char(c) => Some(*c),
        }
    }
}

impl Display for TerminalValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TerminalValue::String(s) => write!(f, "{}", s),
            TerminalValue::Char(c) => write!(f, "{}", c),
        }
    }
}

impl PartialEq<char> for TerminalValue {
    fn eq(&self, other: &char) -> bool {
        match self {
            TerminalValue::String(s) => s.len() == 1 && s.chars().next().unwrap() == *other,
            TerminalValue::Char(c) => c == other,
        }
    }
}

impl PartialEq<[u8]> for TerminalValue {
    fn eq(&self, other: &[u8]) -> bool {
        match self {
            TerminalValue::String(s) => s.as_bytes() == other,
            TerminalValue::Char(c) => ByteToCharIter::new(other).next() == Some(*c),
        }
    }
}

type EbnfExprMaxByteLen = u8;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum EbnfExpr {
    Identifier(String, Option<(SourceManagerDiagnosticInfo, Span)>),
    Alternative(
        Vec<EbnfExpr>,
        HashSet<TerminalValue>,
        EbnfExprMaxByteLen,
    ),
    Concat(Vec<EbnfExpr>, EbnfExprMaxByteLen),
    Exception(Vec<EbnfExpr>, EbnfExprMaxByteLen),
    Extend(Vec<EbnfExpr>, EbnfExprMaxByteLen),
    Optional(Box<EbnfExpr>, EbnfExprMaxByteLen),
    Repetition(Box<EbnfExpr>, EbnfExprMaxByteLen),
    Terminal(TerminalValue),
    Statements(Vec<EbnfExpr>, EbnfExprMaxByteLen),
    Variable {
        name: String,
        expr: Box<EbnfExpr>,
        is_def: bool,
    },
    Range {
        lhs: char,
        rhs: char,
        inclusive: bool,
    },
}

pub(crate) struct EbnfParser<'a> {
    source_manager: &'a SourceManager,
    diagnostic: &'a mut Diagnostic,
    tokens: Vec<Token>,
    cursor: usize,
}

impl<'a> EbnfParser<'a> {
    pub(crate) fn parse(
        tokens: Vec<Token>,
        source_manager: &SourceManager,
        diagnostic: &mut Diagnostic,
    ) -> EbnfExpr {
        let mut parser = EbnfParser {
            source_manager,
            diagnostic,
            tokens,
            cursor: 0,
        };

        parser.parse_statements()
    }

    fn is_empty(&self) -> bool {
        self.cursor >= self.tokens.len()
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.cursor)
    }

    fn peek_kind(&self) -> Option<TokenKind> {
        self.peek().map(|t| t.kind)
    }

    fn next(&mut self) -> Option<&Token> {
        if self.is_empty() {
            return None;
        }
        let token = self.tokens.get(self.cursor);
        self.cursor += 1;
        token
    }

    fn parse_statements(&mut self) -> EbnfExpr {
        let mut statements = Vec::new();

        while !self.is_empty() {
            if let Some((expr, _)) = self.parse_statement() {
                statements.push(expr);
            }
        }

        EbnfExpr::Statements(statements, 1)
    }

    fn get_string_from_token(&self, token: &Token) -> String {
        let lexem = &self.source_manager[token.span];
        decode_unicode_escape_sequence(lexem)
    }

    fn infix_bp(op: TokenKind) -> (u8, u8) {
        match op {
            TokenKind::Pipe => (1, 2),
            TokenKind::Comma => (3, 4),
            TokenKind::Exception => (5, 6),
            TokenKind::Plus => (7, 8),
            TokenKind::Range | TokenKind::RangeEqual => (9, 10),
            _ => (0, 0),
        }
    }

    fn postfix_bp(op: TokenKind) -> Option<u8> {
        match op {
            TokenKind::QuestionMark => Some(11),
            _ => None,
        }
    }

    fn get_terminal_from_token(&self, token: &Token) -> (EbnfExpr, Span) {
        let lexem = &self.source_manager[token.span];
        let lexem = decode_unicode_escape_sequence(lexem);
        let span = token.span;
        let len = lexem.chars().count();
        let terminal = if len == 1 {
            TerminalValue::Char(lexem.chars().next().unwrap())
        } else {
            TerminalValue::String(lexem)
        };
        (EbnfExpr::Terminal(terminal), span)
    }

    fn parse_primary(&mut self) -> Option<(EbnfExpr, Span)> {
        let token = self.next().unwrap().clone();
        let span = token.span;
        match token.kind {
            TokenKind::Identifier => Some((
                EbnfExpr::Identifier(
                    self.get_string_from_token(&token),
                    Some((
                        self.source_manager.get_source_info(span),
                        self.source_manager.fix_span(span),
                    )),
                ),
                span,
            )),
            TokenKind::Terminal => Some(self.get_terminal_from_token(&token)),
            TokenKind::Semicolon => None,
            _ => {
                self.diagnostic
                    .builder()
                    .report(
                        DiagnosticLevel::Error,
                        "Expected identifier or terminal",
                        self.source_manager.get_source_info(token.span),
                        None,
                    )
                    .add_error(
                        "Unknown token",
                        Some(self.source_manager.fix_span(token.span)),
                    )
                    .commit();
                None
            }
        }
    }

    fn parse_paren_expr(&mut self, open: TokenKind, min_bp: u8) -> Option<(EbnfExpr, Span)> {
        let close = ParenMatching::get_other_pair(open).unwrap();

        if let Some((expr, e_span)) = self.parse_expr(min_bp) {
            if self.peek_kind() != Some(close) {
                self.diagnostic
                    .builder()
                    .report(
                        DiagnosticLevel::Error,
                        format!("Expected '{}'", ParenMatching::to_str(close)),
                        self.source_manager.get_source_info(e_span),
                        None,
                    )
                    .add_info(
                        format!("Try add '{}' after this", ParenMatching::to_str(close)),
                        Some(self.source_manager.fix_span(e_span)),
                    )
                    .commit();
                return None;
            }

            let span = self.next().unwrap().span;

            return Some((expr, span));
        }

        None
    }

    fn parse_expr_helper(&mut self) -> Option<(EbnfExpr, Span)> {
        let token = self.peek();
        if token.is_none() {
            return None;
        }
        let token = token.unwrap().clone();

        match token.kind {
            TokenKind::OpenParen => {
                self.next();
                self.parse_paren_expr(TokenKind::OpenParen, 0)
            }
            TokenKind::OpenBracket => {
                self.next();
                self.parse_paren_expr(TokenKind::OpenBracket, 0)
                    .map(|(expr, span)| (EbnfExpr::Optional(Box::new(expr), 1), span))
            }
            TokenKind::OpenBrace => {
                self.next();
                self.parse_paren_expr(TokenKind::OpenBrace, 0)
                    .map(|(expr, span)| (EbnfExpr::Repetition(Box::new(expr), 1), span))
            }
            _ => self.parse_primary(),
        }
    }

    fn parse_expr(&mut self, min_bp: u8) -> Option<(EbnfExpr, Span)> {
        let mut lhs = self.parse_expr_helper();

        loop {
            let token = self.peek();
            if token.is_none() {
                break;
            }
            let token = token.unwrap().clone();

            match token.kind {
                TokenKind::Semicolon | TokenKind::EndOfFile => break,
                _ => {}
            }

            if let Some(bp) = Self::postfix_bp(token.kind) {
                if bp < min_bp {
                    break;
                }
                self.next();
                let (lhs_expr, span) = lhs.unwrap();
                lhs = Some((
                    EbnfExpr::from_unary(token.kind.into(), lhs_expr),
                    span,
                ));
                continue;
            }

            let (op, (left_bp, right_bp)) = match token.kind {
                TokenKind::Pipe
                | TokenKind::Comma
                | TokenKind::Exception
                | TokenKind::Plus
                | TokenKind::Range
                | TokenKind::RangeEqual => (token.kind, Self::infix_bp(token.kind)),
                _ => {
                    break;
                }
            };

            if left_bp < min_bp {
                break;
            }

            self.next();

            if let Some((rhs, rhs_span)) = self.parse_expr(right_bp) {
                let (lhs_expr, lhs_span) = lhs.unwrap();
                if matches!(op, TokenKind::Range | TokenKind::RangeEqual) {
                    if !lhs_expr.is_char() {
                        self.diagnostic
                            .builder()
                            .report(
                                DiagnosticLevel::Error,
                                "Expected terminal",
                                self.source_manager.get_source_info(lhs_span),
                                None,
                            )
                            .add_info(
                                "Use character for range",
                                Some(self.source_manager.fix_span(lhs_span)),
                            )
                            .commit();
                        return None;
                    }
                    if !rhs.is_char() {
                        self.diagnostic
                            .builder()
                            .report(
                                DiagnosticLevel::Error,
                                "Expected terminal",
                                self.source_manager.get_source_info(rhs_span),
                                None,
                            )
                            .add_info(
                                "Use character for range",
                                Some(self.source_manager.fix_span(rhs_span)),
                            )
                            .commit();
                        return None;
                    }
                }

                lhs = Some((
                    EbnfExpr::try_merge_binary(op.into(), lhs_expr, rhs),
                    rhs_span,
                ));
            } else {
                self.diagnostic
                    .builder()
                    .report(
                        DiagnosticLevel::Error,
                        "Expected expression",
                        self.source_manager.get_source_info(token.span),
                        None,
                    )
                    .add_info(
                        "Try add expression after this",
                        Some(self.source_manager.fix_span(token.span)),
                    )
                    .commit();
                return None;
            }
        }

        lhs
    }

    fn parse_statement(&mut self) -> Option<(EbnfExpr, Span)> {
        let lhs_expr = self.parse_primary();
        if lhs_expr.is_none() {
            return None;
        }
        let (mut lhs_expr, mut span) = lhs_expr.unwrap();

        let equal_token = self.next();

        if equal_token.is_none() {
            return None;
        }

        let equal_token = equal_token.unwrap();
        let eq_span = equal_token.span;
        let is_equal = equal_token.kind == TokenKind::Equal;
        let is_def = equal_token.kind == TokenKind::Definition;

        if !(is_equal || is_def) {
            self.diagnostic
                .builder()
                .report(
                    DiagnosticLevel::Error,
                    "Expected '=', '::='",
                    self.source_manager.get_source_info(eq_span),
                    None,
                )
                .add_info(
                    "Try add '=', '::=' after this",
                    Some(self.source_manager.fix_span(eq_span)),
                )
                .commit();
            return None;
        }

        let rhs = self.parse_expr(0);
        if rhs.is_none() {
            self.diagnostic
                .builder()
                .report(
                    DiagnosticLevel::Error,
                    "Expected expression",
                    self.source_manager.get_source_info(span),
                    None,
                )
                .add_info(
                    "Try add expression after this",
                    Some(self.source_manager.fix_span(eq_span)),
                )
                .commit();
            return None;
        }

        let (rhs_expr, rhs_span) = rhs.unwrap();
        span = span.union(&rhs_span);

        lhs_expr = EbnfExpr::Variable {
            name: match lhs_expr {
                EbnfExpr::Identifier(name, ..) => name,
                _ => unreachable!(),
            },
            expr: Box::new(rhs_expr),
            is_def,
        };

        Some((lhs_expr, span))
    }
}

impl EbnfExpr {
    
    fn is_char(&self) -> bool {
        match self {
            EbnfExpr::Terminal(t) => t.is_char(),
            _ => false,
        }
    }

    fn get_max_byte_len(&self, env: Option<&EbnfParserMatcherEnv>) -> u8 {
        match self {
            EbnfExpr::Identifier(id, ..) => {
                if let Some(env) = env {
                    if let Some(el) = env.get(id) {
                        return el.get_max_byte_len(Some(env));
                    }
                }

                1
            }
            EbnfExpr::Alternative(_, _, m)
            | EbnfExpr::Concat(_, m)
            | EbnfExpr::Extend(_, m)
            | EbnfExpr::Optional(_, m)
            | EbnfExpr::Repetition(_, m)
            | EbnfExpr::Statements(_, m)
            | EbnfExpr::Exception(_, m) => *m,
            EbnfExpr::Terminal(t) => t.len() as u8,
            EbnfExpr::Variable { expr, .. } => expr.get_max_byte_len(None),
            EbnfExpr::Range { .. } => 1,
        }
    }

    fn recalculate_max_byte_len(self, env: &mut EbnfParserMatcherEnv) -> Self {
        match self {
            EbnfExpr::Identifier(id, span) => {
                if let Some(el) = env.remove(&id) {
                    let temp = el.recalculate_max_byte_len(env);
                    env.insert(id.clone(), temp);
                }
                Self::Identifier(id, span)
            }
            EbnfExpr::Variable { expr, .. } => expr.recalculate_max_byte_len(env),
            EbnfExpr::Alternative(v, h, ..) => {
                let mut max_byte_len = 1u8;
                for expr in v.iter() {
                    max_byte_len = max_byte_len.max(expr.get_max_byte_len(Some(env)));
                }
                for t in h.iter() {
                    max_byte_len = max_byte_len.max(t.len() as u8);
                }
                Self::Alternative(v, h, max_byte_len)
            }
            EbnfExpr::Concat(v, ..) => {
                let mut max_byte_len = 1u8;
                for expr in v.iter() {
                    max_byte_len = max_byte_len.max(expr.get_max_byte_len(Some(env)));
                }
                Self::Concat(v, max_byte_len)
            }
            EbnfExpr::Exception(v, ..) => {
                let mut max_byte_len = 1u8;
                for expr in v.iter() {
                    max_byte_len = max_byte_len.max(expr.get_max_byte_len(Some(env)));
                }
                Self::Exception(v, max_byte_len)
            }
            EbnfExpr::Extend(v, ..) => {
                let mut max_byte_len = 1u8;
                for expr in v.iter() {
                    max_byte_len = max_byte_len.max(expr.get_max_byte_len(Some(env)));
                }
                Self::Extend(v, max_byte_len)
            }
            EbnfExpr::Optional(o, ..) => {
                let max_byte_len = o.get_max_byte_len(Some(env));
                Self::Optional(o, max_byte_len)
            }
            EbnfExpr::Repetition(o, ..) => Self::Repetition(o, u8::MAX),
            EbnfExpr::Statements(v, ..) => {
                let mut max_byte_len = 1u8;
                for expr in v.iter() {
                    max_byte_len = max_byte_len.max(expr.get_max_byte_len(Some(env)));
                }
                Self::Statements(v, max_byte_len)
            }
            _ => self,
        }
    }

    fn try_move_terminals_to_hash_set(
        items: &mut Vec<EbnfExpr>,
        set: &mut HashSet<TerminalValue>,
    ) {
        let mut i = 0;
        while i < items.len() {
            if let Self::Terminal(_) = &items[i] {
                if let Self::Terminal(value) = items.remove(i) {
                    set.insert(value);
                }
            } else {
                i += 1;
            }
        }
    }

    fn try_merge_binary(
        op: BinaryOperator,
        lhs: EbnfExpr,
        rhs: EbnfExpr,
    ) -> EbnfExpr {
        match (op, lhs, rhs) {
            (
                BinaryOperator::Alternative,
                Self::Alternative(mut lhs, mut l_set, l_max),
                Self::Alternative(rhs, r_set, r_max),
            ) => {
                lhs.extend(rhs);
                l_set.extend(r_set);
                Self::try_move_terminals_to_hash_set(&mut lhs, &mut l_set);
                EbnfExpr::Alternative(lhs, l_set, l_max.max(r_max))
            }
            (BinaryOperator::Concat, Self::Concat(mut lhs, l_max), Self::Concat(rhs, r_max)) => {
                lhs.extend(rhs);
                EbnfExpr::Concat(lhs, l_max.max(r_max))
            }
            (
                BinaryOperator::Exception,
                Self::Exception(mut lhs, l_max),
                Self::Exception(rhs, r_max),
            ) => {
                lhs.extend(rhs);
                EbnfExpr::Exception(lhs, l_max.max(r_max))
            }
            (BinaryOperator::Extend, Self::Extend(mut lhs, l_max), Self::Extend(rhs, r_max)) => {
                lhs.extend(rhs);
                EbnfExpr::Extend(lhs, l_max.max(r_max))
            }
            (BinaryOperator::Alternative, Self::Alternative(mut lhs, mut l_set, l_max), rhs) => {
                let r_max = rhs.get_max_byte_len(None);
                lhs.push(rhs);
                Self::try_move_terminals_to_hash_set(&mut lhs, &mut l_set);
                EbnfExpr::Alternative(lhs, l_set, l_max.max(r_max))
            }
            (BinaryOperator::Concat, Self::Concat(mut lhs, l_max), rhs) => {
                let r_max = rhs.get_max_byte_len(None);
                lhs.push(rhs);
                EbnfExpr::Concat(lhs, l_max.max(r_max))
            }
            (BinaryOperator::Exception, Self::Exception(mut lhs, l_max), rhs) => {
                let r_max = rhs.get_max_byte_len(None);
                lhs.push(rhs);
                EbnfExpr::Exception(lhs, l_max.max(r_max))
            }
            (BinaryOperator::Extend, Self::Extend(mut lhs, l_max), rhs) => {
                let r_max = rhs.get_max_byte_len(None);
                lhs.push(rhs);
                EbnfExpr::Extend(lhs, l_max.max(r_max))
            }
            (_, lhs, rhs) => {
                let l_max = lhs.get_max_byte_len(None);
                let r_max = rhs.get_max_byte_len(None);
                let max_byte_len = l_max.max(r_max);
                match op {
                    BinaryOperator::Alternative => {
                        Self::Alternative(vec![lhs, rhs], HashSet::new(), max_byte_len)
                    }
                    BinaryOperator::Concat => Self::Concat(vec![lhs, rhs], max_byte_len),
                    BinaryOperator::Exception => Self::Exception(vec![lhs, rhs], max_byte_len),
                    BinaryOperator::Extend => Self::Extend(vec![lhs, rhs], max_byte_len),
                    BinaryOperator::Range | BinaryOperator::RangeEqual => {
                        if let (Self::Terminal(lhs), Self::Terminal(rhs)) = (lhs, rhs) {
                            EbnfExpr::Range {
                                lhs: lhs.as_char().unwrap(),
                                rhs: rhs.as_char().unwrap(),
                                inclusive: op == BinaryOperator::RangeEqual,
                            }
                        } else {
                            panic!("Expected terminal")
                        }
                    }
                }
            }
        }
    }

    fn from_unary(op: UnaryOperator, expr: EbnfExpr) -> EbnfExpr {
        let max_byte_len = expr.get_max_byte_len(None);
        match op {
            UnaryOperator::Optional => EbnfExpr::Optional(Box::new(expr), max_byte_len),
        }
    }

    fn match_expr<'a>(
        &self,
        matcher: &EbnfParserMatcher,
        s: &'a [u8],
        env: &EbnfParserMatcherEnv,
        source_manager: RelativeSourceManager<'a>,
        diagnostic: &mut Diagnostic,
    ) -> Option<&'a [u8]> {
        match self {
            EbnfExpr::Identifier(id, .., info) => {
                if NativeCallKind::is_valid_name(id) {
                    let kind = NativeCallKind::from(id.as_str());
                    return kind.call(matcher, s, source_manager, diagnostic);
                }
                if let Some(el) = env.get(id) {
                    el.match_expr(matcher, s, env, source_manager, diagnostic)
                } else {
                    if let Some((info, span)) = info {
                        diagnostic
                            .builder()
                            .report(
                                DiagnosticLevel::Error,
                                format!("Undefined variable '{}'", id),
                                info.clone(),
                                None,
                            )
                            .add_info("Try define variable", Some(*span))
                            .commit();
                    }
                    None
                }
            }
            EbnfExpr::Alternative(v, h, ..) => {
                let mut iter = ByteToCharIter::new(s);
                if let Some(c) = iter.next() {
                    if h.contains(&TerminalValue::Char(c)) {
                        let len = c.len_utf8();
                        return Some(&s[..len]);
                    }

                    for expr in v.iter() {
                        if let Some(s) = expr.match_expr(matcher, s, env, source_manager, diagnostic) {
                            return Some(s);
                        }
                    }
                }
                None
            }
            EbnfExpr::Concat(v, ..) => {
                let mut end = 0usize;

                for expr in v.iter() {
                    let temp_source = &s[end..];
                    if let Some(s_) = expr.match_expr(matcher, temp_source, env, source_manager, diagnostic)
                    {
                        end += s_.len();
                    } else {
                        return None;
                    }
                }
                if end == 0 {
                    None
                } else {
                    Some(&s[..end])
                }
            }
            EbnfExpr::Exception(v, ..) => {
                if v.is_empty() {
                    return None;
                }
                let mut iter = v.iter();
                let first = iter.next().unwrap();

                let matched = first.match_expr(matcher, s, env, source_manager, diagnostic);
                if matched.is_none() {
                    return None;
                }

                let mut matched = matched.unwrap();

                for e in iter {
                    let mut i = 0;
                    while i < matched.len() {
                        let end = ByteToCharIter::new(&matched[i..])
                            .utf8_len_after_skip(e.get_max_byte_len(Some(env)) as usize);
                        let temp_source = &matched[i..(i + end).min(matched.len())];
                        if let Some(_) = e.match_expr(matcher, temp_source, env, source_manager, diagnostic)
                        {
                            matched = &matched[0..i];
                            break;
                        }
                        i += end;
                    }
                }

                if matched.is_empty() {
                    return None;
                }

                Some(matched)
            }
            EbnfExpr::Extend(v, ..) => {
                if v.is_empty() {
                    return None;
                }

                let mut matched: &[u8] = &[];

                let mut end = matched.len();
                let mut start = 0usize;
                for expr in v.iter() {
                    matched = &s[start..];
                    if let Some(s_) = expr.match_expr(matcher, matched, env, source_manager, diagnostic) {
                        start += s_.len();
                        end += s_.len();
                    } else {
                        break;
                    }
                }

                if end == 0 {
                    None
                } else {
                    Some(&s[..end])
                }
            }
            EbnfExpr::Optional(o, ..) => {
                let mut end = 0usize;

                if let Some(s_) = o.match_expr(matcher, s, env, source_manager, diagnostic) {
                    end += s_.len();
                }

                Some(&s[..end])
            }
            EbnfExpr::Repetition(v, ..) => {
                let mut end = 0usize;
                while let Some(s_) = v.match_expr(matcher, &s[end..], env, source_manager, diagnostic) {
                    end += s_.len();
                }

                if end == 0 {
                    None
                } else {
                    Some(&s[..end])
                }

            }
            EbnfExpr::Terminal(t) => {
                let end = t.len_utf8();
                if end > s.len() {
                    return None;
                }

                if t == &s[0..end] {
                    return Some(&s[..end]);
                }

                None
            }
            EbnfExpr::Statements(_, ..) => panic!("Expected expression, but got statements"),
            EbnfExpr::Variable { .. } => panic!("Expected expression, but got variable"),
            EbnfExpr::Range {
                lhs,
                rhs,
                inclusive,
            } => {
                if s.is_empty() {
                    return None;
                }
                let mut iter = ByteToCharIter::new(s);
                let c = iter.next();
                if c.is_none() {
                    return None;
                }

                let c = c.unwrap();
                let lhs = *lhs;
                let rhs = *rhs;

                let is_valid = if *inclusive {
                    (lhs..=rhs).contains(&c)
                } else {
                    (lhs..rhs).contains(&c)
                };

                if is_valid {
                    let len = c.len_utf8();
                    Some(&s[..len])
                } else {
                    None
                }
            }
        }
    }
}

impl Display for EbnfExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EbnfExpr::Statements(exprs, ..) => {
                for expr in exprs {
                    writeln!(f, "{}", expr)?;
                }
                Ok(())
            }
            EbnfExpr::Identifier(name, ..) => write!(f, "{}", name),
            EbnfExpr::Variable { name, expr, is_def } => {
                if *is_def {
                    write!(f, "{} ::= {};", name, expr)
                } else {
                    write!(f, "{} = {};", name, expr)
                }
            }
            EbnfExpr::Terminal(name) => write!(f, "'{}'", name),
            EbnfExpr::Repetition(expr, ..) => {
                write!(f, "{{ ")?;
                write!(f, "{} ", *expr)?;
                write!(f, "}}")
            }
            EbnfExpr::Optional(expr, ..) => {
                write!(f, "[ ")?;
                write!(f, "{} ", *expr)?;
                write!(f, "]")
            }
            EbnfExpr::Alternative(exprs, set, ..) => {
                write!(f, "(")?;
                for t in set.iter() {
                    write!(f, "'{}' | ", t)?
                }
                for expr in exprs.iter() {
                    write!(f, "{} | ", expr)?
                }
                write!(f, ")")
            }
            EbnfExpr::Concat(exprs, ..) => {
                write!(f, "(")?;
                write!(
                    f,
                    "{}",
                    exprs
                        .iter()
                        .map(|e| e.to_string())
                        .collect::<Vec<_>>()
                        .join(" , ")
                )?;
                write!(f, ")")
            }
            EbnfExpr::Exception(exprs, ..) => {
                write!(f, "(")?;
                write!(
                    f,
                    "{}",
                    exprs
                        .iter()
                        .map(|e| e.to_string())
                        .collect::<Vec<_>>()
                        .join(" - ")
                )?;
                write!(f, ")")
            }
            EbnfExpr::Extend(exprs, ..) => {
                write!(f, "(")?;
                write!(
                    f,
                    "{}",
                    exprs
                        .iter()
                        .map(|e| e.to_string())
                        .collect::<Vec<_>>()
                        .join(" + ")
                )?;
                write!(f, ")")
            }
            EbnfExpr::Range {
                lhs,
                rhs,
                inclusive,
            } => {
                write!(f, "{}", lhs)?;
                if *inclusive {
                    write!(f, " ..= ")?;
                } else {
                    write!(f, " .. ")?;
                }
                write!(f, "{}", rhs)
            }
        }
    }
}

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
    fn as_str(&self) -> &'static str {
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

    fn call<'b>(
        &self,
        matcher: &EbnfParserMatcher,
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
            Self::FloatingPoint => matcher.match_native_floating_point(s, source_manager, diagnostic)
        }
    }

    fn is_valid_name(name: &str) -> bool {
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

#[derive(Debug, Clone, PartialEq, Eq)]
enum EbnfParserEnvVariable {
    Expr(EbnfExpr),
    NativeCall(NativeCallKind),
}

impl Display for EbnfParserEnvVariable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Expr(e) => write!(f, "{}", e),
            Self::NativeCall(name) => write!(f, "<Native Call='{name}'>"),
        }
    }
}

impl EbnfParserEnvVariable {
    fn substitute_extend(&mut self, old_name: &str, new_name: &str) -> bool {
        match self {
            Self::Expr(expr) => expr.substitute_extend(old_name, new_name),
            _ => true,
        }
    }

    fn is_native_call(&self) -> bool {
        matches!(self, Self::NativeCall(_))
    }

    fn match_expr<'a>(
        &self,
        matcher: &EbnfParserMatcher,
        s: &'a [u8],
        env: &EbnfParserMatcherEnv,
        source_manager: RelativeSourceManager<'a>,
        diagnostic: &mut Diagnostic,
    ) -> Option<&'a [u8]> {
        match self {
            Self::Expr(expr) => expr.match_expr(matcher, s, env, source_manager, diagnostic),
            Self::NativeCall(func_name) => func_name.call(matcher, s, source_manager, diagnostic),
        }
    }

    fn get_max_byte_len(&self, env: Option<&EbnfParserMatcherEnv>) -> u8 {
        match self {
            Self::Expr(expr) => expr.get_max_byte_len(env),
            Self::NativeCall(_) => 1,
        }
    }

    fn recalculate_max_byte_len(self, env: &mut EbnfParserMatcherEnv) -> Self {
        match self {
            Self::Expr(expr) => Self::Expr(expr.recalculate_max_byte_len(env)),
            _ => self,
        }
    }
}

impl EbnfExpr {
    fn init_env<'a>(
        self,
        env: &mut EbnfParserMatcherEnv,
        def: &mut EbnfParserMatcherDef,
        diagnostic: &mut Diagnostic,
    ) {
        match self {
            EbnfExpr::Statements(exprs, ..) => {
                for expr in exprs {
                    expr.init_env(env, def, diagnostic)
                }
            }
            EbnfExpr::Variable { name, expr, is_def } => {
                let mut new_expr = EbnfParserEnvVariable::Expr(*expr);
                if is_def {
                    if !def.contains(&name) {
                        def.insert(name.clone());
                    }
                }
                if let Some(old_expr) = env.remove(&name) {
                    let new_name = Self::get_unique_name(&name, env);
                    if new_expr.substitute_extend(&name, &new_name) {
                        env.insert(new_name, old_expr);
                    }
                }
                env.insert(name, new_expr);
            }
            _ => {}
        }
    }

    fn get_unique_name(name: &str, env: &EbnfParserMatcherEnv) -> String {
        let mut i = 0;
        let mut new_name = name.to_owned();
        while env.contains(&new_name) || (new_name == name) {
            i += 1;
            new_name = format!("{}_{}", name, i);
        }
        new_name
    }

    fn substitute_extend(&mut self, old_name: &str, new_name: &str) -> bool {
        match self {
            Self::Variable { expr, .. } => expr.substitute_extend(old_name, new_name),
            Self::Alternative(exprs, _, ..)
            | Self::Concat(exprs, ..)
            | Self::Exception(exprs, ..)
            | Self::Extend(exprs, ..) => {
                let mut has_substituted = false;
                for expr in exprs.iter_mut() {
                    has_substituted = expr.substitute_extend(old_name, new_name) || has_substituted;
                }
                has_substituted
            }
            Self::Optional(expr, ..) | Self::Repetition(expr, ..) => {
                expr.substitute_extend(old_name, new_name)
            }
            Self::Identifier(name, ..) => {
                if name == old_name {
                    *name = new_name.to_string();
                    true
                } else {
                    false
                }
            }
            _ => false,
        }
    }
}

struct EbnfParserMatcherDef(HashSet<&'static str>, Vec<UniqueString>);

impl EbnfParserMatcherDef {
    fn new() -> Self {
        Self(HashSet::new(), Vec::new())
    }

    fn contains(&self, name: &str) -> bool {
        self.0.contains(name)
    }

    fn ordered_iter(&self) -> Iter<'_, UniqueString> {
        self.1.iter()
    }

    fn insert(&mut self, name: String) {
        let s = UniqueString::new(name);
        self.0.insert(s.as_str());
        self.1.push(s);
    }

    fn keys(&self) -> std::collections::hash_set::Iter<'_, &'static str> {
        self.0.iter()
    }
}

struct EbnfParserMatcherEnv{
    hash: HashMap<String, EbnfParserEnvVariable>,
    identifiers: Option<EbnfParserEnvVariable>,
    operators: Option<EbnfParserEnvVariable>,
    integer: Option<EbnfParserEnvVariable>,
    floating_point: Option<EbnfParserEnvVariable>,
}

impl EbnfParserMatcherEnv {
    fn new() -> Self {
        Self {
            hash: HashMap::new(),
            identifiers: None,
            operators: None,
            integer: None,
            floating_point: None,
        }
    }

    fn keys(&self) -> Vec<String> {
        let mut temp: Vec<_> = self.hash.keys().map(|s| s.to_owned()).collect();
        if self.identifiers.is_some() {
            temp.push("identifier".to_string());
        }

        if self.operators.is_some() {
            temp.push("operator".to_string());
        }

        if self.integer.is_some() {
            temp.push("integer".to_string());
        }

        if self.floating_point.is_some() {
            temp.push("floating_point".to_string());
        }

        temp
    }

    fn contains(&self, name: &str) -> bool {
        self.hash.contains_key(name) || match name {
            "identifier" => self.identifiers.is_some(),
            "operator" => self.operators.is_some(),
            "integer" => self.integer.is_some(),
            "floating_point" => self.floating_point.is_some(),
            _ => false,
        }
    }

    fn insert(&mut self, name: String, value: EbnfParserEnvVariable) {
        match name.as_str() {
            "identifier" => self.identifiers = Some(value),
            "operator" => self.operators = Some(value),
            "integer" => self.integer = Some(value),
            "floating_point" => self.floating_point = Some(value),
            _ => {
                self.hash.insert(name, value);
            }
        }
    }

    fn remove(&mut self, name: &str) -> Option<EbnfParserEnvVariable> {
        match name {
            "identifier" => self.identifiers.take(),
            "operator" => self.operators.take(),
            "integer" => self.integer.take(),
            "floating_point" => self.floating_point.take(),
            _ => self.hash.remove(name),
        }
    }

    fn get(&self, name: &str) -> Option<&EbnfParserEnvVariable> {
        match name {
            "identifier" => self.identifiers.as_ref(),
            "operator" => self.operators.as_ref(),
            "integer" => self.integer.as_ref(),
            "floating_point" => self.floating_point.as_ref(),
            _ => self.hash.get(name),
        }
    }

    fn get_from_unique_string(&self, name: &UniqueString) -> Option<&EbnfParserEnvVariable> {
        self.get(name.as_str())
    }

    fn insert_unique_string(&mut self, name: UniqueString, value: EbnfParserEnvVariable) {
        self.insert(name.as_str().to_string(), value);
    }

    fn contains_key(&self, name: &UniqueString) -> bool {
        self.contains(name.as_str())
    }
}


pub(crate) struct EbnfParserMatcher {
    env: EbnfParserMatcherEnv,
    def: EbnfParserMatcherDef,
    identifier_sym: UniqueString,
    operator_sym: UniqueString,
    integer_sym: UniqueString,
    fp_sym: UniqueString,
}

impl EbnfParserMatcher {
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
        let rep_expr =
            EbnfExpr::Identifier(NativeCallKind::ContIdentifier.to_string(), None);
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

    pub(crate) fn match_native_identifier<'b>(&self, s: &'b [u8], source_manager: RelativeSourceManager<'b>, diagnostic: &mut Diagnostic) -> Option<&'b [u8]> {
        if s.is_empty() {
            return None;
        }

        if self.match_native(NativeCallKind::StartIdentifier, s, source_manager, diagnostic).is_none() {
            return None;
        }

        let mut i = get_utf8_char_len(s[0]);
        while i < s.len() {
            let end = get_utf8_char_len(s[i]);
            let temp_source = &s[i..(i + end).min(s.len() - 1)];
            if self.match_native(NativeCallKind::ContIdentifier, temp_source, source_manager, diagnostic).is_none() {
                return Some(&s[..i]);
            }
            i += end;
        }
        
        Some(&s[..i])
    }

    fn get_digit<'b>(&self, s: &[u8], source_manager: RelativeSourceManager<'b>, diagnostic: &mut Diagnostic) -> Option<char> {
        self.match_native(NativeCallKind::Digit, s, source_manager, diagnostic).map(|s| ByteToCharIter::new(s).next()).flatten()
    }

    pub(crate) fn match_native_integer<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &mut Diagnostic,
    ) -> Option<&'b [u8]> {
        if s.is_empty() {
            return None;
        }

        let mut iter = ByteToCharIter::new(s).peekable();
        let mut end = 0usize;

        if iter.peek() == Some(&'.') {
            return None;
        }

        let mut has_dot = false;
        let has_hex = s.starts_with(b"0x") || s.starts_with(b"0X");

        let mut has_e = false;
        let mut has_p = false;

        while let Some(ch) = iter.peek() {
            has_dot = (*ch == '.') || has_dot;

            has_e = (*ch == 'e' || *ch == 'E') || has_e;
            has_p = (*ch == 'p' || *ch == 'P') || has_p;

            match *ch {
                ch if ch.is_whitespace() => break,
                _ => {
                    iter.next();
                }
            }
        }

        if has_dot || (!has_hex && has_e) || has_p {
            return None;
        }

        let mut matched = s;

        let ch = self.get_digit(matched, source_manager, diagnostic);
        
        if ch.is_none() {
            return None;
        }

        let ch = ch.unwrap();
        
        let is_start_with_zero = ch == '0';
        
        end += ch.len_utf8();
        matched = &s[end..];

        if is_start_with_zero {
            let ch = ByteToCharIter::new(matched).next();
            let mut underscore_count = 0usize;
            if ch == Some('x') {
                end += 1;
                matched = &s[end..];
                let start = end;
                while let Some(ch) = ByteToCharIter::new(matched).next() {
                    if let Some(ch) = self.match_native(NativeCallKind::HexDigit, matched, source_manager, diagnostic) {
                        end += ch.len();
                        matched = &s[end..];
                    } else if ch == '_' {
                        end += ch.len_utf8();
                        matched = &s[end..];
                        underscore_count += 1;
                    } else {
                        break;
                    }
                }

                if start == (end - underscore_count) {
                    diagnostic.builder().report(DiagnosticLevel::Error, "Expecting hexadecimal after '0x', but found none", source_manager.get_source_info(Span::from_usize(start, start + 1)), None)
                        .add_error("Hex digit must be between 0 and 9 or a and f", Some(source_manager.fix_span(Span::from_usize(start, end))))
                        .commit();
                }
                return Some(&s[..end]);
            }

            if ch == Some('o') {
                end += 1;
                
                matched = &s[end..];
                let start = end;
                while let Some(ch) = ByteToCharIter::new(matched).next() {
                    if let Some(ch) = self.match_native(NativeCallKind::OctDigit, matched, source_manager, diagnostic) {
                        end += ch.len();
                        matched = &s[end..];
                    } else if ch == '_' {
                        end += ch.len_utf8();
                        matched = &s[end..];
                        underscore_count += 1;
                    } else {
                        break;
                    }
                }

                if start == (end - underscore_count) {
                    diagnostic.builder().report(DiagnosticLevel::Error, "Expecting octal after '0o', but found none", source_manager.get_source_info(Span::from_usize(start, start + 1)), None)
                        .add_error("Octal digit must be between 0 and 7", Some(source_manager.fix_span(Span::from_usize(start, end))))
                        .commit();
                }

                return Some(&s[..end]);
            }

            if ch == Some('b') {
                end += 1;
                matched = &s[end..];

                let start = end;

                while let Some(ch) = ByteToCharIter::new(matched).next() {
                    if let Some(ch) = self.match_native(NativeCallKind::BinDigit, matched, source_manager, diagnostic) {
                        end += ch.len();
                        matched = &s[end..];
                    } else if ch == '_' {
                        end += ch.len_utf8();
                        matched = &s[end..];
                        underscore_count += 1;
                    } else {
                        break;
                    }
                }

                if start == (end - underscore_count) {
                    diagnostic.builder().report(DiagnosticLevel::Error, "Expecting binary after '0b', but found none", source_manager.get_source_info(Span::from_usize(start, start + 1)), None)
                        .add_error("Binary digit must be 0 or 1", Some(source_manager.fix_span(Span::from_usize(start, end))))
                        .commit();
                }
                return Some(&s[..end]);
            }
        }

        while let Some(ch) = ByteToCharIter::new(matched).next() {
            if let Some(ch) = self.match_native(NativeCallKind::Digit, matched, source_manager, diagnostic) {
                end += ch.len();
                matched = &s[end..];
            } else if ch == '_' {
                end += ch.len_utf8();
                matched = &s[end..];
            } else {
                break;
            }
        }
        
        if end == 0 {
            return None;
        }

        Some(&s[..end])
    }

    pub(crate) fn match_native_floating_point<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &mut Diagnostic,
    ) -> Option<&'b [u8]> {
        if s.is_empty() {
            return None;
        }

        let mut iter = ByteToCharIter::new(s).peekable();
        let mut end = 0usize;
        let mut matched = s;

        let ch = iter.peek();
        if ch.is_none() {
            return None;
        }

        let ch = unsafe { *ch.unwrap_unchecked() };
        
        let ch = match (self.get_digit(matched, source_manager, diagnostic), ch) {
            (_, '.') => Some('.'),
            (Some(ch), _) => Some(ch),
            _ => None
        };

        if ch.is_none() {
            return None;
        }

        let ch = unsafe { ch.unwrap_unchecked() };

        let is_start_with_zero = ch == '0';
        end += ch.len_utf8();

        matched = &s[end..];

        if is_start_with_zero && (self.get_digit(matched, source_manager, diagnostic) == Some('x')) {
            end += 1;
            let start = end;
            let mut underscore_count = 0usize;
            while let Some(ch) = ByteToCharIter::new(matched).next() {
                if let Some(ch) = self.match_native(NativeCallKind::HexDigit, matched, source_manager, diagnostic) {
                    end += ch.len();
                    matched = &s[end..];
                } else if ch == '_' {
                    end += ch.len_utf8();
                    matched = &s[end..];
                    underscore_count += 1;
                } else {
                    break
                }
            }

            if start == (end - underscore_count) {
                diagnostic.builder().report(DiagnosticLevel::Error, "Expecting hexadecimal after '0x', but found none", source_manager.get_source_info(Span::from_usize(start, start + 1)), None)
                    .add_error("Hex digit must be between 0 and 9 or a and f", Some(source_manager.fix_span(Span::from_usize(start, end))))
                    .commit();
                return None;
            }

            matched = &s[end..];

            if self.get_digit(matched, source_manager, diagnostic) == Some('.') {
                end += 1;
                matched = &s[end..];
                while let Some(ch) = ByteToCharIter::new(matched).next() {
                    if let Some(ch) = self.match_native(NativeCallKind::HexDigit, matched, source_manager, diagnostic) {
                        end += ch.len();
                        matched = &s[end..];
                    } else if ch == '_' {
                        end += ch.len_utf8();
                        matched = &s[end..];
                        underscore_count += 1;
                    } else {
                        break;
                    }
                }
            }

            let ch = ByteToCharIter::new(matched).next();
            if ch == Some('p') || ch == Some('P') {
                end += 1;
                matched = &s[end..];
                let ch = ByteToCharIter::new(matched).next();
                if ch == Some('+') || ch == Some('-') {
                    end += 1;
                    matched = &s[end..];
                }
                while let Some(ch) = ByteToCharIter::new(matched).next() {
                    if let Some(ch) = self.match_native(NativeCallKind::Digit, matched, source_manager, diagnostic) {
                        end += ch.len();
                        matched = &s[end..];
                    } else if ch == '_' {
                        end += ch.len_utf8();
                        matched = &s[end..];
                        underscore_count += 1;
                    } else {
                        break;
                    }
                }
            }

            return Some(&s[..end]);
        }

        while let Some(ch) = ByteToCharIter::new(matched).next() {
            if let Some(ch) = self.match_native(NativeCallKind::Digit, matched, source_manager, diagnostic) {
                end += ch.len();
                matched = &s[end..];
            } else if ch == '_' {
                end += ch.len_utf8();
                matched = &s[end..];
            } else {
                break;
            }
        }

        matched = &s[end..];
        if ByteToCharIter::new(matched).next() == Some('.') {
            end += 1;
            matched = &s[end..];
            while let Some(ch) = ByteToCharIter::new(matched).next() {
                if let Some(ch) = self.match_native(NativeCallKind::Digit, matched, source_manager, diagnostic) {
                    end += ch.len();
                    matched = &s[end..];
                } else if ch == '_' {
                    end += ch.len_utf8();
                    matched = &s[end..];
                } else {
                    break;
                }
            }
        }

        let ch = ByteToCharIter::new(matched).next();
        if ch == Some('e') || ch == Some('E') {
            end += 1;
            matched = &s[end..];
            let ch = ByteToCharIter::new(matched).next();
            if ch == Some('+') || ch == Some('-') {
                end += 1;
                matched = &s[end..];
            }
            while let Some(ch) = ByteToCharIter::new(matched).next() {
                if let Some(ch) = self.match_native(NativeCallKind::Digit, matched, source_manager, diagnostic) {
                    end += ch.len();
                    matched = &s[end..];
                } else if ch == '_' {
                    end += ch.len_utf8();
                    matched = &s[end..];
                } else {
                    break;
                }
            }
        }

        if end == 0 {
            return None;
        }

        Some(&s[..end])
    }

    pub(crate) fn match_native_operator<'b>(&self, s: &'b [u8], source_manager: RelativeSourceManager<'b>, diagnostic: &mut Diagnostic) -> Option<&'b [u8]> {
        if s.is_empty() {
            return None;
        }
        if self.match_native(NativeCallKind::StartOperator, s, source_manager, diagnostic).is_none() {
            return None;
        }

        let mut i = get_utf8_char_len(s[0]);
        while i < s.len() {
            let end = get_utf8_char_len(s[i]);
            let temp_source = &s[i..(i + end).min(s.len() - 1)];
            if self.match_native(NativeCallKind::ContOperator, temp_source, source_manager, diagnostic).is_none() {
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
                    self.match_native_identifier(s, source_manager, diagnostic).map(|s| (s, TokenKind::Identifier))
                } else {
                    expr.match_expr(self, s, &self.env, source_manager, diagnostic)
                        .map(|s| (s, TokenKind::Identifier))
                };
                temp
            }
            _ if key == self.operator_sym.as_str() => {
                let temp = if !self.contains_def(key) {
                    self.match_native_operator(s, source_manager, diagnostic).map(|s| (s, TokenKind::Operator))
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
        &mut self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &mut Diagnostic,
    ) -> Option<(&'b [u8], TokenKind)> {
        for d in self.def.ordered_iter() {
            if let Some(expr) = self.env.get(d.as_str()) {
                let temp = self.match_expr_helper(
                    *d,
                    expr,
                    s,
                    source_manager,
                    diagnostic,
                );

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
            self.match_expr_helper(self.identifier_sym, expr, s, source_manager, diagnostic).map(|(s, _)| s)
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
            self.match_expr_helper(self.operator_sym, expr, s, source_manager, diagnostic).map(|(s, _)| s)
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
        self.match_expr_for(
            kind.as_str(),
            s,
            source_manager,
            diagnostic,
        )
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
        self.contains_def("hex_digit") || self.contains_def("oct_digit") || self.contains_def("bin_digit")
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
