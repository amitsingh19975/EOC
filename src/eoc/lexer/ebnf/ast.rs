#![allow(dead_code)]

use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    hash::Hash,
};

use crate::eoc::{
    lexer::{
        str_utils::ByteToCharIter,
        token::{Token, TokenKind},
        utils::{
            byte_to_char, decode_unicode_escape_sequence,
            is_valid_identifier_continuation_code_point, is_valid_identifier_start_code_point,
            ParenMatching,
        },
    },
    utils::{
        diagnostic::{Diagnostic, DiagnosticLevel, DiagnosticReporter},
        source_manager::SourceManager,
        span::Span,
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum BinaryOperator {
    Alternative, // "|"
    Concat,      // ","
    Exception,   // "-"
    Extend,      // "+"
    Range,       // ".."
    RangeEqual,  // "..."
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum UnaryOperator {
    Optional,
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
            TerminalValue::String(s) => s.len(),
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

#[derive(Debug, Clone, PartialEq, Eq)]
enum EbnfExpr {
    Statements(Vec<EbnfExpr>),
    Identifier(String, Option<Span>),
    BinaryExpr(BinaryOperator, Box<EbnfExpr>, Box<EbnfExpr>),
    UnaryExpr(UnaryOperator, Box<EbnfExpr>),
    Variable {
        name: String,
        expr: Box<EbnfExpr>,
        is_def: bool,
    },
    Terminal(TerminalValue),
    Repetition(Box<EbnfExpr>),
    Grouping(Box<EbnfExpr>),
    Optional(Box<EbnfExpr>),
}

impl EbnfExpr {
    fn is_char(&self) -> bool {
        match self {
            EbnfExpr::Terminal(t) => t.is_char(),
            _ => false,
        }
    }
}

type EbnfExprMaxByteLen = u8;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum FlattenEbnfExpr {
    Identifier(String, Option<Span>),
    Alternative(
        Vec<FlattenEbnfExpr>,
        HashSet<TerminalValue>,
        EbnfExprMaxByteLen,
    ),
    Concat(Vec<FlattenEbnfExpr>, EbnfExprMaxByteLen),
    Exception(Vec<FlattenEbnfExpr>, EbnfExprMaxByteLen),
    Extend(Vec<FlattenEbnfExpr>, EbnfExprMaxByteLen),
    Optional(Box<FlattenEbnfExpr>, EbnfExprMaxByteLen),
    Repetition(Box<FlattenEbnfExpr>, EbnfExprMaxByteLen),
    Terminal(TerminalValue),
    Statements(Vec<FlattenEbnfExpr>, EbnfExprMaxByteLen),
    Variable {
        name: String,
        expr: Box<FlattenEbnfExpr>,
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
    ) -> FlattenEbnfExpr {
        let mut parser = EbnfParser {
            source_manager,
            diagnostic,
            tokens,
            cursor: 0,
        };

        let expr = parser.parse_statements();
        FlattenEbnfExpr::new(expr)
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

        EbnfExpr::Statements(statements)
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
                EbnfExpr::Identifier(self.get_string_from_token(&token), Some(span)),
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
                    .map(|(expr, span)| (EbnfExpr::Grouping(Box::new(expr)), span))
            }
            TokenKind::OpenBracket => {
                self.next();
                self.parse_paren_expr(TokenKind::OpenBracket, 0)
                    .map(|(expr, span)| (EbnfExpr::Optional(Box::new(expr)), span))
            }
            TokenKind::OpenBrace => {
                self.next();
                self.parse_paren_expr(TokenKind::OpenBrace, 0)
                    .map(|(expr, span)| (EbnfExpr::Repetition(Box::new(expr)), span))
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
                    EbnfExpr::UnaryExpr(
                        match token.kind {
                            TokenKind::QuestionMark => UnaryOperator::Optional,
                            _ => unreachable!(),
                        },
                        Box::new(lhs_expr),
                    ),
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
                    EbnfExpr::BinaryExpr(
                        match op {
                            TokenKind::Pipe => BinaryOperator::Alternative,
                            TokenKind::Comma => BinaryOperator::Concat,
                            TokenKind::Exception => BinaryOperator::Exception,
                            TokenKind::Plus => BinaryOperator::Extend,
                            TokenKind::Range => BinaryOperator::Range,
                            TokenKind::RangeEqual => BinaryOperator::RangeEqual,
                            _ => unreachable!(),
                        },
                        Box::new(lhs_expr),
                        Box::new(rhs),
                    ),
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

impl FlattenEbnfExpr {
    fn new(expr: EbnfExpr) -> Self {
        Self::from_expr(expr)
    }

    fn get_max_byte_len(&self) -> u8 {
        match self {
            FlattenEbnfExpr::Identifier(id, ..) => id.len() as u8,
            FlattenEbnfExpr::Alternative(_, _, m)
            | FlattenEbnfExpr::Concat(_, m)
            | FlattenEbnfExpr::Extend(_, m)
            | FlattenEbnfExpr::Optional(_, m)
            | FlattenEbnfExpr::Repetition(_, m)
            | FlattenEbnfExpr::Statements(_, m)
            | FlattenEbnfExpr::Exception(_, m) => *m,
            FlattenEbnfExpr::Terminal(t) => t.len() as u8,
            FlattenEbnfExpr::Variable { expr, .. } => expr.get_max_byte_len(),
            FlattenEbnfExpr::Range { .. } => 1,
        }
    }

    fn from_expr(expr: EbnfExpr) -> Self {
        match expr {
            EbnfExpr::Statements(s) => {
                let mut statements = Vec::new();
                let mut max_byte_len = 1u8;
                for expr in s {
                    let expr = Self::new(expr);
                    max_byte_len = max_byte_len.max(expr.get_max_byte_len());
                    statements.push(expr);
                }
                Self::Statements(statements, max_byte_len)
            }
            EbnfExpr::Identifier(id, info) => Self::Identifier(id, info),
            EbnfExpr::BinaryExpr(op, lhs, rhs) => Self::from_binary(op, *lhs, *rhs),
            EbnfExpr::UnaryExpr(op, expr) => Self::from_unary(op, *expr),
            EbnfExpr::Variable { name, expr, is_def } => Self::Variable {
                name,
                expr: Box::new(Self::new(*expr)),
                is_def,
            },
            EbnfExpr::Terminal(t) => FlattenEbnfExpr::Terminal(t),
            EbnfExpr::Repetition(r) => {
                let r = Self::new(*r);
                let max_byte_len = r.get_max_byte_len();
                Self::Repetition(Box::new(r), max_byte_len)
            }
            EbnfExpr::Grouping(g) => Self::new(*g),
            EbnfExpr::Optional(o) => {
                let o = Self::new(*o);
                let max_byte_len = o.get_max_byte_len();
                Self::Optional(Box::new(o), max_byte_len)
            }
        }
    }

    fn try_move_terminals_to_hash_set(
        items: &mut Vec<FlattenEbnfExpr>,
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
        lhs: FlattenEbnfExpr,
        rhs: FlattenEbnfExpr,
    ) -> FlattenEbnfExpr {
        match (op, lhs, rhs) {
            (
                BinaryOperator::Alternative,
                Self::Alternative(mut lhs, mut l_set, l_max),
                Self::Alternative(rhs, r_set, r_max),
            ) => {
                lhs.extend(rhs);
                l_set.extend(r_set);
                Self::try_move_terminals_to_hash_set(&mut lhs, &mut l_set);
                FlattenEbnfExpr::Alternative(lhs, l_set, l_max.max(r_max))
            }
            (BinaryOperator::Concat, Self::Concat(mut lhs, l_max), Self::Concat(rhs, r_max)) => {
                lhs.extend(rhs);
                FlattenEbnfExpr::Concat(lhs, l_max.max(r_max))
            }
            (
                BinaryOperator::Exception,
                Self::Exception(mut lhs, l_max),
                Self::Exception(rhs, r_max),
            ) => {
                lhs.extend(rhs);
                FlattenEbnfExpr::Exception(lhs, l_max.max(r_max))
            }
            (BinaryOperator::Extend, Self::Extend(mut lhs, l_max), Self::Extend(rhs, r_max)) => {
                lhs.extend(rhs);
                FlattenEbnfExpr::Extend(lhs, l_max.max(r_max))
            }
            (BinaryOperator::Alternative, Self::Alternative(mut lhs, mut l_set, l_max), rhs) => {
                let r_max = rhs.get_max_byte_len();
                lhs.push(rhs);
                Self::try_move_terminals_to_hash_set(&mut lhs, &mut l_set);
                FlattenEbnfExpr::Alternative(lhs, l_set, l_max.max(r_max))
            }
            (BinaryOperator::Concat, Self::Concat(mut lhs, l_max), rhs) => {
                let r_max = rhs.get_max_byte_len();
                lhs.push(rhs);
                FlattenEbnfExpr::Concat(lhs, l_max.max(r_max))
            }
            (BinaryOperator::Exception, Self::Exception(mut lhs, l_max), rhs) => {
                let r_max = rhs.get_max_byte_len();
                lhs.push(rhs);
                FlattenEbnfExpr::Exception(lhs, l_max.max(r_max))
            }
            (BinaryOperator::Extend, Self::Extend(mut lhs, l_max), rhs) => {
                let r_max = rhs.get_max_byte_len();
                lhs.push(rhs);
                FlattenEbnfExpr::Extend(lhs, l_max.max(r_max))
            }
            (_, lhs, rhs) => {
                let l_max = lhs.get_max_byte_len();
                let r_max = rhs.get_max_byte_len();
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
                            FlattenEbnfExpr::Range {
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

    fn from_binary(op: BinaryOperator, lhs: EbnfExpr, rhs: EbnfExpr) -> Self {
        let lhs = Self::new(lhs);
        let rhs = Self::new(rhs);
        Self::try_merge_binary(op, lhs, rhs)
    }

    fn from_unary(op: UnaryOperator, expr: EbnfExpr) -> FlattenEbnfExpr {
        let expr = Self::new(expr);
        let max_byte_len = expr.get_max_byte_len();
        match op {
            UnaryOperator::Optional => FlattenEbnfExpr::Optional(Box::new(expr), max_byte_len),
        }
    }

    fn match_expr<'a>(
        &self,
        s: &'a [u8],
        env: &HashMap<String, EbnfParserEnvVariable>,
        source_manager: &SourceManager,
        diagnostic: &mut Diagnostic,
    ) -> Option<&'a [u8]> {
        match self {
            FlattenEbnfExpr::Identifier(id, span) => {
                if let Some(el) = env.get(id) {
                    el.match_expr(s, env, source_manager, diagnostic)
                } else {
                    if let Some(span) = *span {
                        let info = source_manager.get_source_info(span);
                        diagnostic
                            .builder()
                            .report(
                                DiagnosticLevel::Error,
                                format!("Undefined variable '{}'", id),
                                info,
                                None,
                            )
                            .add_info("Try define variable", Some(source_manager.fix_span(span)))
                            .commit();
                    }
                    None
                }
            }
            FlattenEbnfExpr::Alternative(v, h, ..) => {
                let mut iter = ByteToCharIter::new(s);
                if let Some(c) = iter.next() {
                    if h.contains(&TerminalValue::Char(c)) {
                        let len = c.len_utf8();
                        return Some(&s[..len]);
                    }

                    for expr in v.iter() {
                        if let Some(s) = expr.match_expr(s, env, source_manager, diagnostic) {
                            return Some(s);
                        }
                    }
                }
                None
            }
            FlattenEbnfExpr::Concat(v, ..) => {
                let mut end = 0usize;

                for expr in v.iter() {
                    let temp_source = &s[end..];
                    if let Some(s_) = expr.match_expr(temp_source, env, source_manager, diagnostic) {
                        end += s_.len();
                    } else {
                        return None;
                    }
                }
                Some(&s[..end])
            }
            FlattenEbnfExpr::Exception(v, ..) => {
                if v.is_empty() {
                    return None;
                }
                let mut iter = v.iter();
                let first = iter.next().unwrap();

                let matched = first.match_expr(s, env, source_manager, diagnostic);
                if matched.is_none() {
                    return None;
                }

                let matched = matched.unwrap();
                let mut matched = std::str::from_utf8(matched).unwrap();

                for e in iter {
                    let mut i = 0;
                    while i < matched.len() {
                        let end = (e.get_max_byte_len() as usize + i).min(matched.len());
                        let temp_source = &matched[i..end];
                        if let Some(_) =
                            e.match_expr(temp_source.as_bytes(), env, source_manager, diagnostic)
                        {
                            matched = &matched[0..i];
                            break;
                        }
                        i += end - i;
                    }
                }

                if matched.is_empty() {
                    return None;
                }

                Some(matched.as_bytes())
            }
            FlattenEbnfExpr::Extend(v, ..) => {
                let mut end = 0usize;

                for expr in v.iter() {
                    let temp_source = &s[end..];
                    if let Some(s_) = expr.match_expr(temp_source, env, source_manager, diagnostic)
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
            FlattenEbnfExpr::Optional(o, ..) => {
                let mut end = 0usize;

                if let Some(s_) = o.match_expr(s, env, source_manager, diagnostic) {
                    end += s_.len();
                }

                Some(&s[..end])
            }
            FlattenEbnfExpr::Repetition(v, ..) => {
                let mut end = 0usize;
                let mut len = v.get_max_byte_len() as usize;
                while let Some(s_) = v.match_expr(&s[end..len], env, source_manager, diagnostic) {
                    end += s_.len();
                    len = (end + len).min(s.len());
                }

                Some(&s[..end])
            }
            FlattenEbnfExpr::Terminal(t) => {
                if t.len() != s.len() {
                    return None;
                }

                let end = t.len_utf8();

                if t == &s[0..end] {
                    return Some(&s[..end]);
                }

                None
            }
            FlattenEbnfExpr::Statements(_, ..) => panic!("Expected expression, but got statements"),
            FlattenEbnfExpr::Variable { .. } => panic!("Expected expression, but got variable"),
            FlattenEbnfExpr::Range {
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

impl Display for FlattenEbnfExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FlattenEbnfExpr::Statements(exprs, ..) => {
                for expr in exprs {
                    writeln!(f, "{}", expr)?;
                }
                Ok(())
            }
            FlattenEbnfExpr::Identifier(name, ..) => write!(f, "{}", name),
            FlattenEbnfExpr::Variable { name, expr, is_def } => {
                if *is_def {
                    write!(f, "{} ::= {};", name, expr)
                } else {
                    write!(f, "{} = {};", name, expr)
                }
            }
            FlattenEbnfExpr::Terminal(name) => write!(f, "'{}'", name),
            FlattenEbnfExpr::Repetition(expr, ..) => {
                write!(f, "{{ ")?;
                write!(f, "{} ", *expr)?;
                write!(f, "}}")
            }
            FlattenEbnfExpr::Optional(expr, ..) => {
                write!(f, "[ ")?;
                write!(f, "{} ", *expr)?;
                write!(f, "]")
            }
            FlattenEbnfExpr::Alternative(exprs, set, ..) => {
                write!(f, "(")?;
                for t in set.iter() {
                    write!(f, "'{}' | ", t)?
                }
                for expr in exprs.iter() {
                    write!(f, "{} | ", expr)?
                }
                write!(f, ")")
            }
            FlattenEbnfExpr::Concat(exprs, ..) => {
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
            FlattenEbnfExpr::Exception(exprs, ..) => {
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
            FlattenEbnfExpr::Extend(exprs, ..) => {
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
            FlattenEbnfExpr::Range {
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

enum EbnfParserEnvVariable {
    Expr(FlattenEbnfExpr),
    NativeCall(fn(char) -> bool),
}

impl Display for EbnfParserEnvVariable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Expr(e) => write!(f, "{}", e),
            Self::NativeCall(func) => write!(f, "<Native Call={:p}>", func),
        }
    }
}

impl EbnfParserEnvVariable {
    fn substitute_extend(&mut self, old_name: &str, new_name: &str) -> bool {
        match self {
            Self::Expr(expr) => expr.substitute_extend(old_name, new_name),
            _ => false,
        }
    }

    fn is_native_call(&self) -> bool {
        matches!(self, Self::NativeCall(_))
    }

    fn match_expr<'a>(
        &self,
        s: &'a [u8],
        env: &HashMap<String, EbnfParserEnvVariable>,
        source_manager: &SourceManager,
        diagnostic: &mut Diagnostic,
    ) -> Option<&'a [u8]> {
        match self {
            Self::Expr(expr) => expr.match_expr(s, env, source_manager, diagnostic),
            Self::NativeCall(func) => {
                if let Some(c) = ByteToCharIter::new(s).next() {
                    if func(c) {
                        let len = c.len_utf8();
                        Some(&s[..len])
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
        }
    }
}

impl FlattenEbnfExpr {
    fn init_env<'a>(
        self,
        env: &mut HashMap<String, EbnfParserEnvVariable>,
        def: &mut Vec<String>,
        diagnostic: &mut Diagnostic,
    ) {
        match self {
            FlattenEbnfExpr::Statements(exprs, ..) => {
                for expr in exprs {
                    expr.init_env(env, def, diagnostic)
                }
            }
            FlattenEbnfExpr::Variable { name, expr, is_def } => {
                let mut new_expr = EbnfParserEnvVariable::Expr(*expr);
                if is_def {
                    if !def.contains(&name) {
                        def.push(name.clone());
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

    fn get_unique_name(name: &str, env: &HashMap<String, EbnfParserEnvVariable>) -> String {
        let mut i = 0;
        let mut new_name = name.to_owned();
        while env.contains_key(&new_name) || (new_name == name) {
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
                expr.substitute_extend(old_name, new_name);
                false
            }
            Self::Identifier(name, span) => {
                if name == old_name {
                    *self = Self::Identifier(new_name.to_string(), *span);
                    true
                } else {
                    false
                }
            }
            _ => false,
        }
    }
}

pub(crate) struct EbnfParserMatcher<'a> {
    env: HashMap<String, EbnfParserEnvVariable>,
    def: Vec<String>,
    diagnostic: &'a mut Diagnostic,
}

impl<'a> EbnfParserMatcher<'a> {
    pub(crate) fn new(diagnostic: &'a mut Diagnostic) -> Self {
        Self {
            env: HashMap::new(),
            def: Vec::new(),
            diagnostic,
        }
    }

    fn add_identifier_env(&mut self) {
        self.add_native_call("start_identifier", is_valid_identifier_start_code_point);
        self.add_native_call(
            "cont_identifier",
            is_valid_identifier_continuation_code_point,
        );
        let rep_expr = FlattenEbnfExpr::Identifier("cont_identifier".to_string(), None);
        let expr = FlattenEbnfExpr::Concat(
            vec![
                FlattenEbnfExpr::Identifier("start_identifier".to_string(), None),
                FlattenEbnfExpr::Repetition(Box::new(rep_expr), 1),
            ],
            1,
        );

        let statement = FlattenEbnfExpr::Variable {
            name: "identifier".to_string(),
            expr: Box::new(expr),
            is_def: true,
        };
        let identifier = FlattenEbnfExpr::Statements(vec![statement], 1);
        identifier.init_env(&mut self.env, &mut self.def, self.diagnostic);
    }

    fn add_is_digit(&mut self) {
        self.add_native_call("digit", |c| c.is_digit(10));
    }

    fn add_is_hex_digit(&mut self) {
        self.add_native_call("hex_digit", |c| c.is_digit(16));
    }

    fn add_is_octal_digit(&mut self) {
        self.add_native_call("octal_digit", |c| c.is_digit(8));
    }

    fn add_is_binary_digit(&mut self) {
        self.add_native_call("binary_digit", |c| c.is_digit(2));
    }

    fn add_is_alpha(&mut self) {
        self.add_native_call("alpha", |c| c.is_alphabetic());
    }

    fn add_is_alphanumeric(&mut self) {
        self.add_native_call("alphanumeric", |c| c.is_alphanumeric());
    }

    fn add_is_whitespace(&mut self) {
        self.add_native_call("whitespace", |c| c.is_whitespace());
    }

    fn add_is_newline(&mut self) {
        self.add_native_call("newline", |c| c == '\n');
    }

    fn add_is_tab(&mut self) {
        self.add_native_call("tab", |c| c == '\t');
    }

    pub(crate) fn init(&mut self, expr: FlattenEbnfExpr) {
        self.add_identifier_env();
        self.add_is_digit();
        self.add_is_hex_digit();
        self.add_is_octal_digit();
        self.add_is_binary_digit();
        self.add_is_alpha();
        self.add_is_alphanumeric();
        self.add_is_whitespace();
        self.add_is_newline();
        self.add_is_tab();

        expr.init_env(&mut self.env, &mut self.def, self.diagnostic);
        for (name, value) in self.env.iter() {
            println!("{}: {}", name, value);
        }
        println!(
            "\ndef: {}",
            self.def
                .iter()
                .map(|s| s.clone())
                .collect::<Vec<_>>()
                .join(",")
        );
    }

    pub(crate) fn add_native_call(&mut self, name: &str, func: fn(char) -> bool) {
        self.env
            .insert(name.to_string(), EbnfParserEnvVariable::NativeCall(func));
    }

    pub(crate) fn match_native_identifier(s: &[u8]) -> Option<&[u8]> {
        if s.is_empty() {
            return None;
        }

        let (first_char, _) = byte_to_char(s);
        if first_char.is_none() {
            return None;
        }

        if !is_valid_identifier_start_code_point(first_char.unwrap()) {
            return None;
        }

        let mut i = 0;
        for c in ByteToCharIter::new(s) {
            if !is_valid_identifier_continuation_code_point(c) {
                return Some(&s[i..]);
            }
            i += c.len_utf8();
        }
        None
    }

    // pub(crate) fn match_native_number(s: &[u8]) -> Option<&[u8]> {
    //     let num = self.match_native_integer(s);

    //     if num.is_some() {
    //         return num;
    //     }

    //     self.match_native_floating_point(s)
    // }

    fn match_expr_helper<'b>(
        env: &HashMap<String, EbnfParserEnvVariable>,
        key: &str,
        expr: &EbnfParserEnvVariable,
        s: &'b [u8],
        source_manager: &SourceManager,
        diagnostic: &mut Diagnostic,
    ) -> Option<(&'b [u8], String)> {
        match key {
            "identifier" => {
                let temp = if expr.is_native_call() {
                    Self::match_native_identifier(s).map(|s| (s, key.to_owned()))
                } else {
                    expr.match_expr(s, env, source_manager, diagnostic)
                        .map(|s| (s, key.to_owned()))
                };
                temp
            }
            _ => expr
                .match_expr(s, env, source_manager, diagnostic)
                .map(|s| (s, key.to_owned())),
        }
    }

    pub(crate) fn match_expr<'b>(
        &mut self,
        s: &'b [u8],
        source_manager: &SourceManager,
    ) -> Option<(&'b [u8], String)> {
        for d in self.def.iter() {
            if let Some(expr) = self.env.get(d) {
                let temp =
                    Self::match_expr_helper(&self.env, d, expr, s, source_manager, self.diagnostic);
                if temp.is_some() {
                    return temp;
                }
            }
        }
        None
    }

    pub(crate) fn match_expr_for<'b>(&mut self, var: &str, s: &'b [u8], source_manager: &SourceManager) -> Option<&'b [u8]> {
        if let Some(expr) = self.env.get(var) {
            expr.match_expr(s, &self.env, source_manager, self.diagnostic)
        } else {
            None
        }
    }
}
