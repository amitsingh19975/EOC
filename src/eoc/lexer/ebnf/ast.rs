#![allow(dead_code)]

use std::{
    collections::HashSet,
    slice::Iter
};

use crate::eoc::{
    lexer::{
        str_utils::decode_unicode_escape_sequence,
        token::{Token, TokenKind},
        utils::ParenMatching
    },
    utils::{
        diagnostic::{Diagnostic, DiagnosticLevel},
        source_manager::{SourceManager, SourceManagerDiagnosticInfo},
        span::Span, string::UniqueString,
    },
};

use super::expr::{EbnfExpr, TerminalValue};

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

    pub(crate) fn shift_relative_pos_by(&self, pos: u32) -> Self {
        Self(self.0, self.1 + pos)
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum EbnfParserMode {
    Lexer,
    Parser,
}

pub(crate) struct EbnfParser<'a> {
    source_manager: &'a SourceManager,
    diagnostic: &'a Diagnostic,
    tokens: Vec<Token>,
    mode: EbnfParserMode,
    cursor: usize,
    allow_unbounded: bool,
}

impl<'a> EbnfParser<'a> {
    pub(crate) fn parse(
        tokens: Vec<Token>,
        mode: EbnfParserMode,
        source_manager: &SourceManager,
        diagnostic: &Diagnostic,
        allow_unbounded: bool,
    ) -> EbnfExpr {
        let mut parser = EbnfParser {
            source_manager,
            diagnostic,
            tokens,
            mode,
            cursor: 0,
            allow_unbounded
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
            let span = self.peek().unwrap().span;
            if let Some((expr, _)) = self.parse_statement() {
                let current_span = self.peek().map(|t| Span::new(span.start, t.span.end)).unwrap_or(span);
                let is_unbounded = expr.is_unbounded();
                statements.push(expr);
                if is_unbounded && !self.is_empty() {
                    self.diagnostic
                        .builder()
                        .report(
                            DiagnosticLevel::Error,
                            "Unreachable statement",
                            self.source_manager.get_source_info(current_span),
                            None,
                        )
                        .add_info(
                            "Remove this statement",
                            Some(self.source_manager.fix_span(current_span)),
                        )
                        .commit();
                    break;
                }
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
            TokenKind::Colon => (12, 13),
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
            TokenKind::Identifier => {
                let name = self.get_string_from_token(&token);
                if name == "debug_print" {
                    Some((EbnfExpr::DebugPrint, span))
                } else {
                    Some((
                        EbnfExpr::Identifier(
                            name,
                            Some((
                                self.source_manager.get_source_info(span),
                                self.source_manager.fix_span(span),
                            )),
                        ),
                        span,
                    ))
                }
            },
            TokenKind::Terminal => Some(self.get_terminal_from_token(&token)),
            TokenKind::Dot => Some((EbnfExpr::AnyChar, span)),
            TokenKind::Semicolon => None,
            _ => None
        }
    }

    fn parse_paren_expr(&mut self, open: TokenKind, min_bp: u8, labels: &mut Vec<(UniqueString, Span)>) -> Option<(EbnfExpr, Span)> {
        let close = ParenMatching::get_other_pair(open).unwrap();

        if let Some((expr, e_span)) = self.parse_expr(min_bp, labels) {
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

    fn parse_expr_helper(&mut self, labels: &mut Vec<(UniqueString, Span)>) -> Option<(EbnfExpr, Span)> {
        let token = self.peek();
        if token.is_none() {
            return None;
        }
        let token = token.unwrap().clone();

        match token.kind {
            TokenKind::OpenParen => {
                self.next();
                self.parse_paren_expr(TokenKind::OpenParen, 0, labels)
            }
            TokenKind::OpenBracket => {
                self.next();
                self.parse_paren_expr(TokenKind::OpenBracket, 0, labels)
                    .map(|(expr, span)| (EbnfExpr::Optional(Box::new(expr), 1), span))
            }
            TokenKind::OpenBrace => {
                self.next();
                self.parse_paren_expr(TokenKind::OpenBrace, 0, labels)
                    .map(|(expr, span)| (EbnfExpr::Repetition(Box::new(expr), 1), span))
            }
            TokenKind::Dollar => {
                self.next();
                if !self.allow_unbounded {
                    self.diagnostic
                        .builder()
                        .report(
                            DiagnosticLevel::Error,
                            "Unexpected '$'; labels are only allowed in parser mode",
                            self.source_manager.get_source_info(token.span),
                            None,
                        )
                        .add_info(
                            "Remove this",
                            Some(self.source_manager.fix_span(token.span)),
                        )
                        .commit();
                    return None;
                }

                let Some((EbnfExpr::Identifier(id, lhs_info), span)) = self.parse_primary() else {
                    self.diagnostic
                        .builder()
                        .report(
                            DiagnosticLevel::Error,
                            "Expected label after '$'",
                            self.source_manager.get_source_info(token.span),
                            None,
                        )
                        .add_info(
                            "Try add label after this",
                            Some(self.source_manager.fix_span(token.span)),
                        )
                        .commit();
                    return None;
                };

                let temp_id = id.clone();
                if let Some((_, last_span)) = labels.iter().find(|(id, _)| id == &temp_id).copied() {
                    self.diagnostic
                        .builder()
                        .report(
                            DiagnosticLevel::Error,
                            "Duplicate label",
                            self.source_manager.get_source_info(span),
                            None,
                        )
                        .add_info(
                            "Rename the label",
                            Some(self.source_manager.fix_span(span)),
                        )
                        .commit();

                    self.diagnostic
                        .builder()
                        .report(
                            DiagnosticLevel::Error,
                            "Previous label",
                            self.source_manager.get_source_info(last_span),
                            None,
                        )
                        .add_info(
                            "Previous label here",
                            Some(self.source_manager.fix_span(last_span)),
                        )
                        .commit();
                } else {
                    labels.push((UniqueString::new(temp_id), span));
                }

                if self.peek_kind() != Some(TokenKind::Colon) {
                    return Some(
                        (
                            EbnfExpr::LabelledExpr {
                                label: id.clone(),
                                expr: Box::new(EbnfExpr::Identifier(id, lhs_info)),
                            },
                            span,
                        ),
                    );
                }

                self.next();

                let Some((rhs, r_span)) = self.parse_expr(0, labels) else  {
                    self.diagnostic
                        .builder()
                        .report(
                            DiagnosticLevel::Error,
                            "Expected expression after label",
                            self.source_manager.get_source_info(span),
                            None,
                        )
                        .add_info(
                            "Try add expression after this",
                            Some(self.source_manager.fix_span(span)),
                        )
                        .commit();
                    return None;
                };

                Some((
                    EbnfExpr::LabelledExpr { label: id, expr: Box::new(rhs) },
                    r_span,
                ))
            }
            _ => self.parse_primary(),
        }
    }

    fn parse_expr(&mut self, min_bp: u8, labels: &mut Vec<(UniqueString, Span)>) -> Option<(EbnfExpr, Span)> {
        let mut lhs = self.parse_expr_helper(labels);

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

            if let Some((rhs, rhs_span)) = self.parse_expr(right_bp, labels) {
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
        let mut labels = Vec::new();
        let lhs_expr = self.parse_expr(0, &mut labels);
        if lhs_expr.is_none() {
            return None;
        }
        let (mut lhs_expr, mut span) = lhs_expr.unwrap();

        if lhs_expr.is_debug_print() {
            return Some((lhs_expr, span));
        }

        let equal_token = self.next();

        if equal_token.is_none() {
            return None;
        }

        let equal_token = equal_token.unwrap();
        let eq_span = equal_token.span;
        let is_equal = equal_token.kind == TokenKind::Equal;
        let is_def = equal_token.kind == TokenKind::Definition;

        if !(self.allow_unbounded && self.mode == EbnfParserMode::Parser) {
            if !lhs_expr.is_identifier() {
                self.diagnostic
                    .builder()
                    .report(
                        DiagnosticLevel::Error,
                        "Expected identifier on the left side of expression",
                        self.source_manager.get_source_info(span),
                        None,
                    )
                    .add_info(
                        "Try to remove expression",
                        Some(self.source_manager.fix_span(span)),
                    )
                    .commit();
                return None;
            }
        }

        if !(is_equal || is_def) {
            if self.mode == EbnfParserMode::Parser && self.allow_unbounded {
                return Some((EbnfExpr::UnboundedExpr(Box::new(lhs_expr)), span));
            }
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

        let rhs = self.parse_expr(0, &mut labels);
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

pub(super) struct EbnfParserMatcherDef(HashSet<&'static str>, Vec<UniqueString>);

impl EbnfParserMatcherDef {
    pub(super) fn new() -> Self {
        Self(HashSet::new(), Vec::new())
    }

    pub(super) fn contains(&self, name: &str) -> bool {
        self.0.contains(name)
    }

    pub(super) fn ordered_iter(&self) -> Iter<'_, UniqueString> {
        self.1.iter()
    }

    pub(super) fn insert(&mut self, name: String) {
        let s = UniqueString::new(name);
        self.0.insert(s.as_str());
        self.1.push(s);
    }

    pub(super) fn keys(&self) -> std::collections::hash_set::Iter<'_, &'static str> {
        self.0.iter()
    }
}

