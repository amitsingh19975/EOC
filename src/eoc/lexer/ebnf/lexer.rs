#![allow(dead_code)]

use crate::eoc::{
    lexer::token::{Token, TokenKind},
    utils::{
        diagnostic::{Diagnostic, DiagnosticLevel, DiagnosticReporter},
        source_manager::SourceManager,
        span::Span,
    },
};

/* 
================= EBNF ======================

| Usage         | Notation          |
|---------------|-------------------|
| definition    | =                 |
| termination   | ;                 |
| alternation   | |                 |
| optional      | [ ]               |
| repetition    | { }               |
| grouping      | ( )               |
| exception     | -                 |

=============================================
*/

pub(crate) struct EbnfLexer<'a> {
    source_manager: &'a SourceManager,
    diagnostics: &'a mut Diagnostic,
    end: usize,
    cursor: usize,
}

impl<'a> EbnfLexer<'a> {
    pub(crate) fn new(
        source_manager: &'a SourceManager,
        diagnostics: &'a mut Diagnostic,
        cursor: usize,
        end: usize,
    ) -> Self {
        EbnfLexer {
            source_manager,
            diagnostics,
            cursor,
            end,
        }
    }

    fn next_char(&mut self) -> Option<char> {
        let (ch, len) = self.source_manager.get_char(self.cursor);
        self.cursor = (self.cursor + len).min(self.source_manager.len());
        ch
    }

    fn peek_char(&self) -> Option<char> {
        self.source_manager.get_char(self.cursor).0
    }

    fn skip_while(&mut self, predicate: impl Fn(char) -> bool) -> Span {
        let start = self.cursor;
        while let Some(ch) = self.peek_char() {
            if !predicate(ch) {
                break;
            }
            self.next_char();
        }
        Span::from_usize(start, self.cursor)
    }

    fn skip_whitespace(&mut self) -> Option<Token> {
        let span = self.skip_while(|ch| ch.is_whitespace());
        if span.len() > 0 {
            Some(Token::new(TokenKind::Whitespace, span))
        } else {
            None
        }
    }

    fn lex_identifier(&mut self, tokens: &mut Vec<Token>) {
        let start = self.cursor;
        self.skip_while(|ch| ch.is_alphanumeric() || ch == '_');
        let span = Span::from_usize(start, self.cursor);

        let token = Token::new(TokenKind::Identifier, span);
        tokens.push(token);
    }

    fn lex_terminal(&mut self, tokens: &mut Vec<Token>) {
        let start_span = Span::from_usize(self.cursor, self.cursor + 1);
        let start_quote = self.next_char().unwrap();
        let start = self.cursor;
        let end_quote = match start_quote {
            '\'' => '\'',
            '"' => '"',
            _ => unreachable!(),
        };


        let span = self.skip_while(|ch| ch != end_quote);
        
        if self.peek_char() == Some(end_quote) {
            let span = Span::from_usize(start, self.cursor);
            self.next_char();
            let token = Token::new(TokenKind::Terminal, span);
            tokens.push(token);
        } else {
            let info = self.source_manager.get_source_info(span);
            let current_cursor = self.cursor;
            self.cursor = (start_span.start + 1) as usize;

            let mut quote_span = self.skip_while(|ch| ch != ';');
            self.cursor = current_cursor;
            quote_span.start = start_span.start + 1;

            self.diagnostics
                .builder()
                .report(
                    DiagnosticLevel::Error,
                    "Unterminated string literal",
                    info,
                    None,
                )
                .add_error(
                    format!("Add closing '{}' quote", end_quote),
                    Some(
                        self.source_manager
                            .fix_span(start_span),
                    ),
                )
                .add_info(
                    "String literal started here".to_string(),
                    Some(
                        self.source_manager
                            .fix_span(quote_span),
                    ),
                )
                .commit();
        }
    }

    pub(crate) fn lex(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        loop {
            if self.cursor >= self.end {
                break;
            }

            let c = self.peek_char();
            if c.is_none() {
                break;
            }
            let c = c.unwrap();

            if let Some(_) = self.skip_whitespace() {
                continue;
            }

            match c {
                _ if c.is_alphabetic() => self.lex_identifier(&mut tokens),
                '\'' | '"' => self.lex_terminal(&mut tokens),
                '?' => {
                    let span = Span::from_usize(self.cursor, self.cursor + 1);
                    tokens.push(Token::new(TokenKind::QuestionMark, span));
                    self.next_char();
                }
                ',' => {
                    let span = Span::from_usize(self.cursor, self.cursor + 1);
                    tokens.push(Token::new(TokenKind::Comma, span));
                    self.next_char();
                }
                ';' => {
                    let span = Span::from_usize(self.cursor, self.cursor + 1);
                    tokens.push(Token::new(TokenKind::Semicolon, span));
                    self.next_char();
                }
                '|' => {
                    let span = Span::from_usize(self.cursor, self.cursor + 1);
                    tokens.push(Token::new(TokenKind::Pipe, span));
                    self.next_char();
                }
                '[' => {
                    let span = Span::from_usize(self.cursor, self.cursor + 1);
                    tokens.push(Token::new(TokenKind::OpenBracket, span));
                    self.next_char();
                }
                ']' => {
                    let span = Span::from_usize(self.cursor, self.cursor + 1);
                    tokens.push(Token::new(TokenKind::CloseBracket, span));
                    self.next_char();
                }
                '{' => {
                    let span = Span::from_usize(self.cursor, self.cursor + 1);
                    tokens.push(Token::new(TokenKind::OpenBrace, span));
                    self.next_char();
                }
                '}' => {
                    let span = Span::from_usize(self.cursor, self.cursor + 1);
                    tokens.push(Token::new(TokenKind::CloseBrace, span));
                    self.next_char();
                }
                '(' => {
                    let span = Span::from_usize(self.cursor, self.cursor + 1);
                    tokens.push(Token::new(TokenKind::OpenParen, span));
                    self.next_char();
                }
                ')' => {
                    let span = Span::from_usize(self.cursor, self.cursor + 1);
                    tokens.push(Token::new(TokenKind::CloseParen, span));
                    self.next_char();
                }
                '-' => {
                    let span = Span::from_usize(self.cursor, self.cursor + 1);
                    tokens.push(Token::new(TokenKind::Exception, span));
                    self.next_char();
                }
                '+' => {
                    let span = Span::from_usize(self.cursor, self.cursor + 1);
                    tokens.push(Token::new(TokenKind::Plus, span));
                    self.next_char();
                }
                '.' => {
                    let start = self.cursor;
                    self.next_char();
                    if self.peek_char() == Some('.') {
                        self.next_char();
                        if self.peek_char() == Some('=') {
                            let span = Span::from_usize(start, self.cursor + 1);
                            tokens.push(Token::new(TokenKind::RangeEqual, span));
                            self.next_char();
                        } else {
                            let span = Span::from_usize(start, self.cursor);
                            tokens.push(Token::new(TokenKind::Range, span));
                        }
                    } else {
                        let span = Span::from_usize(start, self.cursor);
                        let info = self.source_manager.get_source_info(span);
                        self.diagnostics
                            .builder()
                            .report(DiagnosticLevel::Error, "Unexpected character", info, None)
                            .add_error(
                                "Expected '..'".to_string(),
                                Some(self.source_manager.fix_span(span)),
                            )
                            .commit();
                    }
                }
                '/' => {
                    self.peek_char();
                    if self.peek_char() == Some('/') {
                        self.skip_while(|ch| ch != '\n');
                    } else {
                        let span = Span::from_usize(self.cursor, self.cursor + 1);
                        let info = self.source_manager.get_source_info(span);
                        self.diagnostics
                            .builder()
                            .report(DiagnosticLevel::Error, "Unexpected character", info, None)
                            .add_error(
                                "Expected comment".to_string(),
                                Some(self.source_manager.fix_span(span)),
                            )
                            .commit();
                    }
                }
                ':' => {
                    self.next_char();
                    if self.peek_char() == Some(':') {
                        self.next_char();
                        if self.peek_char() == Some('=') {
                            let span = Span::from_usize(self.cursor - 2, self.cursor + 1);
                            tokens.push(Token::new(TokenKind::Definition, span));
                            self.next_char();
                        } else {
                            let span = Span::from_usize(self.cursor - 2, self.cursor);
                            let info = self.source_manager.get_source_info(span);
                            self.diagnostics
                                .builder()
                                .report(DiagnosticLevel::Error, "Unexpected character", info, None)
                                .add_error(
                                    "Expected '=' after '::'".to_string(),
                                    Some(self.source_manager.fix_span(span)),
                                )
                                .commit();
                        }
                    } else {
                        self.diagnostics
                            .builder()
                            .report(
                                DiagnosticLevel::Error,
                                "Unexpected character",
                                self.source_manager.get_source_info(Span::from_usize(
                                    self.cursor - 1,
                                    self.cursor,
                                )),
                                None,
                            )
                            .add_error(
                                "Expected '::'".to_string(),
                                Some(self.source_manager.fix_span(Span::from_usize(
                                    self.cursor - 1,
                                    self.cursor,
                                ))),
                            )
                            .commit();
                    }
                    
                }
                '=' => {
                    let span = Span::from_usize(self.cursor, self.cursor + 1);
                    tokens.push(Token::new(TokenKind::Equal, span));
                    self.next_char();
                }
                _ => {
                    let span = Span::from_usize(self.cursor, self.cursor + 1);
                    let info = self.source_manager.get_source_info(span);
                    self.diagnostics
                        .builder()
                        .report(DiagnosticLevel::Error, "Unexpected character", info, None)
                        .add_error(
                            format!("Unexpected character '{}'", c),
                            Some(self.source_manager.fix_span(span)),
                        )
                        .commit();
                    self.next_char();
                }
            }
        }
        tokens
    }
}
