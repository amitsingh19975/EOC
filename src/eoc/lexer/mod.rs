#![allow(dead_code)]

use crate::eoc::lexer::ebnf::ast::EbnfParser;

use self::{
    ebnf::{
        ast::EbnfParserMode,
        basic::LexerEbnfMatcher,
        ir_matcher::IRLexerEbnfParserMatcher,
        lexer::EbnfLexer,
        native_call::NATIVE_CALL_KIND_ID,
        vm::LexerEbnfParserMatcher,
    }, token::{Token, TokenKind}, utils::{
        is_valid_identifier_continuation_code_point, is_valid_identifier_start_code_point,
        CustomOperator, ParenMatching, ParenStack,
    }
};
use super::{
    ast::identifier::Identifier,
    parser::ebnf::parser_matcher::ParserEbnfParserMatcher,
    utils::{
        diagnostic::{Diagnostic, DiagnosticLevel},
        imm_ref::Ref,
        source_manager::{RelativeSourceManager, SourceManager},
        span::Span,
        string::UniqueString,
        trie::Trie,
    },
};
use std::{collections::HashMap, path::Path, vec};
pub(crate) mod ebnf;
pub(crate) mod number;
pub(crate) mod str_utils;
pub(crate) mod token;
pub(crate) mod utils;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum LexerMode {
    Normal,
    IR,
}

impl LexerMode {
    pub(crate) fn is_ir(&self) -> bool {
        matches!(self, LexerMode::IR)
    }

    pub(crate) fn is_normal(&self) -> bool {
        matches!(self, LexerMode::Normal)
    }
}

pub(crate) struct Lexer {
    source_manager: SourceManager,
    cursor: usize,
    paren_balance: ParenStack,
    diagnostics: Diagnostic,
    custom_operators: Vec<CustomOperator>,
    custom_keywords: Vec<Identifier>,
    custom_operators_trie: Trie<u8, usize>,
    custom_keywords_trie: Trie<u8, usize>,
    rewind_stack: Vec<usize>,
    block_lexer_matcher: HashMap<UniqueString, LexerEbnfParserMatcher>,
    block_parser_matcher: HashMap<UniqueString, ParserEbnfParserMatcher>,
    global_parser_matcher: Ref<ParserEbnfParserMatcher>,
    mode: LexerMode,
    shebang_span: Span,
}

impl Lexer {
    pub(crate) fn new<D: Into<Diagnostic>>(source_manager: SourceManager, diagnostic: D) -> Self {
        NATIVE_CALL_KIND_ID.init();

        Self {
            source_manager,
            cursor: 0,
            paren_balance: Default::default(),
            diagnostics: diagnostic.into(),
            custom_operators: Default::default(),
            custom_keywords: Default::default(),
            rewind_stack: Default::default(),
            custom_operators_trie: Default::default(),
            custom_keywords_trie: Default::default(),
            block_lexer_matcher: Default::default(),
            mode: LexerMode::Normal,
            shebang_span: Default::default(),
            block_parser_matcher: Default::default(),
            global_parser_matcher: ParserEbnfParserMatcher::default().into(),
        }
    }

    // pub(crate) fn set_global_lexer_matcher(&mut self, matcher: Arc<RwLock<EbnfParserMatcher>>) {
    //     matcher.read()
    //     self.local_lexer_matcher.merge(matcher);
    //     // self.global_lexer_matcher = Some(matcher);
    // }

    pub(crate) fn new_from_filepath<P: AsRef<Path>, D: Into<Diagnostic>>(
        path: P,
        diagnostic: D,
    ) -> Result<Self, std::io::Error> {
        let source_manager = SourceManager::new(path)?;
        Ok(Self::new(source_manager, diagnostic))
    }

    pub(crate) fn get_source_manager(&self) -> &SourceManager {
        &self.source_manager
    }

    pub(crate) fn get_diagnostics(&self) -> &Diagnostic {
        &self.diagnostics
    }

    pub(crate) fn get_shebang_span(&self) -> Span {
        self.shebang_span
    }

    fn save_cursor(&mut self) {
        self.rewind_stack.push(self.cursor);
    }

    // fn pop_cursor(&mut self) {
    //     self.rewind_stack.pop();
    // }

    fn rewind_cursor(&mut self) {
        if let Some(cursor) = self.rewind_stack.pop() {
            self.cursor = cursor;
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
        let Some(ch) = self.peek_char() else {
            return None;
        };

        let temp = match ch {
            '\n' | '\r' => {
                let span = self.skip_while(|byte| (byte == '\n') || (byte == '\r'));
                Token::new(TokenKind::Newline, span)
            }

            ' ' => {
                let span = self.skip_while(|byte| (byte == ' '));
                Token::new(TokenKind::Space, span)
            }
            '\t' => {
                let span = self.skip_while(|byte| (byte == '\t'));
                Token::new(TokenKind::TabSpace, span)
            }
            _ => {
                let span = self.skip_while(|byte| byte.is_ascii_whitespace());
                Token::new(TokenKind::Whitespace, span)
            }
        };
        if temp.span.is_empty() {
            None
        } else {
            Some(temp)
        }
    }

    pub(crate) fn get_custom_operators(&self) -> &Vec<CustomOperator> {
        &self.custom_operators
    }

    pub(crate) fn get_custom_keywords(&self) -> &Vec<Identifier> {
        &self.custom_keywords
    }

    pub(crate) fn is_ir_mode(&mut self, tokens: &mut Vec<Token>) -> bool {
        // [fn].ir
        self.save_cursor();

        let w1 = self.skip_whitespace();

        let dot_span = Span::from_usize(self.cursor, self.cursor + 1);
        if self.peek_char() == Some('.') {
            self.next_char();
            let w2 = self.skip_whitespace();
            let ir_span = Span::from_usize(self.cursor, self.cursor + 2);
            if self.peek_char() == Some('i') {
                self.next_char();
                if self.peek_char() == Some('r') {
                    self.next_char();
                    w1.map(|t| tokens.push(t));
                    w2.map(|t| tokens.push(t));

                    tokens.push(Token::new(TokenKind::Dot, dot_span));
                    tokens.push(Token::new(TokenKind::Identifier, ir_span));
                    return true;
                }
            }
        }

        self.rewind_cursor();
        false
    }

    fn lex_identifier_helper(
        &mut self,
        matcher: &LexerEbnfParserMatcher,
        span: Span,
        kind: TokenKind,
        tokens: &mut Vec<Token>,
        should_parse_nested_operator: bool,
    ) {
        if !should_parse_nested_operator {
            return;
        }

        if kind == TokenKind::KwOperator {
            let new_tokens = self.lex_custom_operator(matcher, span);
            tokens.extend(new_tokens);
        } else if kind == TokenKind::KwKeyword {
            let new_tokens = self.lex_custom_keyword(span);
            tokens.extend(new_tokens);
        } else if kind == TokenKind::Fn {
            self.mode = if self.is_ir_mode(tokens) {
                LexerMode::IR
            } else {
                LexerMode::Normal
            };
        }
    }

    fn lex_identifier(
        &mut self,
        matcher: &LexerEbnfParserMatcher,
        tokens: &mut Vec<Token>,
        should_parse_nested_operator: bool,
    ) {
        if let Some((slice, _)) = matcher.match_native(
            ebnf::native_call::LexerNativeCallKind::Identifier,
            &self.source_manager.get_source()[self.cursor..],
            RelativeSourceManager::new(&self.source_manager, self.cursor as u32),
            &self.diagnostics,
            None,
        ) {
            let end = self.cursor + slice.len();
            let span = Span::from_usize(self.cursor, end);
            tokens.push(Token::new(TokenKind::Identifier, span));
            self.cursor = end;
            let kind: TokenKind = slice.into();
            self.lex_identifier_helper(matcher, span, kind, tokens, should_parse_nested_operator);
        }
    }

    fn lex_operator(&mut self, matcher: &LexerEbnfParserMatcher, tokens: &mut Vec<Token>) {
        if let Some((slice, _)) = matcher.match_native(
            ebnf::native_call::LexerNativeCallKind::Operator,
            &self.source_manager.get_source()[self.cursor..],
            RelativeSourceManager::new(&self.source_manager, self.cursor as u32),
            &mut self.diagnostics,
            None,
        ) {
            let end = self.cursor + slice.len();
            let span = Span::from_usize(self.cursor, end);
            tokens.push(Token::new(TokenKind::Operator, span));
            self.cursor = end;
        }
    }

    ///   integer_literal  ::= [0-9][0-9_]*
    ///   integer_literal  ::= 0x[0-9a-fA-F][0-9a-fA-F_]*
    ///   integer_literal  ::= 0o[0-7][0-7_]*
    ///   integer_literal  ::= 0b[01][01_]*
    ///   floating_literal ::= [0-9][0-9]_*\.[0-9][0-9_]*
    ///   floating_literal ::= [0-9][0-9]*\.[0-9][0-9_]*[eE][+-]?[0-9][0-9_]*
    ///   floating_literal ::= [0-9][0-9_]*[eE][+-]?[0-9][0-9_]*
    ///   floating_literal ::= 0x[0-9A-Fa-f][0-9A-Fa-f_]*
    ///                          (\.[0-9A-Fa-f][0-9A-Fa-f_]*)?[pP][+-]?[0-9][0-9_]*
    fn lex_number(&mut self, matcher: &LexerEbnfParserMatcher, tokens: &mut Vec<Token>) -> bool {
        let temp = matcher.match_native(
            ebnf::native_call::LexerNativeCallKind::Number,
            &self.source_manager.get_source()[self.cursor..],
            RelativeSourceManager::new(&self.source_manager, self.cursor as u32),
            &mut self.diagnostics,
            None,
        );

        if let Some((bytes, k)) = temp {
            let end = self.cursor + bytes.len();
            let span = Span::from_usize(self.cursor, end);
            tokens.push(Token::new(k, span));
            self.cursor = end;
            return true;
        }

        false
    }

    fn lex_formatting_string(&mut self, matcher: &mut LexerEbnfParserMatcher) -> Vec<Token> {
        self.lex_helper(matcher, vec!['}'], false, false)
    }

    fn lex_double_quoted_string(
        &mut self,
        matcher: &mut LexerEbnfParserMatcher,
        tokens: &mut Vec<Token>,
    ) {
        if self.peek_char() != Some('"') {
            return;
        }

        let mut is_escaping = false;
        let mut is_escaping_formatting = false;

        let start_format_token = Token::new(
            TokenKind::StartFormattingString,
            Span::from_usize(self.cursor, self.cursor + 1),
        );

        let start_quote_span = Span::from_usize(self.cursor, self.cursor + 1);

        self.next_char();
        let mut start = self.cursor;
        let mut end = self.cursor;

        let mut found_format_string = false;
        let mut format_tokens = Vec::new();

        loop {
            let Some(ch) = self.peek_char() else {
                let info = self.source_manager.get_source_info(start_quote_span);
                self.diagnostics
                    .builder()
                    .report(DiagnosticLevel::Error, "Unterminated string", info, None)
                    .add_error(
                        "Unterminated string",
                        Some(self.source_manager.fix_span(start_quote_span)),
                    )
                    .commit();
                break;
            };

            if ch == '{' {
                let end = self.cursor;
                self.next_char();
                if self.peek_char() == Some('{') {
                    is_escaping_formatting = true;
                    self.next_char();
                    continue;
                }
                found_format_string = true;

                let old_parens = self.paren_balance.clone();
                self.paren_balance.clear();
                self.paren_balance.push(TokenKind::OpenBrace, Span::from_usize(end, end + 1));
                let mut has_error = false;

                {
                    let span = Span::from_usize(start, end);
                    format_tokens.push(Token::new(TokenKind::String, span));
                    let temp_format_tokens = self.lex_formatting_string(matcher);

                    if self.peek_char() != Some('}') {
                        let last_seen_quote = format_tokens
                            .iter()
                            .rev()
                            .find(|token| token.kind == TokenKind::StartFormattingString);
                        if let Some((TokenKind::StartFormattingString, q_span)) =
                            last_seen_quote.map(|token| (token.kind, token.span))
                        {
                            let info = self.source_manager.get_source_info(q_span);
                            self.diagnostics
                                .builder()
                                .report(DiagnosticLevel::Error, "Missing matching '}'", info, None)
                                .add_info(
                                    "Add '}' before this '\"'",
                                    Some(self.source_manager.fix_span(q_span)),
                                )
                                .commit();
                            has_error = true;
                        } else {
                            let info = self
                                .source_manager
                                .get_source_info(Span::from_usize(self.cursor, self.cursor + 1));
                            self.diagnostics
                                .builder()
                                .report(DiagnosticLevel::Error, "Missing matching '}'", info, None)
                                .commit();
                        }
                    } else {
                        self.next_char();
                    }
                    format_tokens.extend(temp_format_tokens);
                    self.expect_block_or_paren(TokenKind::CloseBrace);
                }
                self.paren_balance = old_parens;
                start = self.cursor;
                if has_error {
                    break;
                }
                continue;
            }

            if ch == '}' && self.peek_char() == Some('}') && is_escaping_formatting {
                is_escaping_formatting = false;
                continue;
            }

            if ch == '"' && !is_escaping {
                end = self.cursor;
                break;
            }

            if ch == '\\' && !is_escaping {
                is_escaping = true;
                self.next_char();
                continue;
            }

            if is_escaping {
                match ch {
                    'n' | 'r' | 't' | '0' | '\\' | '\'' | '"' | 'x' | 'u' => {
                        is_escaping = false;
                    }
                    _ => {
                        let info = self
                            .source_manager
                            .get_source_info(Span::from_usize(self.cursor, self.cursor + 1));
                        self.diagnostics
                            .builder()
                            .report(
                                DiagnosticLevel::Error,
                                "Invalid escape sequence",
                                info,
                                None,
                            )
                            .add_error(
                                "Invalid escape sequence",
                                Some(
                                    self.source_manager
                                        .fix_span(Span::from_usize(self.cursor, self.cursor + 1)),
                                ),
                            )
                            .commit();
                    }
                }
            }

            self.next_char();
        }

        if found_format_string {
            tokens.push(start_format_token);
        }

        format_tokens = format_tokens
            .into_iter()
            .filter(|t| !t.span.is_empty())
            .collect();
        tokens.extend(format_tokens);

        let span = Span::from_usize(start, end);
        if start_quote_span == span || self.peek_char() != Some('"') {
            return;
        }

        self.next_char();

        if span.is_empty() {
            return;
        }

        if !found_format_string {
            tokens.push(Token::new(TokenKind::String, span));
            return;
        }

        tokens.push(Token::new(TokenKind::String, span));
        tokens.push(Token::new(
            TokenKind::EndFormattingString,
            Span::from_usize(self.cursor, self.cursor + 1),
        ));
    }

    fn lex_character_literal(&mut self, tokens: &mut Vec<Token>) {
        if self.peek_char() != Some('\'') {
            return;
        }

        let mut is_escaping = false;

        self.next_char();
        let start = self.cursor;
        let mut end = self.cursor;

        loop {
            let Some(ch) = self.peek_char() else {
                let info = self
                    .source_manager
                    .get_source_info(Span::from_usize(start, end));
                self.diagnostics
                    .builder()
                    .report(
                        DiagnosticLevel::Error,
                        "Unterminated character literal",
                        info,
                        None,
                    )
                    .add_error(
                        "Unterminated character literal",
                        Some(self.source_manager.fix_span(Span::from_usize(start, end))),
                    )
                    .commit();
                break;
            };

            if ch == '\'' && !is_escaping {
                end = self.cursor;
                self.next_char();
                break;
            }

            if ch == '\\' && !is_escaping {
                is_escaping = true;
                self.next_char();
                continue;
            }

            if is_escaping {
                match ch {
                    'n' | 'r' | 't' | '0' | '\\' | '\'' | '"' | 'x' | 'u' => {
                        is_escaping = false;
                    }
                    _ => {
                        let info = self
                            .source_manager
                            .get_source_info(Span::from_usize(self.cursor, self.cursor + 1));
                        self.diagnostics
                            .builder()
                            .report(
                                DiagnosticLevel::Error,
                                "Invalid escape sequence",
                                info,
                                None,
                            )
                            .add_error(
                                "Invalid escape sequence",
                                Some(
                                    self.source_manager
                                        .fix_span(Span::from_usize(self.cursor, self.cursor + 1)),
                                ),
                            )
                            .commit();
                    }
                }
            }

            self.next_char();
        }

        let span = Span::from_usize(start, end);
        tokens.push(Token::new(TokenKind::Char, span));
    }

    fn lex_single_line_comment(&mut self, tokens: &mut Vec<Token>) {
        let mut kind = TokenKind::SingleLineComment;

        if Some('/') == self.peek_char() {
            self.next_char();
            kind = TokenKind::DocComment;
        }

        let start = self.cursor;
        self.skip_while(|c| c != '\n');
        let span = Span::from_usize(start, self.cursor);

        if Some('\n') == self.peek_char() {
            self.next_char();
        }

        tokens.push(Token::new(kind, span));
    }

    fn lex_multiline_comment(&mut self, tokens: &mut Vec<Token>) {
        let mut nest_level = 1;
        let mut star_count = 1;

        while self.peek_char() == Some('*') {
            star_count += 1;
            self.next_char();
        }

        let start = self.cursor;
        let mut end = self.cursor;
        loop {
            if nest_level == 0 {
                break;
            }

            let Some(ch) = self.peek_char() else {
                let info = self
                    .source_manager
                    .get_source_info(Span::from_usize(self.cursor, self.cursor + 1));
                self.diagnostics
                    .builder()
                    .report(
                        DiagnosticLevel::Error,
                        "Unterminated multi-line comment",
                        info,
                        None,
                    )
                    .add_error(
                        "Unterminated multi-line comment",
                        Some(
                            self.source_manager
                                .fix_span(Span::from_usize(self.cursor, self.cursor + 1)),
                        ),
                    )
                    .commit();
                break;
            };

            if ch == '/' {
                self.next_char();
                let mut temp_star = star_count;
                while self.peek_char() == Some('*') {
                    temp_star -= 1;
                    self.next_char();
                }

                if self.peek_char() == Some('*') && temp_star == 0 {
                    nest_level += 1;
                    self.next_char();
                }
            }

            if ch == '*' {
                end = self.cursor;
                let mut temp_star = star_count;
                while self.peek_char() == Some('*') {
                    temp_star -= 1;
                    self.next_char();
                }

                if self.peek_char() == Some('/') && temp_star == 0 {
                    nest_level -= 1;
                    self.next_char();
                    continue;
                }
            }

            self.next_char();
        }

        let span = Span::from_usize(start, end);
        tokens.push(Token::new(TokenKind::MultiLineComment, span));
    }

    fn lex_comment(&mut self, tokens: &mut Vec<Token>) {
        if self.peek_char() != Some('/') {
            return;
        }

        self.next_char();
        let Some(ch) = self.peek_char() else {
            return;
        };

        if ch == '/' {
            self.next_char();
            self.lex_single_line_comment(tokens);
        } else if ch == '*' {
            self.next_char();
            self.lex_multiline_comment(tokens);
        }
    }

    fn is_valid_comment(&self, ch: char) -> bool {
        if ch != '/' {
            return false;
        }

        let Some(next) = self.peek_char() else {
            return false;
        };

        next == '/' || next == '*'
    }

    fn parse_ebnf_lexer_block(
        &mut self,
        matcher: &mut LexerEbnfParserMatcher,
        name_token: Option<Token>,
        span: Span,
    ) {
        let mut enbf_lexer = EbnfLexer::new(
            &self.source_manager,
            &mut self.diagnostics,
            span.start as usize,
            span.end as usize,
        );
        let tokens = enbf_lexer.lex();
        let program = EbnfParser::parse(
            tokens,
            EbnfParserMode::Lexer,
            &self.source_manager,
            &mut self.diagnostics,
            true,
        );
        if let Some(name_token) = name_token {
            let slice = &self.source_manager[name_token.span];
            let name = unsafe { std::str::from_utf8_unchecked(slice) };
            let mut matcher = LexerEbnfParserMatcher::default();
            matcher.from_expr(program, &self.diagnostics);
            let name = UniqueString::new(name);
            if self.block_lexer_matcher.contains_key(&name) {
                self.diagnostics
                    .builder()
                    .report(
                        DiagnosticLevel::Error,
                        "Duplicate lexer block name",
                        self.source_manager.get_source_info(name_token.span),
                        None,
                    )
                    .add_error(
                        "Change the name of the lexer block",
                        Some(self.source_manager.fix_span(name_token.span)),
                    )
                    .commit();
            }
            self.block_lexer_matcher.insert(name, matcher);
        } else {
            matcher.from_expr(program, &self.diagnostics);
        }
    }

    fn try_lex_using_custom_matcher(
        &mut self,
        tokens: &mut Vec<Token>,
        name: UniqueString,
    ) -> bool {
        if !self.block_lexer_matcher.contains_key(&name) {
            let code_span = self.skip_while_code_block_end(self.cursor);
            self.cursor = code_span.end as usize + 3;
            tokens.push(Token::new(TokenKind::CustomCodeBlock(name), code_span));
            return false;
        }

        let code_start = self.cursor;

        let code_span = Span::from_usize(code_start, code_start);
        let mut code_end_span = code_span;
        let mut matcher = self.block_lexer_matcher.remove(&name).unwrap();
        let temp_tokens = self.lex_helper(&mut matcher, vec![], true, true);

        if !ParenMatching::is_triple_back_tick_block(
            &self.source_manager.get_source(),
            self.cursor,
            b"```",
        ) {
            self.save_cursor();
            let mut found = false;
            while let Some(ch) = self.peek_char() {
                if ch == '`' {
                    if ParenMatching::is_triple_back_tick_block(
                        &self.source_manager.get_source(),
                        self.cursor,
                        b"```",
                    ) {
                        found = true;
                        break;
                    }
                } else if ch == '\\' {
                    self.next_char();
                }
                self.next_char();
            }

            if found {
                code_end_span.start = self.cursor as u32;
                code_end_span.end = self.cursor as u32 + 3;
                self.cursor += 3;

                self.diagnostics
                    .builder()
                    .report(
                        DiagnosticLevel::Error,
                        "Unable to lex the code block",
                        self.source_manager.get_source_info(code_span),
                        None,
                    )
                    .commit();
            } else {
                self.rewind_cursor();
                self.diagnostics
                    .builder()
                    .report(
                        DiagnosticLevel::Error,
                        "Unterminated lexer block",
                        self.source_manager.get_source_info(code_span),
                        None,
                    )
                    .add_error(
                        "Try add '```' to close the code block",
                        Some(self.source_manager.fix_span(code_span)),
                    )
                    .commit();
            }
        } else {
            code_end_span.start = self.cursor as u32;
            self.cursor += 3;
            code_end_span.end = self.cursor as u32;
        }

        tokens.push(Token::new(
            TokenKind::CustomCodeBlockStart(name),
            Span::from_usize(code_start - name.as_ref().as_bytes().len() - 3, code_start),
        ));
        tokens.extend(temp_tokens);
        tokens.push(Token::new(
            TokenKind::CustomCodeBlockEnd(name),
            code_end_span,
        ));
        self.block_lexer_matcher.insert(name, matcher);
        true
    }

    fn skip_while_code_block_end(&mut self, start: usize) -> Span {
        self.cursor = start;
        loop {
            let Some(ch) = self.peek_char() else {
                self.diagnostics
                    .builder()
                    .report(
                        DiagnosticLevel::Error,
                        "Unterminated lexer block",
                        self.source_manager
                            .get_source_info(Span::from_usize(self.cursor, self.cursor + 1)),
                        None,
                    )
                    .add_error(
                        "Try add '```' to close the lexer block",
                        Some(
                            self.source_manager
                                .fix_span(Span::from_usize(self.cursor, self.cursor + 1)),
                        ),
                    )
                    .commit();
                break;
            };

            if ch == '`'
                && ParenMatching::is_triple_back_tick_block(
                    &self.source_manager.get_source(),
                    self.cursor,
                    b"```",
                )
            {
                let span = Span::from_usize(start, self.cursor);
                self.cursor += 3;
                return span;
            } else if ch == '\\' {
                self.next_char();
            }

            self.next_char();
        }
        Span::from_usize(start, self.cursor)
    }

    fn lex_code_block_with_name_helper(
        &mut self,
        matcher: &mut LexerEbnfParserMatcher,
        block_type: &'static str,
    ) -> (Option<Token>, Span) {
        let mut start = self.cursor;
        let mut name_token = None;
        if self.peek_char() == Some('(') {
            let temp_start = self.cursor;
            self.next_char();
            let tokens = self.lex_helper(matcher, vec![')'], true, false);
            self.next_char();
            if tokens.len() != 1 {
                let info = self
                    .source_manager
                    .get_source_info(Span::from_usize(temp_start, self.cursor + 1));
                self.diagnostics
                    .builder()
                    .report(
                        DiagnosticLevel::Error,
                        format!("Invalid {block_type} block name"),
                        info,
                        None,
                    )
                    .add_error(
                        format!("Add a name to the {block_type} block"),
                        Some(
                            self.source_manager
                                .fix_span(Span::from_usize(temp_start, self.cursor + 1)),
                        ),
                    )
                    .commit();
            } else {
                let token = tokens.first().unwrap();
                if !token.is_identifier() {
                    let info = self.source_manager.get_source_info(token.span);
                    self.diagnostics
                        .builder()
                        .report(
                            DiagnosticLevel::Error,
                            format!("Invalid {block_type} block name"),
                            info,
                            None,
                        )
                        .add_error(
                            format!("Invalid {block_type} block name"),
                            Some(self.source_manager.fix_span(token.span)),
                        )
                        .commit();
                } else {
                    name_token = Some(token.clone());
                }
            }
            start = self.cursor;
        }

        let span = self.skip_while_code_block_end(start);
        (name_token, span)
    }

    fn lex_lexer_code_block(&mut self, matcher: &mut LexerEbnfParserMatcher) {
        let (name_token, span) = self.lex_code_block_with_name_helper(matcher, "lexer");
        self.parse_ebnf_lexer_block(matcher, name_token, span);
    }

    fn lex_parser_code_block(&mut self, matcher: &mut LexerEbnfParserMatcher) {
        let (name_token, span) = self.lex_code_block_with_name_helper(matcher, "parser");
        self.parse_ebnf_parser_block(name_token, span);
    }

    fn parse_ebnf_parser_block(&mut self, name_token: Option<Token>, span: Span) {
        let mut enbf_parser = EbnfLexer::new(
            &self.source_manager,
            &mut self.diagnostics,
            span.start as usize,
            span.end as usize,
        );
        let tokens = enbf_parser.lex();

        let program = EbnfParser::parse(
            tokens,
            EbnfParserMode::Parser,
            &self.source_manager,
            &mut self.diagnostics,
            false,
        );
        if let Some(name_token) = name_token {
            let name = std::str::from_utf8(&self.source_manager[name_token.span]).unwrap();
            let name = UniqueString::new(name);
            if self.block_parser_matcher.contains_key(&name) {
                self.diagnostics
                    .builder()
                    .report(
                        DiagnosticLevel::Error,
                        "Duplicate parser block name",
                        self.source_manager.get_source_info(name_token.span),
                        None,
                    )
                    .add_error(
                        "Change the name of the parser block",
                        Some(self.source_manager.fix_span(name_token.span)),
                    )
                    .commit();
            }

            let matcher = ParserEbnfParserMatcher::new(None, program, &self.diagnostics);
            self.block_parser_matcher.insert(name, matcher);
        } else {
            let p = ParserEbnfParserMatcher::new(
                Some(self.global_parser_matcher.take_as_imm_ref()),
                program,
                &self.diagnostics,
            );
            self.global_parser_matcher = p.into();
        }
    }

    fn lex_back_tick(
        &mut self,
        matcher: &mut LexerEbnfParserMatcher,
        tokens: &mut Vec<Token>,
        is_code_block: bool,
    ) -> bool {
        let is_start_code_block = ParenMatching::is_triple_back_tick_block(
            &self.source_manager.get_source(),
            self.cursor,
            b"```",
        );

        if is_code_block && is_start_code_block {
            return false;
        }

        if !is_start_code_block {
            let span = Span::from_usize(self.cursor, self.cursor + 1);
            tokens.push(Token::new(TokenKind::Backtick, span));
            self.next_char();
            return true;
        }

        let quote_span = Span::from_usize(self.cursor, self.cursor + 3);
        self.cursor += 3;

        if !self.is_start_of_identifier(matcher) {
            let dummy = (TokenKind::Unknown, Span::from_usize(0, 0));
            let last = self.paren_balance.last().unwrap_or(&dummy).clone();

            tokens.push(Token::new(TokenKind::TripleBackTick, quote_span));

            let (token, _) = last;

            if token == TokenKind::TripleBackTick {
                self.expect_block_or_paren(TokenKind::TripleBackTick);
            } else {
                let mut to_be_remove_index = -1;
                for (index, (token, _)) in self.paren_balance.iter().enumerate().rev() {
                    if token == &TokenKind::OpenDoubleBrace {
                        break;
                    }
                    if token == &TokenKind::TripleBackTick {
                        to_be_remove_index = index as i32;
                        break;
                    }
                }

                if to_be_remove_index != -1 {
                    self.paren_balance.remove(to_be_remove_index as usize);
                } else {
                    self.paren_balance
                        .push(TokenKind::TripleBackTick, quote_span);
                }
            }

            return true;
        }

        self.lex_identifier(matcher, tokens, false);
        let identifier = tokens.pop().unwrap();
        let id_span = identifier.span.clone();
        let slice = &self.source_manager[id_span];
        let name = unsafe { std::str::from_utf8_unchecked(slice) };

        if name == "lexer" {
            self.skip_whitespace().map(|t| tokens.push(t));
            self.lex_lexer_code_block(matcher);
            return true;
        } else if name == "parser" {
            self.skip_whitespace().map(|t| tokens.push(t));
            self.lex_parser_code_block(matcher);
            return true;
        }

        assert!(!name.is_empty());

        let u_name = UniqueString::new(name);
        self.try_lex_using_custom_matcher(tokens, u_name);

        true
    }

    fn expect_block_or_paren(&mut self, kind: TokenKind) {
        let current_span = Span::from_usize(self.cursor, self.cursor + 1);
        self.paren_balance.expect(kind, current_span, &self.source_manager, &self.diagnostics)
    }

    fn is_start_of_identifier(&self, matcher: &LexerEbnfParserMatcher) -> bool {
        // println!("str: {:?}", std::str::from_utf8(&self.source_manager.get_source()[self.cursor..]);
        matcher
            .match_native(
                ebnf::native_call::LexerNativeCallKind::StartIdentifier,
                &self.source_manager.get_source()[self.cursor..],
                RelativeSourceManager::new(&self.source_manager, self.cursor as u32),
                &self.diagnostics,
                None,
            )
            .is_some()
    }

    fn is_valid_digit(&self, matcher: &LexerEbnfParserMatcher) -> bool {
        // println!("temp: {:?} | {:?}", 3, std::str::from_utf8(&self.source_manager.get_source()[self.cursor..]));
        let temp = matcher.match_native(
            ebnf::native_call::LexerNativeCallKind::Digit,
            &self.source_manager.get_source()[self.cursor..],
            RelativeSourceManager::new(&self.source_manager, self.cursor as u32),
            &self.diagnostics,
            None,
        );
        temp.is_some()
    }

    fn is_valid_custom_operator_start(&self, matcher: &LexerEbnfParserMatcher) -> bool {
        match self.peek_char() {
            Some('[') | Some('_') => return true,
            _ => {}
        }

        self.is_valid_operator_start(matcher) || self.is_start_of_identifier(matcher)
    }

    fn lex_custom_operator_continue(&self, matcher: &LexerEbnfParserMatcher) -> Option<&[u8]> {
        match self.peek_char() {
            Some(']') | Some('[') => {
                return Some(&self.source_manager.get_source()[self.cursor..(self.cursor + 1)])
            }
            Some('_') => return None,
            _ => {}
        }

        if let Some((s, _)) = matcher.match_native(
            ebnf::native_call::LexerNativeCallKind::ContIdentifier,
            &self.source_manager.get_source()[self.cursor..],
            RelativeSourceManager::new(&self.source_manager, self.cursor as u32),
            &self.diagnostics,
            None,
        ) {
            return Some(s);
        }

        if let Some((s, _)) = matcher.match_native(
            ebnf::native_call::LexerNativeCallKind::ContOperator,
            &self.source_manager.get_source()[self.cursor..],
            RelativeSourceManager::new(&self.source_manager, self.cursor as u32),
            &self.diagnostics,
            None,
        ) {
            return Some(s);
        }
        None
    }

    fn is_valid_operator_start(&self, matcher: &LexerEbnfParserMatcher) -> bool {
        matcher
            .match_native(
                ebnf::native_call::LexerNativeCallKind::StartOperator,
                &self.source_manager.get_source()[self.cursor..],
                RelativeSourceManager::new(&self.source_manager, self.cursor as u32),
                &self.diagnostics,
                None,
            )
            .is_some()
    }

    fn is_valid_whitespace(&self, matcher: &LexerEbnfParserMatcher) -> bool {
        matcher
            .match_native(
                ebnf::native_call::LexerNativeCallKind::Whitespace,
                &self.source_manager.get_source()[self.cursor..],
                RelativeSourceManager::new(&self.source_manager, self.cursor as u32),
                &self.diagnostics,
                None,
            )
            .is_some()
    }

    fn lex_helper(
        &mut self,
        matcher: &mut LexerEbnfParserMatcher,
        until: Vec<char>,
        should_check_paren: bool,
        is_code_block: bool,
    ) -> Vec<Token> {
        let mut tokens = Vec::new();
        let mut loop_iterations = 0usize;
        let mut last_cursor: Option<usize> = None;
        let mut is_shebang_valid = true;

        loop {
            if let Some(last_cursor) = last_cursor {
                if last_cursor == self.cursor {
                    if loop_iterations > 5 {
                        self.diagnostics
                            .builder()
                            .report(
                                DiagnosticLevel::Error,
                                "Infinite loop detected",
                                self.source_manager.get_source_info(Span::from_usize(
                                    self.cursor,
                                    self.cursor + 1,
                                )),
                                None,
                            )
                            .add_error(
                                "Infinite loop detected",
                                Some(
                                    self.source_manager
                                        .fix_span(Span::from_usize(self.cursor, self.cursor + 1)),
                                ),
                            )
                            .commit();
                        break;
                    }

                    loop_iterations += 1;
                } else {
                    loop_iterations = 0;
                }
            }

            if self.mode.is_ir() {
                let temp_tokens = self.lex_ir_mode(false);
                tokens.extend(temp_tokens);
            }

            let Some(ch) = self.peek_char() else {
                if until.is_empty() {
                    tokens.push(Token::new_eof(Span::from_usize(self.cursor, self.cursor)));
                }
                break;
            };

            if ch.is_ascii_whitespace() {
                self.skip_whitespace().map(|token| tokens.push(token));
                continue;
            }

            if is_code_block {
                if self.source_manager.get_source()[self.cursor..]
                    .as_ref()
                    .starts_with(b"```")
                {
                    break;
                }
            }
            if until.contains(&ch) {
                break;
            }

            if !matcher.is_default() {
                let temp_matched = matcher.try_match_expr(
                    &self.source_manager.get_source()[self.cursor..],
                    RelativeSourceManager::new(&self.source_manager, self.cursor as u32),
                    &self.diagnostics,
                    None,
                );

                if let Some((span, kind)) = temp_matched {
                    if kind == TokenKind::Identifier {
                        let kind: TokenKind = self.source_manager[span].into();
                        self.lex_identifier_helper(matcher, span, kind, &mut tokens, true);
                    }
                    tokens.push(Token::new(kind, span));
                    self.cursor += span.len();
                    last_cursor = Some(self.cursor);
                    is_shebang_valid = false;
                    continue;
                }

                if matcher.is_scoped() {
                    self.diagnostics
                        .builder()
                        .report(
                            DiagnosticLevel::Error,
                            "Invalid token",
                            self.source_manager
                                .get_source_info(Span::from_usize(self.cursor, self.cursor + 1)),
                            None,
                        )
                        .add_error(
                            "Invalid token",
                            Some(
                                self.source_manager
                                    .fix_span(Span::from_usize(self.cursor, self.cursor + 1)),
                            ),
                        )
                        .commit();
                    break;
                }
            }

            let mut should_run_custom_match = true;

            if ch == '.' {
                let current = self.cursor;
                self.next_char();
                let is_valid = matcher
                    .match_native(
                        ebnf::native_call::LexerNativeCallKind::Digit,
                        &self.source_manager.get_source()[self.cursor..],
                        RelativeSourceManager::new(&self.source_manager, self.cursor as u32),
                        &self.diagnostics,
                        None,
                    )
                    .is_some();
                if is_valid {
                    should_run_custom_match = false;
                }

                self.cursor = current;

                if self.cursor != 0
                    && !self.source_manager.get_source()[self.cursor - 1].is_ascii_whitespace()
                {
                    should_run_custom_match = true;
                }
            }

            if should_run_custom_match {
                if let (Some(token), len) =
                    self.match_custom_operator(&self.source_manager[self.cursor..])
                {
                    tokens.push(token);
                    self.cursor += len;
                    is_shebang_valid = false;
                    continue;
                }

                if let (Some(token), len) =
                    self.match_custom_keyword(&self.source_manager[self.cursor..])
                {
                    tokens.push(token);
                    self.cursor += len;
                    is_shebang_valid = false;
                    continue;
                }
            }

            last_cursor = Some(self.cursor);

            match ch {
                c if self.is_valid_comment(c) => {
                    self.lex_comment(&mut tokens);
                }
                _ if self.is_valid_digit(matcher) || !should_run_custom_match => {
                    self.lex_number(matcher, &mut tokens);
                }
                _ if self.is_start_of_identifier(matcher) => {
                    self.lex_identifier(matcher, &mut tokens, true);
                }
                '$' => {
                    let span = Span::from_usize(self.cursor, self.cursor + 1);
                    tokens.push(Token::new(TokenKind::Dollar, span));
                    self.next_char();
                }
                '(' => {
                    let span = Span::from_usize(self.cursor, self.cursor + 1);
                    self.paren_balance.push(TokenKind::OpenParen, span);
                    tokens.push(Token::new(TokenKind::OpenParen, span));
                    self.next_char();
                }
                ')' => {
                    let span = Span::from_usize(self.cursor, self.cursor + 1);
                    self.expect_block_or_paren(TokenKind::CloseParen);
                    tokens.push(Token::new(TokenKind::CloseParen, span));
                    self.next_char();
                }
                '{' => {
                    let start = self.cursor;
                    let mut end = self.cursor + 1;
                    self.next_char();
                    if self.peek_char() == Some('{') {
                        end += 1;
                        self.next_char();
                    }
                    let span = Span::from_usize(start, end);
                    let len = span.len();
                    if len == 2 {
                        self.paren_balance.push(TokenKind::OpenDoubleBrace, span);
                        tokens.push(Token::new(TokenKind::OpenDoubleBrace, span));
                    } else {
                        self.paren_balance.push(TokenKind::OpenBrace, span);
                        tokens.push(Token::new(TokenKind::OpenBrace, span));
                    }
                }
                '}' => {
                    let start = self.cursor;
                    let mut end = self.cursor + 1;
                    self.next_char();
                    if self.peek_char() == Some('}') {
                        end += 1;
                        self.next_char();
                    }
                    let span = Span::from_usize(start, end);
                    let len = span.len();
                    if len == 2 {
                        self.expect_block_or_paren(TokenKind::CloseDoubleBrace);
                        tokens.push(Token::new(TokenKind::CloseDoubleBrace, span));
                    } else {
                        self.expect_block_or_paren(TokenKind::CloseBrace);
                        tokens.push(Token::new(TokenKind::CloseBrace, span));
                    }
                }
                '[' => {
                    let span = Span::from_usize(self.cursor, self.cursor + 1);
                    self.paren_balance.push(TokenKind::OpenBracket, span);
                    tokens.push(Token::new(TokenKind::OpenBracket, span));
                    self.next_char();
                }
                ']' => {
                    let span = Span::from_usize(self.cursor, self.cursor + 1);
                    self.expect_block_or_paren(TokenKind::CloseBracket);
                    tokens.push(Token::new(TokenKind::CloseBracket, span));
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
                ':' => {
                    let span = Span::from_usize(self.cursor, self.cursor + 1);
                    tokens.push(Token::new(TokenKind::Colon, span));
                    self.next_char();
                }
                _ if self.is_valid_operator_start(matcher) => {
                    self.lex_operator(matcher, &mut tokens);
                }
                '"' => {
                    self.lex_double_quoted_string(matcher, &mut tokens);
                }
                '\'' => {
                    self.lex_character_literal(&mut tokens);
                }
                '@' => {
                    let span = Span::from_usize(self.cursor, self.cursor + 1);
                    tokens.push(Token::new(TokenKind::AtSign, span));
                    self.next_char();
                }
                '\\' => {
                    let span = Span::from_usize(self.cursor, self.cursor + 1);
                    tokens.push(Token::new(TokenKind::Backslash, span));
                    self.next_char();
                }
                '`' => {
                    if !self.lex_back_tick(matcher, &mut tokens, is_code_block) {
                        break;
                    }
                }
                '#' => {
                    let mut span = Span::from_usize(self.cursor, self.cursor + 1);
                    self.next_char();

                    if self.peek_char() == Some('!') {
                        self.skip_while(|c| c != '\n');
                        span.end = self.cursor as u32;

                        if !is_shebang_valid {
                            let info = self.source_manager.get_source_info(span);
                            self.diagnostics
                                .builder()
                                .report(DiagnosticLevel::Error, "Invalid shebang", info, None)
                                .add_error(
                                    "Shebang must be at the start of the file",
                                    Some(self.source_manager.fix_span(span)),
                                )
                                .commit();
                            continue;
                        }

                        self.shebang_span = span;
                    } else {
                        tokens.push(Token::new(TokenKind::Hash, span));
                    }
                }
                _ => {
                    self.paren_balance.check_balanced(&until, &self.source_manager, &self.diagnostics);
                    tokens
                        .iter()
                        .for_each(|token| println!("{}", token.to_string(&self.source_manager)));
                    todo!(
                        "Implement the rest of the lexer: {} | {:?}",
                        self.cursor,
                        ch
                    )
                }
            }
            is_shebang_valid = false;
        }

        if should_check_paren {
            self.paren_balance.check_balanced(&until, &self.source_manager, &self.diagnostics);
        }

        tokens
    }

    fn lex_ir_mode(&mut self, is_file: bool) -> Vec<Token> {
        let mut tokens = Vec::new();
        let mut matcher = LexerEbnfParserMatcher::new_ir();
        let old_parens = self.paren_balance.clone();
        self.paren_balance.clear();

        if !is_file {
            tokens.push(Token::new(
                TokenKind::IrStart,
                Span::from_usize(self.cursor, self.cursor),
            ));
        }

        loop {
            let Some(ch) = self.peek_char() else {
                break;
            };

            if ch.is_ascii_whitespace() {
                self.skip_whitespace().map(|token| tokens.push(token));
                continue;
            }

            let relative_manager =
                RelativeSourceManager::new(&self.source_manager, self.cursor as u32);
            let source = &self.source_manager.get_source()[self.cursor..];

            match ch {
                _ if IRLexerEbnfParserMatcher::is_valid_number_start_code_point(ch) => {
                    let start = self.cursor;
                    if let Some((s, t)) = matcher.as_ir().match_native(
                        ebnf::native_call::LexerNativeCallKind::Number,
                        source,
                        relative_manager,
                        &self.diagnostics,
                        None,
                    ) {
                        let end = start + s.len();
                        let span = Span::from_usize(start, end);
                        tokens.push(Token::new(t, span));
                        self.cursor = end;
                    } else {
                        let info = self
                            .source_manager
                            .get_source_info(Span::from_usize(self.cursor, self.cursor + 1));
                        self.diagnostics
                            .builder()
                            .report(DiagnosticLevel::Error, "Invalid number", info, None)
                            .add_error(
                                "Invalid number",
                                Some(
                                    self.source_manager
                                        .fix_span(Span::from_usize(self.cursor, self.cursor + 1)),
                                ),
                            )
                            .commit();
                        self.next_char();
                    }
                }
                _ if IRLexerEbnfParserMatcher::is_valid_identifier_start_code_point(ch) => {
                    let id = matcher.as_ir().match_native(
                        ebnf::native_call::LexerNativeCallKind::Identifier,
                        source,
                        RelativeSourceManager::new(&self.source_manager, self.cursor as u32),
                        &self.diagnostics,
                        None,
                    );
                    if let Some((id, _)) = id {
                        let end = self.cursor + id.len();
                        let span = Span::from_usize(self.cursor, end);
                        tokens.push(Token::new(TokenKind::Identifier, span));
                        self.cursor = end;
                    } else {
                        let info = self
                            .source_manager
                            .get_source_info(Span::from_usize(self.cursor, self.cursor + 1));
                        self.diagnostics
                            .builder()
                            .report(DiagnosticLevel::Error, "Invalid identifier", info, None)
                            .add_error(
                                "Invalid identifier",
                                Some(
                                    self.source_manager
                                        .fix_span(Span::from_usize(self.cursor, self.cursor + 1)),
                                ),
                            )
                            .commit();
                        self.next_char();
                    }
                }
                '#' => {
                    let span = Span::from_usize(self.cursor, self.cursor + 1);
                    tokens.push(Token::new(TokenKind::Hash, span));
                }
                '(' => {
                    let span = Span::from_usize(self.cursor, self.cursor + 1);
                    self.paren_balance.push(TokenKind::OpenParen, span);
                    tokens.push(Token::new(TokenKind::OpenParen, span));
                    self.next_char();
                }
                ')' => {
                    let span = Span::from_usize(self.cursor, self.cursor + 1);
                    self.expect_block_or_paren(TokenKind::CloseParen);
                    tokens.push(Token::new(TokenKind::CloseParen, span));
                    self.next_char();
                }
                '{' => {
                    let span = Span::from_usize(self.cursor, self.cursor + 1);
                    self.paren_balance.push(TokenKind::OpenBrace, span);
                    tokens.push(Token::new(TokenKind::OpenBrace, span));
                    self.next_char();
                }
                '}' => {
                    let span = Span::from_usize(self.cursor, self.cursor + 1);
                    self.expect_block_or_paren(TokenKind::CloseBrace);
                    tokens.push(Token::new(TokenKind::CloseBrace, span));
                    self.next_char();
                    if self.paren_balance.is_empty() && !is_file {
                        self.mode = LexerMode::Normal;
                        break;
                    }
                }
                ':' => {
                    let span = self.skip_while(|c| c == ':');
                    tokens.push(Token::new(TokenKind::Colon, span));
                }
                '<' => {
                    let temp_tokens = matcher.as_ir().lex_type_content(&mut self.cursor, &self.source_manager, &self.diagnostics);
                    tokens.extend(temp_tokens);
                }
                '>' => {
                    self.diagnostics
                        .builder()
                        .report(
                            DiagnosticLevel::Error,
                            "Found dangling '>'",
                            self.source_manager
                                .get_source_info(Span::from_usize(self.cursor, self.cursor + 1)),
                            None,
                        )
                        .add_error(
                            "Expected '<' to start a type",
                            Some(
                                self.source_manager
                                    .fix_span(Span::from_usize(self.cursor, self.cursor + 1)),
                            ),
                        )
                        .commit();
                    self.next_char();
                }
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
                '-' => {
                    let start = self.cursor;
                    self.next_char();
                    if self.peek_char() == Some('>') {
                        let span = Span::from_usize(start, start + 2);
                        tokens.push(Token::new(TokenKind::Arrow, span));
                        self.cursor += 2;
                    } else {
                        self.diagnostics
                            .builder()
                            .report(
                                DiagnosticLevel::Error,
                                "Invalid token",
                                self.source_manager
                                    .get_source_info(Span::from_usize(start, start + 1)),
                                None,
                            )
                            .add_error(
                                "Invalid token",
                                Some(
                                    self.source_manager
                                        .fix_span(Span::from_usize(start, start + 1)),
                                ),
                            )
                            .commit();
                    }
                }
                '/' => {
                    let start = self.cursor;
                    self.next_char();
                    if self.peek_char() == Some('/') {
                        self.lex_single_line_comment(&mut tokens);
                    } else if self.peek_char() == Some('*') {
                        self.lex_multiline_comment(&mut tokens);
                    } else {
                        self.diagnostics
                            .builder()
                            .report(
                                DiagnosticLevel::Error,
                                "Invalid token",
                                self.source_manager
                                    .get_source_info(Span::from_usize(start, start + 1)),
                                None,
                            )
                            .add_error(
                                "Invalid token",
                                Some(
                                    self.source_manager
                                        .fix_span(Span::from_usize(start, start + 1)),
                                ),
                            )
                            .commit();
                    }
                }
                '=' => {
                    let span = Span::from_usize(self.cursor, self.cursor + 1);
                    tokens.push(Token::new(TokenKind::Equal, span));
                    self.next_char();
                }
                '"' => {
                    if let Some(s) = matcher.as_ir().match_string_literal(
                        source,
                        relative_manager,
                        &mut self.diagnostics,
                    ) {
                        let end = self.cursor + s.len();
                        let span = Span::from_usize(self.cursor, end);
                        tokens.push(Token::new(TokenKind::String, span));
                        self.cursor = end + 1;
                    } else {
                        let info = self
                            .source_manager
                            .get_source_info(Span::from_usize(self.cursor, self.cursor + 1));
                        self.diagnostics
                            .builder()
                            .report(DiagnosticLevel::Error, "Invalid string literal", info, None)
                            .add_error(
                                "Invalid string literal",
                                Some(
                                    self.source_manager
                                        .fix_span(Span::from_usize(self.cursor, self.cursor + 1)),
                                ),
                            )
                            .commit();
                        self.next_char();
                    }
                }
                '[' => {
                    let span = Span::from_usize(self.cursor, self.cursor + 1);
                    self.paren_balance.push(TokenKind::OpenBracket, span);
                    tokens.push(Token::new(TokenKind::OpenBracket, span));
                    self.next_char();
                }
                ']' => {
                    let span = Span::from_usize(self.cursor, self.cursor + 1);
                    self.expect_block_or_paren(TokenKind::CloseBracket);
                    tokens.push(Token::new(TokenKind::CloseBracket, span));
                    self.next_char();
                }
                '\'' => {
                    if let Some(s) = matcher.as_ir().match_character_literal(
                        source,
                        relative_manager,
                        &mut self.diagnostics,
                    ) {
                        let end = self.cursor + s.len();
                        let span = Span::from_usize(self.cursor, end);
                        tokens.push(Token::new(TokenKind::Char, span));
                        self.cursor = end + 1;
                    } else {
                        let info = self
                            .source_manager
                            .get_source_info(Span::from_usize(self.cursor, self.cursor + 1));
                        self.diagnostics
                            .builder()
                            .report(
                                DiagnosticLevel::Error,
                                "Invalid character literal",
                                info,
                                None,
                            )
                            .add_error(
                                "Invalid character literal",
                                Some(
                                    self.source_manager
                                        .fix_span(Span::from_usize(self.cursor, self.cursor + 1)),
                                ),
                            )
                            .commit();
                        self.next_char();
                    }
                }
                '!' => {
                    let span = Span::from_usize(self.cursor, self.cursor + 1);
                    tokens.push(Token::new(TokenKind::ExclamationMark, span));
                    self.next_char();
                }
                '.' => {
                    let span = Span::from_usize(self.cursor, self.cursor + 1);
                    tokens.push(Token::new(TokenKind::Dot, span));
                    self.next_char();
                }
                '`' => {
                    if !self.lex_back_tick(&mut matcher, &mut tokens, false) {
                        break;
                    }
                }
                ';' => {
                    let span = Span::from_usize(self.cursor, self.cursor + 1);
                    tokens.push(Token::new(TokenKind::Semicolon, span));
                    self.next_char();
                }
                _ => {
                    self.paren_balance.check_balanced(&[], &self.source_manager, &self.diagnostics);
                    tokens
                        .iter()
                        .for_each(|token| println!("{}", token.to_string(&self.source_manager)));
                    todo!(
                        "Implement the rest of the lexer: {} | {:?}",
                        self.cursor,
                        ch
                    )
                }
            }
        }

        if !is_file {
            tokens.push(Token::new(
                TokenKind::IrEnd,
                Span::from_usize(self.cursor, self.cursor),
            ));
        }
        self.paren_balance.check_balanced(&[], &self.source_manager, &self.diagnostics);
        self.paren_balance = old_parens;
        tokens
    }

    fn match_custom_operator(&self, source: &[u8]) -> (Option<Token>, usize) {
        let mut valid_op = None;
        let mut len = 0;

        if source.len() == 0 {
            return (valid_op, len);
        }

        let mut start = 0;
        let mut node = &self.custom_operators_trie;
        while let Some(temp_node) = node.try_match(&source[start..(start + 1)]) {
            start += 1;
            node = temp_node;

            if node.is_terminal() {
                valid_op = Some(Token::new(
                    TokenKind::Operator,
                    Span::from_usize(self.cursor, self.cursor + start),
                ));
                len = start;
            }

            if source.len() < (start + 1) {
                break;
            }
        }

        (valid_op, len)
    }

    fn match_custom_keyword(&self, source: &[u8]) -> (Option<Token>, usize) {
        let mut valid_kw = None;
        let mut len = 0;

        if source.len() == 0 {
            return (valid_kw, len);
        }

        let mut start = 0;
        let mut node = &self.custom_keywords_trie;

        while let Some(temp_node) = node.try_match(&source[start..(start + 1)]) {
            start += 1;
            node = temp_node;

            if node.is_terminal() {
                valid_kw = Some(Token::new(
                    TokenKind::CustomKeyword,
                    Span::from_usize(self.cursor, self.cursor + start),
                ));
                len = start;
            }

            if source.len() < (start + 1) {
                break;
            }
        }

        (valid_kw, len)
    }

    fn lex_custom_operator_helper(
        &mut self,
        matcher: &LexerEbnfParserMatcher,
        tokens: &mut Vec<Token>,
        operator_span: Span,
    ) -> bool {
        let start = self.cursor;
        let mut span = Span::from_usize(start, start);
        let mut op_span = Span::from_usize(start, start);
        loop {
            if let Some(s) = self.lex_custom_operator_continue(matcher) {
                self.cursor += s.len();
                op_span.end = self.cursor as u32;
                continue;
            }

            if self.peek_char() == Some('_') {
                let temp = op_span.trim(&self.source_manager);
                if !temp.is_empty() {
                    tokens.push(Token::new(TokenKind::Operator, temp));
                }
                tokens.push(Token::new(TokenKind::Underscore, Span::from_usize(self.cursor, self.cursor + 1)));
                self.next_char();
                op_span.start = self.cursor as u32;
                continue;
            }

            let is_valid_space = matcher.match_native(
                ebnf::native_call::LexerNativeCallKind::Whitespace,
                self.source_manager.get_source()[self.cursor..].as_ref(),
                RelativeSourceManager::new(&self.source_manager, self.cursor as u32),
                &self.diagnostics,
                None,
            );

            if let Some((s, _)) = is_valid_space {
                self.cursor += s.len();
                continue;
            }

            op_span.end = self.cursor as u32;
            op_span = op_span.trim(&self.source_manager);
            if !op_span.is_empty() {
                tokens.push(Token::new(TokenKind::Operator, op_span));
            }
            break;
            
        }

        span.end = self.cursor as u32;
        span = span.trim(&self.source_manager);

        if span.is_empty() {
            self.diagnostics
                .builder()
                .report(
                    DiagnosticLevel::Error,
                    "Expecting an operator after 'operator'",
                    self.source_manager.get_source_info(operator_span),
                    None,
                )
                .commit();
            return false;
        }
        let slice = &self.source_manager[span];
        let mut underscore: [Span; 3] = [Span::default(); 3];

        let underscores_count = slice.iter().filter(|c| **c == b'_').count();
        if underscores_count > 2 {
            let info = self.source_manager.get_source_info(span);
            self.diagnostics
                .builder()
                .report(DiagnosticLevel::Error, "Invalid operator", info, None)
                .add_error(
                    "Max two underscores are allowed",
                    Some(self.source_manager.fix_span(span)),
                )
                .commit();
            return false;
        }

        let slice_iter = slice
            .iter()
            .filter(|c| !c.is_ascii_whitespace())
            .enumerate();
        let slice_len = slice_iter.clone().count();

        if slice_len == underscores_count {
            let info = self.source_manager.get_source_info(span);
            self.diagnostics
                .builder()
                .report(DiagnosticLevel::Error, "Invalid operator", info, None)
                .add_error(
                    "Operator cannot be empty",
                    Some(self.source_manager.fix_span(span)),
                )
                .commit();
            return false;
        }

        for (index, byte) in slice_iter {
            if *byte == b'_' {
                if index == 0 {
                    underscore[0] = Span::from_usize(start + index, start + index + 1);
                } else if index == slice_len - 1 {
                    underscore[2] = Span::from_usize(start + index, start + index + 1);
                } else {
                    underscore[1] = Span::from_usize(start + index, start + index + 1);
                }
            }
        }

        let is_start = !underscore[0].is_empty();
        let is_middle = !underscore[1].is_empty();
        let is_end = !underscore[2].is_empty();

        if is_middle {
            let mid_span = underscore[1];
            let left_span = Span::new(span.start, mid_span.start).trim(&self.source_manager);
            let right_span = Span::new(mid_span.end, span.end).trim(&self.source_manager);
            let op = CustomOperator::Compound {
                open: Identifier::new(left_span),
                close: Identifier::new(right_span),
                span: span.trim(&self.source_manager),
            };
            self.custom_operators.push(op);
        } else if is_start && is_end {
            let op_span =
                Span::new(underscore[0].end, underscore[2].start).trim(&self.source_manager);
            let op = CustomOperator::Infix(Identifier::new(op_span));
            self.custom_operators.push(op);
        } else if is_start {
            let op_span = Span::new(underscore[0].end, (start + slice.len()) as u32)
                .trim(&self.source_manager);
            let op = CustomOperator::Prefix(Identifier::new(op_span));
            self.custom_operators.push(op);
        } else if is_end {
            let op_span = Span::new(start as u32, underscore[2].start).trim(&self.source_manager);
            let op = CustomOperator::Postfix(Identifier::new(op_span));
            self.custom_operators.push(op);
        } else {
            let span = span.trim(&self.source_manager);
            let op = CustomOperator::Unknown(Identifier::new(span));
            self.custom_operators.push(op);
        }

        if let Some(last) = self.custom_operators.last() {
            self.custom_operators_trie.insert(
                last.to_str(&self.source_manager).as_bytes(),
                self.custom_operators.len() - 1,
            );
        }

        return true;
    }

    fn lex_custom_operator(
        &mut self,
        matcher: &LexerEbnfParserMatcher,
        operator_span: Span,
    ) -> Vec<Token> {
        let mut tokens: Vec<Token> = Vec::new();
        let mut operator_found = false;

        let inside_triple_back_tick = self
            .paren_balance
            .iter()
            .any(|(token, _)| token == &TokenKind::TripleBackTick);
        if inside_triple_back_tick {
            let info = self.source_manager.get_source_info(operator_span);
            self.diagnostics
                .builder()
                .report(
                    DiagnosticLevel::Error,
                    "Custom operator is not allowed inside code block",
                    info,
                    None,
                )
                .commit();
            return tokens;
        }

        loop {
            let Some(ch) = self.peek_char() else {
                break;
            };

            if ch.is_ascii_whitespace() {
                self.skip_whitespace().map(|token| tokens.push(token));
                continue;
            }

            // operator ('operator') { ... }

            match ch {
                '(' => {
                    let span = Span::from_usize(self.cursor, self.cursor + 1);
                    self.paren_balance.push(TokenKind::OpenParen, span);
                    tokens.push(Token::new(TokenKind::OpenParen, span));
                    self.next_char();

                    self.skip_whitespace().map(|token| tokens.push(token));
                }
                ')' => {
                    let span = Span::from_usize(self.cursor, self.cursor + 1);
                    self.expect_block_or_paren(TokenKind::CloseParen);
                    tokens.push(Token::new(TokenKind::CloseParen, span));
                    self.next_char();
                    break;
                }
                _ if self.is_valid_custom_operator_start(matcher) => {
                    operator_found =
                        self.lex_custom_operator_helper(matcher, &mut tokens, operator_span);
                }
                _ => {
                    break;
                }
            }
        }

        if !operator_found {
            let info = self.source_manager.get_source_info(operator_span);
            self.diagnostics
                .builder()
                .report(
                    DiagnosticLevel::Error,
                    format!("Expecting an operator or identifier after 'operator'"),
                    info,
                    None,
                )
                .commit();
        }

        tokens
    }

    fn lex_custom_keyword(&mut self, keyword_span: Span) -> Vec<Token> {
        let mut tokens: Vec<Token> = Vec::new();
        let mut keyword_found = false;

        let inside_triple_back_tick = self
            .paren_balance
            .iter()
            .any(|(token, _)| token == &TokenKind::TripleBackTick);
        if inside_triple_back_tick {
            let info = self.source_manager.get_source_info(keyword_span);
            self.diagnostics
                .builder()
                .report(
                    DiagnosticLevel::Error,
                    "Custom keyword is not allowed inside code block",
                    info,
                    None,
                )
                .commit();
            return tokens;
        }

        loop {
            let Some(ch) = self.peek_char() else {
                break;
            };

            if ch.is_ascii_whitespace() {
                self.skip_whitespace().map(|token| tokens.push(token));
                continue;
            }

            // keyword '('? keyword ')'? { ... }

            match ch {
                c if is_valid_identifier_start_code_point(c) => {
                    let span = self.skip_while(|c| is_valid_identifier_continuation_code_point(c));
                    let slice = &self.source_manager[span];
                    tokens.push(Token::new(TokenKind::CustomKeyword, span));
                    let dup = self
                        .custom_keywords
                        .iter()
                        .filter(|op| op.to_str(&self.source_manager).as_bytes() == slice)
                        .last();
                    let is_invalid = dup.is_some();
                    if is_invalid {
                        let info = format!(
                            "The keyword '{}' is already defined",
                            std::str::from_utf8(slice).unwrap()
                        );
                        self.diagnostics
                            .builder()
                            .report(
                                DiagnosticLevel::Error,
                                "Redefinition of keyword is not allowed",
                                self.source_manager.get_source_info(span),
                                None,
                            )
                            .add_error(info, Some(self.source_manager.fix_span(span)))
                            .commit();
                        if let Some(dup) = dup {
                            self.diagnostics
                                .builder()
                                .report(
                                    DiagnosticLevel::Note,
                                    "Previous definition",
                                    self.source_manager.get_source_info(dup.span),
                                    None,
                                )
                                .add_note(
                                    format!(
                                        "The keyword '{}' is already defined",
                                        std::str::from_utf8(slice).unwrap()
                                    ),
                                    Some(self.source_manager.fix_span(dup.span)),
                                )
                                .commit();
                        }
                        continue;
                    }
                    self.custom_keywords.push(Identifier::new(span));
                    self.custom_keywords_trie
                        .insert(slice, self.custom_keywords.len() - 1);
                    keyword_found = true;
                }
                '(' => {
                    let span = Span::from_usize(self.cursor, self.cursor + 1);
                    self.paren_balance.push(TokenKind::OpenParen, span);
                    tokens.push(Token::new(TokenKind::OpenParen, span));
                    self.next_char();
                }
                ')' => {
                    let span = Span::from_usize(self.cursor, self.cursor + 1);
                    self.expect_block_or_paren(TokenKind::CloseParen);
                    tokens.push(Token::new(TokenKind::CloseParen, span));
                    self.next_char();
                    break;
                }
                _ => {
                    break;
                }
            }
        }

        if !keyword_found {
            let info = self.source_manager.get_source_info(keyword_span);
            self.diagnostics
                .builder()
                .report(
                    DiagnosticLevel::Error,
                    format!("Expecting an identifier after 'keyword'"),
                    info,
                    None,
                )
                .commit();
        }

        tokens
    }

    pub fn lex(&mut self) -> Vec<Token> {
        let mut matcher = LexerEbnfParserMatcher::default();
        // let relative = RelativeSourceManager::new(&self.source_manager, self.cursor as u32);
        let cursor = self.cursor;
        let mut tokens = self.lex_helper(&mut matcher, Vec::new(), true, false);
        let mut i = 0;
        for t in tokens.iter().rev() {
            if t.is_eof() {
                i += 1;
            } else {
                break;
            }
        }

        if i == 0 {
            tokens.push(Token::new_eof(Span::from_usize(cursor, cursor)));
        } else if i > 1 {
            tokens.truncate(tokens.len() - (i - 1));
        }

        tokens
    }
}
