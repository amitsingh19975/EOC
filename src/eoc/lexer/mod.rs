use self::{
    token::{Token, TokenKind},
    utils::{
        is_valid_identifier_continuation_code_point, is_valid_identifier_start_code_point,
        is_valid_operator_continuation_code_point, is_valid_operator_start_code_point,
        CustomOperator, ParenMatching,
    },
};
use super::{
    ast::identifier::Identifier,
    utils::{
        diagnostic::{Diagnostic, DiagnosticLevel, DiagnosticReporter},
        source_manager::SourceManager,
        span::Span,
    },
};
use std::{path::Path, vec};
pub(crate) mod token;
pub(crate) mod utils;

pub(crate) struct Lexer {
    source_manager: SourceManager,
    cursor: usize,
    paren_balance: Vec<(TokenKind, Span)>,
    diagnostics: Diagnostic,
    custom_operators: Vec<CustomOperator>,
    custom_keywords: Vec<Identifier>,
}

impl Lexer {
    pub(crate) fn new<D: Into<Diagnostic>>(source_manager: SourceManager, diagnostic: D) -> Self {
        Self {
            source_manager,
            cursor: 0,
            paren_balance: Vec::new(),
            diagnostics: diagnostic.into(),
            custom_operators: Vec::new(),
            custom_keywords: Vec::new(),
        }
    }

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
        let ch = self.peek_char();
        if ch.is_none() {
            return None;
        }

        let ch = unsafe { ch.unwrap_unchecked() };

        Some(match ch {
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
        })
    }

    fn check_balanced_paren(&mut self, until_chars: &[char]) {
        for (token, span) in self.paren_balance.iter().rev() {
            if until_chars.contains(&ParenMatching::to_str(*token).chars().next().unwrap()) {
                continue;
            }

            let other_kind = ParenMatching::get_other_pair(*token).unwrap_or(TokenKind::Unknown);
            if until_chars.contains(&ParenMatching::to_str(other_kind).chars().next().unwrap()) {
                continue;
            }

            let token_name = ParenMatching::get_token_name(*token);

            self.diagnostics
                .builder()
                .report(
                    DiagnosticLevel::Error,
                    format!("Unmatched {token_name}"),
                    self.source_manager.get_source_info(*span),
                    None,
                )
                .add_error(
                    format!("Remove or add matching {token_name}"),
                    Some(self.source_manager.fix_span(*span)),
                )
                .commit();
        }

        self.paren_balance.clear();
    }

    pub(crate) fn get_custom_operators(&self) -> &Vec<CustomOperator> {
        &self.custom_operators
    }

    pub(crate) fn get_custom_keywords(&self) -> &Vec<Identifier> {
        &self.custom_keywords
    }

    fn lex_identifier(&mut self, tokens: &mut Vec<Token>, should_parse_nested_operator: bool) {
        let span = self.skip_while(|c| is_valid_identifier_continuation_code_point(c));
        let slice = &self.source_manager[span];
        let kind: TokenKind = slice.into();
        tokens.push(Token::new(kind, span));

        if should_parse_nested_operator {
            if kind == TokenKind::KwOperator {
                let new_tokens = self.lex_custom_operator(span);
                tokens.extend(new_tokens);
            } else if kind == TokenKind::KwKeyword {
                let new_tokens = self.lex_custom_keyword(span);
                tokens.extend(new_tokens);
            }
        }
    }

    ///   integer_literal  ::= [0-9][0-9_]*
    ///   integer_literal  ::= 0x[0-9a-fA-F][0-9a-fA-F_]*
    ///   integer_literal  ::= 0o[0-7][0-7_]*
    ///   integer_literal  ::= 0b[01][01_]*
    fn lex_integer(&mut self, tokens: &mut Vec<Token>) -> bool {
        if self.peek_char() == Some('.') {
            return false;
        }

        let temp = self.cursor;

        while let Some(ch) = self.peek_char() {
            match ch {
                'e' | 'E' | '.' | 'p' | 'P' => {
                    self.cursor = temp;
                    return false;
                }
                ch if ch.is_whitespace() => break,
                _ => {
                    self.next_char();
                }
            }
        }

        self.cursor = temp;

        let ch = self.peek_char();
        if ch.is_none() {
            return false;
        }

        let ch = unsafe { ch.unwrap_unchecked() };

        if !ch.is_ascii_digit() {
            return false;
        }

        let is_start_with_zero = ch == '0';

        let start = self.cursor;
        self.next_char();

        if is_start_with_zero {
            if self.peek_char() == Some('x') {
                self.next_char();
                let span = self.skip_while(|c| c.is_ascii_hexdigit() || c == '_');
                if self.peek_char() == Some('.') {
                    self.cursor = start;
                    return false;
                }

                let span = Span::from_usize(start, span.end as usize);
                tokens.push(Token::new(TokenKind::Integer, span));
                return true;
            }

            if self.peek_char() == Some('o') {
                self.next_char();
                let span = self.skip_while(|c| c.is_ascii_digit() || c == '_');
                let span = Span::from_usize(start, span.end as usize);
                let slice: Vec<u8> = self.source_manager[span]
                    .iter()
                    .filter(|c| **c != b'_')
                    .map(|c| *c)
                    .collect();
                for (index, byte) in slice.iter().enumerate() {
                    if *byte > b'7' {
                        let start = span.start as usize + 2 + 1;
                        let span = Span::from_usize(start + index, start + index + 1);
                        let info = self.source_manager.get_source_info(span);
                        self.diagnostics
                            .builder()
                            .report(DiagnosticLevel::Error, "Invalid octal digit", info, None)
                            .add_error(
                                "Octal digit must be between 0 and 7",
                                Some(self.source_manager.fix_span(span)),
                            )
                            .commit();
                        return true;
                    }
                }
                tokens.push(Token::new(TokenKind::Integer, span));
                return true;
            }

            if self.peek_char() == Some('b') {
                self.next_char();
                let span = self.skip_while(|c| c == '0' || c == '1' || c == '_');
                let span = Span::from_usize(start, span.end as usize);
                tokens.push(Token::new(TokenKind::Integer, span));
                return true;
            }
        }

        let span = self.skip_while(|c| c.is_ascii_digit() || c == '_');
        if self.peek_char() == Some('.') {
            self.cursor = start;
            return false;
        }
        let span = Span::from_usize(start, span.end as usize);
        tokens.push(Token::new(TokenKind::Integer, span));

        return true;
    }

    ///   floating_literal ::= [0-9][0-9]_*\.[0-9][0-9_]*
    ///   floating_literal ::= [0-9][0-9]*\.[0-9][0-9_]*[eE][+-]?[0-9][0-9_]*
    ///   floating_literal ::= [0-9][0-9_]*[eE][+-]?[0-9][0-9_]*
    ///   floating_literal ::= 0x[0-9A-Fa-f][0-9A-Fa-f_]*
    ///                          (\.[0-9A-Fa-f][0-9A-Fa-f_]*)?[pP][+-]?[0-9][0-9_]*
    fn lex_floating_point(&mut self, tokens: &mut Vec<Token>) -> bool {
        let ch = self.peek_char();
        if ch.is_none() {
            return false;
        }

        let ch = unsafe { ch.unwrap_unchecked() };

        if !ch.is_ascii_digit() && ch != '.' {
            return false;
        }

        let is_start_with_zero = ch == '0';
        let start = self.cursor;
        self.next_char();

        if is_start_with_zero && self.peek_char() == Some('x') {
            self.next_char();
            let span = self.skip_while(|c| c.is_ascii_hexdigit() || c == '_');
            let mut span = Span::from_usize(start, span.end as usize);
            if self.peek_char() == Some('.') {
                self.next_char();
                let dot_span = self.skip_while(|c| c.is_ascii_hexdigit() || c == '_');
                span = Span::from_usize(start, dot_span.end as usize);
            }

            if self.peek_char() == Some('p') || self.peek_char() == Some('P') {
                self.next_char();
                if self.peek_char() == Some('+') || self.peek_char() == Some('-') {
                    self.next_char();
                }
                let p_span = self.skip_while(|c| c.is_ascii_digit() || c == '_');
                span = Span::from_usize(start, p_span.end as usize);
                tokens.push(Token::new(TokenKind::FloatingPoint, span));
                return true;
            }

            let slice = self.source_manager[span]
                .iter()
                .filter(|c| **c != b'_')
                .map(|c| *c);

            let mut dot_count = 0;
            for (index, byte) in slice.enumerate() {
                if byte == b'.' {
                    dot_count += 1;
                }

                let start = span.start as usize + 2 + 1;
                let span = Span::from_usize(start + index, start + index + 1);

                if dot_count > 1 {
                    let info = self.source_manager.get_source_info(span);
                    self.diagnostics
                        .builder()
                        .report(
                            DiagnosticLevel::Error,
                            "Invalid floating point literal",
                            info,
                            None,
                        )
                        .add_error(
                            "Invalid floating point literal",
                            Some(self.source_manager.fix_span(span)),
                        )
                        .commit();
                    return true;
                }
            }

            tokens.push(Token::new(TokenKind::FloatingPoint, span));
            return true;
        }

        let span = self.skip_while(|c| c.is_ascii_digit() || c == '_');
        let mut span = Span::from_usize(start, span.end as usize);
        if self.peek_char() == Some('.') {
            self.next_char();
            let dot_span = self.skip_while(|c| c.is_ascii_digit() || c == '_');
            span = Span::from_usize(start, dot_span.end as usize);
        }

        if self.peek_char() == Some('e') || self.peek_char() == Some('E') {
            self.next_char();
            if self.peek_char() == Some('+') || self.peek_char() == Some('-') {
                self.next_char();
            }
            let e_span = self.skip_while(|c| c.is_ascii_digit() || c == '_');
            span = Span::from_usize(start, e_span.end as usize as usize);
            tokens.push(Token::new(TokenKind::FloatingPoint, span));
            return true;
        }

        let slice: Vec<u8> = self.source_manager[span]
            .iter()
            .filter(|c| **c != b'_')
            .map(|c| *c)
            .collect();

        let mut dot_count = 0;
        for (index, byte) in slice.iter().enumerate() {
            if *byte == b'.' {
                dot_count += 1;
            }

            let start = span.start as usize + 2 + 1;
            let span = Span::from_usize(start + index, start + index + 1);

            if dot_count > 1 {
                let info = self.source_manager.get_source_info(span);
                self.diagnostics
                    .builder()
                    .report(
                        DiagnosticLevel::Error,
                        "Invalid floating point literal",
                        info,
                        None,
                    )
                    .add_error(
                        "Invalid floating point literal",
                        Some(self.source_manager.fix_span(span)),
                    )
                    .commit();
                return true;
            }
        }

        tokens.push(Token::new(TokenKind::FloatingPoint, span));

        return true;
    }

    fn lex_number(&mut self, tokens: &mut Vec<Token>) -> bool {
        let start = self.cursor;
        if self.lex_integer(tokens) {
            return true;
        }

        self.cursor = start;
        self.lex_floating_point(tokens)
    }

    fn lex_formatting_string(&mut self) -> Vec<Token> {
        self.lex_helper(vec!['}'], false)
    }

    fn lex_double_quoted_string(&mut self, tokens: &mut Vec<Token>) {
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
            let ch = self.peek_char();
            if ch.is_none() {
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
            }

            let ch = unsafe { ch.unwrap_unchecked() };

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
                self.paren_balance
                    .push((TokenKind::OpenBrace, Span::from_usize(end, end + 1)));
                let mut has_error = false;

                {
                    let span = Span::from_usize(start, end);
                    format_tokens.push(Token::new(TokenKind::String, span));
                    let temp_format_tokens = self.lex_formatting_string();

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
                    'n' | 'r' | 't' | '0' | '\\' | '\'' | '"' | 'x' => {
                        is_escaping = false;
                        self.next_char();
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
            } else {
                self.next_char();
            }
        }

        if found_format_string {
            tokens.push(start_format_token);
        }

        format_tokens = format_tokens.into_iter().filter(|t| !t.span.is_empty()).collect();
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
            let ch = self.peek_char();
            if ch.is_none() {
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
            }

            let ch = unsafe { ch.unwrap_unchecked() };

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
                    'n' | 'r' | 't' | '0' | '\\' | '\'' | '"' | 'x' => {
                        is_escaping = false;
                        self.next_char();
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
            } else {
                self.next_char();
            }
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

            let ch = self.peek_char();
            if ch.is_none() {
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
            }

            let ch = unsafe { ch.unwrap_unchecked() };

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
        let ch = self.peek_char();
        if ch.is_none() {
            return;
        }

        let ch = unsafe { ch.unwrap_unchecked() };

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

        let next = self.peek_char();

        if next.is_none() {
            return false;
        }

        let next = unsafe { next.unwrap_unchecked() };

        next == '/' || next == '*'
    }

    fn lex_back_tick(&mut self, tokens: &mut Vec<Token>) {
        if ParenMatching::is_triple_back_tick_block(
            &self.source_manager.get_source(),
            self.cursor,
            b"```",
        ) {
            let dummy = (TokenKind::Unknown, Span::from_usize(0, 0));
            let last = self.paren_balance.last().unwrap_or(&dummy);
            let span = Span::from_usize(self.cursor, self.cursor + 3);
            self.cursor += 3;
            tokens.push(Token::new(TokenKind::TripleBackTick, span));

            let (token, _) = last;

            if token == &TokenKind::TripleBackTick {
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
                    self.paren_balance.push((TokenKind::TripleBackTick, span));
                }
            }
        } else {
            let span = Span::from_usize(self.cursor, self.cursor + 1);
            tokens.push(Token::new(TokenKind::Backtick, span));
            self.next_char();
        }
    }

    fn expect_block_or_paren(&mut self, kind: TokenKind) {
        let token_name = ParenMatching::get_token_name(kind);
        let other_paren = ParenMatching::get_other_pair(kind);

        if let Some((paren, span)) = self.paren_balance.last() {
            let paren_str = ParenMatching::to_string(kind);
            let other_paren_str =
                ParenMatching::to_string(other_paren.unwrap_or(TokenKind::Unknown));

            if Some(*paren) != other_paren {
                let info = self
                    .source_manager
                    .get_source_info(Span::from_usize(self.cursor, self.cursor + 1));
                self.diagnostics
                    .builder()
                    .report(
                        DiagnosticLevel::Error,
                        format!("Unmatched {token_name} '{paren_str}'"),
                        info,
                        None,
                    )
                    .add_error(
                        format!("Add a matching pair '{other_paren_str}'"),
                        Some(
                            self.source_manager
                                .fix_span(Span::from_usize(self.cursor, self.cursor + 1)),
                        ),
                    )
                    .commit();

                let token_name = ParenMatching::get_token_name(*paren);
                let info = self.source_manager.get_source_info(*span);
                self.diagnostics
                    .builder()
                    .report(
                        DiagnosticLevel::Info,
                        format!("Change or close the last opened {token_name}"),
                        info,
                        None,
                    )
                    .add_warning(
                        format!(
                            "Last opened {token_name} '{}'",
                            ParenMatching::to_string(*paren)
                        ),
                        Some(self.source_manager.fix_span(*span)),
                    )
                    .commit();
                return;
            }

            self.paren_balance.pop();
        } else {
            let info = self
                .source_manager
                .get_source_info(Span::from_usize(self.cursor, self.cursor + 1));
            let paren = ParenMatching::to_string(kind);
            let other_paren = ParenMatching::to_string(
                ParenMatching::get_other_pair(kind).unwrap_or(TokenKind::Unknown),
            );
            self.diagnostics
                .builder()
                .report(
                    DiagnosticLevel::Error,
                    format!("Unmatched {token_name} '{paren}'"),
                    info,
                    None,
                )
                .add_error(
                    format!("Add a matching pair '{other_paren}'"),
                    Some(
                        self.source_manager
                            .fix_span(Span::from_usize(self.cursor, self.cursor + 1)),
                    ),
                )
                .commit();
        }
    }

    fn lex_helper(&mut self, until: Vec<char>, should_check_paren: bool) -> Vec<Token> {
        let mut tokens = Vec::new();
        loop {
            let ch = self.peek_char();

            if ch.is_none() {
                if until.is_empty() {
                    tokens.push(Token::new_eof(Span::from_usize(self.cursor, self.cursor)));
                }
                break;
            }

            let ch = unsafe { ch.unwrap_unchecked() };

            if ch.is_ascii_whitespace() {
                self.skip_whitespace().map(|token| tokens.push(token));
                continue;
            }

            // if !self.paren_balance.is_empty() {
            //     let temp = &self.source_manager.get_source()[self.cursor..(self.cursor + 10).min(self.source_manager.len())];
            //     let temp = std::str::from_utf8(temp).unwrap();
            //     println!("str: {temp}");
            //     println!("paren_balance: {:#?}", self.paren_balance.last());
            // }

            if until.contains(&ch) {
                break;
            }

            let mut should_run_custom_match = true;

            if ch == '.' {
                let current = self.cursor;
                self.next_char();
                if let Some(c) = self.peek_char() {
                    if c.is_ascii_digit() {
                        should_run_custom_match = false;
                    }
                }
                
                self.cursor = current;
                
                if self.cursor != 0 && !self.source_manager.get_source()[self.cursor - 1].is_ascii_whitespace() {
                    should_run_custom_match = true;
                }
            }

            if should_run_custom_match {
                if let (Some(token), len) =
                    self.match_custom_operator(&self.source_manager[self.cursor..])
                {
                    tokens.push(token);
                    self.cursor += len;
                    continue;
                }
    
                if let (Some(token), len) =
                    self.match_custom_keyword(&self.source_manager[self.cursor..])
                {
                    tokens.push(token);
                    self.cursor += len;
                    continue;
                }
            }

            match ch {
                c if self.is_valid_comment(c) => {
                    self.lex_comment(&mut tokens);
                }
                c if is_valid_identifier_start_code_point(c) => {
                    self.lex_identifier(&mut tokens, true);
                }
                c if c.is_ascii_digit() || !should_run_custom_match => {
                    self.lex_number(&mut tokens);
                }
                '$' => {
                    let span = Span::from_usize(self.cursor, self.cursor + 1);
                    tokens.push(Token::new(TokenKind::Dollar, span));
                    self.next_char();
                }
                '(' => {
                    let span = Span::from_usize(self.cursor, self.cursor + 1);
                    self.paren_balance.push((TokenKind::OpenParen, span));
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
                        self.paren_balance.push((TokenKind::OpenDoubleBrace, span));
                        tokens.push(Token::new(TokenKind::OpenDoubleBrace, span));
                    } else {
                        self.paren_balance.push((TokenKind::OpenBrace, span));
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
                    self.paren_balance.push((TokenKind::OpenBracket, span));
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
                    let span = self.skip_while(|c| c == ',');
                    tokens.push(Token::new(TokenKind::Comma, span));
                }
                ';' => {
                    let span = self.skip_while(|c| c == ';');
                    tokens.push(Token::new(TokenKind::Semicolon, span));
                }
                ':' => {
                    let span = self.skip_while(|c| c == ':');
                    tokens.push(Token::new(TokenKind::Colon, span));
                }
                c if Identifier::is_operator_start_code_point(c) => {
                    let span =
                        self.skip_while(|c| Identifier::is_operator_continuation_code_point(c));
                    tokens.push(Token::new(TokenKind::Operator, span));
                }
                '"' => {
                    self.lex_double_quoted_string(&mut tokens);
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
                    self.lex_back_tick(&mut tokens);
                }
                _ => {
                    self.check_balanced_paren(&until);
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

        if should_check_paren {
            self.check_balanced_paren(&until);
        }

        tokens
    }

    fn match_custom_operator(&self, source: &[u8]) -> (Option<Token>, usize) {
        let mut valid_op = None;
        let mut len = 0;

        for op in self.custom_operators.iter() {
            let bytes = op.to_str(&self.source_manager);
            let bytes = bytes.as_bytes();
            if bytes.len() > source.len() {
                continue;
            }
            let source = &source[..bytes.len()];
            let start = self.cursor;
            let end = start + bytes.len();
            if bytes == source {
                valid_op = Some(Token::new(
                    TokenKind::Operator,
                    Span::from_usize(start, end),
                ));
                len = bytes.len();
                break;
            }
        }

        (valid_op, len)
    }

    fn match_custom_keyword(&self, source: &[u8]) -> (Option<Token>, usize) {
        let mut valid_kw = None;
        let mut len = 0;

        for kw in self.custom_keywords.iter() {
            let bytes = kw.to_str(&self.source_manager).as_bytes();
            if bytes.len() > source.len() {
                continue;
            }
            let source = &source[..bytes.len()];
            let start = self.cursor;
            let end = start + bytes.len();
            if bytes == source {
                valid_kw = Some(Token::new(
                    TokenKind::CustomKeyword,
                    Span::from_usize(start, end),
                ));
                len = bytes.len();
                break;
            }
        }

        (valid_kw, len)
    }

    fn lex_custom_operator(&mut self, operator_span: Span) -> Vec<Token> {
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
            let ch = self.peek_char();
            if ch.is_none() {
                break;
            }

            let ch = unsafe { ch.unwrap_unchecked() };

            if ch.is_ascii_whitespace() {
                self.skip_whitespace().map(|token| tokens.push(token));
                continue;
            }

            // operator ('operator') { ... }

            match ch {
                '(' => {
                    let span = Span::from_usize(self.cursor, self.cursor + 1);
                    self.paren_balance.push((TokenKind::OpenParen, span));
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
                _ if is_valid_operator_start_code_point(ch) => {
                    let start = self.cursor;
                    self.next_char();
                    let mut span = self.skip_while(|c| {
                        is_valid_operator_continuation_code_point(c) || c.is_ascii_whitespace()
                    });
                    span.start = start as u32;
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
                        break;
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
                        break;
                    }

                    let slice_iter = slice.iter().filter(|c| !c.is_ascii_whitespace()).enumerate();
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
                        break;
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
                        let op_span = Span::new(underscore[0].end, underscore[2].start).trim(&self.source_manager);
                        let op = CustomOperator::Infix(Identifier::new(op_span));
                        self.custom_operators.push(op);
                    } else if is_start {
                        let op_span =
                            Span::new(underscore[0].end, (start + slice.len()) as u32).trim(&self.source_manager);
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

                    operator_found = true;
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
            let ch = self.peek_char();
            if ch.is_none() {
                break;
            }

            let ch = unsafe { ch.unwrap_unchecked() };

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
                    self.custom_keywords
                        .push(Identifier::new(span));
                    keyword_found = true;
                }
                '(' => {
                    let span = Span::from_usize(self.cursor, self.cursor + 1);
                    self.paren_balance.push((TokenKind::OpenParen, span));
                    tokens.push(Token::new(TokenKind::OpenParen, span));
                    self.next_char();
                }
                ')' => {
                    let span = Span::from_usize(self.cursor, self.cursor + 1);
                    self.expect_block_or_paren(TokenKind::CloseParen);
                    tokens.push(Token::new(TokenKind::CloseParen, span));
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
        let cursor = self.cursor;
        let mut tokens = self.lex_helper(Vec::new(), true);
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
