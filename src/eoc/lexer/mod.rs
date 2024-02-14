use std::{path::Path, vec};
use self::{token::{Token, TokenKind}, utils::{is_valid_identifier_continuation_code_point, is_valid_identifier_start_code_point, ParenMatching}};
use super::{ast::identifier::Identifier, utils::{diagnostic::{Diagnostic, DiagnosticLevel, DiagnosticReporter}, source_manager::SourceManager, span::{self, Span}}};
pub(crate) mod token;
pub(crate) mod utils;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct CustomOperator {
    pub(crate) identifier: Identifier,
    pub(crate) span: Span
}

pub(crate) struct Lexer {
    source_manager: SourceManager,
    cursor: usize,
    paren_balance: Vec<(TokenKind, Span)>,
    diagnostics: Diagnostic,
    custom_operators: Vec<CustomOperator>,
}

impl Lexer {
    pub(crate) fn new<D: Into<Diagnostic>>(source_manager: SourceManager, diagnostic: D) -> Self {
        Self {
            source_manager,
            cursor: 0,
            paren_balance: Vec::new(),
            diagnostics: diagnostic.into(),
            custom_operators: Vec::new(),
        }
    }

    pub(crate) fn new_from_filepath<P: AsRef<Path>, D: Into<Diagnostic>>(path: P, diagnostic: D) -> Result<Self, std::io::Error> {
        let source_manager = SourceManager::new(path)?;
        Ok(Self::new(source_manager, diagnostic))
    }

    fn next_char(&mut self) -> Option<char> {
        let (ch, len) = self.source_manager.get_char(self.cursor) ;
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

        let ch = unsafe {
            ch.unwrap_unchecked()
        };

        Some(match ch {
            '\n' | '\r' => {
                let span = self.skip_while(|byte| (byte == '\n') || (byte == '\r'));
                let repeat = span.len() as u32;
                Token::new_with_repeat(TokenKind::Newline, span, b"\\n", repeat)
            }

            ' ' => {
                let span = self.skip_while(|byte| (byte == ' '));
                let repeat = span.len() as u32;
                Token::new_with_repeat(TokenKind::Space, span, b" ", repeat)
            }
            '\t' => {
                let span = self.skip_while(|byte| (byte == '\t'));
                let repeat = span.len() as u32;
                Token::new_with_repeat(TokenKind::TabSpace, span, b"\\t", repeat)
            }
            _ => {
                let span = self.skip_while(|byte| byte.is_ascii_whitespace());
                let repeat = span.len() as u32;
                let slice = &self.source_manager[span];
                Token::new_with_repeat(TokenKind::Whitespace, span, slice, repeat)
            }
        })
    }

    fn check_balanced_paren(&mut self, until_chars: &[char]) {
        for (token, span) in self.paren_balance.iter().rev() {
            if until_chars.contains(&ParenMatching::to_str(*token).chars().next().unwrap()) {
                continue;
            }
            
            let other_kind = ParenMatching::get_other_pair(*token).unwrap_or(TokenKind::Dot);
            if until_chars.contains(&ParenMatching::to_str(other_kind).chars().next().unwrap()) {
                continue;
            }

            let token_name = ParenMatching::get_token_name(*token);

            self.diagnostics.builder()
                .report(DiagnosticLevel::Error, format!("Unmatched {token_name}"), self.source_manager.get_source_info(*span), None)
                .add_error(format!("Remove or add matching {token_name}"), Some(self.source_manager.fix_span(*span)))
                .commit();
        }

        self.paren_balance.clear();
    }

    pub(crate) fn get_custom_operators(&self) -> &Vec<CustomOperator> {
        &self.custom_operators
    }

    fn lex_identifier(&mut self, tokens: &mut Vec<Token>, should_parse_nested_operator: bool) {
        let span = self.skip_while(|c| is_valid_identifier_continuation_code_point(c));
        let slice = &self.source_manager[span];
        let kind: TokenKind = slice.into();
        tokens.push(Token::new(kind, span, slice));

        if should_parse_nested_operator {
            if b"operator" == slice {
                let new_tokens = self.lex_custom_operator();
                tokens.extend(new_tokens);
            }
        }
    }

    fn lex_dot(&mut self, tokens: &mut Vec<Token>) {
        let span = self.skip_while(|c| c == '.');
        let len = span.len();
        if let Some(next_char) = self.peek_char() {
            if next_char.is_ascii_digit() && len == 1 {
                self.lex_number(tokens);
                return;
            }
        }

        if span.len() == 3 {
            let slice = &self.source_manager[span];
            tokens.push(Token::new(TokenKind::Ellipsis, span, slice));
        } else {
            let slice = &self.source_manager[span];
            tokens.push(Token::new_with_repeat(TokenKind::Dot, span, slice, len as u32));
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

        let ch = self.peek_char();
        if ch.is_none() {
            return false;
        }

        let ch = unsafe {
            ch.unwrap_unchecked()
        };

        if !ch.is_ascii_digit() {
            return false;
        }

        let start = self.cursor;
        self.next_char();

        if self.peek_char() == Some('x') {
            self.next_char();
            let span = self.skip_while(|c| c.is_ascii_hexdigit() || c == '_');
            if self.peek_char() == Some('.') {
                self.cursor = start;
                return false;
            }

            let span = Span::from_usize(start, span.end as usize);
            let slice: Vec<u8> = self.source_manager[span].iter().filter(|c| **c != b'_').map(|c| *c).collect();
            tokens.push(Token::new(TokenKind::Integer, span, slice));
        } else if self.peek_char() == Some('o') {
            self.next_char();
            let span = self.skip_while(|c| c.is_ascii_digit() || c == '_');
            let span = Span::from_usize(start, span.end as usize);
            let slice: Vec<u8> = self.source_manager[span].iter().filter(|c| **c != b'_').map(|c| *c).collect();
            for (index, byte) in slice.iter().enumerate() {
                if *byte > b'7' {
                    let start = span.start as usize + 2 + 1;
                    let span = Span::from_usize(start + index, start + index + 1);
                    let info = self.source_manager.get_source_info(span);
                    self.diagnostics.builder()
                        .report(DiagnosticLevel::Error, "Invalid octal digit", info, None)
                        .add_error("Octal digit must be between 0 and 7", Some(self.source_manager.fix_span(span)))
                        .commit();
                    return true;
                }
            }
            tokens.push(Token::new(TokenKind::Integer, span, slice));
        } else if self.peek_char() == Some('b') {
            self.next_char();
            let span = self.skip_while(|c| c == '0' || c == '1' || c == '_');
            let span = Span::from_usize(start, span.end as usize);
            let slice: Vec<u8> = self.source_manager[span].iter().filter(|c| **c != b'_').map(|c| *c).collect();
            tokens.push(Token::new(TokenKind::Integer, span, slice));
        } else {
            let span = self.skip_while(|c| c.is_ascii_digit() || c == '_');
            if self.peek_char() == Some('.') {
                self.cursor = start;
                return false;
            }
            let span = Span::from_usize(start, span.end as usize);
            let slice: Vec<u8> = self.source_manager[span].iter().filter(|c| **c != b'_').map(|c| *c).collect();
            tokens.push(Token::new(TokenKind::Integer, span, slice));
        }
        
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

        let ch = unsafe {
            ch.unwrap_unchecked()
        };

        if !ch.is_ascii_digit() && ch != '.' {
            return false;
        }

        let start = self.cursor;
        self.next_char();

        if ch == '0' && self.peek_char() == Some('x') {
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
                let slice: Vec<u8> = self.source_manager[span].iter().filter(|c| **c != b'_').map(|c| *c).collect();
                tokens.push(Token::new(TokenKind::FloatingPoint, span, slice));
                return true;
            }

            let slice: Vec<u8> = self.source_manager[span].iter().filter(|c| **c != b'_').map(|c| *c).collect();

            let mut dot_count = 0;
            for (index, byte) in slice.iter().enumerate() {
                if *byte == b'.' {
                    dot_count += 1;
                }

                let start = span.start as usize + 2 + 1;
                let span = Span::from_usize(start + index, start + index + 1);

                if dot_count > 1 {
                    let info = self.source_manager.get_source_info(span);
                    self.diagnostics.builder()
                        .report(DiagnosticLevel::Error, "Invalid floating point literal", info, None)
                        .add_error("Invalid floating point literal", Some(self.source_manager.fix_span(span)))
                        .commit();
                    return true;
                }
            }

            tokens.push(Token::new(TokenKind::FloatingPoint, span, slice));
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
            let slice: Vec<u8> = self.source_manager[span].iter().filter(|c| **c != b'_').map(|c| *c).collect();
            tokens.push(Token::new(TokenKind::FloatingPoint, span, slice));
            return true;
        }

        let slice: Vec<u8> = self.source_manager[span].iter().filter(|c| **c != b'_').map(|c| *c).collect();

        let mut dot_count = 0;
        for (index, byte) in slice.iter().enumerate() {
            if *byte == b'.' {
                dot_count += 1;
            }

            let start = span.start as usize + 2 + 1;
            let span = Span::from_usize(start + index, start + index + 1);

            if dot_count > 1 {
                let info = self.source_manager.get_source_info(span);
                self.diagnostics.builder()
                    .report(DiagnosticLevel::Error, "Invalid floating point literal", info, None)
                    .add_error("Invalid floating point literal", Some(self.source_manager.fix_span(span)))
                    .commit();
                return true;
            }
        }

        tokens.push(Token::new(TokenKind::FloatingPoint, span, slice));

        return true;
    }

    fn lex_number(&mut self, tokens: &mut Vec<Token>) {
        if self.lex_integer(tokens) {
            return;
        }

        self.lex_floating_point(tokens);
    }

    fn lex_formatting_string(&mut self) -> Vec<Token> {
        self.lex_helper(vec!['}']).0
    }

    fn lex_double_quoted_string(&mut self, tokens: &mut Vec<Token>) {
        if self.peek_char() != Some('"') {
            return;
        }

        let mut is_escaping = false;
        let mut is_escaping_formatting = false;

        tokens.push(Token::new(TokenKind::StartFormattingString, Span::from_usize(self.cursor, self.cursor + 1), b"\""));

        self.next_char();
        let mut start = self.cursor;
        let mut end = self.cursor;
        

        loop {
            let ch = self.peek_char();
            if ch.is_none() {
                let info = self.source_manager.get_source_info(Span::from_usize(start, end));
                self.diagnostics.builder()
                    .report(DiagnosticLevel::Error, "Unterminated string", info, None)
                    .add_error("Unterminated string", Some(self.source_manager.fix_span(Span::from_usize(start, end))))
                    .commit();
                break;
            }

            let ch = unsafe {
                ch.unwrap_unchecked()
            };
            
            if ch == '{' {
                let end = self.cursor;
                self.next_char();
                if self.peek_char() == Some('{')  {
                    is_escaping_formatting = true;
                    self.next_char();
                    continue;
                }

                tokens.push(Token::new(TokenKind::String, Span::from_usize(start, end), &self.source_manager[Span::from_usize(start, end)]));
                let format_tokens = self.lex_formatting_string();
                tokens.extend(format_tokens);
                start = self.cursor;
                continue;
            }

            if ch == '}' && self.peek_char() == Some('}') && is_escaping_formatting {
                is_escaping_formatting = false;
                continue;
            }

            if ch == '"' && !is_escaping {
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
                        let info = self.source_manager.get_source_info(Span::from_usize(self.cursor, self.cursor + 1));
                        self.diagnostics.builder()
                            .report(DiagnosticLevel::Error, "Invalid escape sequence", info, None)
                            .add_error("Invalid escape sequence", Some(self.source_manager.fix_span(Span::from_usize(self.cursor, self.cursor + 1))))
                            .commit();
                    }       
                }
            } else {
                self.next_char();
            }
        }

        let span = Span::from_usize(start, end);
        let slice = &self.source_manager[span];
        tokens.push(Token::new(TokenKind::String, span, slice));
        tokens.push(Token::new(TokenKind::EndFormattingString, Span::from_usize(self.cursor, self.cursor + 1), b"\""));

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
                let info = self.source_manager.get_source_info(Span::from_usize(start, end));
                self.diagnostics.builder()
                    .report(DiagnosticLevel::Error, "Unterminated character literal", info, None)
                    .add_error("Unterminated character literal", Some(self.source_manager.fix_span(Span::from_usize(start, end))))
                    .commit();
                break;
            }

            let ch = unsafe {
                ch.unwrap_unchecked()
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
                    'n' | 'r' | 't' | '0' | '\\' | '\'' | '"' | 'x' => {
                        is_escaping = false;
                        self.next_char();   
                    }
                    _ => {
                        let info = self.source_manager.get_source_info(Span::from_usize(self.cursor, self.cursor + 1));
                        self.diagnostics.builder()
                            .report(DiagnosticLevel::Error, "Invalid escape sequence", info, None)
                            .add_error("Invalid escape sequence", Some(self.source_manager.fix_span(Span::from_usize(self.cursor, self.cursor + 1))))
                            .commit();
                    }       
                }
            } else {
                self.next_char();
            }
        }

        let span = Span::from_usize(start, end);
        let slice = &self.source_manager[span];
        tokens.push(Token::new(TokenKind::Char, span, slice));
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


        let slice = &self.source_manager[span];
        tokens.push(Token::new(kind, span, slice));
    }
    
    fn lex_multiline_comment(&mut self, tokens: &mut Vec<Token>) {
        let mut nest_level = 1;
        let mut star_count = 1;

        while self.peek_char() == Some('*') {
            star_count += 1;
            self.next_char();
        }

        println!("star_count: {} | '{:#?}'", star_count, self.peek_char());

        let start = self.cursor;
        let mut end = self.cursor;
        loop {
            if nest_level == 0 {
                break;
            }

            let ch = self.peek_char();
            if ch.is_none() {
                let info = self.source_manager.get_source_info(Span::from_usize(self.cursor, self.cursor + 1));
                self.diagnostics.builder()
                    .report(DiagnosticLevel::Error, "Unterminated multi-line comment", info, None)
                    .add_error("Unterminated multi-line comment", Some(self.source_manager.fix_span(Span::from_usize(self.cursor, self.cursor + 1))))
                    .commit();
                break;
            }

            let ch = unsafe {
                ch.unwrap_unchecked()
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
        let slice = &self.source_manager[span];
        tokens.push(Token::new(TokenKind::MultiLineComment, span, slice));
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

        let ch = unsafe {
            ch.unwrap_unchecked()
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

        let next = self.peek_char();

        if next.is_none() {
            return false;
        }

        let next = unsafe {
            next.unwrap_unchecked()
        };

        next == '/' || next == '*'
    }

    fn lex_back_tick(&mut self, tokens: &mut Vec<Token>) {
        if ParenMatching::is_reflection_block(&self.source_manager.get_source(), self.cursor, b"```") {
            let dummy = (TokenKind::Unknown, Span::from_usize(0, 0));
            let last = self.paren_balance.last().unwrap_or(&dummy);
            let span = Span::from_usize(self.cursor, self.cursor + 3);
            self.cursor += 3;

            let (token, _) = last;

            if token == &TokenKind::ReflectionStart {
                self.expect_block_or_paren(TokenKind::ReflectionEnd);
                tokens.push(Token::new_with_repeat(TokenKind::ReflectionEnd, span, b"`", 3));
            } else {
                self.paren_balance.push((TokenKind::ReflectionStart, span));
                tokens.push(Token::new_with_repeat(TokenKind::ReflectionStart, span, b"`", 3));
            }

        } else {
            let span = Span::from_usize(self.cursor, self.cursor + 1);
            tokens.push(Token::new(TokenKind::Backtick, span, b"`"));
            self.next_char();

            if let Some(last) = self.paren_balance.last() {
                if last.0 == TokenKind::Backtick {
                    self.paren_balance.pop();
                } else {
                    self.paren_balance.push((TokenKind::Backtick, span));
                }
            }
        }
    }

    fn expect_block_or_paren(&mut self, kind: TokenKind) {
        let token_name = ParenMatching::get_token_name(kind);
        let other_paren = ParenMatching::get_other_pair(kind);

        if let Some((paren, span)) = self.paren_balance.last() {
            if Some(*paren) != other_paren {
                let info = self.source_manager.get_source_info(Span::from_usize(self.cursor, self.cursor + 1));
                let paren_str = ParenMatching::to_string(kind);
                let other_paren_str = ParenMatching::to_string(other_paren.unwrap_or(TokenKind::Unknown));
                self.diagnostics.builder()
                    .report(DiagnosticLevel::Error, format!("Unmatched {token_name} '{paren_str}'"), info, None)
                    .add_error(format!("Add a matching pair '{other_paren_str}'"), Some(self.source_manager.fix_span(Span::from_usize(self.cursor, self.cursor + 1))))
                    .commit();

                let token_name = ParenMatching::get_token_name(*paren);
                let info = self.source_manager.get_source_info(*span);
                self.diagnostics.builder()
                    .report(DiagnosticLevel::Info, format!("Change or close the last opened {token_name}"), info, None)
                    .add_warning(format!("Last opened {token_name} '{}'", ParenMatching::to_string(*paren)), Some(self.source_manager.fix_span(*span)))
                    .commit();
                return;
            }
            self.paren_balance.pop();
        } else {
            let info = self.source_manager.get_source_info(Span::from_usize(self.cursor, self.cursor + 1));
            let paren = ParenMatching::to_string(kind);
            let other_paren = ParenMatching::to_string(ParenMatching::get_other_pair(kind).unwrap_or(TokenKind::Unknown));
            self.diagnostics.builder()
                .report(DiagnosticLevel::Error, format!("Unmatched {token_name} '{paren}'"), info, None)
                .add_error(format!("Add a matching pair '{other_paren}'"), Some(self.source_manager.fix_span(Span::from_usize(self.cursor, self.cursor + 1))))
                .commit();
        }
    }
    
    fn lex_helper(&mut self, until: Vec<char>) -> (Vec<Token>, &Diagnostic) {
        let mut tokens = Vec::new();
        loop {
            let ch = self.peek_char();
            if ch.is_none() {
                if until.is_empty() {
                    tokens.push(Token::new_eof(Span::from_usize(self.cursor, self.cursor + 1)));
                }
                break;
            }

            let ch = unsafe {
                ch.unwrap_unchecked()
            };

            if ch.is_ascii_whitespace() {
                self.skip_whitespace().map(|token| tokens.push(token));
                continue;
            }

            if until.contains(&ch) {
                break;
            }

            if let (Some(token), len) = self.validate_custom_operator(&self.source_manager[self.cursor..]) {
                tokens.push(token);
                self.cursor += len;
                continue;
            }

            match ch {
                c if self.is_valid_comment(c) => {
                    self.lex_comment(&mut tokens);
                }
                c if is_valid_identifier_start_code_point(c) => {
                    self.lex_identifier(&mut tokens, true);
                }
                c if c.is_ascii_digit() => {
                    self.lex_number(&mut tokens);
                }
                '$' => {
                    let span = Span::from_usize(self.cursor, self.cursor + 1);
                    let slice = &self.source_manager[span];
                    tokens.push(Token::new(TokenKind::Dollar, span, slice));
                    self.next_char();
                }
                '(' => {
                    let span = Span::from_usize(self.cursor, self.cursor + 1);
                    self.paren_balance.push((TokenKind::OpenParen, span));
                    tokens.push(Token::new(TokenKind::OpenParen, span, b"("));
                    self.next_char();
                }
                ')' => {
                    let span = Span::from_usize(self.cursor, self.cursor + 1);
                    self.expect_block_or_paren(TokenKind::CloseParen);
                    tokens.push(Token::new(TokenKind::CloseParen, span, b")"));
                    self.next_char();
                }
                '{' => {
                    let span = Span::from_usize(self.cursor, self.cursor + 1);
                    self.paren_balance.push((TokenKind::OpenBrace, span));
                    tokens.push(Token::new(TokenKind::OpenBrace, span, b"{"));
                    self.next_char();
                }
                '}' => {
                    let span = Span::from_usize(self.cursor, self.cursor + 1);
                    self.expect_block_or_paren(TokenKind::CloseBrace);
                    tokens.push(Token::new(TokenKind::CloseBrace, span, b"}"));
                    self.next_char();
                }
                '[' => {
                    let span = Span::from_usize(self.cursor, self.cursor + 1);
                    self.paren_balance.push((TokenKind::OpenBracket, span));
                    tokens.push(Token::new(TokenKind::OpenBracket, span, b"["));
                    self.next_char();
                }
                ']' => {
                    let span = Span::from_usize(self.cursor, self.cursor + 1);
                    self.expect_block_or_paren(TokenKind::CloseBracket);
                    tokens.push(Token::new(TokenKind::CloseBracket, span, b"]"));
                    self.next_char();
                }
                '.' => {
                    self.lex_dot(&mut tokens);
                }
                ',' => {
                    let span = self.skip_while(|c| c == ',');
                    tokens.push(Token::new_with_repeat(TokenKind::Comma, span, b",", span.len() as u32));
                }
                ';' => {
                    let span = self.skip_while(|c| c == ';');
                    tokens.push(Token::new_with_repeat(TokenKind::Semicolon, span, b";", span.len() as u32));
                }
                ':' => {
                    let span = self.skip_while(|c| c == ':');
                    tokens.push(Token::new_with_repeat(TokenKind::Colon, span, b":", span.len() as u32));
                }
                c if Identifier::is_operator_start_code_point(c) => {
                    let span = self.skip_while(|c| Identifier::is_operator_continuation_code_point(c));
                    let slice = &self.source_manager[span];
                    tokens.push(Token::new(TokenKind::Operator, span, slice));
                }
                '"' => {
                    self.lex_double_quoted_string(&mut tokens);
                }
                '\'' => {
                    self.lex_character_literal(&mut tokens);
                }
                '@' => {
                    let span = Span::from_usize(self.cursor , self.cursor + 1);
                    let slice = &self.source_manager[span];
                    tokens.push(Token::new(TokenKind::AtSign, span, slice));
                    self.next_char();
                }
                '\\' => {
                    let span = Span::from_usize(self.cursor , self.cursor + 1);
                    let slice = &self.source_manager[span];
                    tokens.push(Token::new(TokenKind::Backslash, span, slice));
                    self.next_char();
                }
                '`' => {
                    self.lex_back_tick(&mut tokens);
                }
                _ => {
                    self.check_balanced_paren(&until);
                    tokens.iter().for_each(|token| println!("{}", token));
                    todo!("Implement the rest of the lexer: {} | {:?}", self.cursor, ch)
                }
            }
        }

        self.check_balanced_paren(&until);
        (tokens, &self.diagnostics)
    }

    fn validate_custom_operator(&self, source: &[u8]) -> (Option<Token>, usize){
        let mut valid_op = None;
        let mut len = 0;

        for op in self.custom_operators.iter() {
            let bytes = op.identifier.as_str().as_bytes();
            if bytes.len() > source.len() {
                continue;
            }
            let source = &source[..bytes.len()];
            let start = self.cursor;
            let end = start + bytes.len();
            if bytes == source {
                valid_op = Some(Token::new(TokenKind::Operator, Span::from_usize(start, end), bytes));
                len = bytes.len();
                break;
            }
        }

        (valid_op, len)
    }

    fn lex_custom_operator(&mut self) -> Vec<Token> {
        let mut tokens: Vec<Token> = Vec::new();

        loop {
            let ch = self.peek_char();
            if ch.is_none() {
                break;
            }

            let ch = unsafe {
                ch.unwrap_unchecked()
            };

            if ch.is_ascii_whitespace() {
                self.skip_whitespace().map(|token| tokens.push(token));
                continue;
            }

            // operator ('operator') { ... }

            match ch {
                c if Identifier::is_operator_start_code_point(c) => {
                    let span = self.skip_while(|c| Identifier::is_operator_continuation_code_point(c));
                    let slice = &self.source_manager[span];
                    tokens.push(Token::new(TokenKind::Operator, span, slice));
                    let dup = self.custom_operators.iter().filter(|op| op.identifier.as_str() == std::str::from_utf8(slice).unwrap()).last();
                    let is_invalid = dup.is_some() ||
                        slice == b"." || slice == b"...";
                    if is_invalid {
                        let info = format!("The operator '{}' is already defined", std::str::from_utf8(slice).unwrap());
                        self.diagnostics.builder()
                            .report(DiagnosticLevel::Error, "Redefinition of operator is not allowed", self.source_manager.get_source_info(span), None)
                            .add_error(info, Some(self.source_manager.fix_span(span)))
                            .commit();
                        if let Some(dup) = dup {
                            self.diagnostics.builder()
                                .report(DiagnosticLevel::Note, "Previous definition", self.source_manager.get_source_info(dup.span), None)
                                .add_note(format!("The operator '{}' is already defined", std::str::from_utf8(slice).unwrap()), Some(self.source_manager.fix_span(dup.span)))
                                .commit();
                        }
                        continue;
                    }
                    let identifier = Identifier::new(std::str::from_utf8(slice).unwrap().to_string());
                    self.custom_operators.push(CustomOperator { identifier, span })
                }
                '(' => {
                    let span = Span::from_usize(self.cursor, self.cursor + 1);
                    self.paren_balance.push((TokenKind::OpenParen, span));
                    tokens.push(Token::new(TokenKind::OpenParen, span, b"("));
                    self.next_char();
                }
                ')' => {
                    let span = Span::from_usize(self.cursor, self.cursor + 1);
                    self.expect_block_or_paren(TokenKind::CloseParen);
                    tokens.push(Token::new(TokenKind::CloseParen, span, b")"));
                    self.next_char();
                    break;
                }
                _ => {
                    break;
                }
            }
        }
        tokens
    }

    pub fn lex(&mut self) -> (Vec<Token>, &Diagnostic) {
        let cursor = self.cursor;
        let (mut tokens, diag) = self.lex_helper(Vec::new());
        let mut i = 0;
        for t in tokens.iter().rev() {
            if t.is_eof() {
                i += 1;
            } else {
                break;
            }
        }

        if i == 0 {
            tokens.push(Token::new_eof(Span::from_usize(cursor, cursor + 1)));
        } else if i > 1 {
            tokens.truncate(tokens.len() - (i - 1));
        }

        (tokens, diag)
    }

}