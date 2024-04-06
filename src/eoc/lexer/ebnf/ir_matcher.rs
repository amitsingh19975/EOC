use crate::eoc::{
    lexer::{str_utils::ByteToCharIter, token::{Token, TokenKind}, utils::{ParenMatching, ParenStack}},
    utils::{diagnostic::{Diagnostic, DiagnosticLevel}, source_manager::{RelativeSourceManager, SourceManager}, span::Span, string::UniqueString},
};

use super::{
    basic::{EbnfIdentifierMatcher, EbnfNodeMatcher, LexerEbnfMatcher, LexerMatchResult},
    native_call::LexerNativeCallKind,
    vm::VmNode,
    vm_state::LexerVmState,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct IRLexerEbnfParserMatcher;

impl IRLexerEbnfParserMatcher {
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

    fn validate_type_content(&self, parens: &Vec<Token>, kind: TokenKind, source_manager: &SourceManager, diagnostic: &Diagnostic) {
        let other_kind = ParenMatching::get_other_pair(kind).unwrap();
        let token = parens.last().expect("validate_type_content: last").clone();
        if token.kind == other_kind {
            let info = source_manager.get_source_info(token.span);
            diagnostic
                .builder()
                .report(
                    DiagnosticLevel::Error,
                    "Empty type content",
                    info,
                    None,
                )
                .add_error(
                    "Empty type content",
                    Some(source_manager.fix_span(token.span)),
                )
                .commit();
        }
    }

    pub(crate) fn lex_type_content (
        &self,
        cursor: &mut usize,
        source_manager: &SourceManager,
        diagnostic: &Diagnostic,
    ) -> Vec<Token> {
        let mut tokens = Vec::new();
        let mut current_paren_stack = ParenStack::new();

        let mut temp_cursor = *cursor;
        let s = source_manager.get_source();

        if s[temp_cursor] != b'<' {
            return tokens;
        } else {
            let span = Span::from_usize(temp_cursor, temp_cursor + 1);
            current_paren_stack.push(TokenKind::OpenAngle, span);
            tokens.push(Token::new(TokenKind::OpenAngle, span));
            temp_cursor += 1;
        }

        while !current_paren_stack.is_empty() {
            if temp_cursor >= s.len() {
                let temp_span = Span::from_usize(temp_cursor - 1, temp_cursor);
                let info = source_manager.get_source_info(temp_span);
                diagnostic
                    .builder()
                    .report(
                        DiagnosticLevel::Error,
                        "Unterminated type content",
                        info,
                        None,
                    )
                    .add_error(
                        "Unterminated type content",
                        Some(source_manager.fix_span(temp_span)),
                    )
                    .commit();
                break;
            }

            let c = s[temp_cursor];

            {
                let Some(c) = ByteToCharIter::new(&s[temp_cursor..]).next() else {
                    break;
                };

                if c.is_whitespace() {
                    temp_cursor += c.len_utf8();
                    continue;
                }
            }

            match c {
                b'<' => {
                    let span = source_manager.make_span(temp_cursor, temp_cursor + 1);
                    current_paren_stack.push(TokenKind::OpenAngle, span);
                    tokens.push(Token::new(TokenKind::OpenAngle, span));
                }
                b'>' => {
                    self.validate_type_content(&tokens, TokenKind::CloseAngle, source_manager, diagnostic);
                    let span = source_manager.make_span(temp_cursor, temp_cursor + 1);
                    current_paren_stack.expect(TokenKind::CloseAngle, span, source_manager.into(), diagnostic);
                    tokens.push(Token::new(TokenKind::CloseAngle, span));
                }
                b'(' => {
                    let span = source_manager.make_span(temp_cursor, temp_cursor + 1);
                    current_paren_stack.push(TokenKind::OpenParen, span);
                    tokens.push(Token::new(TokenKind::OpenParen, span));
                }
                b')' => {
                    self.validate_type_content(&tokens, TokenKind::CloseParen, source_manager, diagnostic);
                    let span = source_manager.make_span(temp_cursor, temp_cursor + 1);
                    current_paren_stack.expect(TokenKind::CloseParen, span, source_manager.into(), diagnostic);
                    tokens.push(Token::new(TokenKind::CloseParen, span));
                }
                b'[' => {
                    let span = source_manager.make_span(temp_cursor, temp_cursor + 1);
                    current_paren_stack.push(TokenKind::OpenBracket, span);
                    tokens.push(Token::new(TokenKind::OpenBracket, span));
                }
                b']' => {
                    self.validate_type_content(&tokens, TokenKind::CloseBracket, source_manager, diagnostic);
                    let span = source_manager.make_span(temp_cursor, temp_cursor + 1);
                    current_paren_stack.expect(TokenKind::CloseBracket, span, source_manager.into(), diagnostic);
                    tokens.push(Token::new(TokenKind::CloseBracket, span));
                }
                b'{' => {
                    let span = source_manager.make_span(temp_cursor, temp_cursor + 1);
                    current_paren_stack.push(TokenKind::OpenBrace, span);
                    tokens.push(Token::new(TokenKind::OpenBrace, span));
                }
                b'}' => {
                    self.validate_type_content(&tokens, TokenKind::CloseBrace, source_manager, diagnostic);
                    let span = source_manager.make_span(temp_cursor, temp_cursor + 1);
                    current_paren_stack.expect(TokenKind::CloseBrace, span, source_manager.into(), diagnostic);
                    tokens.push(Token::new(TokenKind::CloseBrace, span));
                }
                _ => {
                    // AnyThing except '<', '>', '(', ')', '[', ']', '{', '}', and '\0'
                    const INVALID_CHARS: &[u8] = &[b'<', b'>', b'(', b')', b'[', b']', b'{', b'}'];
                    let start = temp_cursor;
                    while temp_cursor < s.len() {
                        let c = s[temp_cursor];
                        if INVALID_CHARS.contains(&c) {
                            break;
                        }
                        temp_cursor += 1;
                    }

                    let span = source_manager.make_span(start, temp_cursor);
                    tokens.push(Token::new(TokenKind::TypeContent, span));
                    temp_cursor -= 1;
                }
            }

            temp_cursor += 1;
            
        }

        *cursor = temp_cursor;
        current_paren_stack.check_balanced(&[], source_manager, diagnostic);
        return tokens;
    }
}

impl LexerEbnfMatcher for IRLexerEbnfParserMatcher {
    fn match_native<'b>(
        &self,
        kind: LexerNativeCallKind,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
        state: Option<&LexerVmState>,
    ) -> Option<(&'b [u8], TokenKind)> {
        
        kind.call(
            self,
            s,
            source_manager,
            diagnostic,
            state,
        )
    }

    fn is_ir(&self) -> bool {
        true
    }

    fn try_match_expr<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
        state: Option<&LexerVmState>,
    ) -> LexerMatchResult {
        if s[0] == b'"' {
            if let Some(s) = self.match_string_literal(s, source_manager, diagnostic) {
                return Some((Span::from_usize(0, s.len()), TokenKind::String));
            }
        } else if s[0] == b'\'' {
            if let Some(s) = self.match_character_literal(s, source_manager, diagnostic) {
                return Some((Span::from_usize(0, s.len()), TokenKind::Char));
            }
        }

        for k in [
            LexerNativeCallKind::Identifier,
            LexerNativeCallKind::Number,
        ] {
            if let Some((s, k)) = self.match_native(k, s, source_manager, diagnostic, state) {
                return Some((Span::from_usize(0, s.len()), k));
            }
        }
        None
    }

    fn match_for<'b>(
        &self,
        _addr: usize,
        _s: &'b [u8],
        _source_manager: RelativeSourceManager<'b>,
        _diagnostic: &Diagnostic,
    ) -> LexerMatchResult {
        unreachable!("IRLexerEbnfParserMatcher::match_for")
    }
    
    fn is_default(&self) -> bool {
        false
    }

    fn is_scoped(&self) -> bool {
        false
    }
}

impl EbnfIdentifierMatcher for IRLexerEbnfParserMatcher {
    fn get_identifier(&self, _name: UniqueString) -> Option<usize> {
        None
    }
}

impl EbnfNodeMatcher for IRLexerEbnfParserMatcher {
    fn get_node(&self, _id: usize) -> Option<&VmNode> {
        None
    }
}
