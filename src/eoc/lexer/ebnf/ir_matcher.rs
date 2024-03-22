use crate::eoc::{
    lexer::{str_utils::ByteToCharIter, token::TokenKind},
    utils::{diagnostic::Diagnostic, span::Span, string::UniqueString},
};

use super::{
    ast::RelativeSourceManager,
    basic::{EbnfIdentifierMatcher, EbnfNodeMatcher, LexerEbnfMatcher, LexerMatchResult},
    default_matcher::DefaultLexerEbnfParserMatcher,
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

    pub(crate) fn match_number<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
    ) -> Option<(&'b [u8], TokenKind)> {
        let def = DefaultLexerEbnfParserMatcher::new();
        if let Some(s) = def.match_native_integer(s, source_manager, diagnostic, None) {
            return Some((s, TokenKind::Integer));
        }

        if let Some(s) = def.match_native_floating_point(s, source_manager, diagnostic, None) {
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
            &IRLexerEbnfParserMatcher::new(),
            s,
            source_manager,
            diagnostic,
            state,
        )
    }

    fn is_ir(&self) -> bool {
        true
    }

    // fn match_expr_for<'a>(
    //     &self,
    //     var: &str,
    //     s: &'a [u8],
    //     source_manager: RelativeSourceManager<'a>,
    //     diagnostic: &Diagnostic,
    // ) -> LexerMatchResult {
    //     match var {
    //         _ if var == NATIVE_CALL_KIND_ID.identifier_sym => self
    //             .match_identifier(s, source_manager, diagnostic)
    //             .map(|s| smallvec![(s, TokenKind::Identifier)])
    //             .unwrap_or_default(),
    //         _ if var == NATIVE_CALL_KIND_ID.operator_sym => self
    //             .match_operator(s, source_manager, diagnostic)
    //             .map(|s| smallvec![(s, TokenKind::Operator)])
    //             .unwrap_or_default(),
    //         _ if var == NATIVE_CALL_KIND_ID.fp_sym => self
    //             .match_number(s, source_manager, diagnostic)
    //             .map(|(s, _)| smallvec![(s, TokenKind::FloatingPoint)])
    //             .unwrap_or_default(),
    //         _ if var == NATIVE_CALL_KIND_ID.integer_sym => self
    //             .match_number(s, source_manager, diagnostic)
    //             .map(|(s, _)| smallvec![(s, TokenKind::Integer)])
    //             .unwrap_or_default(),
    //         _ if var == NATIVE_CALL_KIND_ID.string_sym => self
    //             .match_string_literal(s, source_manager, diagnostic)
    //             .map(|s| smallvec![(s, TokenKind::String)])
    //             .unwrap_or_default(),
    //         _ if var == NATIVE_CALL_KIND_ID.char_sym => self
    //             .match_character_literal(s, source_manager, diagnostic)
    //             .map(|s| smallvec![(s, TokenKind::Char)])
    //             .unwrap_or_default(),
    //         _ => smallvec![],
    //     }
    // }

    fn try_match_expr<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
        state: Option<&LexerVmState>,
    ) -> LexerMatchResult {
        todo!("Implement this")
        // for k in &[
        //     NATIVE_CALL_KIND_ID.identifier_sym,
        //     NATIVE_CALL_KIND_ID.operator_sym,
        //     NATIVE_CALL_KIND_ID.fp_sym,
        //     NATIVE_CALL_KIND_ID.integer_sym,
        //     NATIVE_CALL_KIND_ID.string_sym,
        //     NATIVE_CALL_KIND_ID.char_sym,
        // ] {
        //     if let Some(s) = self.match_expr_for(k.as_str(), s, source_manager, diagnostic) {
        //         return Some((s, TokenKind::CustomToken(*k)));
        //     }
        // }
        // None
    }

    fn match_for<'b>(
        &self,
        addr: usize,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
    ) -> LexerMatchResult {
        todo!("Implement this")
    }
}

impl EbnfIdentifierMatcher for IRLexerEbnfParserMatcher {
    fn get_identifier(&self, _name: UniqueString) -> Option<usize> {
        None
    }
}

impl EbnfNodeMatcher for IRLexerEbnfParserMatcher {
    fn get_node(&self, id: usize) -> Option<&VmNode> {
        todo!()
    }
}
