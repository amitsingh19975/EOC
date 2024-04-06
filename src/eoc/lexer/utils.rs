use std::borrow::Cow;

use crate::eoc::{ast::identifier::Identifier, utils::{diagnostic::{Diagnostic, DiagnosticLevel}, source_manager::{self, SourceManager}, span::Span}};

use super::token::TokenKind;

pub(crate) fn is_valid_identifier_continuation_code_point(c: char) -> bool {
    let ch = c;
    let c = c as u32;
    if c < 0x80 {
        return ch.is_alphanumeric() || ch == '_' || ch == '$';
    }
    

    // N1518: Recommendations for extended identifier characters for C and C++
    // Proposed Annex X.1: Ranges of characters allowed
    return c == 0x00A8 || c == 0x00AA || c == 0x00AD || c == 0x00AF
    || (c >= 0x00B2 && c <= 0x00B5) || (c >= 0x00B7 && c <= 0x00BA)
    || (c >= 0x00BC && c <= 0x00BE) || (c >= 0x00C0 && c <= 0x00D6)
    || (c >= 0x00D8 && c <= 0x00F6) || (c >= 0x00F8 && c <= 0x00FF)

    || (c >= 0x0100 && c <= 0x167F)
    || (c >= 0x1681 && c <= 0x180D)
    || (c >= 0x180F && c <= 0x1FFF)

    || (c >= 0x200B && c <= 0x200D)
    || (c >= 0x202A && c <= 0x202E)
    || (c >= 0x203F && c <= 0x2040)
    || c == 0x2054
    || (c >= 0x2060 && c <= 0x206F)

    || (c >= 0x2070 && c <= 0x218F)
    || (c >= 0x2460 && c <= 0x24FF)
    || (c >= 0x2776 && c <= 0x2793)
    || (c >= 0x2C00 && c <= 0x2DFF)
    || (c >= 0x2E80 && c <= 0x2FFF)

    || (c >= 0x3004 && c <= 0x3007)
    || (c >= 0x3021 && c <= 0x302F)
    || (c >= 0x3031 && c <= 0x303F)

    || (c >= 0x3040 && c <= 0xD7FF)

    || (c >= 0xF900 && c <= 0xFD3D)
    || (c >= 0xFD40 && c <= 0xFDCF)
    || (c >= 0xFDF0 && c <= 0xFE44)
    || (c >= 0xFE47 && c <= 0xFFF8)

    || (c >= 0x10000 && c <= 0x1FFFD)
    || (c >= 0x20000 && c <= 0x2FFFD)
    || (c >= 0x30000 && c <= 0x3FFFD)
    || (c >= 0x40000 && c <= 0x4FFFD)
    || (c >= 0x50000 && c <= 0x5FFFD)
    || (c >= 0x60000 && c <= 0x6FFFD)
    || (c >= 0x70000 && c <= 0x7FFFD)
    || (c >= 0x80000 && c <= 0x8FFFD)
    || (c >= 0x90000 && c <= 0x9FFFD)
    || (c >= 0xA0000 && c <= 0xAFFFD)
    || (c >= 0xB0000 && c <= 0xBFFFD)
    || (c >= 0xC0000 && c <= 0xCFFFD)
    || (c >= 0xD0000 && c <= 0xDFFFD)
    || (c >= 0xE0000 && c <= 0xEFFFD);

}

pub(crate) fn is_valid_identifier_start_code_point(c: char) -> bool {
    if !is_valid_identifier_continuation_code_point(c) {
        return false;
    }

    let ch = c;
    let c = c as u32;

    if (c < 0x80) && (ch.is_digit(10) || ch == '$') {
        return false;
    }

    // N1518: Recommendations for extended identifier characters for C and C++
    // Proposed Annex X.2: Ranges of characters disallowed initially
    if (c >= 0x0300 && c <= 0x036F) || (c >= 0x1DC0 && c <= 0x1DFF) || (c >= 0x20D0 && c <= 0x20FF) || (c >= 0xFE20 && c <= 0xFE2F)
    {
        return false;
    }

    return true;
}

pub(crate) struct ParenMatching;

impl ParenMatching {
    pub(crate) fn get_other_pair(c: TokenKind) -> Option<TokenKind> {
        match c {
            TokenKind::OpenParen => Some(TokenKind::CloseParen),
            TokenKind::CloseParen => Some(TokenKind::OpenParen),
            TokenKind::OpenBracket => Some(TokenKind::CloseBracket),
            TokenKind::CloseBracket => Some(TokenKind::OpenBracket),
            TokenKind::OpenDoubleBrace => Some(TokenKind::CloseDoubleBrace),
            TokenKind::CloseDoubleBrace => Some(TokenKind::OpenDoubleBrace),
            TokenKind::OpenBrace => Some(TokenKind::CloseBrace),
            TokenKind::CloseBrace => Some(TokenKind::OpenBrace),
            TokenKind::TripleBackTick => Some(TokenKind::TripleBackTick),
            TokenKind::OpenAngle => Some(TokenKind::CloseAngle),
            TokenKind::CloseAngle => Some(TokenKind::OpenAngle),
            _ => None,
        }
    }

    pub(crate) fn is_triple_back_tick_block(source: &[u8], cursor: usize, reflection_start_code_points: &[u8]) -> bool {
        let start = cursor;
        let end = start + reflection_start_code_points.len();
        let slice = &source[start..end.min(source.len())];
        slice == reflection_start_code_points
    }

    pub(crate) fn to_string(t: TokenKind) -> String {
        Self::to_str(t).to_string()
    }

    pub(crate) fn to_str(t: TokenKind) -> &'static str {
        match t {
            TokenKind::OpenParen => "(",
            TokenKind::CloseParen => ")",
            TokenKind::OpenDoubleBrace => "{{",
            TokenKind::CloseDoubleBrace => "}}",
            TokenKind::OpenBracket => "[",
            TokenKind::CloseBracket => "]",
            TokenKind::OpenBrace => "{",
            TokenKind::CloseBrace => "}",
            TokenKind::OpenAngle => "<",
            TokenKind::CloseAngle => ">",
            TokenKind::TripleBackTick => "```",
            _ => "",
        }
    }

    pub(crate) fn get_token_name(t: TokenKind) -> String {
        match t {
            TokenKind::OpenParen | TokenKind::CloseParen => "parentheses".to_owned(),
            TokenKind::OpenBracket | TokenKind::CloseBracket => "brackets".to_owned(),
            TokenKind::OpenDoubleBrace | TokenKind::CloseDoubleBrace => "double braces".to_owned(),
            TokenKind::OpenBrace | TokenKind::CloseBrace => "braces".to_owned(),
            TokenKind::TripleBackTick => "code block".to_owned(),
            TokenKind::OpenAngle | TokenKind::CloseAngle => "angle brackets".to_owned(),
            _ => "".to_owned(),
        }
    }

}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum CustomOperator {
    Unknown(Identifier),
    Infix(Identifier),
    Postfix(Identifier),
    Prefix(Identifier),
    Compound{
        open: Identifier,
        close: Identifier,
        span: Span
    }
}


impl CustomOperator {
    pub(crate) fn to_str<'a>(&'a self, source_manager: &'a source_manager::SourceManager) -> Cow<'a, str> {
        match self {
            CustomOperator::Unknown(id) => Cow::Borrowed(id.to_str(source_manager)),
            CustomOperator::Infix(id) => Cow::Borrowed(id.to_str(source_manager)),
            CustomOperator::Postfix(id) => Cow::Borrowed(id.to_str(source_manager)),
            CustomOperator::Prefix(id) => Cow::Borrowed(id.to_str(source_manager)),
            CustomOperator::Compound{open, close, ..} => Cow::Owned(format!("{}_{}", open.to_str(source_manager), close.to_str(source_manager)))
        }
    }

    pub(crate) fn to_debug_string(&self, source_manager: &source_manager::SourceManager) -> String {
        match self {
            CustomOperator::Unknown(id) => format!("Unknown({})", id.to_str(source_manager)),
            CustomOperator::Infix(id) => format!("Infix({})", id.to_str(source_manager)),
            CustomOperator::Postfix(id) => format!("Postfix({})", id.to_str(source_manager)),
            CustomOperator::Prefix(id) => format!("Prefix({})", id.to_str(source_manager)),
            CustomOperator::Compound{open, close, span} => {
                let id = std::str::from_utf8(&source_manager[*span]).unwrap();
                format!("Compound('{}' '{}' => '{}')", open.to_str(source_manager), close.to_str(source_manager), id)
            }
        }
    }
}

#[derive(Clone)]
pub(super) struct ParenStack {
    stack: Vec<(TokenKind, Span)>,
}

impl Default for ParenStack {
    fn default() -> Self {
        Self::new()
    }
}

impl ParenStack {
    pub(super) fn new() -> Self {
        Self { stack: Vec::new() }
    }

    pub(super) fn push(&mut self, token: TokenKind, span: Span) {
        self.stack.push((token, span));
    }

    pub(super) fn pop(&mut self) -> Option<(TokenKind, Span)> {
        self.stack.pop()
    }

    pub(super) fn is_empty(&self) -> bool {
        self.stack.is_empty()
    }

    pub(super) fn clear(&mut self) {
        self.stack.clear()
    }

    pub(super) fn remove(&mut self, index: usize) -> (TokenKind, Span) {
        self.stack.remove(index)
    }

    pub(crate) fn extend(&mut self, other: Self) {
        self.stack.extend(other.stack)
    }

    pub(crate) fn last(&self) -> Option<&(TokenKind, Span)> {
        self.stack.last()
    }

    pub(crate) fn iter(&self) -> std::slice::Iter<'_, (TokenKind, Span)> {
        self.stack.iter()
    }

    pub(super) fn expect(&mut self, kind: TokenKind, current_span: Span, source_manager: &SourceManager, diagnostic: &Diagnostic) {
        let token_name = ParenMatching::get_token_name(kind);
        let other_paren = ParenMatching::get_other_pair(kind);

        if let Some((paren, span)) = self.stack.last() {
            let paren_str = ParenMatching::to_string(kind);
            let other_paren_str =
                ParenMatching::to_string(other_paren.unwrap_or(TokenKind::Unknown));

            if Some(*paren) != other_paren {
                let info = source_manager
                    .get_source_info(current_span);
                diagnostic
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
                            source_manager
                                .fix_span(current_span),
                        ),
                    )
                    .commit();

                let token_name = ParenMatching::get_token_name(*paren);
                let info = source_manager.get_source_info(*span);
                diagnostic
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
                        Some(source_manager.fix_span(*span)),
                    )
                    .commit();
                return;
            }

            self.stack.pop();
        } else {
            let info = source_manager
                .get_source_info(current_span);
            let paren = ParenMatching::to_string(kind);
            let other_paren = ParenMatching::to_string(
                ParenMatching::get_other_pair(kind).unwrap_or(TokenKind::Unknown),
            );
            diagnostic
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
                        source_manager
                            .fix_span(current_span),
                    ),
                )
                .commit();
        }
    }

    pub(crate) fn check_balanced(&mut self, until_chars: &[char], source_manager: &SourceManager, diagnostic: &Diagnostic) {
        for (token, span) in self.stack.iter().rev() {
            if until_chars.contains(&ParenMatching::to_str(*token).chars().next().unwrap()) {
                continue;
            }

            let other_kind = ParenMatching::get_other_pair(*token).unwrap_or(TokenKind::Unknown);
            if until_chars.contains(&ParenMatching::to_str(other_kind).chars().next().unwrap()) {
                continue;
            }

            let token_name = ParenMatching::get_token_name(*token);

            diagnostic
                .builder()
                .report(
                    DiagnosticLevel::Error,
                    format!("Unmatched {token_name}"),
                    source_manager.get_source_info(*span),
                    None,
                )
                .add_error(
                    format!("Remove or add matching {token_name}"),
                    Some(source_manager.fix_span(*span)),
                )
                .commit();
        }

        self.stack.clear();
    }
}
