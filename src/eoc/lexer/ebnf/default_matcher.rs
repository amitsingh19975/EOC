use smallvec::smallvec;

use crate::eoc::{
    ast::identifier::Identifier,
    lexer::{
        token::TokenKind,
        utils::{
            is_valid_identifier_continuation_code_point, is_valid_identifier_start_code_point,
        },
    },
    utils::{diagnostic::Diagnostic, span::Span, string::UniqueString},
};

use super::{
    ast::RelativeSourceManager,
    basic::{EbnfIdentifierMatcher, EbnfNodeMatcher, LexerEbnfMatcher, LexerMatchResult},
    native_call::{LexerNativeCallKind, NATIVE_CALL_KIND_ID},
    vm::VmNode,
    vm_state::LexerVmState,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct DefaultLexerEbnfParserMatcher {
    nodes: Vec<VmNode>,
}

impl Default for DefaultLexerEbnfParserMatcher {
    fn default() -> Self {
        Self::new()
    }
}

impl DefaultLexerEbnfParserMatcher {
    pub(crate) fn new() -> Self {
        let nodes = vec![
            VmNode::NativeCall(LexerNativeCallKind::Identifier), //NATIVE_CALL_KIND_ID.to_native_call_kind(NATIVE_CALL_KIND_ID.identifier_sym).unwrap()),
            VmNode::NativeCall(LexerNativeCallKind::Operator), //NATIVE_CALL_KIND_ID.to_native_call_kind(NATIVE_CALL_KIND_ID.operator_sym).unwrap()),
            VmNode::NativeCall(LexerNativeCallKind::FloatingPoint), //NATIVE_CALL_KIND_ID.to_native_call_kind(NATIVE_CALL_KIND_ID.fp_sym).unwrap()),
            VmNode::NativeCall(LexerNativeCallKind::Integer), //NATIVE_CALL_KIND_ID.to_native_call_kind(NATIVE_CALL_KIND_ID.integer_sym).unwrap()),
            VmNode::NativeCall(LexerNativeCallKind::Number), //NATIVE_CALL_KIND_ID.to_native_call_kind(NATIVE_CALL_KIND_ID.number_sym).unwrap()),
            VmNode::NativeCall(LexerNativeCallKind::Unknown), //NATIVE_CALL_KIND_ID.to_native_call_kind(NATIVE_CALL_KIND_ID.string_sym).unwrap()),
            VmNode::NativeCall(LexerNativeCallKind::Unknown), //NATIVE_CALL_KIND_ID.to_native_call_kind(NATIVE_CALL_KIND_ID.char_sym).unwrap()),
            VmNode::NativeCall(LexerNativeCallKind::StartIdentifier), //NATIVE_CALL_KIND_ID.to_native_call_kind(NATIVE_CALL_KIND_ID.start_identifier_sym).unwrap()),
            VmNode::NativeCall(LexerNativeCallKind::ContIdentifier), //NATIVE_CALL_KIND_ID.to_native_call_kind(NATIVE_CALL_KIND_ID.cont_identifier_sym).unwrap()),
            VmNode::NativeCall(LexerNativeCallKind::StartOperator), //NATIVE_CALL_KIND_ID.to_native_call_kind(NATIVE_CALL_KIND_ID.start_operator_sym).unwrap()),
            VmNode::NativeCall(LexerNativeCallKind::ContOperator), //NATIVE_CALL_KIND_ID.to_native_call_kind(NATIVE_CALL_KIND_ID.cont_operator_sym).unwrap()),
            VmNode::NativeCall(LexerNativeCallKind::Whitespace), //NATIVE_CALL_KIND_ID.to_native_call_kind(NATIVE_CALL_KIND_ID.whitespace_sym).unwrap()),
            VmNode::NativeCall(LexerNativeCallKind::NewLine), //NATIVE_CALL_KIND_ID.to_native_call_kind(NATIVE_CALL_KIND_ID.new_line_sym).unwrap()),
            VmNode::NativeCall(LexerNativeCallKind::Tab), //NATIVE_CALL_KIND_ID.to_native_call_kind(NATIVE_CALL_KIND_ID.tab_sym).unwrap()),
            VmNode::NativeCall(LexerNativeCallKind::Letter), //NATIVE_CALL_KIND_ID.to_native_call_kind(NATIVE_CALL_KIND_ID.letter_sym).unwrap()),
            VmNode::NativeCall(LexerNativeCallKind::AlphaNumeric), //NATIVE_CALL_KIND_ID.to_native_call_kind(NATIVE_CALL_KIND_ID.alpha_numeric_sym).unwrap()),
            VmNode::NativeCall(LexerNativeCallKind::BinDigit), //NATIVE_CALL_KIND_ID.to_native_call_kind(NATIVE_CALL_KIND_ID.binary_digit_sym).unwrap()),
            VmNode::NativeCall(LexerNativeCallKind::OctDigit), //NATIVE_CALL_KIND_ID.to_native_call_kind(NATIVE_CALL_KIND_ID.oct_digit_sym).unwrap()),
            VmNode::NativeCall(LexerNativeCallKind::HexDigit), //NATIVE_CALL_KIND_ID.to_native_call_kind(NATIVE_CALL_KIND_ID.hex_digit_sym).unwrap()),
            VmNode::NativeCall(LexerNativeCallKind::Digit), //NATIVE_CALL_KIND_ID.to_native_call_kind(NATIVE_CALL_KIND_ID.digit_sym).unwrap()),
        ];
        Self { nodes }
    }

    pub(crate) fn is_valid_identifier_start_code_point(c: char) -> bool {
        is_valid_identifier_start_code_point(c)
    }

    pub(crate) fn is_valid_identifier_continuation_code_point(c: char) -> bool {
        is_valid_identifier_continuation_code_point(c)
    }

    pub(crate) fn is_operator_start_code_point(c: char) -> bool {
        Identifier::is_operator_start_code_point(c)
    }

    pub(crate) fn is_operator_continuation_code_point(c: char) -> bool {
        Identifier::is_operator_continuation_code_point(c)
    }

    fn key_to_id(&self, key: UniqueString) -> Option<usize> {
        self.nodes.iter().position(|n| {
            let Some(i) = n.as_native_kind() else {
                return false;
            };
            if *i == LexerNativeCallKind::Unknown {
                return false;
            }
            i.as_unique_str() == key
        })
    }
}

impl EbnfNodeMatcher for DefaultLexerEbnfParserMatcher {
    fn get_node(&self, id: usize) -> Option<&VmNode> {
        self.nodes.get(id)
    }
}

impl EbnfIdentifierMatcher for DefaultLexerEbnfParserMatcher {
    fn get_identifier(&self, name: UniqueString) -> Option<usize> {
        self.key_to_id(name)
    }
}

impl LexerEbnfMatcher for DefaultLexerEbnfParserMatcher {
    fn match_native<'b>(
        &self,
        kind: LexerNativeCallKind,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
        state: Option<&LexerVmState>,
    ) -> Option<(&'b [u8], TokenKind)> {
        kind.call(self, s, source_manager, diagnostic, state)
    }

    fn try_match_expr<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
        state: Option<&LexerVmState>,
    ) -> LexerMatchResult {
        for k in [
            LexerNativeCallKind::Identifier,
            LexerNativeCallKind::Operator,
            LexerNativeCallKind::Number,
        ] {
            let temp = self.match_native(k, s, source_manager, diagnostic, state);
            if let Some((s, k)) = temp {
                return smallvec![(source_manager.abs_span(Span::from_usize(0, s.len())), k)];
            }
        }
        smallvec![]
    }

    fn match_for<'b>(
        &self,
        addr: usize,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
    ) -> LexerMatchResult {
        let key = NATIVE_CALL_KIND_ID.id_to_string(addr).unwrap();
        let key = NATIVE_CALL_KIND_ID.to_native_call_kind(key).unwrap();
        self.match_native(key, s, source_manager, diagnostic, None)
            .map(|(s, k)| smallvec![(source_manager.abs_span(Span::from_usize(0, s.len())), k)])
            .unwrap_or_default()
    }

    fn is_default(&self) -> bool {
        true
    }
}
