use std::collections::HashMap;

use crate::eoc::{
    ast::identifier::Identifier,
    lexer::{
        token::TokenKind,
        utils::{
            is_valid_identifier_continuation_code_point, is_valid_identifier_start_code_point,
        },
    },
    utils::{diagnostic::Diagnostic, source_manager::RelativeSourceManager, span::Span, string::UniqueString},
};

use super::{
    basic::{EbnfIdentifierMatcher, EbnfNodeMatcher, LexerEbnfMatcher, LexerMatchResult},
    native_call::{LexerNativeCallKind, NATIVE_CALL_KIND_ID},
    vm::VmNode,
    vm_state::LexerVmState,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct DefaultLexerEbnfParserMatcher {
    nodes: HashMap<UniqueString, usize>,
    node_ids: Vec<VmNode>,
}

impl Default for DefaultLexerEbnfParserMatcher {
    fn default() -> Self {
        Self::new()
    }
}

impl DefaultLexerEbnfParserMatcher {
    pub(crate) fn new() -> Self {
        let mut node_ids = Vec::new();
        let mut nodes = HashMap::new();

        {
            let mut insert = |node: VmNode| {
                let id = node_ids.len();
                let Some(name) = node.as_native_kind().map(|k| k.as_unique_str()) else {
                    return
                };
                node_ids.push(node);
                nodes.insert(name, id);
            };

            insert(VmNode::NativeCall(LexerNativeCallKind::Identifier));
            insert(VmNode::NativeCall(LexerNativeCallKind::Operator));
            insert(VmNode::NativeCall(LexerNativeCallKind::FloatingPoint));
            insert(VmNode::NativeCall(LexerNativeCallKind::Integer));
            insert(VmNode::NativeCall(LexerNativeCallKind::Number));
            insert(VmNode::NativeCall(LexerNativeCallKind::StartIdentifier));
            insert(VmNode::NativeCall(LexerNativeCallKind::ContIdentifier));
            insert(VmNode::NativeCall(LexerNativeCallKind::StartOperator));
            insert(VmNode::NativeCall(LexerNativeCallKind::ContOperator));
            insert(VmNode::NativeCall(LexerNativeCallKind::Whitespace));
            insert(VmNode::NativeCall(LexerNativeCallKind::NewLine));
            insert(VmNode::NativeCall(LexerNativeCallKind::Tab));
            insert(VmNode::NativeCall(LexerNativeCallKind::Letter));
            insert(VmNode::NativeCall(LexerNativeCallKind::AlphaNumeric));
            insert(VmNode::NativeCall(LexerNativeCallKind::BinDigit));
            insert(VmNode::NativeCall(LexerNativeCallKind::OctDigit));
            insert(VmNode::NativeCall(LexerNativeCallKind::HexDigit));
            insert(VmNode::NativeCall(LexerNativeCallKind::Digit));
        }
        Self { nodes, node_ids }
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
}

impl EbnfNodeMatcher for DefaultLexerEbnfParserMatcher {
    fn get_node(&self, id: usize) -> Option<&VmNode> {
        self.node_ids.get(id)
    }
}

impl EbnfIdentifierMatcher for DefaultLexerEbnfParserMatcher {
    fn get_identifier(&self, name: UniqueString) -> Option<usize> {
        self.nodes.get(&name).copied()
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
                return Some((source_manager.abs_span(Span::from_usize(0, s.len())), k));
            }
        }
        None
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
            .map(|(s, k)| (source_manager.abs_span(Span::from_usize(0, s.len())), k))
    }

    fn is_default(&self) -> bool {
        true
    }
    
    fn is_ir(&self) -> bool {
        false
    }

    fn is_scoped(&self) -> bool {
        false
    }
}
