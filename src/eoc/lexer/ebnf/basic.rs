use std::fmt::Debug;

use lazy_static::lazy_static;

use crate::eoc::{
    lexer::{
        number::{parse_floating_point, parse_integer},
        str_utils::ByteToCharIter,
        token::TokenKind,
    },
    utils::{
        diagnostic::Diagnostic, imm_ref::{ImmRef, Ref}, source_manager::RelativeSourceManager, span::Span, string::UniqueString
    },
};

use super::{
    default_matcher::DefaultLexerEbnfParserMatcher,
    ir_matcher::IRLexerEbnfParserMatcher,
    native_call::LexerNativeCallKind,
    vm::{LexerVm, VmNode}, vm_state::LexerVmState,
};

pub(crate) type LexerMatchResult = Option<(Span, TokenKind)>;

pub(crate) trait LexerEbnfMatcher {
    fn is_digit<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
        state: Option<&LexerVmState>
    ) -> Option<char> {
        self.match_native(LexerNativeCallKind::Digit, s, source_manager, diagnostic, state)
            .map(|(s, _)| ByteToCharIter::new(s).next().unwrap())
    }

    fn is_hex_digit<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
        state: Option<&LexerVmState>
    ) -> Option<char> {
        self.match_native(LexerNativeCallKind::HexDigit, s, source_manager, diagnostic, state)
            .map(|(s, _)| ByteToCharIter::new(s).next().unwrap())
    }

    fn is_oct_digit<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
        state: Option<&LexerVmState>
    ) -> Option<char> {
        self.match_native(LexerNativeCallKind::OctDigit, s, source_manager, diagnostic, state)
            .map(|(s, _)| ByteToCharIter::new(s).next().unwrap())
    }

    fn is_binary_digit<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
        state: Option<&LexerVmState>
    ) -> Option<char> {
        self.match_native(LexerNativeCallKind::BinDigit, s, source_manager, diagnostic, state)
            .map(|(s, _)| ByteToCharIter::new(s).next().unwrap())
    }

    fn match_native_integer<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
        state: Option<&LexerVmState>
    ) -> Option<&'b [u8]> {
        parse_integer(
            s,
            |s, sm, d| self.is_digit(s, sm, d, state),
            |s, sm, d| self.is_hex_digit(s, sm, d, state),
            |s, sm, d| self.is_oct_digit(s, sm, d, state),
            |s, sm, d| self.is_binary_digit(s, sm, d, state),
            source_manager,
            diagnostic,
        )
    }

    fn match_native_floating_point<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
        state: Option<&LexerVmState>
    ) -> Option<&'b [u8]> {
        parse_floating_point(
            s,
            |s, sm, d| self.is_digit(s, sm, d, state),
            |s, sm, d| self.is_hex_digit(s, sm, d, state),
            source_manager,
            diagnostic,
        )
    }

    fn match_for<'b>(
        &self,
        addr: usize,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic
    ) -> LexerMatchResult;

    fn match_native<'b>(
        &self,
        kind: LexerNativeCallKind,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
        state: Option<&LexerVmState>
    ) -> Option<(&'b [u8], TokenKind)>;

    fn try_match_expr<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
        state: Option<&LexerVmState>
    ) -> LexerMatchResult;

    fn is_ir(&self) -> bool;
    fn is_default(&self) -> bool;

    fn is_scoped(&self) -> bool;
}

pub(crate) trait EbnfIdentifierMatcher {
    fn get_identifier(&self, name: UniqueString) -> Option<usize>;
}

pub(crate) trait EbnfNodeMatcher {
    fn get_node(&self, id: usize) -> Option<&VmNode>;
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum EbnfParserMatcherInner<D, V, R> {
    Default(D),
    Vm(V),
    IR(R),
}

impl<D, V, R> EbnfParserMatcherInner<D, V, R> {
    pub(crate) fn is_default(&self) -> bool {
        matches!(self, EbnfParserMatcherInner::Default(_))
    }

    pub(crate) fn is_ir(&self) -> bool {
        matches!(self, EbnfParserMatcherInner::IR(_))
    }

    pub(crate) fn is_vm(&self) -> bool {
        matches!(self, EbnfParserMatcherInner::Vm(_))
    }

    // function inliner to avoid performance overhead due to
    // environment lookups
    pub(crate) fn get_vm_code_for(&self, nodes: &mut Vec<VmNode>, id: usize, current_parent_node: Option<&VmNode>) -> (usize, u16)
    where
        Self: EbnfNodeMatcher,
    {
        let Some(node) = self.get_node(id) else {
            return (0, 0);
        };
        // println!("node: {:?} | {:?}", node, nodes);
        let start_len = nodes.len();
        match node {
            VmNode::Call(addr) => return self.get_vm_code_for(nodes, *addr as usize, current_parent_node),
            VmNode::NativeCall(k) => {
                nodes.push(VmNode::NativeCall(*k));
            }
            VmNode::Terminal(t) => nodes.push(VmNode::Terminal(t.clone())),
            VmNode::TerminalHash(v, h) => nodes.push(VmNode::TerminalHash(v.clone(), h.clone())),
            VmNode::Range(a, b, i) => nodes.push(VmNode::Range(*a, *b, *i)),
            VmNode::AnyChar => nodes.push(VmNode::AnyChar),
            VmNode::Alternative(count, _) => {
                let start = nodes.len();
                let mut i = id + 1;
                let mut size = 0;
                for _ in 0..*count {
                    let (p, s) = self.get_vm_code_for(nodes, i, Some(node));
                    i += p;
                    size += s;
                }
                let off = nodes.len() - start + 1;
                if let Some(VmNode::Alternative(_, _)) = current_parent_node {
                    // merge the two alternative nodes
                } else {
                    nodes.insert(start, VmNode::Alternative(size, off as u16));
                    return (1, 1);
                }
            }
            VmNode::Concat(count, _) => {
                let start = nodes.len();
                let mut i = id + 1;
                let mut size = 0;
                for _ in 0..*count {
                    let (p, s) = self.get_vm_code_for(nodes, i, Some(node));
                    i += p;
                    size += s;
                }
                let off = nodes.len() - start + 1;
                if let Some(VmNode::Concat(_, _)) = current_parent_node {
                    // merge the two concat nodes
                } else {
                    nodes.insert(start, VmNode::Concat(size, off as u16));
                    return (1, 1);
                }
            }
            VmNode::Exception(count, _) => {
                let start = nodes.len();
                let mut i = id + 1;
                let mut size = 0;
                for _ in 0..*count {
                    let (p, s) = self.get_vm_code_for(nodes, i, Some(node));
                    i += p;
                    size += s;
                }
                let off = nodes.len() - start + 1;
                if let Some(VmNode::Exception(_, _)) = current_parent_node {
                    // merge the two exception nodes
                } else {
                    nodes.insert(start, VmNode::Exception(size, off as u16));
                    return (1, 1);
                }
            }
            VmNode::Optional(_) => {
                let start = nodes.len();
                self.get_vm_code_for(nodes, id + 1, Some(node));
                let off = nodes.len() - start + 1;
                nodes.push(VmNode::Optional(off as u16));
            }
            VmNode::Repetition(_) => {
                let start = nodes.len();
                self.get_vm_code_for(nodes, id + 1, Some(node));
                let off = nodes.len() - start + 1;
                nodes.push(VmNode::Repetition(off as u16));
            }
            VmNode::Label(l, e, _) => {
                let start = nodes.len();
                self.get_vm_code_for(nodes, *e as usize, Some(node));
                let off = nodes.len() - start + 1;
                nodes.push(VmNode::Label(l.clone(), start as u16, off as u16));
            }
            VmNode::DebugPrint => {}
            VmNode::ErrorScope { error_id, off: _, span } => {
                let start = nodes.len();
                self.get_vm_code_for(nodes, id + 1, Some(node));
                let off = nodes.len() - start + 1;
                nodes.push(VmNode::ErrorScope {
                    error_id: *error_id,
                    off: off as u16,
                    span: *span,
                });
            }
        };
        let size = (nodes.len() - start_len) as u16;
        return (1, size);
    }
}

impl<D, V, R> EbnfNodeMatcher for EbnfParserMatcherInner<D, V, R>
where
    D: EbnfNodeMatcher,
    V: EbnfNodeMatcher,
    R: EbnfNodeMatcher,
{
    fn get_node(&self, id: usize) -> Option<&VmNode> {
        match self {
            EbnfParserMatcherInner::Default(d) => d.get_node(id),
            EbnfParserMatcherInner::Vm(v) => v.get_node(id),
            EbnfParserMatcherInner::IR(r) => r.get_node(id),
        }
    }
}

impl<D: Default + EbnfIdentifierMatcher, V, R> EbnfParserMatcherInner<D, V, R> {
    pub(crate) fn new() -> Self {
        EbnfParserMatcherInner::Default(Default::default())
    }
}

impl<D: EbnfIdentifierMatcher, V: EbnfIdentifierMatcher, R: EbnfIdentifierMatcher>
    EbnfIdentifierMatcher for EbnfParserMatcherInner<D, V, R>
{
    fn get_identifier(&self, name: UniqueString) -> Option<usize> {
        match self {
            EbnfParserMatcherInner::Default(d) => d.get_identifier(name),
            EbnfParserMatcherInner::Vm(v) => v.get_identifier(name),
            EbnfParserMatcherInner::IR(r) => r.get_identifier(name),
        }
    }
}

pub(super) type LexerEbnfParserMatcherInner =
    EbnfParserMatcherInner<DefaultLexerEbnfParserMatcher, LexerVm, IRLexerEbnfParserMatcher>;

#[derive(Debug)]
pub(crate) struct EbnfParserMatcher<D, V, R> {
    pub(crate) parent_scope: Option<ImmRef<EbnfParserMatcher<D, V, R>>>,
    pub(crate) current_scope: Ref<EbnfParserMatcherInner<D, V, R>>,
}

impl<D: EbnfIdentifierMatcher + Debug, V: EbnfIdentifierMatcher + Debug, R: EbnfIdentifierMatcher + Debug>
    EbnfParserMatcher<D, V, R>
{
    pub(crate) fn get_identifier_with_scope<'a>(
        &self,
        name: UniqueString,
    ) -> Option<(usize, ImmRef<EbnfParserMatcherInner<D, V, R>>)> {
        let mut scope = self;
        if let Some(id) = scope.current_scope.get_identifier(name) {
            return Some((id, scope.current_scope.to_imm_ref()));
        }

        while let Some(parent_scope) = scope.parent_scope.as_ref() {
            if let Some(id) = parent_scope.current_scope.get_identifier(name) {
                return Some((id, parent_scope.current_scope.to_imm_ref()));
            }
            scope = parent_scope.as_ref();
        }
        None
    }
}

impl<D: EbnfIdentifierMatcher, V: EbnfIdentifierMatcher, R: EbnfIdentifierMatcher>
    EbnfIdentifierMatcher for EbnfParserMatcher<D, V, R>
{
    fn get_identifier(&self, name: UniqueString) -> Option<usize> {
        let mut scope = self;
        while let Some(parent_scope) = scope.parent_scope.as_ref() {
            if let Some(id) = parent_scope.current_scope.as_ref().get_identifier(name) {
                return Some(id);
            }
            scope = parent_scope.as_ref();
        }
        None
    }
}

lazy_static! {
    pub(crate) static ref DEFAULT_LEXER_EBNF_PARSER_MATCHER: Ref<EbnfParserMatcher<DefaultLexerEbnfParserMatcher, LexerVm, IRLexerEbnfParserMatcher>> =
        Ref::new(EbnfParserMatcher {
            parent_scope: None,
            current_scope: Ref::new(EbnfParserMatcherInner::Default(Default::default())),
        });
}
