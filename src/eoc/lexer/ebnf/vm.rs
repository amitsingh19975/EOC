use std::{
    collections::{HashMap, HashSet}, fmt::Debug, sync::mpsc::channel
};

use crate::eoc::{
    lexer::{str_utils::ByteToCharIter, token::TokenKind},
    utils::{
        diagnostic::{Diagnostic, DiagnosticLevel},
        imm_ref::ImmRef,
        span::Span,
        string::UniqueString,
    },
};

use super::{
    ast::RelativeSourceManager,
    basic::{
        EbnfIdentifierMatcher, EbnfNodeMatcher, EbnfParserMatcher, EbnfParserMatcherInner,
        LexerEbnfMatcher, LexerMatchResult,
    },
    default_matcher::DefaultLexerEbnfParserMatcher,
    expr::{EbnfExpr, TerminalValue},
    ir_matcher::IRLexerEbnfParserMatcher,
    native_call::LexerNativeCallKind, vm_state::LexerVmState,
};

#[derive(Debug, PartialEq, Eq, Clone)]
pub(crate) enum VmNode {
    Call(u16),
    NativeCall(LexerNativeCallKind),
    Terminal(TerminalValue),
    TerminalHash(Vec<TerminalValue>, HashSet<TerminalValue>),
    Range(char, char, bool),
    AnyChar,

    // ===== Binary Operators =====
    Alternative(u16, u16), // Alternative a, b
    Concat(u16, u16),      // Concat a, b
    Exception(u16, u16),   // Exception a, b
    // ===========================

    // ===== Unary Operators =====
    Optional(u16), // Optional a
    // ===========================
    Repetition(u16),               // Repeat if [stack-value]
    Label(UniqueString, u16, u16), // Label a
    DebugPrint,                    // Debug print the VM code
}

impl VmNode {
    pub(crate) fn as_native_kind(&self) -> Option<&LexerNativeCallKind> {
        match self {
            VmNode::NativeCall(k) => Some(k),
            _ => None,
        }
    }
}

pub(crate) type LexerEbnfParserMatcher =
    EbnfParserMatcher<DefaultLexerEbnfParserMatcher, LexerVm, IRLexerEbnfParserMatcher>;

pub(crate) trait EbnfVm<D, V, R> {
    fn get_nodes(&self) -> &Vec<VmNode>;
    fn get_mut_nodes(&mut self) -> &mut Vec<VmNode>;
    fn add_def(&mut self, name: UniqueString, index: usize, is_def: bool) -> Option<usize>;
    fn get_identifier_with_scope<'a>(
        &self,
        name: UniqueString,
        scopes: &'a EbnfParserMatcher<D, V, R>,
        look_in_current_scope: bool,
    ) -> Option<(usize, Option<ImmRef<EbnfParserMatcherInner<D, V, R>>>)>;
    fn get_identifier_from_str<'a, S: AsRef<str>>(
        &self,
        name: S,
        scopes: &'a EbnfParserMatcher<D, V, R>,
        look_in_current_scope: bool,
    ) -> Option<(usize, Option<ImmRef<EbnfParserMatcherInner<D, V, R>>>)>;

    fn from_expr<'a>(
        &mut self,
        expr: EbnfExpr,
        scopes: &'a EbnfParserMatcher<D, V, R>,
        diagnostic: &Diagnostic,
        is_defined: &mut Vec<UniqueString>,
    ) where
        D: EbnfNodeMatcher + Debug,
        V: EbnfNodeMatcher + Debug,
        R: EbnfNodeMatcher + Debug,
    {
        match expr {
            EbnfExpr::Identifier(s, info) => {
                let should_look_in_current_scope =
                    is_defined.iter().find(|i| s.as_str() == *i).is_some();
                let Some((id, scope)) =
                    self.get_identifier_from_str(s.as_str(), scopes, should_look_in_current_scope)
                else {
                    if let Some((info, span)) = info {
                        diagnostic
                            .builder()
                            .report(
                                DiagnosticLevel::Error,
                                format!("Identifier '{}' is not defined", s),
                                info,
                                None,
                            )
                            .add_info("Try to define the identifier", Some(span))
                            .commit();
                    }
                    return;
                };
                if let Some(scope) = scope {
                    let temp = scope.as_ref();
                    let nodes = self.get_mut_nodes();
                    temp.get_vm_code_for(nodes, id);
                } else {
                    self.get_mut_nodes().push(VmNode::Call(id as u16));
                }
            }
            EbnfExpr::Alternative(mut v, h, _) => {
                let mut terms = Vec::new();
                let mut i = 0;

                while i < v.len() {
                    if let EbnfExpr::Terminal(_) = &v[i] {
                        let item = v.remove(i);
                        if let EbnfExpr::Terminal(t) = item {
                            terms.push(t);
                        } else {
                            unreachable!();
                        }
                        i += 1;
                    } else {
                        break;
                    }
                }

                let current_len = self.len();

                let mut size = 0;

                if !terms.is_empty() || !h.is_empty() {
                    self.get_mut_nodes().push(VmNode::TerminalHash(terms, h));
                    size += 1;
                }

                if v.is_empty() && (current_len != self.len()) {
                    return;
                }

                size += v.len() as u16;
                for item in v {
                    self.from_expr(item, scopes, diagnostic, is_defined);
                }

                let off = self.get_mut_nodes().len() - current_len + 1;
                self.get_mut_nodes()
                    .insert(current_len, VmNode::Alternative(size, off as u16));
            }
            EbnfExpr::Concat(v, _) | EbnfExpr::Extend(v, _) => {
                let current_len = self.len();
                let size = v.len() as u16;
                for item in v {
                    self.from_expr(item, scopes, diagnostic, is_defined);
                }

                let off = self.get_mut_nodes().len() - current_len + 1;
                self.get_mut_nodes()
                    .insert(current_len, VmNode::Concat(size, off as u16));
            }
            EbnfExpr::Exception(mut v, _) => {
                let current_len = self.len();
                let size = v.len() as u16;
                if v.is_empty() {
                    return;
                }

                let first = v.remove(0);
                self.from_expr(first, scopes, diagnostic, is_defined);

                if v.len() > 1 {
                    let alternative_start = self.len();

                    let mut hash = HashSet::new();
                    let mut terms = Vec::new();
                    let mut count = 0;
                    for item in v {
                        match item {
                            EbnfExpr::Terminal(t) => {
                                if t.is_char() {
                                    hash.insert(t);
                                } else {
                                    terms.push(t);
                                }
                            }
                            _ => {
                                self.from_expr(item, scopes, diagnostic, is_defined);
                                count += 1;
                            }
                        }
                    }

                    if !terms.is_empty() || !hash.is_empty() {
                        self.get_mut_nodes().push(VmNode::TerminalHash(terms, hash));
                        count += 1;
                    }

                    let off = self.len() - alternative_start + 1;
                    self.get_mut_nodes().insert(
                        alternative_start,
                        VmNode::Alternative(count as u16, off as u16),
                    );
                } else {
                    let item = v.remove(0);
                    self.from_expr(item, scopes, diagnostic, is_defined);
                }

                let off = self.get_mut_nodes().len() - current_len + 1;
                self.get_mut_nodes()
                    .insert(current_len, VmNode::Exception(size, off as u16));
            }
            EbnfExpr::Optional(e, _) => {
                let current_len = self.len();
                self.from_expr(*e, scopes, diagnostic, is_defined);
                let off = self.get_mut_nodes().len() - current_len + 1;
                self.get_mut_nodes()
                    .insert(current_len, VmNode::Optional(off as u16));
            }
            EbnfExpr::Repetition(e, _) => {
                let current_len = self.len();
                self.from_expr(*e, scopes, diagnostic, is_defined);
                let off = self.get_mut_nodes().len() - current_len + 1;
                self.get_mut_nodes()
                    .insert(current_len, VmNode::Repetition(off as u16));
            }
            EbnfExpr::Terminal(t) => {
                self.get_mut_nodes().push(VmNode::Terminal(t));
            }
            EbnfExpr::Statements(v, _) => {
                v.into_iter()
                    .for_each(|item| self.from_expr(item, scopes, diagnostic, is_defined));
            }
            EbnfExpr::Variable { name, expr, is_def } => {
                let name = UniqueString::new(name);
                let _ = self.add_def(name, self.len(), is_def);
                let is_already_exists = self
                    .get_identifier_with_scope(name, scopes, false)
                    .is_some();
                let should_add = is_def || !is_already_exists;
                if should_add {
                    is_defined.push(name);
                }
                self.from_expr(*expr, scopes, diagnostic, is_defined);
                if !should_add {
                    is_defined.push(name);
                }
            }
            EbnfExpr::Range {
                lhs,
                rhs,
                inclusive,
            } => {
                self.get_mut_nodes()
                    .push(VmNode::Range(lhs, rhs, inclusive));
            }
            EbnfExpr::AnyChar => {
                self.get_mut_nodes().push(VmNode::AnyChar);
            }
            EbnfExpr::UnboundedExpr(e) => {
                self.from_expr(*e, scopes, diagnostic, is_defined);
            }
            EbnfExpr::LabelledExpr { label, expr } => {
                let pos = self.len();
                self.from_expr(*expr, scopes, diagnostic, is_defined);
                let off = self.len() - pos + 1;
                self.get_mut_nodes().insert(
                    pos,
                    VmNode::Label(UniqueString::new(label), pos as u16, off as u16),
                );
            }
            EbnfExpr::DebugPrint => {
                self.print_in_range(0, self.len());
            }
        }
    }

    fn len(&self) -> usize {
        self.get_nodes().len()
    }

    fn print_in_range(&self, _start: usize, _end: usize);
}

type LexerVmNode = VmNode;

impl LexerVmNode {
    fn exec(
        vm: &LexerVm,
        state: &mut LexerVmState,
        s: &[u8],
        source_manager: RelativeSourceManager,
        diagnostic: &Diagnostic,
    ) -> bool {
        let pc = state.pc;
        if pc >= vm.nodes.len() {
            return false;
        }

        state.push_call_stack(pc);

        let (is_valid, off) = Self::exec_helper(vm, state, s, source_manager, diagnostic);
        state.pc = (pc + off).min(vm.nodes.len() - 1);

        state.pop_call_stack();
        return is_valid;
    }

    fn exec_helper(
        vm: &LexerVm,
        state: &mut LexerVmState,
        s: &[u8],
        source_manager: RelativeSourceManager,
        diagnostic: &Diagnostic,
    ) -> (bool, usize) {
        let node = &vm.nodes[state.pc];
        state.next_pc();

        match node {
            VmNode::Call(addr) => {
                state.pc = *addr as usize;
                if !Self::exec(vm, state, s, source_manager, diagnostic) {
                    return (false, 1);
                }
            }
            VmNode::NativeCall(k) => {
                let Some((res, tk)) = k.call(
                    vm,
                    s[state.cursor..].as_ref(),
                    source_manager.shift_relative_pos_by(state.cursor as u32),
                    diagnostic,
                    Some(state)
                ) else {
                    return (false, 1);
                };

                let start = state.cursor;
                state.cursor += res.len();

                state
                    .result
                    .push((Span::from_usize(start, state.cursor), tk));
            }
            VmNode::Terminal(t) => {
                let slice = s[state.cursor..].as_ref();
                let is_matched = match t {
                    TerminalValue::Char(c) => ByteToCharIter::new(slice).next() == Some(*c),
                    TerminalValue::String(s) => slice.starts_with(s.as_bytes()),
                };

                if is_matched {
                    let start = state.cursor;
                    state.cursor += t.len_utf8();
                    state
                        .result
                        .push((Span::from_usize(start, state.cursor), TokenKind::Unknown))
                } else {
                    return (false, 1);
                }
            }
            VmNode::TerminalHash(v, h) => {
                let slice = s[state.cursor..].as_ref();
                let Some(ch) = ByteToCharIter::new(slice).next() else {
                    return (false, 1);
                };

                if h.contains(&TerminalValue::Char(ch)) {

                    let start = state.cursor;
                    state.cursor += 1;
                    state
                        .result
                        .push((Span::from_usize(start, state.cursor), TokenKind::Unknown));
                    return (true, 1);
                }

                for t in v {
                    match t {
                        TerminalValue::Char(_) => panic!("Char in TerminalHash"),
                        TerminalValue::String(s) => {
                            if slice.starts_with(s.as_bytes()) {
                                let start = state.cursor;
                                state.cursor += s.len();
                                state.result.push((
                                    Span::from_usize(start, state.cursor),
                                    TokenKind::Unknown,
                                ));
                                return (true, 1);
                            }
                        }
                    }
                }
                return (false, 1);
            }
            VmNode::Range(a, b, inclusive) => {
                let l = *a;
                let r = *b;

                let slice = s[state.cursor..].as_ref();
                let Some(ch) = ByteToCharIter::new(slice).next() else {
                    return (false, 1);
                };

                if *inclusive {
                    if (l..=r).contains(&ch) {
                        let start = state.cursor;
                        state.cursor += ch.len_utf8();
                        state
                            .result
                            .push((Span::from_usize(start, state.cursor), TokenKind::Unknown));
                    } else {
                        return (false, 1);
                    }
                } else {
                    if (l..r).contains(&ch) {
                        let start = state.cursor;
                        state.cursor += ch.len_utf8();
                        state
                            .result
                            .push((Span::from_usize(start, state.cursor), TokenKind::Unknown));
                    } else {
                        return (false, 1);
                    }
                }
            }
            VmNode::AnyChar => {
                let slice = s[state.cursor..].as_ref();
                let Some(ch) = ByteToCharIter::new(slice).next() else {
                    return (false, 1);
                };

                let start = state.cursor;
                state.cursor += ch.len_utf8();
                state
                    .result
                    .push((Span::from_usize(start, state.cursor), TokenKind::Unknown));
            }
            VmNode::Alternative(count, off) => {
                let off = *off as usize;
                let mut found = false;
                for _ in 0..*count {
                    let is_valid = Self::exec(vm, state, s, source_manager, diagnostic);
                    if is_valid {
                        found = true;
                        break;
                    }
                }

                return (found, off);
            }
            VmNode::Concat(count, off) => {
                let off = *off as usize;
                let old_res = state.result.clone();
                let start = state.cursor;
                state.result.clear();
                state.result.reserve(*count as usize);
                for _ in 0..*count {
                    let res = Self::exec(vm, state, s, source_manager, diagnostic);
                    if !res {
                        state.result = old_res;
                        return (false, off);
                    }
                }

                let end = state.cursor;
                state.result = old_res;
                state.result.push((Span::from_usize(start, end), TokenKind::Unknown));

                return (true, off);
            }
            VmNode::Exception(count, off) => {
                let off = *off as usize;
                let start_cursor = state.cursor;
                let old_res = state.result.clone();

                let res = Self::exec(vm, state, s, source_manager, diagnostic);
                let mut end_cursor = state.cursor;

                if !res || start_cursor == end_cursor {
                    state.cursor = start_cursor;
                    return (false, off);
                }

                for _ in 1..*count {
                    let pc = state.pc;
                    for i in (start_cursor..end_cursor).rev() {
                        state.pc = pc;
                        state.cursor = i;
                        let temp_start = state.cursor;
                        let res = Self::exec(vm, state, s, source_manager, diagnostic);
                        let temp_end = state.cursor;
                        let len = temp_end - temp_start;
                        if res {
                            end_cursor -= len.min(end_cursor - start_cursor);
                            break;
                        }
                    }

                    if start_cursor == end_cursor {
                        break;
                    }
                }

                state.cursor = end_cursor;
                let res = start_cursor < end_cursor;
                if !res {
                    state.result = old_res;
                }
                return (res, off);
            }
            VmNode::Optional(off) => {
                let off = *off as usize;
                let _ = Self::exec(vm, state, s, source_manager, diagnostic);
                return (true, off);
            }
            VmNode::Repetition(off) => {
                let off = *off as usize;

                if s.is_empty() {
                    return (false, off);
                }

                let current_pc = state.pc;
                let old_cursor = state.cursor;
                let old_res = state.result.clone();
                state.result.clear();

                loop {
                    if s.len() <= state.cursor {
                        break;
                    }

                    let current_cursor = state.cursor;
                    let res = Self::exec(vm, state, s, source_manager, diagnostic);

                    if !res {
                        break;
                    }

                    if current_cursor == state.cursor {
                        break;
                    }

                    state.pc = current_pc;
                }

                let res = state.cursor != old_cursor;
                state.result = old_res;
                if res {
                    state.result.push((
                        Span::from_usize(old_cursor, state.cursor),
                        TokenKind::Unknown,
                    ));
                }
                return (res, off);
            }
            VmNode::Label(l, addr, off) => {
                let pc = state.pc;
                let off = *off as usize;
                state.pc = *addr as usize;
                let is_valid = Self::exec(vm, state, s, source_manager, diagnostic);
                if is_valid {
                    let (_, t) = state.result.last_mut().unwrap();
                    *t = TokenKind::CustomToken(*l);
                }
                state.pc = pc + off;
            }
            VmNode::DebugPrint => {}
        }

        (true, 1)
    }
}


#[derive(Debug, PartialEq, Eq)]
pub(crate) struct LexerVm {
    nodes: Vec<LexerVmNode>,
    identifiers: HashMap<UniqueString, (usize, bool)>,
    def_identifiers: Vec<(usize, UniqueString)>,
}

impl EbnfIdentifierMatcher for LexerVm {
    fn get_identifier(&self, name: UniqueString) -> Option<usize> {
        self.identifiers.get(&name).map(|(id, _)| *id)
    }
}

impl LexerVm {
    pub(crate) fn new() -> Self {
        Self {
            nodes: Vec::new(),
            identifiers: HashMap::new(),
            def_identifiers: Vec::new(),
        }
    }

    pub(super) fn get_def_identifiers(&self) -> &Vec<(usize, UniqueString)> {
        &self.def_identifiers
    }

    pub fn run<'b>(
        &self,
        id: usize,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
        state: Option<&LexerVmState>
    ) -> LexerMatchResult {
        let mut temp_state = LexerVmState::new(id);
        state.map(|s| temp_state.merge_call_stack(s));

        let is_valid = LexerVmNode::exec(self, &mut temp_state, s, source_manager, diagnostic);

        if !is_valid {
            LexerMatchResult::new()
        } else {
            temp_state.result.clone().into_iter().map(|(r, k)| (r, k)).collect()
        }
    }

    pub(super) fn print(&self) {
        self.print_in_range(0, self.nodes.len());
    }

    fn contains_def(&self, name: UniqueString) -> bool {
        self.identifiers
            .get(&name)
            .map(|(_, is_def)| *is_def)
            .unwrap_or(false)
    }
}

impl EbnfNodeMatcher for LexerVm {
    fn get_node(&self, id: usize) -> Option<&LexerVmNode> {
        self.nodes.get(id)
    }
}

impl EbnfVm<DefaultLexerEbnfParserMatcher, LexerVm, IRLexerEbnfParserMatcher> for LexerVm {
    fn get_nodes(&self) -> &Vec<LexerVmNode> {
        &self.nodes
    }

    fn get_mut_nodes(&mut self) -> &mut Vec<LexerVmNode> {
        &mut self.nodes
    }

    fn add_def(&mut self, name: UniqueString, index: usize, is_def: bool) -> Option<usize> {
        if let Some((id, is_def)) = self.identifiers.get(&name).copied() {
            self.identifiers.insert(name, (index, is_def));
            Some(id)
        } else {
            if is_def {
                self.def_identifiers.push((index, name));
            }
            self.identifiers.insert(name, (index, is_def));
            None
        }
    }

    fn get_identifier_with_scope<'a>(
        &self,
        name: UniqueString,
        scopes: &'a LexerEbnfParserMatcher,
        should_look_in_current_scope: bool,
    ) -> Option<(
        usize,
        Option<
            ImmRef<
                EbnfParserMatcherInner<
                    DefaultLexerEbnfParserMatcher,
                    LexerVm,
                    IRLexerEbnfParserMatcher,
                >,
            >,
        >,
    )> {
        if should_look_in_current_scope {
            let temp = self
                .identifiers
                .get(&name)
                .copied()
                .map(|(id, _)| (id, None));

            if temp.is_some() {
                return temp;
            }
        }

        scopes
            .get_identifier_with_scope(name)
            .map(|(id, scope)| (id, Some(scope)))
    }

    fn get_identifier_from_str<'a, S: AsRef<str>>(
        &self,
        name: S,
        scopes: &'a LexerEbnfParserMatcher,
        should_look_in_current_scope: bool,
    ) -> Option<(
        usize,
        Option<
            ImmRef<
                EbnfParserMatcherInner<
                    DefaultLexerEbnfParserMatcher,
                    LexerVm,
                    IRLexerEbnfParserMatcher,
                >,
            >,
        >,
    )> {
        let name = UniqueString::try_new(name)?;
        self.get_identifier_with_scope(name, scopes, should_look_in_current_scope)
    }

    fn print_in_range(&self, start: usize, end: usize) {
        for (i, node) in self.nodes[start..end.min(self.nodes.len())]
            .iter()
            .enumerate()
        {
            if let Some((s, _)) = self.identifiers.iter().find(|(_, p)| p.0 == i) {
                println!("{:04}: {:?} => {}", i, node, s);
            } else {
                println!("{:04}: {:?}", i, node);
            }
        }
        println!("\nIdentifiers: {:?}", self.def_identifiers);
    }
}

impl LexerEbnfMatcher for LexerVm {
    fn match_native<'b>(
        &self,
        kind: LexerNativeCallKind,
        s: &'b [u8],
        source_manager: super::ast::RelativeSourceManager<'b>,
        diagnostic: &crate::eoc::utils::diagnostic::Diagnostic,
        state: Option<&LexerVmState>
    ) -> Option<(&'b [u8], TokenKind)> {
        if let Some((id, _)) = self.identifiers.get(&kind.as_unique_str()).copied() {
            self.run(id, s, source_manager, diagnostic, state)
                .pop()
                .map(|(r, k)| (s[r.as_range()].as_ref(), k))
        } else {
            kind.call(self, s, source_manager, diagnostic, state)
        }
    }

    fn try_match_expr<'b>(
        &self,
        s: &'b [u8],
        source_manager: super::ast::RelativeSourceManager<'b>,
        diagnostic: &crate::eoc::utils::diagnostic::Diagnostic,
        state: Option<&LexerVmState>
    ) -> LexerMatchResult {
        let (tx, rx) = channel();
        rayon::scope(|scope| {
            for (k, name) in self.def_identifiers.iter().rev().copied() {
                let tx = tx.clone();
                scope.spawn(move |_| {
                    let mut temp = self.run(k, s, source_manager, diagnostic, state);
                    temp.iter_mut().for_each(|(s, t)| {
                        *s = source_manager.abs_span(*s);
                        if *t == TokenKind::Unknown {
                            *t = TokenKind::CustomToken(name);
                        }
                    });
                    let _ = tx.send((k, temp));
                });
            }
        });
        let mut res = Vec::new();
        for _ in 0..(self.def_identifiers.len()) {
            if let Ok(temp) = rx.recv() {
                if !temp.1.is_empty() {
                    res.push(temp)
                }
            }
        }
        res.sort_unstable_by_key(|(k, _)| *k);
        res.pop().map(|(_, v)| v).unwrap_or_default()
    }

    fn match_for<'b>(
        &self,
        addr: usize,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
    ) -> LexerMatchResult {
        self.run(addr, s, source_manager, diagnostic, None)
    }
    
    fn is_default(&self) -> bool {
        false
    }

    fn is_ir(&self) -> bool {
        false
    }
}
