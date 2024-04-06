use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    sync::mpsc::channel,
};

use smallvec::SmallVec;

use crate::eoc::{
    lexer::{str_utils::ByteToCharIter, token::TokenKind},
    utils::{
        diagnostic::{Diagnostic, DiagnosticBag, DiagnosticLevel}, imm_ref::ImmRef, source_manager::RelativeSourceManager, span::Span, string::UniqueString
    },
};

use super::{
    basic::{
        EbnfIdentifierMatcher, EbnfNodeMatcher, EbnfParserMatcher, EbnfParserMatcherInner,
        LexerEbnfMatcher, LexerMatchResult,
    },
    default_matcher::DefaultLexerEbnfParserMatcher,
    expr::{EbnfExpr, TerminalValue},
    ir_matcher::IRLexerEbnfParserMatcher,
    native_call::LexerNativeCallKind,
    vm_state::{LexerVmState, VmState},
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

    ErrorScope { error_id: u16, off: u16, span: Span },
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

type IdentifierDefinitionResult<D, V, R> = (usize, Option<ImmRef<EbnfParserMatcherInner<D, V, R>>>);

pub(crate) trait EbnfVm<D, V, R> {
    fn get_nodes(&self) -> &Vec<VmNode>;
    fn get_mut_nodes(&mut self) -> &mut Vec<VmNode>;
    fn add_def(&mut self, name: UniqueString, index: usize, is_def: bool) -> Option<usize>;
    fn get_identifier_with_scope<'a>(
        &self,
        name: UniqueString,
        scopes: &'a EbnfParserMatcher<D, V, R>,
        look_in_current_scope: bool,
        import_list: &HashSet<String>,
    ) -> Option<IdentifierDefinitionResult<D, V, R>>;
    fn get_identifier_from_str<'a, S: AsRef<str>>(
        &self,
        name: S,
        scopes: &'a EbnfParserMatcher<D, V, R>,
        look_in_current_scope: bool,
        import_list: &HashSet<String>,
    ) -> Option<IdentifierDefinitionResult<D, V, R>>;

    fn add_error(&mut self, message: String) -> u16;

    fn set_is_scoped(&mut self, _is_scoped: bool) {}

    fn from_expr<'a>(
        &mut self,
        mut expr: EbnfExpr,
        scopes: &'a EbnfParserMatcher<D, V, R>,
        diagnostic: &Diagnostic,
        is_defined: &mut HashMap<UniqueString, IdentifierDefinitionResult<D, V, R>>,
    ) where
        D: EbnfNodeMatcher + Debug,
        V: EbnfNodeMatcher + Debug,
        R: EbnfNodeMatcher + Debug,
    {
        let mut import_list = HashSet::new();
        let mut errors = HashMap::new();
        match &mut expr {
            EbnfExpr::Statements(s, _) => {
                let import = s.remove(0);
                if let EbnfExpr::Import(i) = import {
                    import_list = i;
                } else {
                    unreachable!();
                }

                let errors_item = s.remove(0);
                if let EbnfExpr::Errors(e) = errors_item {
                    for (name, message) in e.into_iter() {
                        let id = self.add_error(message);
                        errors.insert(name, id as u16);
                    }
                } else {
                    unreachable!();
                }
            }
            _ => {}
        }

        self.set_is_scoped(!import_list.contains("@all"));

        self.from_expr_helper(expr, scopes, diagnostic, is_defined, &import_list, &errors);
    }

    fn from_expr_helper<'a>(
        &mut self,
        expr: EbnfExpr,
        scopes: &'a EbnfParserMatcher<D, V, R>,
        diagnostic: &Diagnostic,
        is_defined: &mut HashMap<UniqueString, IdentifierDefinitionResult<D, V, R>>,
        import_list: &HashSet<String>,
        errors: &HashMap<String, u16>,
    ) where
        D: EbnfNodeMatcher + Debug,
        V: EbnfNodeMatcher + Debug,
        R: EbnfNodeMatcher + Debug,
    {
        match expr {
            EbnfExpr::Identifier(s, info) => {
                let Some(s) = UniqueString::try_new(s.as_str()) else {
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
                let temp = if let Some((id, scope)) = is_defined.get(&s) {
                    Some((*id, scope.as_ref().map(|s| s.clone())))
                } else {
                    self.get_identifier_with_scope(s, scopes, true, import_list)
                };

                let Some((id, scope)) = temp else {
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
                    self.from_expr_helper(
                        item,
                        scopes,
                        diagnostic,
                        is_defined,
                        import_list,
                        errors,
                    );
                }

                let off = self.get_mut_nodes().len() - current_len + 1;
                self.get_mut_nodes()
                    .insert(current_len, VmNode::Alternative(size, off as u16));
            }
            EbnfExpr::Concat(v, _) | EbnfExpr::Extend(v, _) => {
                let current_len = self.len();
                let size = v.len() as u16;
                for item in v {
                    self.from_expr_helper(
                        item,
                        scopes,
                        diagnostic,
                        is_defined,
                        import_list,
                        errors,
                    );
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
                self.from_expr_helper(first, scopes, diagnostic, is_defined, import_list, errors);

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
                                self.from_expr_helper(
                                    item,
                                    scopes,
                                    diagnostic,
                                    is_defined,
                                    import_list,
                                    errors,
                                );
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
                    self.from_expr_helper(
                        item,
                        scopes,
                        diagnostic,
                        is_defined,
                        import_list,
                        errors,
                    );
                }

                let off = self.get_mut_nodes().len() - current_len + 1;
                self.get_mut_nodes()
                    .insert(current_len, VmNode::Exception(size, off as u16));
            }
            EbnfExpr::Optional(e, _) => {
                let current_len = self.len();
                self.from_expr_helper(*e, scopes, diagnostic, is_defined, import_list, errors);
                let off = self.get_mut_nodes().len() - current_len + 1;
                self.get_mut_nodes()
                    .insert(current_len, VmNode::Optional(off as u16));
            }
            EbnfExpr::Repetition(e, _) => {
                let current_len = self.len();
                self.from_expr_helper(*e, scopes, diagnostic, is_defined, import_list, errors);
                let off = self.get_mut_nodes().len() - current_len + 1;
                self.get_mut_nodes()
                    .insert(current_len, VmNode::Repetition(off as u16));
            }
            EbnfExpr::Terminal(t) => {
                self.get_mut_nodes().push(VmNode::Terminal(t));
            }
            EbnfExpr::Statements(v, _) => {
                v.into_iter().for_each(|item| {
                    self.from_expr_helper(item, scopes, diagnostic, is_defined, import_list, errors)
                });
            }
            EbnfExpr::Variable { name, expr, is_def } => {
                let name = UniqueString::new(name);
                let _ = self.add_def(name, self.len(), is_def);

                let current_len = self.len();

                if is_def {
                    is_defined.insert(name, (current_len, None));
                } else {
                    if !is_defined.contains_key(&name) {
                        let id_var =
                            self.get_identifier_with_scope(name, scopes, false, import_list);
                        if let Some((id, scope)) = id_var {
                            is_defined.insert(name, (id, scope));
                        }
                    }
                }

                self.from_expr_helper(*expr, scopes, diagnostic, is_defined, import_list, errors);

                if !is_def {
                    let id_var = self.get_identifier_with_scope(name, scopes, true, import_list);
                    if let Some((id, scope)) = id_var {
                        is_defined.insert(name, (id, scope));
                    }
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
                self.from_expr_helper(*e, scopes, diagnostic, is_defined, import_list, errors);
            }
            EbnfExpr::LabelledExpr { label, expr } => {
                let pos = self.len();
                self.from_expr_helper(*expr, scopes, diagnostic, is_defined, import_list, errors);
                let off = self.len() - pos + 1;
                self.get_mut_nodes().insert(
                    pos,
                    VmNode::Label(UniqueString::new(label), pos as u16, off as u16),
                );
            }
            EbnfExpr::DebugPrint => {
                self.print_in_range(0, self.len());
            }
            EbnfExpr::Import(_) => unreachable!("import is already consumed"),
            EbnfExpr::Errors(_) => unreachable!("errors is already consumed"),
            EbnfExpr::ErrorLabel { label, expr, span } => {
                let pos = self.len();
                self.from_expr_helper(*expr, scopes, diagnostic, is_defined, import_list, errors);
                let off = self.len() - pos + 1;
                self.get_mut_nodes().insert(
                    pos,
                    VmNode::ErrorScope {
                        error_id: errors.get(&label).copied().unwrap(),
                        off: off as u16,
                        span,
                    },
                );
            }
        }
    }

    fn len(&self) -> usize {
        self.get_nodes().len()
    }

    fn print_in_range(&self, _start: usize, _end: usize);
}

impl LexerVmNode {
    fn exec(
        vm: &LexerVm,
        state: &mut LexerVmState,
        s: &[u8],
        source_manager: RelativeSourceManager,
        global_diagnostic: &Diagnostic,
        local_diagnostic: &mut Option<Diagnostic>,
    ) -> bool {
        let pc = state.pc;
        if pc >= vm.nodes.len() {
            return false;
        }

        state.push_call_stack(pc);

        let (is_valid, off) = Self::exec_helper(
            vm,
            state,
            s,
            source_manager,
            global_diagnostic,
            local_diagnostic,
        );
        state.pc = (pc + off).min(vm.nodes.len() - 1);

        state.pop_call_stack();
        return is_valid;
    }

    fn exec_helper(
        vm: &LexerVm,
        state: &mut LexerVmState,
        s: &[u8],
        source_manager: RelativeSourceManager,
        global_diagnostic: &Diagnostic,
        local_diagnostic: &mut Option<Diagnostic>,
    ) -> (bool, usize) {
        let node = &vm.nodes[state.pc];
        state.next_pc();

        // state.print_call_stack(vm);

        match node {
            VmNode::Call(addr) => {
                state.pc = *addr as usize;
                if !Self::exec(
                    vm,
                    state,
                    s,
                    source_manager,
                    global_diagnostic,
                    local_diagnostic,
                ) {
                    return (false, 1);
                }
            }
            VmNode::NativeCall(k) => {
                let Some((res, tk)) = k.call(
                    vm,
                    s[state.cursor..].as_ref(),
                    source_manager.shift_relative_pos_by(state.cursor as u32),
                    global_diagnostic,
                    Some(state),
                ) else {
                    return (false, 1);
                };

                let start = state.cursor;
                state.cursor += res.len();

                state.result = Some((Span::from_usize(start, state.cursor), tk));
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
                    state.result =
                        Some((Span::from_usize(start, state.cursor), TokenKind::Unknown));
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
                    state.result =
                        Some((Span::from_usize(start, state.cursor), TokenKind::Unknown));
                    return (true, 1);
                }

                for t in v {
                    match t {
                        TerminalValue::Char(_) => panic!("Char in TerminalHash"),
                        TerminalValue::String(s) => {
                            if slice.starts_with(s.as_bytes()) {
                                let start = state.cursor;
                                state.cursor += s.len();
                                state.result = Some((
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
                        state.result =
                            Some((Span::from_usize(start, state.cursor), TokenKind::Unknown));
                    } else {
                        return (false, 1);
                    }
                } else {
                    if (l..r).contains(&ch) {
                        let start = state.cursor;
                        state.cursor += ch.len_utf8();
                        state.result =
                            Some((Span::from_usize(start, state.cursor), TokenKind::Unknown));
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
                state.result = Some((Span::from_usize(start, state.cursor), TokenKind::Unknown));
            }
            VmNode::Alternative(count, off) => {
                let off = *off as usize;
                let mut found = false;
                for _ in 0..*count {
                    let is_valid = Self::exec(
                        vm,
                        state,
                        s,
                        source_manager,
                        global_diagnostic,
                        local_diagnostic,
                    );
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
                for _ in 0..*count {
                    let res = Self::exec(
                        vm,
                        state,
                        s,
                        source_manager,
                        global_diagnostic,
                        local_diagnostic,
                    );
                    if !res {
                        state.result = old_res;
                        return (false, off);
                    }
                }

                let end = state.cursor;
                state.result = Some((Span::from_usize(start, end), TokenKind::Unknown));

                return (true, off);
            }
            VmNode::Exception(count, off) => {
                let off = *off as usize;
                let start_cursor = state.cursor;
                let old_res = state.result.clone();

                let res = Self::exec(
                    vm,
                    state,
                    s,
                    source_manager,
                    global_diagnostic,
                    local_diagnostic,
                );
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
                        let res = Self::exec(
                            vm,
                            state,
                            s,
                            source_manager,
                            global_diagnostic,
                            local_diagnostic,
                        );
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
                } else {
                    state.result = Some((
                        Span::from_usize(start_cursor, end_cursor),
                        TokenKind::Unknown,
                    ));
                }
                return (res, off);
            }
            VmNode::Optional(off) => {
                let off = *off as usize;
                let _ = Self::exec(
                    vm,
                    state,
                    s,
                    source_manager,
                    global_diagnostic,
                    local_diagnostic,
                );
                return (true, off);
            }
            VmNode::Repetition(off) => {
                let off = *off as usize;

                if s.is_empty() {
                    return (false, off);
                }

                let current_pc = state.pc;
                let old_cursor = state.cursor;

                loop {
                    if s.len() <= state.cursor {
                        break;
                    }

                    let current_cursor = state.cursor;
                    let res = Self::exec(
                        vm,
                        state,
                        s,
                        source_manager,
                        global_diagnostic,
                        local_diagnostic,
                    );
                    if !res {
                        break;
                    }

                    if current_cursor == state.cursor {
                        break;
                    }

                    state.pc = current_pc;
                }

                let res = state.cursor != old_cursor;
                if res {
                    state.result = Some((
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
                let is_valid = Self::exec(
                    vm,
                    state,
                    s,
                    source_manager,
                    global_diagnostic,
                    local_diagnostic,
                );
                if is_valid {
                    let (_, t) = state.result.as_mut().unwrap();
                    *t = TokenKind::CustomToken(*l);
                }
                state.pc = pc + off;
            }
            VmNode::DebugPrint => {}
            VmNode::ErrorScope {
                error_id,
                off,
                span,
            } => {
                let off = *off as usize;
                if s.is_empty() {
                    return (false, off);
                }

                let current_cursor = state.cursor;
                if !Self::exec(
                    vm,
                    state,
                    s,
                    source_manager,
                    global_diagnostic,
                    local_diagnostic,
                ) {
                    let l_diag = local_diagnostic
                        .take()
                        .unwrap_or(DiagnosticBag::new(global_diagnostic.get_filepath()).into());

                    {
                        let text_span =
                            Span::from_usize(current_cursor, state.cursor.max(current_cursor + 1));
                        let error = vm.errors.get(*error_id as usize).unwrap();
                        let info = source_manager.get_source_info(text_span);
                        l_diag
                            .builder()
                            .report(
                                DiagnosticLevel::Error,
                                format!("Error: unable to lex the input"),
                                info,
                                None,
                            )
                            .add_error(error.clone(), Some(source_manager.fix_span(text_span)))
                            .commit();
                    }

                    {
                        let span = *span;
                        let info = source_manager.0.get_source_info(span);
                        l_diag
                            .builder()
                            .report(DiagnosticLevel::Info, "All Rules failed", info, None)
                            .add_info("Rule defined here", Some(source_manager.0.fix_span(span)))
                            .commit();
                    }
                    local_diagnostic.replace(l_diag);
                    return (false, off);
                }

                return (true, off);
            }
        }

        (true, 1)
    }
}

type LexerVmNode = VmNode;

#[derive(Debug)]
pub(crate) struct LexerVm {
    nodes: Vec<LexerVmNode>,
    identifiers: HashMap<UniqueString, (usize, bool)>,
    def_identifiers: Vec<(usize, UniqueString)>,
    errors: SmallVec<[String; 1]>,
    is_scoped: bool,
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
            errors: SmallVec::new(),
            is_scoped: false,
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
        global_diagnostic: &Diagnostic,
        local_diagnostic: &mut Option<Diagnostic>,
        state: Option<&LexerVmState>,
    ) -> LexerMatchResult {
        let mut temp_state = VmState::new(id);
        state.map(|s| temp_state.merge_call_stack(s));

        let is_valid = LexerVmNode::exec(
            self,
            &mut temp_state,
            s,
            source_manager,
            global_diagnostic,
            local_diagnostic,
        );

        if !is_valid {
            None
        } else {
            temp_state.result
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

    fn set_is_scoped(&mut self, is_scoped: bool) {
        self.is_scoped = is_scoped;
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
        import_list: &HashSet<String>,
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
        let has_none = import_list.contains("@none");
        let has_all = import_list.contains("@all");

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

        if has_none {
            return None;
        }

        if has_all || import_list.contains(name.as_str()) {
            scopes
                .get_identifier_with_scope(name)
                .map(|(id, scope)| (id, Some(scope)))
        } else {
            None
        }
    }

    fn get_identifier_from_str<'a, S: AsRef<str>>(
        &self,
        name: S,
        scopes: &'a LexerEbnfParserMatcher,
        should_look_in_current_scope: bool,
        import_list: &HashSet<String>,
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
        self.get_identifier_with_scope(name, scopes, should_look_in_current_scope, import_list)
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

    fn add_error(&mut self, message: String) -> u16 {
        let id = self.errors.len() as u16;
        self.errors.push(message);
        id
    }
}

impl LexerEbnfMatcher for LexerVm {
    fn match_native<'b>(
        &self,
        kind: LexerNativeCallKind,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &crate::eoc::utils::diagnostic::Diagnostic,
        state: Option<&LexerVmState>,
    ) -> Option<(&'b [u8], TokenKind)> {
        if let Some((id, _)) = self.identifiers.get(&kind.as_unique_str()).copied() {
            let temp = self
                .run(id, s, source_manager, diagnostic, &mut None, state)
                .map(|(r, k)| (s[r.as_range()].as_ref(), k));
            temp
        } else {
            kind.call(self, s, source_manager, diagnostic, state)
        }
    }

    fn try_match_expr<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &crate::eoc::utils::diagnostic::Diagnostic,
        state: Option<&LexerVmState>,
    ) -> LexerMatchResult {
        let (tx, rx) = channel();
        rayon::scope(|scope| {
            for (k, name) in self.def_identifiers.iter().rev().copied() {
                let tx = tx.clone();
                scope.spawn(move |_| {
                    let mut local_diagnostic = None;
                    let mut temp = self.run(
                        k,
                        s,
                        source_manager,
                        diagnostic,
                        &mut local_diagnostic,
                        state,
                    );
                    temp.iter_mut().for_each(|(s, t)| {
                        *s = source_manager.abs_span(*s);
                        if *t == TokenKind::Unknown {
                            *t = TokenKind::CustomToken(name);
                        }
                    });
                    let _ = tx.send((k, temp, local_diagnostic));
                });
            }
        });
        let mut res = Vec::new();
        let mut diags = Vec::new();
        for _ in 0..(self.def_identifiers.len()) {
            if let Ok((temp, data, diag)) = rx.recv() {
                if let Some((sp, k)) = data {
                    res.push((temp, (sp, k)))
                }

                if let Some(d) = diag {
                    diags.push(d);
                }
            }
        }

        if res.is_empty() {
            diags.into_iter().for_each(|d| diagnostic.append(d));
        }
        res.sort_unstable_by_key(|(k, _)| *k);
        res.pop().map(|(_, v)| v)
    }

    fn match_for<'b>(
        &self,
        addr: usize,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
    ) -> LexerMatchResult {
        self.run(addr, s, source_manager, diagnostic, &mut None, None)
    }

    fn is_default(&self) -> bool {
        false
    }

    fn is_ir(&self) -> bool {
        false
    }

    fn is_scoped(&self) -> bool {
        self.is_scoped
    }
}
