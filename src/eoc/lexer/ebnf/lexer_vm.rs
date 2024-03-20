use std::{
    collections::{HashMap, HashSet},
    rc::Rc, sync::mpsc::channel,
};

use crate::eoc::{
    lexer::{
        str_utils::{get_utf8_char_len, ByteToCharIter},
        token::TokenKind,
    }, utils::{
        diagnostic::{Diagnostic, DiagnosticLevel},
        string::UniqueString,
    }
};

use super::{
    ast::RelativeSourceManager,
    expr::{EbnfExpr, TerminalValue},
    lexer_matcher::LexerEbnfMatcher,
    native_call::{LexerNativeCallKind, NATIVE_CALL_KIND_ID},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum Value {
    Bool(bool),
}

impl Value {
    fn as_bool(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum LexerVmNode {
    Call(u16, String),
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
    Repetition(u16), // Repeat if [stack-value]
    DebugPrint, // Debug print the VM code
}

impl LexerVmNode {
    fn exec<'b>(
        vm: &LexerVm,
        state: &mut VmState,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
    ) {
        let pc = state.pc;
        if pc >= vm.nodes.len() {
            return;
        }
        
        state.push_call_stack(pc);

        let off = Self::exec_helper(vm, state, s, source_manager, diagnostic);
        state.pc = (pc + off).min(vm.nodes.len() - 1);
        
        state.pop_call_stack();
    }

    fn exec_helper<'b>(
        vm: &LexerVm,
        state: &mut VmState,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
    ) -> usize {
        let node = &vm.nodes[state.pc];
        state.next_pc();
        match node {
            LexerVmNode::Terminal(t) => {
                let slice = s[state.cursor..].as_ref();

                let is_matched = match t {
                    TerminalValue::String(t) => slice.starts_with(t.as_bytes()),
                    TerminalValue::Char(c) => ByteToCharIter::new(slice).next() == Some(*c),
                };

                if is_matched {
                    state.cursor += t.len_utf8();
                    state.push_bool(true);
                } else {
                    state.push_bool(false);
                }
            }
            LexerVmNode::Call(addr, ..) => {
                state.pc = *addr as usize;
                Self::exec(vm, state, s, source_manager, diagnostic);
            }
            LexerVmNode::NativeCall(k) => {
                if let Some(temp_s) =
                    k.call_vm(vm, s[state.cursor..].as_ref(), source_manager, diagnostic)
                {
                    state.cursor += temp_s.len();
                    state.push_bool(true);
                } else {
                    state.push_bool(false);
                }
            }
            LexerVmNode::TerminalHash(v, h) => {
                let slice = s[state.cursor..].as_ref();
                let Some(ch) = ByteToCharIter::new(slice).next() else {
                    state.push_bool(false);
                    return 1;
                };

                if h.contains(&TerminalValue::Char(ch)) {
                    state.cursor += ch.len_utf8();
                    state.push_bool(true);
                    return 1;
                }

                let mut found = false;
                for t in v {
                    match t {
                        TerminalValue::Char(c) => panic!("Invalid terminal value: {:?}", c),
                        TerminalValue::String(a) => {
                            if slice.starts_with(a.as_bytes()) {
                                state.cursor += a.len();
                                found = true;
                                break;
                            }
                        }
                    }
                }

                state.stack.push(Value::Bool(found));
            }
            LexerVmNode::Range(a, b, inclusive) => {
                let l = *a;
                let r = *b;

                let slice = s[state.cursor..].as_ref();
                let Some(ch) = ByteToCharIter::new(slice).next() else {
                    state.push_bool(false);
                    return 1;
                };

                if *inclusive {
                    if (l..=r).contains(&ch) {
                        state.cursor += ch.len_utf8();
                        state.push_bool(true);
                    } else {
                        state.push_bool(false);
                    }
                } else {
                    if (l..r).contains(&ch) {
                        state.cursor += ch.len_utf8();
                        state.push_bool(true);
                    } else {
                        state.push_bool(false);
                    }
                }
            }
            LexerVmNode::AnyChar => {
                let slice = s[state.cursor..].as_ref();
                let Some(ch) = ByteToCharIter::new(slice).next() else {
                    state.push_bool(false);
                    return 1;
                };
                
                state.cursor += ch.len_utf8();
                state.push_bool(true);
            }
            LexerVmNode::Alternative(count, off) => {
                let off = *off as usize;
                let mut found = false;

                for _ in 0..*count {
                    Self::exec(vm, state, s, source_manager, diagnostic);
                    let res = state.pop_bool();
                    if res {
                        found = true;
                        break;
                    }
                }

                state.push_bool(found);
                return off;
            }
            LexerVmNode::Concat(count, off) => {
                let off = *off as usize;
                for _ in 0..*count {
                    Self::exec(vm, state, s, source_manager, diagnostic);
                    let res = state.pop_bool();
                    if !res {
                        state.push_bool(false);
                        return off;
                    }
                }

                state.push_bool(true);
                return off;
            }
            LexerVmNode::Exception(count, off) => {
                let off = *off as usize;
                let start_cursor = state.cursor;
                
                Self::exec(vm, state, s, source_manager, diagnostic);
                let mut end_cursor = state.cursor;

                if !state.pop_bool() || start_cursor == end_cursor {
                    state.cursor = start_cursor;
                    state.push_bool(false);
                    return off;
                }
                
                for _ in 1..*count {
                    let pc = state.pc;
                    for i in (start_cursor..end_cursor).rev() {
                        state.pc = pc;
                        state.cursor = i;
                        let temp_start = state.cursor;
                        Self::exec(vm, state, s, source_manager, diagnostic);
                        let temp_end = state.cursor;
                        let res = state.pop_bool();
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
                state.push_bool(start_cursor < end_cursor);
                return off;
            }
            LexerVmNode::Optional(off) => {
                let off = *off as usize;
                Self::exec(vm, state, s, source_manager, diagnostic);
                let _ = state.pop_bool();
                state.push_bool(true);
                return off;
            }
            LexerVmNode::Repetition(off) => {
                let off = *off as usize;

                if s.is_empty() {
                    state.push_bool(false);
                    return off;
                }

                let current_pc = state.pc;
                let old_cursor = state.cursor;
                loop {
                    if s.len() <= state.cursor {
                        break;
                    }
                    
                    let current_cursor = state.cursor;
                    Self::exec(vm, state, s, source_manager, diagnostic);
                    
                    let res = state.pop_bool();
                    
                    if !res {
                        break;
                    }

                    if current_cursor == state.cursor {
                        break;
                    }

                    state.pc = current_pc;
                }

                state
                    .stack
                    .push(Value::Bool(old_cursor != state.cursor));

                return off;
            }
            LexerVmNode::DebugPrint => {
                // Do nothing
            }
        }
        1
    }
}

#[cfg(debug_assertions)]
struct VmState {
    stack: Vec<Value>,
    call_stack: Vec<usize>,
    pc: usize,
    cursor: usize
}

#[cfg(not(debug_assertions))]
struct VmState {
    stack: Vec<Value>,
    pc: usize,
    cursor: usize
}

impl VmState {
    #[cfg(debug_assertions)]
    fn new(pc: usize) -> Self {
        Self {
            stack: Vec::new(),
            call_stack: Vec::new(),
            pc,
            cursor: 0
        }
    }

    #[cfg(not(debug_assertions))]
    fn new(pc: usize) -> Self {
        Self {
            stack: Vec::new(),
            pc,
            cursor: 0
        }
    }

    fn next_pc(&mut self) -> usize {
        let pc = self.pc;
        self.pc += 1;
        pc
    }

    fn push_bool(&mut self, b: bool) {
        self.stack.push(Value::Bool(b));
    }

    fn pop_bool(&mut self) -> bool {
        self.stack.pop().expect("Stack is empty").as_bool()
    }

    fn peek_bool(&self) -> bool {
        self.stack.last().expect("Stack is empty").as_bool()
    }

    #[cfg(debug_assertions)]
    fn print_call_stack(&self, vm: &LexerVm) {
        println!("Call Stack: ============================");
        for pc in self.call_stack.iter().rev() {
            if let Some((_, s)) = vm.def_identifiers.iter().find(|(i, _)| *i == *pc) {
                println!("{}: {:?} => {}", pc, vm.nodes[*pc], s);
            } else {
                println!("{}: {:?}", pc, vm.nodes[*pc]);
            }
        }
        println!("========================================\n");
    }

    #[cfg(not(debug_assertions))]
    fn print_call_stack(&self, _vm: &LexerVm) {}

    #[cfg(debug_assertions)]
    fn push_call_stack(&mut self, id: usize) {
        self.call_stack.push(id);
    }

    #[cfg(not(debug_assertions))]
    fn push_call_stack(&mut self, _id: usize) {}
    
    #[cfg(debug_assertions)]
    fn pop_call_stack(&mut self) {
        self.call_stack.pop();
    }

    #[cfg(not(debug_assertions))]
    fn pop_call_stack(&mut self) {}
}

#[derive(Debug, Clone)]
pub(crate) struct LexerVm {
    nodes: Vec<LexerVmNode>,
    identifiers: HashMap<String, (usize, bool)>,
    def_identifiers: Vec<(usize, UniqueString)>,
}

impl LexerVm {
    pub fn new() -> Self {
        Self {
            nodes: Vec::new(),
            identifiers: HashMap::new(),
            def_identifiers: Vec::new(),
        }
    }

    pub fn run<'b>(
        &self,
        id: usize,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
    ) -> Option<&'b [u8]> {

        let mut state = VmState::new(id);
        
        LexerVmNode::exec(self, &mut state, s, source_manager, diagnostic);

        let temp_s = s[..state.cursor.min(s.len())].as_ref();

        let value = state.pop_bool();
        if !value {
            None
        } else {
            Some(temp_s)
        }
    }

    fn match_native_operator<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager,
        diagnostic: &Diagnostic,
    ) -> Option<&'b [u8]> {
        if s.is_empty() {
            return None;
        }
        if self
            .match_native(LexerNativeCallKind::StartOperator, s, source_manager, diagnostic)
            .is_none()
        {
            return None;
        }

        let mut i = get_utf8_char_len(s[0]);
        while i < s.len() {
            let end = get_utf8_char_len(s[i]);
            let temp_source = &s[i..];
            if self
                .match_native(
                    LexerNativeCallKind::ContOperator,
                    temp_source,
                    source_manager,
                    diagnostic,
                )
                .is_none()
            {
                break;
            }

            i += end;
        }

        Some(&s[..i])
    }

    fn match_expr_helper<'b>(
        &self,
        id: usize,
        symbol: UniqueString,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
    ) -> Option<(&'b [u8], TokenKind)> {
        let key = symbol.as_str();
        match symbol {
            u_str if u_str == NATIVE_CALL_KIND_ID.identifier_sym => {
                let temp = if !self.contains_def(key) {
                    self.match_native_identifier(s, source_manager, diagnostic)
                        .map(|s| (s, TokenKind::Identifier))
                } else {
                    self.run(id, s, source_manager, diagnostic)
                        .map(|s| (s, TokenKind::Identifier))
                };
                temp
            }
            u_str if u_str == NATIVE_CALL_KIND_ID.operator_sym => {
                let temp = if !self.contains_def(key) {
                    self.match_native_operator(s, source_manager, diagnostic)
                        .map(|s| (s, TokenKind::Operator))
                } else {
                    self.run(id, s, source_manager, diagnostic)
                        .map(|s| (s, TokenKind::Operator))
                };
                temp
            }
            u_str if u_str == NATIVE_CALL_KIND_ID.fp_sym => {
                let temp = if !self.contains_def(key) {
                    self.match_native_floating_point(s, source_manager, diagnostic)
                        .map(|s| (s, TokenKind::FloatingPoint))
                } else {
                    self.run(id, s, source_manager, diagnostic)
                        .map(|s| (s, TokenKind::FloatingPoint))
                };
                temp
            }
            u_str if u_str == NATIVE_CALL_KIND_ID.integer_sym => {
                let temp = if !self.contains_def(key) {
                    self.match_native_integer(s, source_manager, diagnostic)
                        .map(|s| (s, TokenKind::Integer))
                } else {
                    self.run(id, s, source_manager, diagnostic)
                        .map(|s| (s, TokenKind::Integer))
                };
                temp
            }
            _ => self
                .run(id, s, source_manager, diagnostic)
                .map(|s| (s, TokenKind::CustomToken(symbol))),
        }
    }

    pub(crate) fn match_native_identifier<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
    ) -> Option<&'b [u8]> {
        if s.is_empty() {
            return None;
        }

        if self
            .match_native(
                LexerNativeCallKind::StartIdentifier,
                s,
                source_manager,
                diagnostic,
            )
            .is_none()
        {
            return None;
        }

        let mut i = get_utf8_char_len(s[0]);
        while i < s.len() {
            let end = get_utf8_char_len(s[i]);
            let temp_source = &s[i..(i + end).min(s.len() - 1)];
            if self
                .match_native(
                    LexerNativeCallKind::ContIdentifier,
                    temp_source,
                    source_manager,
                    diagnostic,
                )
                .is_none()
            {
                return Some(&s[..i]);
            }
            i += end;
        }

        Some(&s[..i])
    }

    pub(super) fn print(&self) {
        self.print_in_range(0, self.nodes.len());
    }

    pub(super) fn print_in_range(&self, start: usize, end: usize) {
        for (i, node) in self.nodes[start..end.min(self.nodes.len())].iter().enumerate() {
            if let Some((s, _)) = self.identifiers.iter().find(|(_, p)| p.0 == i) {
                println!("{:04}: {:?} => {}", i, node, s);
            } else {
                println!("{:04}: {:?}", i, node);
            }
            
        }
        println!("\nIdentifiers: {:?}", self.def_identifiers);
    }

    fn set_base_offset(&mut self, base_off: usize) {
        for node in self.nodes.iter_mut() {
            match node {
                LexerVmNode::Call(addr, _) => *addr += base_off as u16,
                _ => {}
            }
        }

        for (_, (id, _)) in self.identifiers.iter_mut() {
            *id += base_off;
        }

        for (id, _) in self.def_identifiers.iter_mut() {
            *id += base_off;
        }
    }

    fn merge(&mut self, mut vm: LexerVm) {
        let base_off = self.nodes.len();
        vm.set_base_offset(base_off);
        let mut replace_offsets = HashMap::new();

        let identifier = vm.identifiers.drain();

        for (k, (off, is_def)) in identifier.into_iter() {
            if let Some(c_p) = self.identifiers.get(&k).copied() {
                replace_offsets.insert(off, c_p.0);
                self.def_identifiers.iter_mut().for_each(|(id, _)| {
                    if *id == c_p.0 {
                        *id = off;
                    }
                });
            } else {
                self.def_identifiers
                    .push((off, UniqueString::new(k.as_str())));
                self.identifiers.insert(k, (off, is_def));
            }
        }

        vm.replace_call(replace_offsets);
    }

    fn replace_call(&mut self, replace_offsets: HashMap<usize, usize>) {
        for node in self.nodes.iter_mut() {
            match node {
                LexerVmNode::Call(addr, ..) => {
                    if let Some(new_addr) = replace_offsets.get(&(*addr as usize)) {
                        *addr = *new_addr as u16;
                    }
                }
                _ => {}
            }
        }
    }

    fn id_to_string(&self, id: usize) -> String {
        if let Some((_, s)) = self.def_identifiers.iter().find(|(i, _)| *i == id) {
            s.as_str().to_string()
        } else {
            id.to_string()
        }
    }
}

impl LexerEbnfMatcher for LexerVm {
    fn init<'b>(
        &mut self,
        expr: Option<EbnfExpr>,
        _source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
    ) {
        let Some(expr) = expr else {
            return;
        };

        let mut builder = VmBuilder::new();
        builder.from(expr, diagnostic);
        let vm = builder.build();
        self.merge(vm);
    }

    fn contains_def(&self, name: &str) -> bool {
        self.identifiers.get(name).map(|(_, is_def)| *is_def).unwrap_or(false)
    }

    fn match_operator<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
    ) -> Option<&'b [u8]> {
        if let Some((id, _)) = self
            .identifiers
            .get(NATIVE_CALL_KIND_ID.operator_sym.as_ref())
            .copied()
        {
            self.run(id, s, source_manager, diagnostic)
        } else {
            self.match_native_operator(s, source_manager, diagnostic)
        }
    }

    fn match_identifier<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
    ) -> Option<&'b [u8]> {
        if let Some((id, _)) = self
            .identifiers
            .get(NATIVE_CALL_KIND_ID.identifier_sym.as_ref())
            .copied()
        {
            self.run(id, s, source_manager, diagnostic)
        } else {
            self.match_native_identifier(s, source_manager, diagnostic)
        }
    }

    fn match_native<'b>(
        &self,
        kind: super::native_call::LexerNativeCallKind,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
    ) -> Option<&'b [u8]> {
        if let Some((id, _)) = self.identifiers.get(kind.as_str()).copied() {
            self.run(id, s, source_manager, diagnostic)
        } else {
            kind.call_vm(self, s, source_manager, diagnostic)
        }
    }

    fn match_expr_for<'a>(
        &self,
        var: &str,
        s: &'a [u8],
        source_manager: RelativeSourceManager<'a>,
        diagnostic: &Diagnostic,
    ) -> Option<&'a [u8]> {
        if let Some((id, _)) = self.identifiers.get(var).copied() {
            self.run(id, s, source_manager, diagnostic)
        } else {
            None
        }
    }

    fn try_match_expr<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
    ) -> Option<(&'b [u8], crate::eoc::lexer::token::TokenKind)> {
        let (tx, rx) = channel();
        rayon::scope(|scope| {
            for (k, symbol) in self.def_identifiers.iter().rev().copied() {
                let tx = tx.clone();
                scope.spawn(move |_| {
                    if let Some((s, kind)) =
                        self.match_expr_helper(k, symbol, s, source_manager, diagnostic)
                    {
                        tx.send(Some((k, s, kind))).unwrap();
                    } else {
                        tx.send(None).unwrap();
                    }
                });
            }
        });
        let mut res = Vec::new();
        for _ in 0..(self.def_identifiers.len()) {
            if let Ok(Some((k, s, kind))) = rx.recv() {
                res.push((k, s, kind));
            }
        }
        res.sort_unstable_by_key(|(k, _, _)| *k);
        res.pop().map(|(_, s, kind)| (s, kind))
    }

    fn is_binary_digit<'b>(
            &self,
            s: &'b [u8],
            source_manager: RelativeSourceManager<'b>,
            diagnostic: &Diagnostic,
        ) -> Option<char> {
        if let Some((id, true)) = self.identifiers.get(NATIVE_CALL_KIND_ID.is_binary_digit.as_ref()) {
            if let Some(s) = self.run(*id, s, source_manager, diagnostic) {
                return ByteToCharIter::new(s).next();
            }
        } else {
            if let Some(s) = self.match_native(LexerNativeCallKind::BinDigit, s, source_manager, diagnostic) {
                return ByteToCharIter::new(s).next();
            }
        }
        None
    }

    fn is_digit<'b>(
            &self,
            s: &'b [u8],
            source_manager: RelativeSourceManager<'b>,
            diagnostic: &Diagnostic,
        ) -> Option<char> {
        if let Some((id, true)) = self.identifiers.get(NATIVE_CALL_KIND_ID.is_digit.as_ref()) {
            if let Some(s) = self.run(*id, s, source_manager, diagnostic) {
                return ByteToCharIter::new(s).next();
            }
        } else {
            if let Some(s) = self.match_native(LexerNativeCallKind::Digit, s, source_manager, diagnostic) {
                return ByteToCharIter::new(s).next();
            }
        }
        None
    }

    fn is_hex_digit<'b>(
            &self,
            s: &'b [u8],
            source_manager: RelativeSourceManager<'b>,
            diagnostic: &Diagnostic,
        ) -> Option<char> {
        if let Some((id, true)) = self.identifiers.get(NATIVE_CALL_KIND_ID.is_hex_digit.as_ref()) {
            if let Some(s) = self.run(*id, s, source_manager, diagnostic) {
                return ByteToCharIter::new(s).next();
            }
        } else {
            if let Some(s) = self.match_native(LexerNativeCallKind::HexDigit, s, source_manager, diagnostic) {
                return ByteToCharIter::new(s).next();
            }
        }
        None
    }

    fn is_oct_digit<'b>(
            &self,
            s: &'b [u8],
            source_manager: RelativeSourceManager<'b>,
            diagnostic: &Diagnostic,
        ) -> Option<char> {
        if let Some((id, true)) = self.identifiers.get(NATIVE_CALL_KIND_ID.is_oct_digit.as_ref()) {
            if let Some(s) = self.run(*id, s, source_manager, diagnostic) {
                return ByteToCharIter::new(s).next();
            }
        } else {
            if let Some(s) = self.match_native(LexerNativeCallKind::OctDigit, s, source_manager, diagnostic) {
                return ByteToCharIter::new(s).next();
            }
        }
        None
    }
}

pub(super) struct VmBuilder {
    nodes: Vec<LexerVmNode>,
    def_identifiers: Rc<Vec<(usize, UniqueString)>>,
    all_defined_identifiers: Rc<HashMap<String, (usize, bool)>>,
}

impl VmBuilder {
    pub(super) fn new() -> Self {
        Self {
            nodes: Vec::new(),
            def_identifiers: Rc::new(Vec::new()),
            all_defined_identifiers: Rc::new(HashMap::new()),
        }
    }

    pub(super) fn build(self) -> LexerVm {
        LexerVm {
            nodes: self.nodes,
            def_identifiers: Rc::try_unwrap(self.def_identifiers).unwrap(),
            identifiers: Rc::try_unwrap(self.all_defined_identifiers).unwrap(),
        }
    }

    pub(super) fn add_identifier(&mut self, key: String, id: usize, is_def: bool) {
        if self.all_defined_identifiers.contains_key(&key) {
            return;
        }

        if is_def {
            Rc::get_mut(&mut self.def_identifiers)
                .unwrap()
                .push((id, UniqueString::new(key.as_str())));
        }

        Rc::get_mut(&mut self.all_defined_identifiers).unwrap().insert(key, (id, is_def));
    }

    pub(super) fn get_identifier(&self, id: &str) -> Option<(usize, bool)> {
        self.all_defined_identifiers.get(id).copied()
    }

    pub(super) fn len(&self) -> usize {
        self.nodes.len()
    }

    pub(super) fn is_empty(&self) -> bool {
        self.nodes.is_empty()
    }

    pub(super) fn print_in_range(&self, start: usize, end: usize) {
        for (i, node) in self.nodes[start..end.min(self.nodes.len())].iter().enumerate() {
            if let Some((s, _)) = self.all_defined_identifiers.iter().find(|(_, p)| p.0 == i) {
                println!("{:04}: {:?} => {}", i, node, s);
            } else {
                println!("{:04}: {:?}", i, node);
            }
            
        }
        println!("\nIdentifiers: {:?}", self.def_identifiers);
    }

    pub(super) fn from<'b>(&mut self, value: EbnfExpr, diagnostic: &Diagnostic) {
        match value {
            EbnfExpr::Identifier(s, info) => {
                if LexerNativeCallKind::is_valid_name(&s) {
                    self.nodes.push(LexerVmNode::NativeCall(LexerNativeCallKind::from(s.as_str())));
                } else {
                    if let Some((id, _)) = self.get_identifier(s.as_str()) {
                        self.nodes.push(LexerVmNode::Call(id as u16, s));
                    } else {
                        if let Some((info, span)) = info {
                            diagnostic
                                .builder()
                                .report(DiagnosticLevel::Error, "Unknown identifier", info, None)
                                .add_error(format!("Try defining '{s}'"), Some(span))
                                .commit();
                        } else {
                            panic!("Unknown identifier: {}", s);
                        }
                    }
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
                    self.nodes.push(LexerVmNode::TerminalHash(terms, h));
                    size += 1;
                }

                if v.is_empty() && (current_len != self.len()) {
                    return;
                }

                size += v.len() as u16;
                for item in v {
                    self.from(item, diagnostic);
                }

                let off = self.nodes.len() - current_len + 1;
                self.nodes
                    .insert(current_len, LexerVmNode::Alternative(size, off as u16));
            }
            EbnfExpr::Concat(v, _) | EbnfExpr::Extend(v, _) => {
                let current_len = self.len();
                let size = v.len() as u16;
                for item in v {
                    self.from(item, diagnostic);
                }

                let off = self.nodes.len() - current_len + 1;
                self.nodes
                    .insert(current_len, LexerVmNode::Concat(size, off as u16));
            }
            EbnfExpr::Exception(mut v, _) => {
                let current_len = self.len();
                let size = v.len() as u16;
                if v.is_empty() {
                    return;
                }

                let first = v.remove(0);
                self.from(first, diagnostic);

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
                                self.from(item, diagnostic);
                                count += 1;
                            }
                        }
                    }

                    if !terms.is_empty() || !hash.is_empty() {
                        self.nodes.push(LexerVmNode::TerminalHash(terms, hash));
                        count += 1;
                    }

                    let off = self.len() - alternative_start + 1;
                    self.nodes
                        .insert(alternative_start, LexerVmNode::Alternative(count as u16, off as u16));
                } else {
                    let item = v.remove(0);
                    self.from(item, diagnostic);
                }

                let off = self.nodes.len() - current_len + 1;
                self.nodes
                    .insert(current_len, LexerVmNode::Exception(size, off as u16));
            }
            EbnfExpr::Optional(e, _) => {
                let current_len = self.len();
                self.from(*e, diagnostic);
                let off = self.nodes.len() - current_len + 1;
                self.nodes.insert(current_len, LexerVmNode::Optional(off as u16));
            }
            EbnfExpr::Repetition(e, _) => {
                let current_len = self.len();
                self.from(*e, diagnostic);
                let off = self.nodes.len() - current_len + 1;
                self.nodes
                    .insert(current_len, LexerVmNode::Repetition(off as u16));
            }
            EbnfExpr::Terminal(t) => {
                self.nodes.push(LexerVmNode::Terminal(t));
            }
            EbnfExpr::Statements(v, _) => {
                v.into_iter().for_each(|item| self.from(item, diagnostic));
            }
            EbnfExpr::Variable { name, expr, is_def } => {
                self.add_identifier(name, self.len(), is_def);
                self.from(*expr, diagnostic);
            }
            EbnfExpr::Range {
                lhs,
                rhs,
                inclusive,
            } => {
                self.nodes.push(LexerVmNode::Range(lhs, rhs, inclusive));
            }
            EbnfExpr::AnyChar => {
                self.nodes.push(LexerVmNode::AnyChar);
            }
            EbnfExpr::UnboundedExpr(_) => panic!("Unbounded expressions not supported"),
            EbnfExpr::LabelledExpr { .. } => panic!("labelled expressions not supported"),
            EbnfExpr::DebugPrint => {
                self.print_in_range(0, self.len());
            }
        }
    }
}
