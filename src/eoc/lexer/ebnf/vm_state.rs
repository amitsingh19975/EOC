#[cfg(debug_assertions)]
use std::sync::{Arc, RwLock};

use crate::eoc::{lexer::token::TokenKind, utils::span::Span};
use super::vm::LexerVm;


#[cfg(debug_assertions)]
pub(crate) struct VmState<T> {
    pub(crate) call_stack: Arc<RwLock<Vec<usize>>>,
    pub(crate) pc: usize,
    pub(crate) cursor: usize,
    pub(crate) result: Option<T>,
}

#[cfg(not(debug_assertions))]
pub(crate) struct VmState<T> {
    pub(crate) pc: usize,
    pub(crate) cursor: usize,
    pub(crate) result: Option<T>,
}

impl<T> Default for VmState<T> {
    fn default() -> Self {
        Self::new(0)
    }
}

impl<T> VmState<T> {
    #[cfg(debug_assertions)]
    pub(crate) fn new(pc: usize) -> Self {
        Self {
            call_stack: Default::default(),
            pc,
            cursor: 0,
            result: None,
        }
    }

    #[cfg(not(debug_assertions))]
    pub(crate) fn new(pc: usize) -> Self {
        Self {
            pc,
            cursor: 0,
            result: None,
        }
    }

    pub(crate) fn next_pc(&mut self) -> usize {
        let pc = self.pc;
        self.pc += 1;
        pc
    }

    #[cfg(debug_assertions)]
    pub(crate) fn print_call_stack(&self, vm: &LexerVm) {
        use crate::eoc::lexer::ebnf::vm::EbnfVm;

        println!("Call Stack: ============================");
        for pc in self.call_stack.read().unwrap().iter().rev() {
            if let Some((_, s)) = vm.get_def_identifiers().iter().find(|(i, _)| *i == *pc) {
                println!("{}: {:?} => {}", pc, vm.get_nodes()[*pc], s);
            } else {
                println!("{}: {:?}", pc, vm.get_nodes()[*pc]);
            }
        }
        println!("========================================\n");
    }

    #[cfg(not(debug_assertions))]
    pub(crate) fn print_call_stack(&self, _vm: &LexerVm) {}

    #[cfg(debug_assertions)]
    pub(crate) fn push_call_stack(&mut self, id: usize) {
        self.call_stack.write().unwrap().push(id);
    }

    #[cfg(not(debug_assertions))]
    pub(crate) fn push_call_stack(&mut self, _id: usize) {}

    #[cfg(debug_assertions)]
    pub(crate) fn pop_call_stack(&mut self) {
        self.call_stack.write().unwrap().pop();
    }

    #[cfg(not(debug_assertions))]
    pub(crate) fn pop_call_stack(&mut self) {}

    #[cfg(debug_assertions)]
    pub(crate) fn merge_call_stack(&mut self, other: &Self) {
        self.call_stack.write().unwrap().extend(other.call_stack.read().unwrap().iter().cloned());
    }

    #[cfg(not(debug_assertions))]
    pub(crate) fn merge_call_stack(&mut self, _other: &Self) {}
}

pub(crate) type LexerVmState = VmState<(Span, TokenKind)>;
