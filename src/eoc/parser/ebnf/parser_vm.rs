#![allow(dead_code, unused_variables)]

use std::{collections::HashMap, fmt::Display};

use crate::eoc::{
    lexer::{
        ebnf::{
            basic::{EbnfIdentifierMatcher, EbnfNodeMatcher, EbnfParserMatcherInner},
            vm::{EbnfVm, VmNode},
        },
        token::Token,
    },
    utils::{imm_ref::ImmRef, string::UniqueString},
};

use super::{
    default_matcher::DefaultParserEbnfMatcher,
    ir_parser::IRParserMatcher,
    parser_matcher::{ParserEbnfMatcher, ParserEbnfMatcherResult, ParserEbnfParserMatcher},
};

type ParserVmNode = VmNode;

impl Display for ParserVmNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserVmNode::Call(id) => write!(f, "Call({})", id),
            ParserVmNode::NativeCall(k) => write!(f, "NativeCall({:?})", k),
            ParserVmNode::Terminal(t) => write!(f, "Terminal('{}')", t),
            ParserVmNode::TerminalHash(terms, hash) => {
                write!(f, "TerminalHash({:?}, {:?})", terms, hash)
            }
            ParserVmNode::Range(lhs, rhs, inclusive) => {
                write!(f, "Range({}, {}, {})", lhs, rhs, inclusive)
            }
            ParserVmNode::AnyChar => write!(f, "AnyChar"),
            ParserVmNode::Alternative(a, b) => write!(f, "Alternative({}, {})", a, b),
            ParserVmNode::Concat(a, b) => write!(f, "Concat({}, {})", a, b),
            ParserVmNode::Exception(a, b) => write!(f, "Exception({}, {})", a, b),
            ParserVmNode::Optional(a) => write!(f, "Optional({})", a),
            ParserVmNode::Repetition(a) => write!(f, "Repetition({})", a),
            ParserVmNode::Label(a, addr, off) => write!(f, "Label({}, {}, {})", a, addr, off),
            ParserVmNode::DebugPrint => write!(f, "DebugPrint"),
        }
    }
}

#[derive(Debug)]
pub(crate) struct ParserVm {
    nodes: Vec<ParserVmNode>,
    identifiers: HashMap<UniqueString, (usize, bool)>,
}

impl ParserEbnfMatcher for ParserVm {
    fn match_tokens(&self, id: u16, tokens: &[Token], cursor: usize) -> ParserEbnfMatcherResult {
        ParserEbnfMatcherResult::default()
    }
}

impl EbnfNodeMatcher for ParserVm {
    fn get_node(&self, id: usize) -> Option<&VmNode> {
        None
    }
}

impl ParserVm {
    pub(super) fn new() -> Self {
        Self {
            nodes: Vec::new(),
            identifiers: HashMap::new(),
        }
    }

    pub(super) fn contains_def(&self, name: &UniqueString) -> bool {
        self.identifiers
            .get(name)
            .map(|(_, is_def)| *is_def)
            .unwrap_or(false)
    }

    pub(super) fn print(&self) {
        self.print_in_range(0, self.len());
    }
}

impl EbnfIdentifierMatcher for ParserVm {
    fn get_identifier(&self, name: UniqueString) -> Option<usize> {
        self.identifiers.get(&name).map(|(id, _)| *id)
    }
}

impl EbnfVm<DefaultParserEbnfMatcher, ParserVm, IRParserMatcher> for ParserVm {
    fn get_nodes(&self) -> &Vec<VmNode> {
        &self.nodes
    }

    fn get_mut_nodes(&mut self) -> &mut Vec<VmNode> {
        &mut self.nodes
    }

    fn add_def(&mut self, name: UniqueString, index: usize, is_def: bool) -> Option<usize> {
        if let Some((id, is_def)) = self.identifiers.get(&name).copied() {
            self.identifiers.insert(name, (index, is_def));
            Some(id)
        } else {
            self.identifiers.insert(name, (index, is_def));
            None
        }
    }

    fn get_identifier_with_scope<'a>(
        &self,
        name: UniqueString,
        scopes: &'a ParserEbnfParserMatcher,
        should_look_in_current_scope: bool,
    ) -> Option<(
        usize,
        Option<ImmRef<EbnfParserMatcherInner<DefaultParserEbnfMatcher, ParserVm, IRParserMatcher>>>,
    )> {
        if should_look_in_current_scope {
            if let Some((id, _)) = self.identifiers.get(&name).copied() {
                return Some((id, None));
            }
        }

        scopes
            .get_identifier_with_scope(name)
            .map(|(id, scope)| (id, Some(scope)))
    }

    fn get_identifier_from_str<'a, S: AsRef<str>>(
        &self,
        name: S,
        scopes: &'a ParserEbnfParserMatcher,
        should_look_in_current_scope: bool,
    ) -> Option<(
        usize,
        Option<ImmRef<EbnfParserMatcherInner<DefaultParserEbnfMatcher, ParserVm, IRParserMatcher>>>,
    )> {
        let name = UniqueString::try_new(name)?;
        self.get_identifier_with_scope(name, scopes, should_look_in_current_scope)
    }

    fn print_in_range(&self, start: usize, end: usize) {
        for (i, node) in self.nodes[start..end.min(self.len())].iter().enumerate() {
            if let Some((s, _)) = self.identifiers.iter().find(|(_, p)| p.0 == i) {
                print!("{:04}: {} => {}", i, node, s);
            } else {
                print!("{:04}: {}", i, node);
            }
            println!("");
        }
        println!("\nIdentifiers: {:?}", self.identifiers);
    }
}
