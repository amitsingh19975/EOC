#![allow(dead_code)]

use std::{collections::{HashMap, HashSet}, fmt::Display, rc::Rc};

use crate::eoc::{lexer::{ebnf::expr::{EbnfExpr, TerminalValue}, token::Token}, utils::{diagnostic::{Diagnostic, DiagnosticLevel}, source_manager::SourceManagerDiagnosticInfo, string::UniqueString}};

use super::parser_matcher::{ParserEbnfMatcher, ParserEbnfMatcherResult, ParserEbnfParserMatcher, ParserEbnfParserMatcherInner};


enum ParserVmNode {
    Call(u16),
    ParentScope(u16, Rc<ParserEbnfParserMatcherInner>),
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
    Repetition(u16), // Repeat
    Label(UniqueString, u16), // Label a
    DebugPrint // Debug print the Vm code
}

impl Display for ParserVmNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserVmNode::Call(id) => write!(f, "Call({})", id),
            ParserVmNode::ParentScope(id, _) => write!(f, "ParentScope({})", id),
            ParserVmNode::Terminal(t) => write!(f, "Terminal('{}')", t),
            ParserVmNode::TerminalHash(terms, hash) => write!(f, "TerminalHash({:?}, {:?})", terms, hash),
            ParserVmNode::Range(lhs, rhs, inclusive) => write!(f, "Range({}, {}, {})", lhs, rhs, inclusive),
            ParserVmNode::AnyChar => write!(f, "AnyChar"),
            ParserVmNode::Alternative(a, b) => write!(f, "Alternative({}, {})", a, b),
            ParserVmNode::Concat(a, b) => write!(f, "Concat({}, {})", a, b),
            ParserVmNode::Exception(a, b) => write!(f, "Exception({}, {})", a, b),
            ParserVmNode::Optional(a) => write!(f, "Optional({})", a),
            ParserVmNode::Repetition(a) => write!(f, "Repetition({})", a),
            ParserVmNode::Label(a, b) => write!(f, "Label({}, {})", a, b),
            ParserVmNode::DebugPrint => write!(f, "DebugPrint"),
        }
    }
}

pub(super) struct ParserVm {
    nodes: Vec<ParserVmNode>,
    identifiers: HashMap<UniqueString, (usize, bool)>,
}

impl ParserEbnfMatcher for ParserVm {
    fn match_tokens(&self, id: u16, tokens: &[Token], cursor: usize) -> ParserEbnfMatcherResult {
        ParserEbnfMatcherResult::default()
    }

    fn get_identifier<'a>(&self, name: &UniqueString) -> Option<usize> {
        self.identifiers.get(name).map(|(id, _)| *id)
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
        self.identifiers.get(name).map(|(_, is_def)| *is_def).unwrap_or(false)
    }

    pub(super) fn add_def(&mut self, name: UniqueString, index: usize, is_def: bool) -> Option<usize> {
        if let Some((id, is_def)) = self.identifiers.get(&name).copied() {
            self.identifiers.insert(name, (index, is_def));
            Some(id)
        } else {
            self.identifiers.insert(name, (index, is_def));
            None
        }
    }

    fn get_identifier_with_scope<'a>(&self, name: &UniqueString, scopes: &'a ParserEbnfParserMatcher) -> Option<(usize, Option<Rc<ParserEbnfParserMatcherInner>>)> {
        if let Some((id, _)) =  self.identifiers.get(name).copied() {
            return Some((id, None))
        }

        scopes.get_identifier_with_scope(name).map(|(id, scope)| (id, Some(scope)))
    }

    pub(super) fn get_identifier_from_str<'a, S: AsRef<str>>(&self, name: S, scopes: &'a ParserEbnfParserMatcher) -> Option<(usize, Option<Rc<ParserEbnfParserMatcherInner>>)> {
        let name = UniqueString::try_new(name)?;
        self.get_identifier_with_scope(&name, scopes)
    }

    pub(super) fn len(&self) -> usize {
        self.nodes.len()
    }

    pub(super) fn from<'a>(&mut self, expr: EbnfExpr, scopes: &'a ParserEbnfParserMatcher, diagnostic: &Diagnostic) {
        match expr {
            EbnfExpr::Identifier(s, info) => {
                let Some((id, scope)) = self.get_identifier_from_str(s.as_str(), scopes) else {
                    if let Some((info, span)) = info {
                        diagnostic.builder()
                            .report(DiagnosticLevel::Error, format!("Identifier '{}' is not defined", s), info, None)
                            .add_info("Try to define the identifier", Some(span))
                            .commit();
                    }
                    return;
                };
                if let Some(scope) = scope {
                    self.nodes.push(ParserVmNode::ParentScope(id as u16, scope));
                } else {
                    self.nodes.push(ParserVmNode::Call(id as u16));
                }
            },
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
                    self.nodes.push(ParserVmNode::TerminalHash(terms, h));
                    size += 1;
                }

                if v.is_empty() && (current_len != self.len()) {
                    return;
                }

                size += v.len() as u16;
                for item in v {
                    self.from(item, scopes, diagnostic);
                }

                let off = self.len() - current_len + 1;
                self.nodes
                    .insert(current_len, ParserVmNode::Alternative(size, off as u16));

            },
            EbnfExpr::Concat(v, _) | EbnfExpr::Extend(v, _)  => {
                let current_len = self.len();
                let size = v.len() as u16;
                for item in v {
                    self.from(item, scopes, diagnostic);
                }

                let off = self.len() - current_len + 1;
                self.nodes
                    .insert(current_len, ParserVmNode::Concat(size, off as u16));
            },
            EbnfExpr::Exception(mut v, _) => {
                let current_len = self.len();
                let size = v.len() as u16;
                if v.is_empty() {
                    return;
                }

                let first = v.remove(0);
                self.from(first, scopes, diagnostic);

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
                                self.from(item, scopes, diagnostic);
                                count += 1;
                            }
                        }
                    }

                    if !terms.is_empty() || !hash.is_empty() {
                        self.nodes.push(ParserVmNode::TerminalHash(terms, hash));
                        count += 1;
                    }

                    let off = self.len() - alternative_start + 1;
                    self.nodes
                        .insert(alternative_start, ParserVmNode::Alternative(count as u16, off as u16));
                } else {
                    let item = v.remove(0);
                    self.from(item, scopes, diagnostic);
                }

                let off = self.len() - current_len + 1;
                self.nodes
                    .insert(current_len, ParserVmNode::Exception(size, off as u16));
            },
            EbnfExpr::Optional(e, _) => {
                let current_len = self.len();
                self.from(*e, scopes, diagnostic);
                let off = self.len() - current_len + 1;
                self.nodes.insert(current_len, ParserVmNode::Optional(off as u16));
            },
            EbnfExpr::Repetition(e, _) => {
                let current_len = self.len();
                self.from(*e, scopes, diagnostic);
                let off = self.len() - current_len + 1;
                self.nodes
                    .insert(current_len, ParserVmNode::Repetition(off as u16));
            },
            EbnfExpr::Terminal(t) => {
                self.nodes.push(ParserVmNode::Terminal(t));
            },
            EbnfExpr::Statements(s, _) => {
                for item in s {
                    self.from(item, scopes, diagnostic);
                }
            },
            EbnfExpr::UnboundedExpr(e) => {
                self.from(*e, scopes, diagnostic);
            },
            EbnfExpr::LabelledExpr { label, expr } => {
                let pos = self.len();
                self.from(*expr, scopes, diagnostic);
                let off = self.len() - pos + 1;
                self.nodes.insert(pos, ParserVmNode::Label(UniqueString::new(label), off as u16));
            },
            EbnfExpr::Variable { name, expr, is_def } => {
                let off = self.len();
                let old_id = self.add_def(UniqueString::new(name), off, is_def);
                self.from(*expr, scopes, diagnostic);
                if let Some(old_id) = old_id {
                    self.nodes[off..].iter_mut().for_each(|node| {
                        if let ParserVmNode::Call(id) = node {
                            if off == *id as usize {
                                *id = old_id as u16;
                            }
                        }
                    });
                }
            },
            EbnfExpr::Range { lhs, rhs, inclusive } => {
                self.nodes.push(ParserVmNode::Range(lhs, rhs, inclusive));
            },
            EbnfExpr::AnyChar => {
                self.nodes.push(ParserVmNode::AnyChar);
            },
            EbnfExpr::DebugPrint => {
                self.print();
            }
        }
    }

    pub(super) fn print(&self) {
        self.print_in_range(0, self.len());
    }

    pub(super) fn print_in_range(&self, start: usize, end: usize) {
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

