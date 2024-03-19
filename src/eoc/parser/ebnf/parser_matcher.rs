#![allow(dead_code)]

use std::{collections::HashMap, fmt::Display, rc::Rc};

use crate::eoc::{lexer::{ebnf::expr::EbnfExpr, token::Token}, utils::{diagnostic::Diagnostic, span::Span, string::UniqueString}};

use super::{default_matcher::DefaultParserEbnfMatcher, parser_vm::ParserVm};

pub(crate) struct ParserEbnfMatcherResult {
    token_ids: Vec<Span>,
    labels: HashMap<UniqueString, Span>
}

impl Default for ParserEbnfMatcherResult {
    fn default() -> Self {
        Self::new()
    }
}

impl ParserEbnfMatcherResult {
    pub(crate) fn new() -> Self {
        Self {
            token_ids: Vec::new(),
            labels: HashMap::new()
        }
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.token_ids.is_empty() && self.labels.is_empty()
    }

    pub(crate) fn add_token_id(&mut self, start: usize, end: usize) {
        self.token_ids.push(Span::from_usize(start, end));
    }

    pub(crate) fn add_label(&mut self, label: UniqueString, start: usize, end: usize) {
        self.add_token_id(start, end);
        let span = Span::from_usize(start, end);
        self.labels.insert(label, span);
    }

    pub(crate) fn get_token<'a>(&self, index: usize, tokens: &'a [Token]) -> &'a [Token] {
        let Some(span) = self.token_ids.get(index) else {
            return &[];
        };
        let start = span.start as usize;
        let end = span.end as usize;
        tokens[start..end].as_ref()
    }

    pub(crate) fn get_label<'a>(&self, label: UniqueString, tokens: &'a [Token]) -> &'a [Token] {
        let Some(span) = self.labels.get(&label) else {
            return &[];
        };

        let start = span.start as usize;
        let end = span.end as usize;
        tokens[start..end].as_ref()
    }

    pub(crate) fn get_label_from_str<'a, S: AsRef<str>>(&self, label: S, tokens: &'a [Token]) -> &'a [Token] {
        let Some(label) = UniqueString::try_new(label) else {
            return &[];
        };
        self.get_label(label, tokens)
    }

    pub(crate) fn get_label_span(&self, label: UniqueString) -> Option<Span> {
        self.labels.get(&label).copied()
    }

    pub(crate) fn get_label_span_from_str<S: AsRef<str>>(&self, label: S) -> Option<Span> {
        let Some(label) = UniqueString::try_new(label) else {
            return None;
        };
        self.get_label_span(label)
    }

    pub(crate) fn get_token_span(&self, index: usize) -> Option<&Span> {
        self.token_ids.get(index)
    }

    pub(crate) fn get_token_ids(&self) -> &[Span] {
        self.token_ids.as_ref()
    }

    pub(crate) fn get_labels(&self) -> &HashMap<UniqueString, Span> {
        &self.labels
    }
}

pub(crate) trait ParserEbnfMatcher {
    fn match_tokens(&self, id: u16, tokens: &[Token], cursor: usize) -> ParserEbnfMatcherResult;
    fn get_identifier<'a>(&self, name: &UniqueString) -> Option<usize>;
}

pub(super) enum ParserEbnfParserMatcherInner {
    Default(DefaultParserEbnfMatcher),
    Vm(ParserVm)
}

impl ParserEbnfMatcher for ParserEbnfParserMatcherInner {
    fn match_tokens(&self, id: u16, tokens: &[Token], cursor: usize) -> ParserEbnfMatcherResult {
        match self {
            Self::Default(default_matcher) => default_matcher.match_tokens(id, tokens, cursor),
            Self::Vm(vm) => vm.match_tokens(id, tokens, cursor)
        }
    }

    fn get_identifier<'a>(&self, name: &UniqueString) -> Option<usize> {
        match self {
            Self::Default(default_matcher) => default_matcher.get_identifier(name),
            Self::Vm(vm) => vm.get_identifier(name)
        }
    }
}

impl ParserEbnfParserMatcherInner {
    pub(crate) fn new() -> Self {
        Self::Default(DefaultParserEbnfMatcher::new())
    }

    pub(crate) fn from_expr<'a>(expr: EbnfExpr, scopes: &'a ParserEbnfParserMatcher, diagnostic: &Diagnostic) -> Self {
        let mut vm = ParserVm::new();
        vm.from(expr, scopes, diagnostic);
        Self::Vm(vm)
    }

    pub(crate) fn print(&self) {
        match self {
            Self::Default(default_matcher) => default_matcher.print(),
            Self::Vm(vm) => vm.print()
        }
    }
}

#[derive(Clone)]
pub(crate) struct ParserEbnfParserMatcher {
    pub(super) parent_scope: Option<Rc<ParserEbnfParserMatcher>>,
    pub(super) current_scope: Rc<ParserEbnfParserMatcherInner>
}

impl ParserEbnfMatcher for ParserEbnfParserMatcher {
    fn match_tokens(&self, id: u16, tokens: &[Token], cursor: usize) -> ParserEbnfMatcherResult {
        self.current_scope.match_tokens(id, tokens, cursor)
    }

    fn get_identifier<'a>(&self, name: &UniqueString) -> Option<usize> {
        let mut scope = self;
        while let Some(parent_scope) = scope.parent_scope.as_ref() {
            if let Some(id) = parent_scope.current_scope.get_identifier(name) {
                return Some(id);
            }
            scope = parent_scope.as_ref();
        }
        None
    }
}

impl Default for ParserEbnfParserMatcher {
    fn default() -> Self {
        Self {
            parent_scope: None,
            current_scope: Rc::new(ParserEbnfParserMatcherInner::new())
        }
    }
}

impl ParserEbnfParserMatcher {
    pub(crate) fn new(parent_scope: Option<Rc<ParserEbnfParserMatcher>>, expr: EbnfExpr, diagnostic: &Diagnostic) -> Self { 
        let parent = parent_scope.unwrap_or(Rc::new(Self::default()));
        let  temp = ParserEbnfParserMatcherInner::from_expr(expr, &parent, diagnostic);
        Self {
            parent_scope: Some(parent),
            current_scope: Rc::new(temp)
        }
    }

    pub(super) fn get_identifier_with_scope(&self, name: &UniqueString) -> Option<(usize, Rc<ParserEbnfParserMatcherInner>)> {
        let mut scope = self;
        while let Some(parent_scope) = scope.parent_scope.as_ref() {
            if let Some(id) = parent_scope.current_scope.get_identifier(name) {
                return Some((id, parent_scope.current_scope.clone()));
            }
            scope = parent_scope.as_ref();
        }
        None
    }

    fn print(&self) {
        println!("{}", self);
    }
}

impl Display for ParserEbnfParserMatcher {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        {
            self.current_scope.print();
        }

        if let Some(parent_scope) = self.parent_scope.as_ref() {
            writeln!(f, "Parent Scope: ")?;
            parent_scope.print();
        }

        Ok(())
    }
}