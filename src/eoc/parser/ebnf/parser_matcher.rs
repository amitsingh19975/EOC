#![allow(dead_code)]

use std::{collections::HashMap, fmt::Display};

use crate::eoc::{lexer::{ebnf::{expr::EbnfExpr, basic::{EbnfParserMatcher, EbnfParserMatcherInner}, vm::EbnfVm}, token::Token}, utils::{diagnostic::Diagnostic, span::Span, string::UniqueString}};
use crate::eoc::utils::imm_ref::{Ref, ImmRef};
use super::{default_matcher::DefaultParserEbnfMatcher, ir_parser::IRParserMatcher, parser_vm::ParserVm};

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
}

pub(super) type ParserEbnfParserMatcherInner = EbnfParserMatcherInner<DefaultParserEbnfMatcher, ParserVm, IRParserMatcher>;

impl ParserEbnfMatcher for ParserEbnfParserMatcherInner {
    fn match_tokens(&self, id: u16, tokens: &[Token], cursor: usize) -> ParserEbnfMatcherResult {
        match self {
            Self::Default(default_matcher) => default_matcher.match_tokens(id, tokens, cursor),
            Self::Vm(vm) => vm.match_tokens(id, tokens, cursor),
            Self::IR(ir_matcher) => ir_matcher.match_tokens(id, tokens, cursor),
        }
    }
}

impl ParserEbnfParserMatcherInner {

    pub(crate) fn from_expr<'a>(expr: EbnfExpr, scopes: &'a ParserEbnfParserMatcher, diagnostic: &Diagnostic) -> Self {
        let mut vm = ParserVm::new();
        vm.from_expr(expr, scopes, diagnostic, &mut Default::default());
        Self::Vm(vm)
    }

    pub(crate) fn print(&self) {
        match self {
            Self::Default(default_matcher) => default_matcher.print(),
            Self::Vm(vm) => vm.print(),
            Self::IR(ir_matcher) => ir_matcher.print(),
        }
    }
}

pub(crate) type ParserEbnfParserMatcher = EbnfParserMatcher<DefaultParserEbnfMatcher, ParserVm, IRParserMatcher>;

impl ParserEbnfMatcher for ParserEbnfParserMatcher {
    fn match_tokens(&self, id: u16, tokens: &[Token], cursor: usize) -> ParserEbnfMatcherResult {
        self.current_scope.as_ref().match_tokens(id, tokens, cursor)
    }
}

impl Default for ParserEbnfParserMatcher {
    fn default() -> Self {
        Self {
            parent_scope: None,
            current_scope: ParserEbnfParserMatcherInner::new().into()
        }
    }
}

impl ParserEbnfParserMatcher {
    pub(crate) fn new(parent_scope: Option<ImmRef<ParserEbnfParserMatcher>>, expr: EbnfExpr, diagnostic: &Diagnostic) -> Self { 
        let def = Ref::new(ParserEbnfParserMatcher::default()).take_as_imm_ref();
        let parent = parent_scope.unwrap_or(def);
        let  temp = ParserEbnfParserMatcherInner::from_expr(expr, &parent, diagnostic);
        Self {
            parent_scope: Some(parent),
            current_scope: temp.into()
        }
    }

    fn print(&self) {
        println!("{}", self);
    }
}

impl Display for ParserEbnfParserMatcher {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        {
            self.current_scope.as_ref().print();
        }

        if let Some(parent_scope) = self.parent_scope.as_ref() {
            writeln!(f, "Parent Scope: ")?;
            parent_scope.print();
        }

        Ok(())
    }
}