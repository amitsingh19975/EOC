use std::collections::HashMap;

use crate::eoc::{
    lexer::{token::Token, utils::CustomOperator},
    utils::{
        diagnostic::Diagnostic, source_manager::SourceManager, span::Span, string::UniqueString,
    },
};

use super::ebnf::parser_matcher::ParserEbnfParserMatcher;

pub(crate) enum OperatorFixity {
    Prefix,
    Postfix,
    Infix,
}

pub(crate) enum OperatorAssociativity {
    Left,
    Right,
}

pub(crate) enum NodeKind {
    IntegerLiteral,
    FloatLiteral,
}

pub(crate) struct AstNode {
    pub(super) kind: NodeKind,
    pub(super) attributes_id: usize,
    pub(super) token_id: usize,
    pub(super) children_span: Span,
}

pub(crate) struct AstAttribute {
    name: UniqueString,
    args_start: usize,  // Token start index
    args_end: usize,    // Token end index
    open_paren: usize,  // Token index
    close_paren: usize, // Token index
}

pub(crate) struct AstContext {
    pub(super) nodes: Vec<AstNode>,
    pub(super) attributes: Vec<AstAttribute>,
    pub(super) tokens: Vec<Token>,
    pub(super) source_manager: SourceManager,
    pub(super) custom_operators: Vec<CustomOperator>,
    pub(super) custom_keywords: Vec<UniqueString>,
    pub(super) diagnostics: Diagnostic,
    pub(super) import_context_ids: Vec<u32>,
    pub(super) current_id: u32,
    pub(super) exports: Vec<UniqueString>,
    pub(super) global_ebnf_parser: ParserEbnfParserMatcher,
    pub(super) ebnf_parser: HashMap<UniqueString, ParserEbnfParserMatcher>,
}
