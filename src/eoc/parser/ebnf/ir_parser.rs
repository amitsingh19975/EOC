#![allow(dead_code, unused_variables)]
use crate::eoc::{
    lexer::ebnf::{
        basic::{EbnfIdentifierMatcher, EbnfNodeMatcher, EbnfParserMatcher, EbnfParserMatcherInner},
        vm::{EbnfVm, VmNode},
    },
    utils::string::UniqueString,
};

use super::{
    default_matcher::DefaultParserEbnfMatcher, parser_matcher::ParserEbnfMatcher,
    parser_vm::ParserVm,
};

#[derive(Debug)]
pub(crate) struct IRParserMatcher;

impl ParserEbnfMatcher for IRParserMatcher {
    fn match_tokens(
        &self,
        id: u16,
        tokens: &[crate::eoc::lexer::token::Token],
        cursor: usize,
    ) -> super::parser_matcher::ParserEbnfMatcherResult {
        todo!()
    }
}

impl IRParserMatcher {
    pub(crate) fn print(&self) {
        todo!()
    }
}

impl EbnfNodeMatcher for IRParserMatcher {
    fn get_node(&self, id: usize) -> Option<&VmNode> {
        None
    }
}

impl EbnfVm<DefaultParserEbnfMatcher, ParserVm, IRParserMatcher> for IRParserMatcher {
    fn get_nodes(&self) -> &Vec<VmNode> {
        todo!()
    }

    fn get_mut_nodes(
        &mut self,
    ) -> &mut Vec<VmNode> {
        todo!()
    }

    fn add_def(&mut self, name: UniqueString, index: usize, is_def: bool) -> Option<usize> {
        todo!()
    }

    fn get_identifier_with_scope<'a>(
        &self,
        name: UniqueString,
        scopes: &'a EbnfParserMatcher<
            DefaultParserEbnfMatcher,
            ParserVm,
            IRParserMatcher,
        >,
        should_look_in_current_scope: bool,
    ) -> Option<(
        usize,
        Option<
            crate::eoc::utils::imm_ref::ImmRef<
                EbnfParserMatcherInner<
                    DefaultParserEbnfMatcher,
                    ParserVm,
                    IRParserMatcher,
                >,
            >,
        >,
    )> {
        todo!()
    }

    fn get_identifier_from_str<'a, S: AsRef<str>>(
        &self,
        name: S,
        scopes: &'a EbnfParserMatcher<
            DefaultParserEbnfMatcher,
            ParserVm,
            IRParserMatcher,
        >,
        should_look_in_current_scope: bool,
    ) -> Option<(
        usize,
        Option<
            crate::eoc::utils::imm_ref::ImmRef<
                EbnfParserMatcherInner<
                    DefaultParserEbnfMatcher,
                    ParserVm,
                    IRParserMatcher,
                >,
            >,
        >,
    )> {
        todo!()
    }

    fn print_in_range(&self, _start: usize, _end: usize) {
        todo!()
    }
}

impl EbnfIdentifierMatcher for IRParserMatcher {
    fn get_identifier(&self, name: UniqueString) -> Option<usize> {
        todo!()
    }
}
