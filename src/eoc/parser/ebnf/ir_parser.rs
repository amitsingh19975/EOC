#![allow(dead_code, unused_variables)]
use std::collections::HashSet;

use smallvec::SmallVec;

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
pub(crate) struct IRParserMatcher {
    errors: SmallVec<[String; 1]>,
}

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
        import_list: &HashSet<String>
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
        import_list: &HashSet<String>
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
    
    fn add_error(&mut self, message: String) -> u16 {
        let id = self.errors.len() as u16;
        self.errors.push(message);
        id
    }
}

impl EbnfIdentifierMatcher for IRParserMatcher {
    fn get_identifier(&self, name: UniqueString) -> Option<usize> {
        todo!()
    }
}
