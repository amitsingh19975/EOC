use crate::eoc::{lexer::token::Token, utils::string::UniqueString};

use super::parser_matcher::{ParserEbnfMatcher, ParserEbnfMatcherResult};

pub(super) struct DefaultParserEbnfMatcher;

impl DefaultParserEbnfMatcher {
    pub(crate) fn new() -> Self {
        Self
    }

    pub(crate) fn print(&self) {
        println!("<Native Matcher>");
    }
}

impl ParserEbnfMatcher for DefaultParserEbnfMatcher {
    fn match_tokens(&self, id: u16, tokens: &[Token], cursor: usize) -> ParserEbnfMatcherResult {
        ParserEbnfMatcherResult::default()
    }

    fn get_identifier<'a>(&self, name: &UniqueString) -> Option<usize> {
        None
    }
}
