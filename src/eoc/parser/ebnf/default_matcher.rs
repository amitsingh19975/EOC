use lazy_static::lazy_static;

use crate::eoc::{lexer::token::Token, utils::string::UniqueString};

use super::parser_matcher::{ParserEbnfMatcher, ParserEbnfMatcherResult};

struct NativaKind {
    expr: UniqueString,
    block: UniqueString,
    statement: UniqueString,
    binary_op: UniqueString,
    unary_op: UniqueString,
    literal: UniqueString,
    identifier: UniqueString,
    type_: UniqueString,
}

impl NativaKind {
    fn new() -> Self {
        Self {
            expr: UniqueString::new("expr"),
            block: UniqueString::new("block"),
            statement: UniqueString::new("statement"),
            binary_op: UniqueString::new("binary_op"),
            unary_op: UniqueString::new("unary_op"),
            literal: UniqueString::new("literal"),
            identifier: UniqueString::new("identifier"),
            type_: UniqueString::new("type"),
        }
    }
}

lazy_static! {
    static ref NATIVE_KINDS_NAMES: NativaKind = NativaKind::new();
}

enum ParserNativeKind {
    Expr,
    Block,
    Statement,
    BinaryOp,
    UnaryOp,
    Literal,
    Identifier,
    Type,
}

impl ParserNativeKind {
    fn from_str(name: UniqueString) -> Option<Self> {
        match name {
            _ if NATIVE_KINDS_NAMES.expr == name => Some(Self::Expr),
            _ if NATIVE_KINDS_NAMES.block == name => Some(Self::Block),
            _ if NATIVE_KINDS_NAMES.statement == name => Some(Self::Statement),
            _ if NATIVE_KINDS_NAMES.binary_op == name => Some(Self::BinaryOp),
            _ if NATIVE_KINDS_NAMES.unary_op == name => Some(Self::UnaryOp),
            _ if NATIVE_KINDS_NAMES.literal == name => Some(Self::Literal),
            _ if NATIVE_KINDS_NAMES.identifier == name => Some(Self::Identifier),
            _ if NATIVE_KINDS_NAMES.type_ == name => Some(Self::Type),
            _ => None,
        }
    }
    
    fn to_id(&self) -> u16 {
        match self {
            Self::Expr => 0,
            Self::Block => 1,
            Self::Statement => 2,
            Self::BinaryOp => 3,
            Self::UnaryOp => 4,
            Self::Literal => 5,
            Self::Identifier => 6,
            Self::Type => 7,
        }
    }
}

pub(super) struct DefaultParserEbnfMatcher(UniqueString);

impl DefaultParserEbnfMatcher {
    pub(crate) fn new() -> Self {
        // FIX: this is a way make lazy_static eagerly initialized
        Self(NATIVE_KINDS_NAMES.expr)
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
        ParserNativeKind::from_str(*name).map(|kind| kind.to_id() as usize)
    }
}
