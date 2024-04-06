#![allow(dead_code)]

use std::collections::HashMap;

use smallvec::SmallVec;

use crate::eoc::{
    lexer::token::Token,
    utils::{
        diagnostic::Diagnostic, source_manager::SourceManager, span::Span, string::UniqueString,
    },
};

use super::ebnf::parser_matcher::ParserEbnfParserMatcher;

pub(crate) enum OperatorFixity {
    Prefix,
    Infix,
    Postfix,
    Compound(Span, Span),
    Default,
}

pub(crate) enum OperatorAssociativity {
    Left,
    Right,
}

pub(crate) struct Operator {
    pub(super) precedence: u32,
    pub(super) associativity: OperatorAssociativity,
    pub(super) fixity: OperatorFixity,
}

pub(crate) struct Keyword {
    pub(crate) name: UniqueString,
    pub(crate) token_id: usize,
}

pub(crate) enum NodeKind {
    IntegerLiteral,
    FloatLiteral,
    ValueIdentifier, // %value
    CaretIdentifier, // '^', (Identifier)
    Operations, // OperationResultList?, (GenericOperation | CustomOperation), TrailingLocation?
    OperationResultList, // OperationResult (',' , OperationResult)*
    OperationResult, // ValueIdentifier, (':', IntegerLiteral)?
    SuccessorList, // '[', Successor (',', Successor)*, ']'
    Successor, // ValueIdentifier, (':', IntegerLiteral)?
    Type, // TypeAlias | DialectType | BuiltInType
    TypeAlias, // '!', Identifier,
    TypeAliasDef, // '!', Identifier, '=', Type
    DialectType, // '!', (OpaqueDialectType | PrettyDialectType),
    OpaqueDialectType, // Identifier, DialectTypeBody?
    PrettyDialectType, // Identifier, '.', [A-Za-z][A-Za-z0-9._]*, DialectTypeBody?
    DialectTypeBody, // '<', DialectTypeContent+, '>'
    DialectTypeContent, // DialectTypeBody | '(' DialectTypeContent+ ')' | '[' DialectTypeContent+ ']' | '{' DialectTypeContent+ '}' | AnyThing except '<', '>', '(', ')', '[', ']', '{', '}', and '\0'
    ValueIdentifierWithType, // ValueIdentifier, (':', ValueIdentifier)?
    ValueIdentifierWithTypeList, // ValueIdentifierWithType (',', ValueIdentifierWithType)*
    BlockArgsList, // '(', ValueIdentifierWithTypeList?, ')'
    GenericOperation, // StringLiteral, '(', ValueUseList?, ')', SuccessorList?, DictionaryProperties?, RegionList?, DictionaryAttribute?, ':', FunctionType?
    FunctionType, // (Type | TypeListParens) '->', (Type | TypeListParens)
    TypeListNoParens, // Type, {',', Type}?
    TypeListParens, // '(', ')' | '(', TypeListNoParens ,')'
    SsaUseAndType, // SsaUse, ':', Type
    SsaUse, // ValueUse
    SsaUseAndTypeList, // SsaUseAndType, {',', SsaUseAndType}?
    CustomOperation, // Identifier, CustomOperationFormat
    DictionaryProperties, // '<', DictionaryAttribute, '>'
    DictionaryAttribute, // '{', [AttributeEntry, {',' AttributeEntry}?] ,'}'
    TrailingLocation, // 'loc', '(' , Location, ')'
    RegionList, // '(', 'Region', {',', Region}?, ')'
    Region, // '{', EntryBlock?, { Block }? ,'}'
    EntryBlock, // { Operation }
    AttributeEntry, // (Identifier | StringLiteral), '=', AttributeValue
    AttributeValue, // AttributeAlias | DialectAttribute | BuiltinAttribute
    ValueUseList, // ValueUse, { ',', ValueUse }?
    ValueUse, // ValueIdentifier, ('#', ValueIdentifier)?
    AttributeAliasDef, // '#', Identifier, '=', AttributeValue
    AttributeAlias, // '#', Identifier
}

pub(crate) struct AstNode {
    pub(super) kind: NodeKind,
    pub(super) attr: Span,
    pub(super) token: Span,
    pub(super) children_span: Span,
}

pub(crate) enum BasicAttributeValue {
    Integer(i64),
    Float(f64),
    Boolean(bool),
    String(UniqueString),
    Node(usize),
}

pub(crate) enum AttributeValue {
    Basic(BasicAttributeValue),
    Array(Vec<AttributeValue>),
    Map(HashMap<UniqueString, AttributeValue>),
    Node(usize),
    KeyVal(UniqueString, usize),
}

pub(crate) struct AstAttribute {
    name: UniqueString,
    args: SmallVec<[AttributeValue; 1]>
}

pub(crate) struct AstContext {
    pub(super) nodes: Vec<AstNode>,
    pub(super) attributes: Vec<AstAttribute>,
    pub(super) tokens: Vec<Token>,
    pub(super) source_manager: SourceManager,
    pub(super) custom_operators: Vec<Operator>,
    pub(super) custom_keywords: Vec<Keyword>,
    pub(super) diagnostics: Diagnostic,
    pub(super) import_context_ids: Vec<u32>,
    pub(super) current_id: u32,
    pub(super) exports: Vec<UniqueString>,
    pub(super) global_ebnf_parser: ParserEbnfParserMatcher,
    pub(super) ebnf_parser: HashMap<UniqueString, ParserEbnfParserMatcher>,
}
