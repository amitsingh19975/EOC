use std::collections::HashMap;

use smallvec::SmallVec;

use crate::eoc::{
    ast::identifier::Identifier,
    utils::{span::Span, string::UniqueString},
    parser::ebnf::parser_matcher::ParserEbnfParserMatcher,
};

pub(crate) enum OperationInner {
    GenericOperation,
    CustomOperation,
}

// OperationResultList?, (GenericOperation | CustomOperation), TrailingLocation?
pub(crate) struct Operation {
    result_list: SmallVec<[OperationResult; 1]>,
    inner: OperationInner,
    location: Option<Span>,
}

// ValueIdentifier, // '%', Identifier
// ValueIdentifier, (':', IntegerLiteral)?
pub(crate) struct OperationResult {
    value: Identifier,
    arity: u32,
}

// StringLiteral, '(', ValueUseList?, ')', SuccessorList?, DictionaryProperties?, RegionList?, DictionaryAttribute?, ':', FunctionType?
pub(crate) struct GenericOperation {
    name: Identifier,
    value_use_list: SmallVec<[ValueUse; 1]>,
    successor_list: SmallVec<[Successor; 1]>,
    dictionary_properties: SmallVec<[DictionaryAttribute; 1]>,
    region_list: SmallVec<[Region; 1]>,
    dictionary_attribute: Option<DictionaryAttribute>,
    function_type: Option<FunctionType>,
}

// ValueIdentifier, ('#', ValueIdentifier)?
pub(crate) struct ValueUse {
    value: Identifier,
    access: Option<Identifier>,
}

// CaretIdentifier, (':', BlockArgsList)?
pub(crate) struct Successor {
    value: Identifier,
    block_args_list: SmallVec<[ValueIdentifierWithType; 1]>,
}

// ValueIdentifier, (':', ValueIdentifier)?
pub(crate) struct ValueIdentifierWithType {
    value: Identifier,
    type_: Option<Identifier>,
}

// AttributeAlias | DialectAttribute | BuiltinAttribute
pub(crate) enum AttributeValue {
    AttributeAlias(Identifier),
    DialectAttribute(DialectAttribute),
    BuiltinAttribute(BuiltinAttribute),
}

// '{', [AttributeEntry, {',' AttributeEntry}?] ,'}'
pub(crate) struct DictionaryAttribute {
    inner: HashMap<UniqueString, AttributeValue>,
}

// '#', (OpaqueDialectAttribute | PrettyDialectAttribute),
pub(crate) enum DialectAttribute {
    OpaqueDialectType(OpaqueDialectAttribute),
    PrettyDialectType(PrettyDialectAttribute),
}

// https://mlir.llvm.org/docs/Dialects/Builtin/
pub(crate) enum BuiltinAttribute {}

pub(crate) struct OpaqueDialectAttribute {
    name: Identifier,
    body: Option<DialectAttributeBody>,
}

pub(crate) enum ParenKind {
    Round,
    Square,
    Curly,
    Angle,
}

pub(crate) struct DialectAttributeBody {
    content: SmallVec<[DialectAttributeContent; 1]>,
    paren_kind: ParenKind,
}

pub(crate) enum DialectAttributeContent {
    Content(Identifier),
    AttrBody(SmallVec<[u16; 1]>),
}

// Identifier, '.', [A-Za-z][A-Za-z0-9._]*, DialectAttributeBody?
pub(crate) struct PrettyDialectAttribute {
    name: Identifier,
    member: SmallVec<[Identifier; 1]>,
    body: Option<DialectAttributeBody>,
}

// '{', EntryBlock?, { Block }? ,'}'
pub(crate) struct Region {
    entry_block: SmallVec<[Operation; 1]>,
    blocks: SmallVec<[Block; 1]>,
}

// CaretIdentifier, BlockArgsList?, ':', OperationList
pub(crate) struct Block {
    label: Identifier,
    args_list: SmallVec<[ValueIdentifierWithType; 1]>,
    operations: SmallVec<[Operation; 1]>,
}

// (Type | TypeListParens) '->', (Type | TypeListParens)
// TypeListNoParens ::= Type, {',', Type}?
// TypeListParens ::= '(', ')' | '(', TypeListNoParens ,')'
pub(crate) struct FunctionType {
    input: Type,
    output: Type,
}

// TypeAlias | DialectType | BuiltInType
pub(crate) enum Type {
    TypeAlias(Identifier),
    DialectType(DialectType),
    BuiltInType(BuiltInType),
}

pub(crate) enum DialectType {
    OpaqueDialectType(OpaqueDialectType),
    PrettyDialectType(PrettyDialectType),
}

// Identifier, DialectTypeBody?
pub(crate) struct OpaqueDialectType {
    name: Identifier,
    body: Option<DialectTypeBody>,
}

// Identifier, '.', [A-Za-z][A-Za-z0-9._]*, DialectTypeBody?
pub(crate) struct PrettyDialectType {
    name: Identifier,
    member: SmallVec<[Identifier; 1]>,
    body: Option<DialectTypeBody>,
}

// '<', DialectTypeContent+, '>'
pub(crate) struct DialectTypeBody {
    content: SmallVec<[DialectTypeContent; 1]>,
    paren_kind: ParenKind,
}

// DialectTypeBody | '(' DialectTypeContent+ ')' | '[' DialectTypeContent+ ']' | '{' DialectTypeContent+ '}' | AnyThing except '<', '>', '(', ')', '[', ']', '{', '}', and '\0'
pub(crate) enum DialectTypeContent {
    Content(Identifier),
    AttrBody(SmallVec<[u16; 1]>),
}

// https://mlir.llvm.org/docs/Dialects/Builtin/
pub(crate) enum BuiltInType {
    BFloat16,
    Complex {
        element_id: usize,
    },
    Float8E4M3B11FNUZ,
    Float8E4M3FN,
    Float8E4M3FNUZ,
    Float8E5M2,
    Float8E5M2FNUZ,
    Float16,
    Float32,
    Float64,
    Float80,
    Float128,
    FloatTF32,
    Function {
        input_id: usize,
        output_id: usize,
    },
    Index,
    Integer {
        width: u32,
        is_signed: bool,
    },
    MemRef {
        element_id: usize,
        extent: SmallVec<[i64; 1]>,
        layout: Option<AttributeValue>,
        memory_space: Option<AttributeValue>,
    },
    None,
    Opaque {
        element_id: usize,
    },
    RankedTensor {
        element_id: usize,
        extent: SmallVec<[i64; 1]>,
        encoding: Option<AttributeValue>,
    },
    Tuple {
        element_ids: SmallVec<[usize; 1]>,
    },
    UnrankedMemRef {
        element_id: usize,
        memory_space: Option<AttributeValue>,
    },
    UnrankedTensor {
        element_id: usize,
    },
    Vector {
        element_id: usize,
        extent: SmallVec<[i64; 1]>,
        scalable: bool,
    },
}

pub(crate) struct CustomOperation {
    name: Identifier,
    format: ParserEbnfParserMatcher,
}
