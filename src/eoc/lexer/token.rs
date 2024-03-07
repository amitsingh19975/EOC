#![allow(dead_code)]

use crate::eoc::{lexer::utils::escape_string, utils::{source_manager::SourceManager, span::Span, string::UniqueString}};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum TokenKind {
    // Literals
    Identifier,
    Integer,
    FloatingPoint,
    String,
    StartFormattingString,
    EndFormattingString,
    Char,

    // Keywords
    Let, // let
    Var, // var
    Fn, // fn
    If, // if
    Else, // else
    For, // for
    While, // while
    Return, // return
    Break, // break
    Continue, // continue
    True, // true
    False, // false
    Nil, // null
    Import, // import
    As, // as
    Is, // is
    Defer, // defer
    Match, // match
    KwOperator, // operator
    Operator, // +, -, ..
    Enum, // enum
    Public, // public
    Private, // private
    KwSelf, // self
    KwSelfType, // Self
    Mutable, // mut
    In, // in
    Out, // out
    InOut, // inout
    Dollar, // $
    KwKeyword, // keyword
    CustomKeyword, // custom keyword
    CompileTime, // comptime

    // Punctuation
    OpenParen, // (
    CloseParen, // )
    OpenBrace, // {
    CloseBrace, // }
    OpenDoubleBrace, // {{
    CloseDoubleBrace, // }}
    OpenBracket, // [
    CloseBracket, // ]
    Comma, // ,
    Colon, // :
    Semicolon, // ;
    Arrow, // ->
    FatArrow, // =>
    QuestionMark, // \?
    AtSign, // @
    Backslash, // \
    Backtick, // `
    SingleQuote, // '
    DoubleQuote, // "
    Hash, // #
    TripleBackTick, // ````


    // Comment, 
    SingleLineComment,
    MultiLineComment,
    DocComment,

    Space,
    TabSpace,
    Whitespace,
    Newline,
    EndOfFile,

    // ========= Start Ebnf ==========

    Terminal, // This is for the terminal token used in the EBNF grammar. Example: `'0'` in `A = '0'`
    NonTerminal, // This is for the non-terminal token used in the EBNF grammar. Example: `A` in `A = B`
    Pipe, // This is for the pipe token used in the EBNF grammar. Example: `|` in `A = B | C`
    Asterisk, // This is for the asterisk token used in the EBNF grammar. Example: `*` in `A = B* C`
    Plus, // This is for the plus token used in the EBNF grammar. Example: `+` in `A = B+ C`
    Equal, // This is for the equal token used in the EBNF grammar. Example: `=` in `A = '0'`
    Dot, // This is for the dot token used in the EBNF grammar. Example: `.` in `A = B . C`
    Comment, // This is a catch-all for any comment
    Exception, // This is for the exception token used in the EBNF grammar. Example: `-` in `A = B - C`
    LeftExtend, // This is for the extend token used in the EBNF grammar. Example: `<<` in `A <+= C`
    RightExtend, // This is for the extend token used in the EBNF grammar. Example: `<<` in `A +>= C`
    Definition, // This is for the definition token used in the EBNF grammar. Example: `::=` in `A ::= B`
    Range, // This is for the range token used in the EBNF grammar. Example: `..` in `A = B .. C`
    RangeEqual, // This is for the range equal token used in the EBNF grammar. Example: `..=` in `A = B ..= C`

    // ========= End Ebnf ==========

    CustomToken(UniqueString),
    Unknown,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct Token {
    pub(crate) kind: TokenKind,
    pub(crate) span: Span,
}

impl Token {
    pub(crate) fn new(kind: TokenKind, span: Span) -> Token {
        Token {
            kind,
            span,
        }
    }

    pub(crate) fn new_eof(span: Span) -> Token {
        Self {
            kind: TokenKind::EndOfFile,
            span,
        }
    }

    pub(crate) fn len(&self) -> usize {
        self.span.len()
    }

    pub(crate) fn as_str<'a>(&self, source_manager: &'a SourceManager) -> &'a str {
        let source = &source_manager[self.span];
        std::str::from_utf8(source).expect("invalid utf-8 string slice")
    }

    pub(crate) fn is_eof(&self) -> bool {
        self.kind == TokenKind::EndOfFile
    }

    pub(crate) fn is_newline(&self) -> bool {
        self.kind == TokenKind::Newline
    }

    pub(crate) fn is_whitespace(&self) -> bool {
        self.kind == TokenKind::Whitespace
    }
    
    pub(crate) fn is_space(&self) -> bool {
        self.kind == TokenKind::Space
    }

    pub(crate) fn is_tab_space(&self) -> bool {
        self.kind == TokenKind::TabSpace
    }

    pub(crate) fn is_any_whitespace(&self) -> bool {
        self.is_newline() || self.is_whitespace() || self.is_space() || self.is_tab_space()
    }

    pub(crate) fn is_comment(&self) -> bool {
        self.kind == TokenKind::SingleLineComment || self.kind == TokenKind::MultiLineComment
    }

    pub(crate) fn is_punctuation(&self) -> bool {
        match self.kind {
            TokenKind::OpenParen | 
            TokenKind::CloseParen | 
            TokenKind::OpenBrace |
            TokenKind::CloseBrace |
            TokenKind::OpenBracket |
            TokenKind::CloseBracket |
            TokenKind::Comma |
            TokenKind::Colon |
            TokenKind::Semicolon |
            TokenKind::Arrow |
            TokenKind::FatArrow |
            TokenKind::QuestionMark |
            TokenKind::AtSign |
            TokenKind::Backslash |
            TokenKind::Backtick |
            TokenKind::SingleQuote |
            TokenKind::DoubleQuote |
            TokenKind::Hash => true,
            _ => false,
            
        }
    }

    pub(crate) fn is_arrow(&self, source_manager: &SourceManager) -> bool {
        let source = &source_manager[self.span];
        (self.kind == TokenKind::Arrow) || ((self.kind == TokenKind::Operator) && (source == b"->"))
    }

    pub(crate) fn is_fat_arrow(&self, source_manager: &SourceManager) -> bool {
        let source = &source_manager[self.span];
        (self.kind == TokenKind::FatArrow) || ((self.kind == TokenKind::Operator) && (source == b"=>"))
    }

    pub(crate) fn is_operator(&self) -> bool {
        self.kind == TokenKind::Operator
    }

    pub(crate) fn is_triple_back_tick(&self) -> bool {
        self.kind == TokenKind::TripleBackTick
    }

    pub(crate) fn is_formatting_string_start(&self) -> bool {
        self.kind == TokenKind::StartFormattingString
    }

    pub(crate) fn is_formatting_string_end(&self) -> bool {
        self.kind == TokenKind::EndFormattingString
    }

    pub(crate) fn is_comma(&self) -> bool {
        self.kind == TokenKind::Comma
    }

    pub(crate) fn is_colon(&self) -> bool {
        self.kind == TokenKind::Colon
    }

    pub(crate) fn is_semicolon(&self) -> bool {
        self.kind == TokenKind::Semicolon
    }

    pub(crate) fn is_at_sign(&self) -> bool {
        self.kind == TokenKind::AtSign
    }

    pub(crate) fn is_backslash(&self) -> bool {
        self.kind == TokenKind::Backslash
    }

    pub(crate) fn is_backtick(&self) -> bool {
        self.kind == TokenKind::Backtick
    }

    pub(crate) fn is_single_quote(&self) -> bool {
        self.kind == TokenKind::SingleQuote
    }

    pub(crate) fn is_double_quote(&self) -> bool {
        self.kind == TokenKind::DoubleQuote
    }

    pub(crate) fn is_hash(&self) -> bool {
        self.kind == TokenKind::Hash
    }

    pub(crate) fn is_identifier(&self) -> bool {
        self.kind == TokenKind::Identifier
    }

    pub(crate) fn is_integer(&self) -> bool {
        self.kind == TokenKind::Integer
    }

    pub(crate) fn is_floating_point(&self) -> bool {
        self.kind == TokenKind::FloatingPoint
    }

    pub(crate) fn is_string(&self) -> bool {
        self.kind == TokenKind::String
    }

    pub(crate) fn is_char(&self) -> bool {
        self.kind == TokenKind::Char
    }

    pub(crate) fn is_open_paren(&self) -> bool {
        self.kind == TokenKind::OpenParen
    }

    pub(crate) fn is_close_paren(&self) -> bool {
        self.kind == TokenKind::CloseParen
    }

    pub(crate) fn is_open_brace(&self) -> bool {
        self.kind == TokenKind::OpenBrace
    }

    pub(crate) fn is_close_brace(&self) -> bool {
        self.kind == TokenKind::CloseBrace
    }

    pub(crate) fn is_open_bracket(&self) -> bool {
        self.kind == TokenKind::OpenBracket
    }

    pub(crate) fn is_close_bracket(&self) -> bool {
        self.kind == TokenKind::CloseBracket
    }
    
    pub(crate) fn is_operator_keyword(&self) -> bool {
        self.kind == TokenKind::KwOperator
    }

    pub(crate) fn is_enum_keyword(&self) -> bool {
        self.kind == TokenKind::Enum
    }

    pub(crate) fn is_public_keyword(&self) -> bool {
        self.kind == TokenKind::Public
    }

    pub(crate) fn is_private_keyword(&self) -> bool {
        self.kind == TokenKind::Private
    }

    pub(crate) fn is_self_keyword(&self) -> bool {
        self.kind == TokenKind::KwSelf
    }

    pub(crate) fn is_self_type_keyword(&self) -> bool {
        self.kind == TokenKind::KwSelfType
    }

    pub(crate) fn is_mutable_keyword(&self) -> bool {
        self.kind == TokenKind::Mutable
    }

    pub(crate) fn is_in_keyword(&self) -> bool {
        self.kind == TokenKind::In
    }

    pub(crate) fn is_out_keyword(&self) -> bool {
        self.kind == TokenKind::Out
    }

    pub(crate) fn is_inout_keyword(&self) -> bool {
        self.kind == TokenKind::InOut
    }

    pub(crate) fn is_dollar_keyword(&self) -> bool {
        self.kind == TokenKind::Dollar
    }

    pub(crate) fn is_if_keyword(&self) -> bool {
        self.kind == TokenKind::If
    }

    pub(crate) fn is_else_keyword(&self) -> bool {
        self.kind == TokenKind::Else
    }

    pub(crate) fn is_for_keyword(&self) -> bool {
        self.kind == TokenKind::For
    }

    pub(crate) fn is_while_keyword(&self) -> bool {
        self.kind == TokenKind::While
    }

    pub(crate) fn is_return_keyword(&self) -> bool {
        self.kind == TokenKind::Return
    }

    pub(crate) fn is_break_keyword(&self) -> bool {
        self.kind == TokenKind::Break
    }

    pub(crate) fn is_continue_keyword(&self) -> bool {
        self.kind == TokenKind::Continue
    }

    pub(crate) fn is_true_keyword(&self) -> bool {
        self.kind == TokenKind::True
    }

    pub(crate) fn is_false_keyword(&self) -> bool {
        self.kind == TokenKind::False
    }

    pub(crate) fn is_nil_keyword(&self) -> bool {
        self.kind == TokenKind::Nil
    }

    pub(crate) fn is_import_keyword(&self) -> bool {
        self.kind == TokenKind::Import
    }

    pub(crate) fn is_as_keyword(&self) -> bool {
        self.kind == TokenKind::As
    }

    pub(crate) fn is_is_keyword(&self) -> bool {
        self.kind == TokenKind::Is
    }

    pub(crate) fn is_defer_keyword(&self) -> bool {
        self.kind == TokenKind::Defer
    }

    pub(crate) fn is_match_keyword(&self) -> bool {
        self.kind == TokenKind::Match
    }

    pub(crate) fn is_keyword_keyword(&self) -> bool {
        self.kind == TokenKind::KwKeyword
    }

    pub(crate) fn is_custom_keyword(&self) -> bool {
        self.kind == TokenKind::CustomKeyword
    }

    pub(crate) fn is_compile_time(&self) -> bool {
        self.kind == TokenKind::CompileTime
    }

    pub(crate) fn is_keyword(&self) -> bool {
        match self.kind {
            TokenKind::Let |
            TokenKind::Var |
            TokenKind::Fn |
            TokenKind::If |
            TokenKind::Else |
            TokenKind::For |
            TokenKind::While |
            TokenKind::Return |
            TokenKind::Break |
            TokenKind::Continue |
            TokenKind::True |
            TokenKind::False |
            TokenKind::Nil |
            TokenKind::Import |
            TokenKind::Public |
            TokenKind::Private |
            TokenKind::As |
            TokenKind::Is |
            TokenKind::Defer |
            TokenKind::Match |
            TokenKind::KwOperator |
            TokenKind::Enum |
            TokenKind::KwSelf |
            TokenKind::KwSelfType |
            TokenKind::Mutable |
            TokenKind::In |
            TokenKind::Out |
            TokenKind::InOut |
            TokenKind::Dollar |
            TokenKind::CustomKeyword |
            TokenKind::KwKeyword |
            TokenKind::CompileTime => true,
            _ => false,
        }
    }

    pub(crate) fn is_literal(&self) -> bool {
        match self.kind {
            TokenKind::Integer |
            TokenKind::FloatingPoint |
            TokenKind::String |
            TokenKind::Char => true,
            _ => false,
        }
    }

    pub(crate) fn is_match_paren(&self, other: &Token) -> bool {
        match (self.kind, other.kind) {
            (TokenKind::OpenParen, TokenKind::CloseParen) |
            (TokenKind::OpenBrace, TokenKind::CloseBrace) |
            (TokenKind::OpenBracket, TokenKind::CloseBracket) => true,
            _ => false,
        }
    }

    pub(crate) fn to_string(&self, source_manager: &SourceManager) -> String {
        if let TokenKind::CustomToken(s) = self.kind {
            format!("Token(CustomToken('{}'), '{}', {:?}, len={})", s, escape_string(self.as_str(source_manager)), self.span, self.len())
        } else {
            format!("Token({:?}, '{}', {:?}, len={})", self.kind, escape_string(self.as_str(source_manager)), self.span, self.len())
        }
    }
}

impl From<&[u8]> for TokenKind {
    fn from(value: &[u8]) -> Self {
        match value {
            b"let" => TokenKind::Let,
            b"var" => TokenKind::Var,
            b"fn" => TokenKind::Fn,
            b"if" => TokenKind::If,
            b"else" => TokenKind::Else,
            b"for" => TokenKind::For,
            b"while" => TokenKind::While,
            b"return" => TokenKind::Return,
            b"break" => TokenKind::Break,
            b"continue" => TokenKind::Continue,
            b"true" => TokenKind::True,
            b"false" => TokenKind::False,
            b"nil" => TokenKind::Nil,
            b"import" => TokenKind::Import,
            b"as" => TokenKind::As,
            b"is" => TokenKind::Is,
            b"defer" => TokenKind::Defer,
            b"match" => TokenKind::Match,
            b"operator" => TokenKind::KwOperator,
            b"enum" => TokenKind::Enum,
            b"public" => TokenKind::Public,
            b"private" => TokenKind::Private,
            b"self" => TokenKind::KwSelf,
            b"Self" => TokenKind::KwSelfType,
            b"mut" => TokenKind::Mutable,
            b"in" => TokenKind::In,
            b"out" => TokenKind::Out,
            b"inout" => TokenKind::InOut,
            b"$" => TokenKind::Dollar,
            b"keyword" => TokenKind::KwKeyword,
            b"comptime" => TokenKind::CompileTime,
            _ => TokenKind::Identifier,
        }
    }
}