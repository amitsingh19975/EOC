use std::{collections::HashSet, fmt::Display};

use crate::eoc::{
    lexer::{str_utils::ByteToCharIter, token::TokenKind},
    utils::{source_manager::SourceManagerDiagnosticInfo, span::Span},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum BinaryOperator {
    Alternative, // "|"
    Concat,      // ","
    Exception,   // "-"
    Extend,      // "+"
    Range,       // ".."
    RangeEqual,  // "..."
}

impl From<TokenKind> for BinaryOperator {
    fn from(kind: TokenKind) -> Self {
        match kind {
            TokenKind::Pipe => Self::Alternative,
            TokenKind::Comma => Self::Concat,
            TokenKind::Exception => Self::Exception,
            TokenKind::Plus => Self::Extend,
            TokenKind::Range => Self::Range,
            TokenKind::RangeEqual => Self::RangeEqual,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum UnaryOperator {
    Optional,
}

impl From<TokenKind> for UnaryOperator {
    fn from(kind: TokenKind) -> Self {
        match kind {
            TokenKind::QuestionMark => Self::Optional,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum TerminalValue {
    String(String),
    Char(char),
}

impl TerminalValue {
    pub(crate) fn is_empty(&self) -> bool {
        match self {
            TerminalValue::String(s) => s.is_empty(),
            TerminalValue::Char(_) => false,
        }
    }

    pub(crate) fn len(&self) -> usize {
        match self {
            TerminalValue::String(s) => s.chars().count(),
            TerminalValue::Char(_) => 1,
        }
    }

    pub(crate) fn len_utf8(&self) -> usize {
        match self {
            TerminalValue::String(s) => s.as_bytes().len(),
            TerminalValue::Char(c) => c.len_utf8(),
        }
    }

    pub(crate) fn is_char(&self) -> bool {
        match self {
            TerminalValue::String(_) => false,
            TerminalValue::Char(_) => true,
        }
    }

    pub(crate) fn as_char(&self) -> Option<char> {
        match self {
            TerminalValue::String(s) => s.chars().next(),
            TerminalValue::Char(c) => Some(*c),
        }
    }
}

impl Display for TerminalValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TerminalValue::String(s) => write!(f, "{}", s),
            TerminalValue::Char(c) => write!(f, "{}", c),
        }
    }
}

impl PartialEq<char> for TerminalValue {
    fn eq(&self, other: &char) -> bool {
        match self {
            TerminalValue::String(s) => s.len() == 1 && s.chars().next().unwrap() == *other,
            TerminalValue::Char(c) => c == other,
        }
    }
}

impl PartialEq<[u8]> for TerminalValue {
    fn eq(&self, other: &[u8]) -> bool {
        match self {
            TerminalValue::String(s) => s.as_bytes() == other,
            TerminalValue::Char(c) => ByteToCharIter::new(other).next() == Some(*c),
        }
    }
}

type EbnfExprMaxByteLen = u8;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum EbnfExpr {
    Identifier(String, Option<(SourceManagerDiagnosticInfo, Span)>),
    Alternative(Vec<EbnfExpr>, HashSet<TerminalValue>, EbnfExprMaxByteLen),
    Concat(Vec<EbnfExpr>, EbnfExprMaxByteLen),
    Exception(Vec<EbnfExpr>, EbnfExprMaxByteLen),
    Extend(Vec<EbnfExpr>, EbnfExprMaxByteLen),
    Optional(Box<EbnfExpr>, EbnfExprMaxByteLen),
    Repetition(Box<EbnfExpr>, EbnfExprMaxByteLen),
    Terminal(TerminalValue),
    Statements(Vec<EbnfExpr>, EbnfExprMaxByteLen),
    Variable {
        name: String,
        expr: Box<EbnfExpr>,
        is_def: bool,
    },
    Range {
        lhs: char,
        rhs: char,
        inclusive: bool,
    },
    AnyChar,
}

impl EbnfExpr {
    fn substitute_extend(&mut self, old_name: &str, new_name: &str) -> bool {
        match self {
            Self::Variable { expr, .. } => expr.substitute_extend(old_name, new_name),
            Self::Alternative(exprs, _, ..)
            | Self::Concat(exprs, ..)
            | Self::Exception(exprs, ..)
            | Self::Extend(exprs, ..) => {
                let mut has_substituted = false;
                for expr in exprs.iter_mut() {
                    has_substituted = expr.substitute_extend(old_name, new_name) || has_substituted;
                }
                has_substituted
            }
            Self::Optional(expr, ..) | Self::Repetition(expr, ..) => {
                expr.substitute_extend(old_name, new_name)
            }
            Self::Identifier(name, ..) => {
                if name == old_name {
                    *name = new_name.to_string();
                    true
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    pub(super) fn is_char(&self) -> bool {
        match self {
            EbnfExpr::Terminal(t) => t.is_char(),
            _ => false,
        }
    }

    fn try_move_terminals_to_hash_set(items: &mut Vec<EbnfExpr>, set: &mut HashSet<TerminalValue>) {
        let mut i = 0;
        while i < items.len() {
            if let Self::Terminal(_) = &items[i] {
                if let Self::Terminal(value) = items.remove(i) {
                    set.insert(value);
                }
            } else {
                i += 1;
            }
        }
    }

    pub(super) fn try_merge_binary(op: BinaryOperator, lhs: EbnfExpr, rhs: EbnfExpr) -> EbnfExpr {
        match (op, lhs, rhs) {
            (
                BinaryOperator::Alternative,
                Self::Alternative(mut lhs, mut l_set, l_max),
                Self::Alternative(rhs, r_set, r_max),
            ) => {
                lhs.extend(rhs);
                l_set.extend(r_set);
                Self::try_move_terminals_to_hash_set(&mut lhs, &mut l_set);
                EbnfExpr::Alternative(lhs, l_set, l_max.max(r_max))
            }
            (BinaryOperator::Concat, Self::Concat(mut lhs, l_max), Self::Concat(rhs, r_max)) => {
                lhs.extend(rhs);
                EbnfExpr::Concat(lhs, l_max.max(r_max))
            }
            (
                BinaryOperator::Exception,
                Self::Exception(mut lhs, l_max),
                Self::Exception(rhs, r_max),
            ) => {
                lhs.extend(rhs);
                EbnfExpr::Exception(lhs, l_max.max(r_max))
            }
            (BinaryOperator::Extend, Self::Extend(mut lhs, l_max), Self::Extend(rhs, r_max)) => {
                lhs.extend(rhs);
                EbnfExpr::Extend(lhs, l_max.max(r_max))
            }
            (BinaryOperator::Alternative, Self::Alternative(mut lhs, mut l_set, l_max), rhs) => {
                let r_max = 1;
                lhs.push(rhs);
                Self::try_move_terminals_to_hash_set(&mut lhs, &mut l_set);
                EbnfExpr::Alternative(lhs, l_set, l_max.max(r_max))
            }
            (BinaryOperator::Concat, Self::Concat(mut lhs, l_max), rhs) => {
                let r_max = 1;
                lhs.push(rhs);
                EbnfExpr::Concat(lhs, l_max.max(r_max))
            }
            (BinaryOperator::Exception, Self::Exception(mut lhs, l_max), rhs) => {
                let r_max = 1;
                lhs.push(rhs);
                EbnfExpr::Exception(lhs, l_max.max(r_max))
            }
            (BinaryOperator::Extend, Self::Extend(mut lhs, l_max), rhs) => {
                let r_max = 1;
                lhs.push(rhs);
                EbnfExpr::Extend(lhs, l_max.max(r_max))
            }
            (_, lhs, rhs) => {
                let l_max = 1;
                let r_max = 1;
                let max_byte_len = l_max.max(r_max);
                match op {
                    BinaryOperator::Alternative => {
                        let mut set = HashSet::new();
                        let mut items = vec![lhs, rhs];
                        Self::try_move_terminals_to_hash_set(&mut items, &mut set);
                        Self::Alternative(items, set, max_byte_len)
                    }
                    BinaryOperator::Concat => Self::Concat(vec![lhs, rhs], max_byte_len),
                    BinaryOperator::Exception => Self::Exception(vec![lhs, rhs], max_byte_len),
                    BinaryOperator::Extend => Self::Extend(vec![lhs, rhs], max_byte_len),
                    BinaryOperator::Range | BinaryOperator::RangeEqual => {
                        if let (Self::Terminal(lhs), Self::Terminal(rhs)) = (lhs, rhs) {
                            EbnfExpr::Range {
                                lhs: lhs.as_char().unwrap(),
                                rhs: rhs.as_char().unwrap(),
                                inclusive: op == BinaryOperator::RangeEqual,
                            }
                        } else {
                            panic!("Expected terminal")
                        }
                    }
                }
            }
        }
    }

    pub(super) fn from_unary(op: UnaryOperator, expr: EbnfExpr) -> EbnfExpr {
        let max_byte_len = 1;
        match op {
            UnaryOperator::Optional => EbnfExpr::Optional(Box::new(expr), max_byte_len),
        }
    }

    pub(crate) fn is_empty(&self) -> bool {
        match self {
            EbnfExpr::Statements(exprs, ..) => {
                exprs.iter().fold(true, |acc, e| acc && !e.is_empty())
            }
            EbnfExpr::Alternative(exprs, set, ..) => {
                exprs.iter().fold(true, |acc, e| acc && !e.is_empty()) && set.is_empty()
            }
            EbnfExpr::Concat(exprs, ..) => exprs.iter().fold(true, |acc, e| acc && !e.is_empty()),
            EbnfExpr::Exception(exprs, ..) => {
                exprs.iter().fold(true, |acc, e| acc && !e.is_empty())
            }
            EbnfExpr::Extend(exprs, ..) => exprs.iter().fold(true, |acc, e| acc && !e.is_empty()),
            EbnfExpr::Variable { expr, .. } => expr.is_empty(),
            _ => false,
        }
    }
}

impl Display for EbnfExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EbnfExpr::Statements(exprs, ..) => {
                for expr in exprs {
                    writeln!(f, "{}", expr)?;
                }
                Ok(())
            }
            EbnfExpr::Identifier(name, ..) => write!(f, "{}", name),
            EbnfExpr::Variable { name, expr, is_def } => {
                if *is_def {
                    write!(f, "{} ::= {};", name, expr)
                } else {
                    write!(f, "{} = {};", name, expr)
                }
            }
            EbnfExpr::Terminal(name) => write!(f, "'{}'", name),
            EbnfExpr::Repetition(expr, ..) => {
                write!(f, "{{ ")?;
                write!(f, "{} ", *expr)?;
                write!(f, "}}")
            }
            EbnfExpr::Optional(expr, ..) => {
                write!(f, "[ ")?;
                write!(f, "{} ", *expr)?;
                write!(f, "]")
            }
            EbnfExpr::Alternative(exprs, set, ..) => {
                write!(f, "(")?;
                for t in set.iter() {
                    write!(f, "'{}' | ", t)?
                }
                for expr in exprs.iter() {
                    write!(f, "{} | ", expr)?
                }
                write!(f, ")")
            }
            EbnfExpr::Concat(exprs, ..) => {
                write!(f, "(")?;
                write!(
                    f,
                    "{}",
                    exprs
                        .iter()
                        .map(|e| e.to_string())
                        .collect::<Vec<_>>()
                        .join(" , ")
                )?;
                write!(f, ")")
            }
            EbnfExpr::Exception(exprs, ..) => {
                write!(f, "(")?;
                write!(
                    f,
                    "{}",
                    exprs
                        .iter()
                        .map(|e| e.to_string())
                        .collect::<Vec<_>>()
                        .join(" - ")
                )?;
                write!(f, ")")
            }
            EbnfExpr::Extend(exprs, ..) => {
                write!(f, "(")?;
                write!(
                    f,
                    "{}",
                    exprs
                        .iter()
                        .map(|e| e.to_string())
                        .collect::<Vec<_>>()
                        .join(" + ")
                )?;
                write!(f, ")")
            }
            EbnfExpr::Range {
                lhs,
                rhs,
                inclusive,
            } => {
                write!(f, "{}", lhs)?;
                if *inclusive {
                    write!(f, " ..= ")?;
                } else {
                    write!(f, " .. ")?;
                }
                write!(f, "{}", rhs)
            }
            EbnfExpr::AnyChar => write!(f, "."),
        }
    }
}
