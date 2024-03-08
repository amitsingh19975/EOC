use std::{collections::HashSet, fmt::Display};

use crate::eoc::{
    lexer::{str_utils::ByteToCharIter, token::TokenKind},
    utils::{
        diagnostic::{Diagnostic, DiagnosticLevel, DiagnosticReporter},
        source_manager::SourceManagerDiagnosticInfo,
        span::Span,
    },
};

use super::{
    ast::{EbnfParserMatcherDef, EbnfParserMatcherEnv, RelativeSourceManager},
    matcher::CustomEbnfParserMatcher,
    native_call::NativeCallKind,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum EbnfParserEnvVariable {
    Expr(EbnfExpr),
    NativeCall(NativeCallKind),
}

impl Display for EbnfParserEnvVariable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Expr(e) => write!(f, "{}", e),
            Self::NativeCall(name) => write!(f, "<Native Call='{name}'>"),
        }
    }
}

impl EbnfParserEnvVariable {
    fn substitute_extend(&mut self, old_name: &str, new_name: &str) -> bool {
        match self {
            Self::Expr(expr) => expr.substitute_extend(old_name, new_name),
            _ => true,
        }
    }

    fn is_native_call(&self) -> bool {
        matches!(self, Self::NativeCall(_))
    }

    pub(super) fn match_expr<'a>(
        &self,
        matcher: &CustomEbnfParserMatcher,
        s: &'a [u8],
        env: &EbnfParserMatcherEnv,
        source_manager: RelativeSourceManager<'a>,
        diagnostic: &mut Diagnostic,
    ) -> Option<&'a [u8]> {
        match self {
            Self::Expr(expr) => expr.match_expr(matcher, s, env, source_manager, diagnostic),
            Self::NativeCall(func_name) => func_name.call(matcher, s, source_manager, diagnostic),
        }
    }

    fn get_max_byte_len(&self, env: Option<&EbnfParserMatcherEnv>) -> u8 {
        match self {
            Self::Expr(expr) => expr.get_max_byte_len(env),
            Self::NativeCall(_) => 1,
        }
    }

    pub(super) fn recalculate_max_byte_len(self, env: &mut EbnfParserMatcherEnv) -> Self {
        match self {
            Self::Expr(expr) => Self::Expr(expr.recalculate_max_byte_len(env)),
            _ => self,
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
}

impl EbnfExpr {
    pub(super) fn init_env<'a>(
        self,
        env: &mut EbnfParserMatcherEnv,
        def: &mut EbnfParserMatcherDef,
        diagnostic: &mut Diagnostic,
    ) {
        match self {
            EbnfExpr::Statements(exprs, ..) => {
                for expr in exprs {
                    expr.init_env(env, def, diagnostic)
                }
            }
            EbnfExpr::Variable { name, expr, is_def } => {
                let mut new_expr = EbnfParserEnvVariable::Expr(*expr);
                if is_def {
                    if !def.contains(&name) {
                        def.insert(name.clone());
                    }
                }
                if let Some(old_expr) = env.remove(&name) {
                    let new_name = Self::get_unique_name(&name, env);
                    if new_expr.substitute_extend(&name, &new_name) {
                        env.insert(new_name, old_expr);
                    }
                }
                env.insert(name, new_expr);
            }
            _ => {}
        }
    }

    fn get_unique_name(name: &str, env: &EbnfParserMatcherEnv) -> String {
        let mut i = 0;
        let mut new_name = name.to_owned();
        while env.contains(&new_name) || (new_name == name) {
            i += 1;
            new_name = format!("{}_{}", name, i);
        }
        new_name
    }

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

    fn get_max_byte_len(&self, env: Option<&EbnfParserMatcherEnv>) -> u8 {
        match self {
            EbnfExpr::Identifier(id, ..) => {
                if let Some(env) = env {
                    if let Some(el) = env.get(id) {
                        return el.get_max_byte_len(Some(env));
                    }
                }

                1
            }
            EbnfExpr::Alternative(_, _, m)
            | EbnfExpr::Concat(_, m)
            | EbnfExpr::Extend(_, m)
            | EbnfExpr::Optional(_, m)
            | EbnfExpr::Repetition(_, m)
            | EbnfExpr::Statements(_, m)
            | EbnfExpr::Exception(_, m) => *m,
            EbnfExpr::Terminal(t) => t.len() as u8,
            EbnfExpr::Variable { expr, .. } => expr.get_max_byte_len(None),
            EbnfExpr::Range { .. } => 1,
        }
    }

    fn recalculate_max_byte_len(self, env: &mut EbnfParserMatcherEnv) -> Self {
        match self {
            EbnfExpr::Identifier(id, span) => {
                if let Some(el) = env.remove(&id) {
                    let temp = el.recalculate_max_byte_len(env);
                    env.insert(id.clone(), temp);
                }
                Self::Identifier(id, span)
            }
            EbnfExpr::Variable { expr, .. } => expr.recalculate_max_byte_len(env),
            EbnfExpr::Alternative(v, h, ..) => {
                let mut max_byte_len = 1u8;
                for expr in v.iter() {
                    max_byte_len = max_byte_len.max(expr.get_max_byte_len(Some(env)));
                }
                for t in h.iter() {
                    max_byte_len = max_byte_len.max(t.len() as u8);
                }
                Self::Alternative(v, h, max_byte_len)
            }
            EbnfExpr::Concat(v, ..) => {
                let mut max_byte_len = 1u8;
                for expr in v.iter() {
                    max_byte_len = max_byte_len.max(expr.get_max_byte_len(Some(env)));
                }
                Self::Concat(v, max_byte_len)
            }
            EbnfExpr::Exception(v, ..) => {
                let mut max_byte_len = 1u8;
                for expr in v.iter() {
                    max_byte_len = max_byte_len.max(expr.get_max_byte_len(Some(env)));
                }
                Self::Exception(v, max_byte_len)
            }
            EbnfExpr::Extend(v, ..) => {
                let mut max_byte_len = 1u8;
                for expr in v.iter() {
                    max_byte_len = max_byte_len.max(expr.get_max_byte_len(Some(env)));
                }
                Self::Extend(v, max_byte_len)
            }
            EbnfExpr::Optional(o, ..) => {
                let max_byte_len = o.get_max_byte_len(Some(env));
                Self::Optional(o, max_byte_len)
            }
            EbnfExpr::Repetition(o, ..) => Self::Repetition(o, u8::MAX),
            EbnfExpr::Statements(v, ..) => {
                let mut max_byte_len = 1u8;
                for expr in v.iter() {
                    max_byte_len = max_byte_len.max(expr.get_max_byte_len(Some(env)));
                }
                Self::Statements(v, max_byte_len)
            }
            _ => self,
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
                let r_max = rhs.get_max_byte_len(None);
                lhs.push(rhs);
                Self::try_move_terminals_to_hash_set(&mut lhs, &mut l_set);
                EbnfExpr::Alternative(lhs, l_set, l_max.max(r_max))
            }
            (BinaryOperator::Concat, Self::Concat(mut lhs, l_max), rhs) => {
                let r_max = rhs.get_max_byte_len(None);
                lhs.push(rhs);
                EbnfExpr::Concat(lhs, l_max.max(r_max))
            }
            (BinaryOperator::Exception, Self::Exception(mut lhs, l_max), rhs) => {
                let r_max = rhs.get_max_byte_len(None);
                lhs.push(rhs);
                EbnfExpr::Exception(lhs, l_max.max(r_max))
            }
            (BinaryOperator::Extend, Self::Extend(mut lhs, l_max), rhs) => {
                let r_max = rhs.get_max_byte_len(None);
                lhs.push(rhs);
                EbnfExpr::Extend(lhs, l_max.max(r_max))
            }
            (_, lhs, rhs) => {
                let l_max = lhs.get_max_byte_len(None);
                let r_max = rhs.get_max_byte_len(None);
                let max_byte_len = l_max.max(r_max);
                match op {
                    BinaryOperator::Alternative => {
                        Self::Alternative(vec![lhs, rhs], HashSet::new(), max_byte_len)
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
        let max_byte_len = expr.get_max_byte_len(None);
        match op {
            UnaryOperator::Optional => EbnfExpr::Optional(Box::new(expr), max_byte_len),
        }
    }

    pub(super) fn match_expr<'a>(
        &self,
        matcher: &CustomEbnfParserMatcher,
        s: &'a [u8],
        env: &EbnfParserMatcherEnv,
        source_manager: RelativeSourceManager<'a>,
        diagnostic: &mut Diagnostic,
    ) -> Option<&'a [u8]> {
        match self {
            EbnfExpr::Identifier(id, .., info) => {
                if NativeCallKind::is_valid_name(id) {
                    let kind = NativeCallKind::from(id.as_str());
                    return kind.call(matcher, s, source_manager, diagnostic);
                }
                if let Some(el) = env.get(id) {
                    el.match_expr(matcher, s, env, source_manager, diagnostic)
                } else {
                    if let Some((info, span)) = info {
                        diagnostic
                            .builder()
                            .report(
                                DiagnosticLevel::Error,
                                format!("Undefined variable '{}'", id),
                                info.clone(),
                                None,
                            )
                            .add_info("Try define variable", Some(*span))
                            .commit();
                    }
                    None
                }
            }
            EbnfExpr::Alternative(v, h, ..) => {
                let mut iter = ByteToCharIter::new(s);
                if let Some(c) = iter.next() {
                    if h.contains(&TerminalValue::Char(c)) {
                        let len = c.len_utf8();
                        return Some(&s[..len]);
                    }

                    for expr in v.iter() {
                        if let Some(s) =
                            expr.match_expr(matcher, s, env, source_manager, diagnostic)
                        {
                            return Some(s);
                        }
                    }
                }
                None
            }
            EbnfExpr::Concat(v, ..) => {
                let mut end = 0usize;

                for expr in v.iter() {
                    let temp_source = &s[end..];
                    if let Some(s_) =
                        expr.match_expr(matcher, temp_source, env, source_manager, diagnostic)
                    {
                        end += s_.len();
                    } else {
                        return None;
                    }
                }
                if end == 0 {
                    None
                } else {
                    Some(&s[..end])
                }
            }
            EbnfExpr::Exception(v, ..) => {
                if v.is_empty() {
                    return None;
                }
                let mut iter = v.iter();
                let first = iter.next().unwrap();

                let matched = first.match_expr(matcher, s, env, source_manager, diagnostic);
                if matched.is_none() {
                    return None;
                }

                let mut matched = matched.unwrap();

                for e in iter {
                    let mut i = 0;
                    while i < matched.len() {
                        let end = ByteToCharIter::new(&matched[i..])
                            .utf8_len_after_skip(e.get_max_byte_len(Some(env)) as usize);
                        let temp_source = &matched[i..(i + end).min(matched.len())];
                        if let Some(_) =
                            e.match_expr(matcher, temp_source, env, source_manager, diagnostic)
                        {
                            matched = &matched[0..i];
                            break;
                        }
                        i += end;
                    }
                }

                if matched.is_empty() {
                    return None;
                }

                Some(matched)
            }
            EbnfExpr::Extend(v, ..) => {
                if v.is_empty() {
                    return None;
                }

                let mut matched: &[u8] = &[];

                let mut end = matched.len();
                let mut start = 0usize;
                for expr in v.iter() {
                    matched = &s[start..];
                    if let Some(s_) =
                        expr.match_expr(matcher, matched, env, source_manager, diagnostic)
                    {
                        start += s_.len();
                        end += s_.len();
                    } else {
                        break;
                    }
                }

                if end == 0 {
                    None
                } else {
                    Some(&s[..end])
                }
            }
            EbnfExpr::Optional(o, ..) => {
                let mut end = 0usize;

                if let Some(s_) = o.match_expr(matcher, s, env, source_manager, diagnostic) {
                    end += s_.len();
                }

                Some(&s[..end])
            }
            EbnfExpr::Repetition(v, ..) => {
                let mut end = 0usize;
                while let Some(s_) =
                    v.match_expr(matcher, &s[end..], env, source_manager, diagnostic)
                {
                    end += s_.len();
                }

                if end == 0 {
                    None
                } else {
                    Some(&s[..end])
                }
            }
            EbnfExpr::Terminal(t) => {
                let end = t.len_utf8();
                if end > s.len() {
                    return None;
                }

                if t == &s[0..end] {
                    return Some(&s[..end]);
                }

                None
            }
            EbnfExpr::Statements(_, ..) => panic!("Expected expression, but got statements"),
            EbnfExpr::Variable { .. } => panic!("Expected expression, but got variable"),
            EbnfExpr::Range {
                lhs,
                rhs,
                inclusive,
            } => {
                if s.is_empty() {
                    return None;
                }
                let mut iter = ByteToCharIter::new(s);
                let c = iter.next();
                if c.is_none() {
                    return None;
                }

                let c = c.unwrap();
                let lhs = *lhs;
                let rhs = *rhs;

                let is_valid = if *inclusive {
                    (lhs..=rhs).contains(&c)
                } else {
                    (lhs..rhs).contains(&c)
                };

                if is_valid {
                    let len = c.len_utf8();
                    Some(&s[..len])
                } else {
                    None
                }
            }
        }
    }

    pub(crate) fn is_empty(&self) -> bool {
        match self {
            EbnfExpr::Statements(exprs, ..) => exprs.iter().fold(true, |acc, e| acc && !e.is_empty()),
            EbnfExpr::Alternative(exprs, set, ..) => exprs.iter().fold(true, |acc, e| acc && !e.is_empty()) && set.is_empty(),
            EbnfExpr::Concat(exprs, ..) => exprs.iter().fold(true, |acc, e| acc && !e.is_empty()),
            EbnfExpr::Exception(exprs, ..) => exprs.iter().fold(true, |acc, e| acc && !e.is_empty()),
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
        }
    }
}
