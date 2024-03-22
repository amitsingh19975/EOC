use crate::eoc::{
    lexer::token::TokenKind,
    utils::{diagnostic::Diagnostic, imm_ref::Ref},
};

use super::{
    ast::RelativeSourceManager,
    basic::{
        EbnfParserMatcher, EbnfParserMatcherInner, LexerEbnfMatcher, LexerEbnfParserMatcherInner,
        LexerMatchResult,
    },
    default_matcher::DefaultLexerEbnfParserMatcher,
    expr::EbnfExpr,
    ir_matcher::IRLexerEbnfParserMatcher,
    native_call::LexerNativeCallKind,
    vm::{EbnfVm, LexerEbnfParserMatcher, LexerVm},
    vm_state::LexerVmState,
};

impl LexerEbnfMatcher for LexerEbnfParserMatcherInner {
    fn match_native<'b>(
        &self,
        kind: LexerNativeCallKind,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
        state: Option<&LexerVmState>,
    ) -> Option<(&'b [u8], TokenKind)> {
        match self {
            EbnfParserMatcherInner::Default(d) => {
                d.match_native(kind, s, source_manager, diagnostic, state)
            }
            EbnfParserMatcherInner::Vm(v) => {
                v.match_native(kind, s, source_manager, diagnostic, state)
            }
            EbnfParserMatcherInner::IR(r) => {
                r.match_native(kind, s, source_manager, diagnostic, state)
            }
        }
    }

    fn try_match_expr<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
        state: Option<&LexerVmState>,
    ) -> LexerMatchResult {
        match self {
            EbnfParserMatcherInner::Default(d) => {
                d.try_match_expr(s, source_manager, diagnostic, state)
            }
            EbnfParserMatcherInner::Vm(v) => v.try_match_expr(s, source_manager, diagnostic, state),
            EbnfParserMatcherInner::IR(r) => r.try_match_expr(s, source_manager, diagnostic, state),
        }
    }

    fn is_digit<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
        state: Option<&LexerVmState>,
    ) -> Option<char> {
        match self {
            EbnfParserMatcherInner::Default(d) => d.is_digit(s, source_manager, diagnostic, state),
            EbnfParserMatcherInner::Vm(v) => v.is_digit(s, source_manager, diagnostic, state),
            EbnfParserMatcherInner::IR(r) => r.is_digit(s, source_manager, diagnostic, state),
        }
    }

    fn is_hex_digit<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
        state: Option<&LexerVmState>,
    ) -> Option<char> {
        match self {
            EbnfParserMatcherInner::Default(d) => {
                d.is_hex_digit(s, source_manager, diagnostic, state)
            }
            EbnfParserMatcherInner::Vm(v) => v.is_hex_digit(s, source_manager, diagnostic, state),
            EbnfParserMatcherInner::IR(r) => r.is_hex_digit(s, source_manager, diagnostic, state),
        }
    }

    fn is_oct_digit<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
        state: Option<&LexerVmState>,
    ) -> Option<char> {
        match self {
            EbnfParserMatcherInner::Default(d) => {
                d.is_oct_digit(s, source_manager, diagnostic, state)
            }
            EbnfParserMatcherInner::Vm(v) => v.is_oct_digit(s, source_manager, diagnostic, state),
            EbnfParserMatcherInner::IR(r) => r.is_oct_digit(s, source_manager, diagnostic, state),
        }
    }

    fn is_binary_digit<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
        state: Option<&LexerVmState>,
    ) -> Option<char> {
        match self {
            EbnfParserMatcherInner::Default(d) => {
                d.is_binary_digit(s, source_manager, diagnostic, state)
            }
            EbnfParserMatcherInner::Vm(v) => {
                v.is_binary_digit(s, source_manager, diagnostic, state)
            }
            EbnfParserMatcherInner::IR(r) => {
                r.is_binary_digit(s, source_manager, diagnostic, state)
            }
        }
    }

    // fn contains_def(&self, name: &str) -> bool {
    //     match self {
    //         EbnfParserMatcherInner::Default(d) => d.contains_def(name),
    //         EbnfParserMatcherInner::Vm(v) => v.contains_def(name),
    //         EbnfParserMatcherInner::IR(r) => r.contains_def(name),
    //     }
    // }

    fn match_native_integer<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
        state: Option<&LexerVmState>,
    ) -> Option<&'b [u8]> {
        match self {
            EbnfParserMatcherInner::Default(d) => {
                d.match_native_integer(s, source_manager, diagnostic, state)
            }
            EbnfParserMatcherInner::Vm(v) => {
                v.match_native_integer(s, source_manager, diagnostic, state)
            }
            EbnfParserMatcherInner::IR(r) => {
                r.match_native_integer(s, source_manager, diagnostic, state)
            }
        }
    }

    fn match_native_floating_point<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
        state: Option<&LexerVmState>,
    ) -> Option<&'b [u8]> {
        match self {
            EbnfParserMatcherInner::Default(d) => {
                d.match_native_floating_point(s, source_manager, diagnostic, state)
            }
            EbnfParserMatcherInner::Vm(v) => {
                v.match_native_floating_point(s, source_manager, diagnostic, state)
            }
            EbnfParserMatcherInner::IR(r) => {
                r.match_native_floating_point(s, source_manager, diagnostic, state)
            }
        }
    }

    fn match_for<'b>(
        &self,
        addr: usize,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
    ) -> LexerMatchResult {
        match self {
            EbnfParserMatcherInner::Default(d) => d.match_for(addr, s, source_manager, diagnostic),
            EbnfParserMatcherInner::Vm(v) => v.match_for(addr, s, source_manager, diagnostic),
            EbnfParserMatcherInner::IR(r) => r.match_for(addr, s, source_manager, diagnostic),
        }
    }
    
    fn is_default(&self) -> bool {
        matches!(self, EbnfParserMatcherInner::Default(_))
    }
    
    fn is_ir(&self) -> bool {
        matches!(self, EbnfParserMatcherInner::IR(_))
    }
}

impl LexerEbnfParserMatcherInner {
    pub(crate) fn from_expr<'a>(
        expr: EbnfExpr,
        scopes: &'a LexerEbnfParserMatcher,
        diagnostic: &Diagnostic,
    ) -> Self {
        if expr.is_empty() {
            return Self::new();
        }

        let mut vm = LexerVm::new();
        vm.from_expr(expr, scopes, diagnostic, &mut Default::default());
        EbnfParserMatcherInner::Vm(vm)
    }
}

impl From<DefaultLexerEbnfParserMatcher> for LexerEbnfParserMatcherInner {
    fn from(d: DefaultLexerEbnfParserMatcher) -> Self {
        EbnfParserMatcherInner::Default(d)
    }
}

impl From<LexerVm> for LexerEbnfParserMatcherInner {
    fn from(v: LexerVm) -> Self {
        EbnfParserMatcherInner::Vm(v)
    }
}

impl EbnfParserMatcher<DefaultLexerEbnfParserMatcher, LexerVm, IRLexerEbnfParserMatcher> {
    pub(crate) fn from_expr<'a>(&mut self, expr: EbnfExpr, diagnostic: &Diagnostic) {
        let temp = LexerEbnfParserMatcherInner::from_expr(expr, &self, diagnostic);

        if self.current_scope.is_default() {
            self.current_scope = Ref::new(temp);
            return;
        }

        let current_scope = self.current_scope.take();
        let parent_scope = self.parent_scope.take();
        let old_self = Self {
            parent_scope,
            current_scope,
        };

        *self = Self {
            parent_scope: Some(Ref::new(old_self).take_as_imm_ref()),
            current_scope: Ref::new(temp),
        }
    }
}

impl Default
    for EbnfParserMatcher<DefaultLexerEbnfParserMatcher, LexerVm, IRLexerEbnfParserMatcher>
{
    fn default() -> Self {
        Self {
            parent_scope: None,
            current_scope: EbnfParserMatcherInner::new().into(),
        }
    }
}

impl LexerEbnfMatcher for LexerEbnfParserMatcher {
    fn match_native<'b>(
        &self,
        kind: LexerNativeCallKind,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
        state: Option<&LexerVmState>,
    ) -> Option<(&'b [u8], TokenKind)> {
        self.current_scope
            .as_ref()
            .match_native(kind, s, source_manager, diagnostic, state)
    }

    fn try_match_expr<'b>(
        &self,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
        state: Option<&LexerVmState>,
    ) -> LexerMatchResult {
        self.current_scope
            .as_ref()
            .try_match_expr(s, source_manager, diagnostic, state)
    }

    fn match_for<'b>(
        &self,
        addr: usize,
        s: &'b [u8],
        source_manager: RelativeSourceManager<'b>,
        diagnostic: &Diagnostic,
    ) -> LexerMatchResult {
        self.current_scope
            .as_ref()
            .match_for(addr, s, source_manager, diagnostic)
    }
    
    fn is_default(&self) -> bool {
        self.current_scope.as_ref().is_default()
    }

    fn is_ir(&self) -> bool {
        self.current_scope.as_ref().is_ir()
    }
}

impl From<DefaultLexerEbnfParserMatcher> for LexerEbnfParserMatcher {
    fn from(d: DefaultLexerEbnfParserMatcher) -> Self {
        EbnfParserMatcher {
            parent_scope: None,
            current_scope: Ref::new(d.into()),
        }
    }
}

impl From<LexerVm> for LexerEbnfParserMatcher {
    fn from(v: LexerVm) -> Self {
        EbnfParserMatcher {
            parent_scope: None,
            current_scope: Ref::new(v.into()),
        }
    }
}

impl From<IRLexerEbnfParserMatcher> for LexerEbnfParserMatcher {
    fn from(r: IRLexerEbnfParserMatcher) -> Self {
        EbnfParserMatcher {
            parent_scope: None,
            current_scope: Ref::new(LexerEbnfParserMatcherInner::IR(r)),
        }
    }
}

impl LexerEbnfParserMatcher {
    pub(crate) fn new_ir() -> Self {
        EbnfParserMatcher {
            parent_scope: None,
            current_scope: Ref::new(LexerEbnfParserMatcherInner::IR(
                IRLexerEbnfParserMatcher::new(),
            )),
        }
    }

    pub(crate) fn is_ir(&self) -> bool {
        self.current_scope.as_ref().is_ir()
    }

    pub(crate) fn as_ir(&self) -> &IRLexerEbnfParserMatcher {
        match self.current_scope.as_ref() {
            EbnfParserMatcherInner::IR(r) => r,
            _ => panic!("Expected IR, but got {:?}", self.current_scope.as_ref()),
        }
    }
}
