use crate::eoc::utils::diagnostic::Diagnostic;

use super::ast::RelativeSourceManager;

pub(crate) struct Vm;

impl Vm {
    pub(super) fn is_digit<'b>(&self, s: &'b [u8], source_manager: RelativeSourceManager<'b>, diagnostics: &mut Diagnostic) -> Option<char> {
        None
    }
    
    pub(super) fn is_hex_digit<'b>(&self, s: &'b [u8], source_manager: RelativeSourceManager<'b>, diagnostics: &mut Diagnostic) -> Option<char> {
        None
    }
    
    pub(super) fn is_oct_digit<'b>(&self, s: &'b [u8], source_manager: RelativeSourceManager<'b>, diagnostics: &mut Diagnostic) -> Option<char> {
        None
    }
    
    pub(super) fn is_binary_digit<'b>(&self, s: &'b [u8], source_manager: RelativeSourceManager<'b>, diagnostics: &mut Diagnostic) -> Option<char> {
        None
    }
}