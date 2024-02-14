use std::fmt::{Display, Formatter};
use std::path::{Path, PathBuf};

use super::source_manager::SourceManagerDiagnosticInfo;
use super::span::{self, Span};
use colored::Colorize;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DiagnosticLevel {
    Error,
    Warning,
    Info,
    Note,
}

impl Display for DiagnosticLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DiagnosticLevel::Error => write!(f, "{}", "error".red()),
            DiagnosticLevel::Warning => write!(f, "{}", "warning".yellow()),
            DiagnosticLevel::Info => write!(f, "{}", "info".blue()),
            DiagnosticLevel::Note => write!(f, "{}", "note".cyan()),
        }
    }
}

trait DiagnosticLevelExt {
    fn colorize(&self, level: DiagnosticLevel) -> String;
}

impl DiagnosticLevelExt for String {
    fn colorize(&self, level: DiagnosticLevel) -> String {
        match level {
            DiagnosticLevel::Error => self.red().to_string(),
            DiagnosticLevel::Warning => self.yellow().to_string(),
            DiagnosticLevel::Info => self.blue().to_string(),
            DiagnosticLevel::Note => self.cyan().to_string(),
        }
    }
}

impl DiagnosticLevelExt for &str {
    fn colorize(&self, level: DiagnosticLevel) -> String {
        match level {
            DiagnosticLevel::Error => self.red().to_string(),
            DiagnosticLevel::Warning => self.yellow().to_string(),
            DiagnosticLevel::Info => self.blue().to_string(),
            DiagnosticLevel::Note => self.cyan().to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct DiagnosticMessage {
    level: DiagnosticLevel,
    message: String,
    span: Option<Span>,
}

impl DiagnosticMessage {
    pub fn new(level: DiagnosticLevel, message: String, span: Option<Span>) -> Self {
        Self { level, message, span }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct DiagnosticBase {
    messages: Vec<DiagnosticMessage>,
    notes: Vec<DiagnosticMessage>,
    source_info: SourceManagerDiagnosticInfo,
    has_error: bool,
}

macro_rules! print_message_fn {
    ($fn_name:ident, $W:ty, $R:ty) => {
        fn $fn_name(base: &DiagnosticBase, filepath: &Path, f: &mut $W) -> $R {
            for message in base.messages.iter().filter(|m| m.span.is_none()) {
                writeln!(f, "{}: {}", message.level.to_string().bold(), message.message.bold())?;
            }

            let line_number = format!("{}", base.source_info.line);
            let line_number_width = line_number.len() + 2;
            let line_number = line_number.bold().magenta();
    
            writeln!(f, "{}{}:{}:{}", "  --> ".magenta(), filepath.display(), base.source_info.line, base.source_info.column + 1)?;
            writeln!(f, "{:width$}{}", ' ', "|".magenta(), width=line_number_width)?;
            
            writeln!(f, " {} {} {}", line_number, "|".magenta(), base.source_info.source.trim_end())?;
    
            let suggestions = DiagnosticBase::resolve_underline(base.messages.iter().filter(|m| m.span.is_some()).collect::<Vec<_>>());
    
            if !suggestions.is_empty() {
                write!(f, "{:width$}{}", ' ', "|".magenta(), width=line_number_width)?;
            }
    
            {
                let mut last_span = 0;
                for (message, span) in suggestions.iter() {
                    let diff = ((span.start - last_span) as isize - 1).max(0) as usize; 
                    // println!("diff: {} | {:#?}", diff, span);
                    write!(f, " {}", " ".repeat(diff))?;
                    write!(f, "{}", "^".repeat(span.len()).colorize(message.level))?;
                    last_span = span.end;
                }
            }
    
            for (depth, (message, span)) in suggestions.iter().rev().enumerate() {
                let len = span.len();
                for _ in 0..depth {
                    write!(f, "{:width$}{}{}{}", ' ', "|".magenta(), " ".repeat(len), "|".colorize(message.level), width=line_number_width)?;
                }
                if depth == 0 {
                    writeln!(f, " {}", message.message.colorize(message.level))?;
                } else {
                    writeln!(f, "\n{:width$}{}{}{}", ' ', "|".magenta(), " ".repeat(len), message.message.colorize(message.level), width=line_number_width)?;
                }
            }
    
            if base.notes.is_empty() {
                return Ok(());
            }
            
            writeln!(f, "{:width$}{} {}", ' ', "= note:".magenta(), base.notes[0].message, width=line_number_width)?;
    
            for message in base.notes.iter().skip(1) {
                writeln!(f,  "          {}", message.message)?;
            }
            
            Ok(())
        }
    };
}

print_message_fn!(print_message_io, Box<dyn std::io::Write>, std::io::Result<()>);
print_message_fn!(print_message_fmt, Formatter<'_>, std::fmt::Result);

impl DiagnosticBase {
    pub fn new(message: DiagnosticMessage, source_info: SourceManagerDiagnosticInfo) -> Self {
        let has_error = message.level == DiagnosticLevel::Error;
        Self {
            messages: vec![message],
            notes: Vec::new(),
            source_info: source_info,
            has_error
        }
    }

    pub fn add_error(&mut self, message: String, span: Option<Span>) {
        self.messages.push(DiagnosticMessage::new(DiagnosticLevel::Error, message, span));
        self.has_error = true;
    }

    pub fn add_warning(&mut self, message: String, span: Option<Span>) {
        self.messages.push(DiagnosticMessage::new(DiagnosticLevel::Warning, message, span));
    }

    pub fn add_info(&mut self, message: String, span: Option<Span>) {
        self.messages.push(DiagnosticMessage::new(DiagnosticLevel::Info, message, span));
    }

    pub fn add_note(&mut self, note: DiagnosticMessage) {
        self.notes.push(note);
    }

    fn resolve_underline(mut messages: Vec<&DiagnosticMessage>) -> Vec<(&DiagnosticMessage, Span)> {
        if messages.is_empty() {
            return Vec::new();
        }

        let mut suggestions = Vec::new();
        messages.sort_by_key(|m| m.span.unwrap().start);
        let mut last_span = messages[0].span.unwrap();
        let mut last_message = messages[0];

        for message in messages.iter().skip(1) {
            let (span, other_span) = last_span.split_if_intersect(&message.span.unwrap());
            suggestions.push((last_message, span));
            last_span = other_span;
            last_message = message;
        }
        suggestions.push((last_message, last_span));
        suggestions
    }
}

enum DiagnosticBuilderBase<'a> {
    Bag(&'a mut DiagnosticBag),
    Stream(&'a mut StreamingDiagnosticBag),
}

pub(crate) struct DiagnosticBuilder<'a> {
    inner: Option<DiagnosticBase>,
    base: DiagnosticBuilderBase<'a>
}

impl<'a> DiagnosticBuilder<'a> {
    pub(crate) fn report<S: AsRef<str>>(&mut self, level: DiagnosticLevel, message: S, source_info: SourceManagerDiagnosticInfo, span: Option<Span>) -> &mut Self {
        self.inner = Some(DiagnosticBase::new(DiagnosticMessage::new(level, message.as_ref().to_owned(), span), source_info));
        return self;
    }

    pub(crate) fn add_note<S: AsRef<str>>(&mut self, message: S, span: Option<Span>) -> &mut Self {
        if let Some(ref mut inner) = self.inner {
            inner.add_note(DiagnosticMessage::new(DiagnosticLevel::Note, message.as_ref().to_owned(), span));
        }
        return self;
    }

    pub(crate) fn add_error<S: AsRef<str>>(&mut self, message: S, span: Option<Span>) -> &mut Self {
        if let Some(ref mut inner) = self.inner {
            inner.add_error(message.as_ref().to_owned(), span);
        } else {
            self.inner = Some(DiagnosticBase::new(DiagnosticMessage::new(DiagnosticLevel::Error, message.as_ref().to_owned(), span), SourceManagerDiagnosticInfo::default()));
        }
        return self;
    }

    pub(crate) fn add_warning<S: AsRef<str>>(&mut self, message: S, span: Option<Span>) -> &mut Self {
        if let Some(ref mut inner) = self.inner {
            inner.add_warning(message.as_ref().to_owned(), span);
        } else {
            self.inner = Some(DiagnosticBase::new(DiagnosticMessage::new(DiagnosticLevel::Warning, message.as_ref().to_owned(), span), SourceManagerDiagnosticInfo::default()));
        }
        return self;
    }

    pub(crate) fn add_info<S: AsRef<str>>(&mut self, message: S, span: Option<Span>) -> &mut Self {
        if let Some(ref mut inner) = self.inner {
            inner.add_info(message.as_ref().to_owned(), span);
        } else {
            self.inner = Some(DiagnosticBase::new(DiagnosticMessage::new(DiagnosticLevel::Info, message.as_ref().to_owned(), span), SourceManagerDiagnosticInfo::default()));
        }
        return self;
    }

    pub(crate) fn commit(&mut self) {
        let inner = self.inner.take().expect("DiagnosticBuilder::finish called without a message!");
        match &mut self.base {
            DiagnosticBuilderBase::Bag(b) => b.add_message(inner),
            DiagnosticBuilderBase::Stream(b) => b.add_message(inner),
        }
    }
}

pub trait DiagnosticReporter {
    fn add_message(&mut self, message: DiagnosticBase);
    fn get_filepath(&self) -> &Path;
    fn has_error(&self) -> bool;
    fn builder(&mut self) -> DiagnosticBuilder;
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct DiagnosticBag {
    messages: Vec<DiagnosticBase>,
    filepath: PathBuf,
}

impl DiagnosticBag {
    pub fn new<P: AsRef<Path>>(filepath: P) -> Self {
        Self {
            messages: Vec::new(),
            filepath: filepath.as_ref().to_path_buf(),
        }
    }

    fn iter(&self) -> impl Iterator<Item = &DiagnosticBase> {
        self.messages.iter()
    }
}

impl DiagnosticReporter for DiagnosticBag {
    fn add_message(&mut self, message: DiagnosticBase) {
        self.messages.push(message);
    }

    fn get_filepath(&self) -> &Path {
        &self.filepath
    }

    fn builder(&mut self) -> DiagnosticBuilder {
        DiagnosticBuilder {
            inner: None,
            base: DiagnosticBuilderBase::Bag(self),
        }
    }

    fn has_error(&self) -> bool {
        self.messages.iter().any(|m| m.has_error)
    }
}

impl Display for DiagnosticBag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for message in self.iter() {
            print_message_fmt(message, self.get_filepath(), f)?;
        }
        Ok(())
    }
}

pub struct StreamingDiagnosticBag {
    writer: Box<dyn std::io::Write>,
    filepath: PathBuf,
    has_error: bool,
}

impl StreamingDiagnosticBag {
    pub fn new<P: AsRef<Path>>(writer: Box<dyn std::io::Write>, filepath: P) -> Self {
        Self {
            writer,
            filepath: filepath.as_ref().to_path_buf(),
            has_error: false,
        }
    }

    fn iter(&self) -> impl Iterator<Item = &DiagnosticBase> {
        std::iter::empty()
    }
}

impl DiagnosticReporter for StreamingDiagnosticBag {
    fn add_message(&mut self, message: DiagnosticBase) {
        self.has_error = message.has_error;
        let path = self.get_filepath().to_path_buf();
        let mut writer = &mut self.writer;
        print_message_io(&message, &path, &mut writer).unwrap();
    }

    fn get_filepath(&self) -> &Path {
        &self.filepath
    }

    fn builder(&mut self) -> DiagnosticBuilder {
        DiagnosticBuilder {
            inner: None,
            base: DiagnosticBuilderBase::Stream(self),
        }
    }

    fn has_error(&self) -> bool {
        self.has_error
    }
}

pub enum Diagnostic {
    Stream(StreamingDiagnosticBag),
    Bag(DiagnosticBag),
}

impl Display for StreamingDiagnosticBag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for message in self.iter() {
            print_message_fmt(message, self.get_filepath(), f)?;
        }
        Ok(())
    }
}

impl Display for Diagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Diagnostic::Stream(s) => s.fmt(f),
            Diagnostic::Bag(b) => b.fmt(f),
        }
    }
}

impl DiagnosticReporter for Diagnostic {
    fn add_message(&mut self, message: DiagnosticBase) {
        match self {
            Diagnostic::Stream(s) => s.add_message(message),
            Diagnostic::Bag(b) => b.add_message(message),
        }
    }

    fn get_filepath(&self) -> &Path {
        match self {
            Diagnostic::Stream(s) => s.get_filepath(),
            Diagnostic::Bag(b) => b.get_filepath(),
        }
    }

    fn builder(&mut self) -> DiagnosticBuilder {
        match self {
            Diagnostic::Stream(s) => s.builder(),
            Diagnostic::Bag(b) => b.builder(),
        }
    }
    fn has_error(&self) -> bool {
        match self {
            Diagnostic::Stream(s) => s.has_error(),
            Diagnostic::Bag(b) => b.has_error(),
        }
    }
}

impl From<StreamingDiagnosticBag> for Diagnostic {
    fn from(stream: StreamingDiagnosticBag) -> Self {
        Diagnostic::Stream(stream)
    }
}

impl From<DiagnosticBag> for Diagnostic {
    fn from(bag: DiagnosticBag) -> Self {
        Diagnostic::Bag(bag)
    }
}
