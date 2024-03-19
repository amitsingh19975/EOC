#![allow(dead_code)]

use std::fmt::{Display, Formatter};
use std::path::{Path, PathBuf};
use std::sync::{Arc, RwLock, RwLockWriteGuard};

use super::source_manager::SourceManagerDiagnosticInfo;
use super::span::Span;
use colored::Colorize;

fn is_redirecting() -> bool {
    !atty::is(atty::Stream::Stderr) || !atty::is(atty::Stream::Stdout)
}

trait ColorExt {
    fn bold_ext(&self, is_redirecting: bool) -> String;
    fn red_ext(&self, is_redirecting: bool) -> String;
    fn yellow_ext(&self, is_redirecting: bool) -> String;
    fn blue_ext(&self, is_redirecting: bool) -> String;
    fn cyan_ext(&self, is_redirecting: bool) -> String;
    fn magenta_ext(&self, is_redirecting: bool) -> String;
}

impl ColorExt for String {
    fn bold_ext(&self, is_redirecting: bool) -> String {
        if is_redirecting {
            self.to_string()
        } else {
            self.bold().to_string()
        }
    }

    fn red_ext(&self, is_redirecting: bool) -> String {
        if is_redirecting {
            self.to_string()
        } else {
            self.red().to_string()
        }
    }

    fn yellow_ext(&self, is_redirecting: bool) -> String {
        if is_redirecting {
            self.to_string()
        } else {
            self.yellow().to_string()
        }
    }

    fn blue_ext(&self, is_redirecting: bool) -> String {
        if is_redirecting {
            self.to_string()
        } else {
            self.blue().to_string()
        }
    }

    fn cyan_ext(&self, is_redirecting: bool) -> String {
        if is_redirecting {
            self.to_string()
        } else {
            self.cyan().to_string()
        }
    }

    fn magenta_ext(&self, is_redirecting: bool) -> String {
        if is_redirecting {
            self.to_string()
        } else {
            self.magenta().to_string()
        }
    }
}

impl ColorExt for &str {
    fn bold_ext(&self, is_redirecting: bool) -> String {
        if is_redirecting {
            self.to_string()
        } else {
            self.bold().to_string()
        }
    }

    fn red_ext(&self, is_redirecting: bool) -> String {
        if is_redirecting {
            self.to_string()
        } else {
            self.red().to_string()
        }
    }

    fn yellow_ext(&self, is_redirecting: bool) -> String {
        if is_redirecting {
            self.to_string()
        } else {
            self.yellow().to_string()
        }
    }

    fn blue_ext(&self, is_redirecting: bool) -> String {
        if is_redirecting {
            self.to_string()
        } else {
            self.blue().to_string()
        }
    }

    fn cyan_ext(&self, is_redirecting: bool) -> String {
        if is_redirecting {
            self.to_string()
        } else {
            self.cyan().to_string()
        }
    }

    fn magenta_ext(&self, is_redirecting: bool) -> String {
        if is_redirecting {
            self.to_string()
        } else {
            self.magenta().to_string()
        }
    }

}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DiagnosticLevel {
    Error,
    Warning,
    Info,
    Note,
}

impl DiagnosticLevel {
    pub fn to_string(&self, is_redirecting: bool) -> String {
        match self {
            DiagnosticLevel::Error => "error".red_ext(is_redirecting),
            DiagnosticLevel::Warning => "warning".yellow_ext(is_redirecting),
            DiagnosticLevel::Info => "info".blue_ext(is_redirecting),
            DiagnosticLevel::Note => "note".cyan_ext(is_redirecting),
        }
    }
}

trait DiagnosticLevelExt {
    fn colorize(&self, level: DiagnosticLevel, is_redirecting: bool) -> String;
}

impl DiagnosticLevelExt for String {
    fn colorize(&self, level: DiagnosticLevel, is_redirecting: bool) -> String {
        match level {
            DiagnosticLevel::Error => self.red_ext(is_redirecting),
            DiagnosticLevel::Warning => self.yellow_ext(is_redirecting),
            DiagnosticLevel::Info => self.blue_ext(is_redirecting),
            DiagnosticLevel::Note => self.cyan_ext(is_redirecting),
        }
    }
}

impl DiagnosticLevelExt for &str {
    fn colorize(&self, level: DiagnosticLevel, is_redirecting: bool) -> String {
        match level {
            DiagnosticLevel::Error => self.red_ext(is_redirecting),
            DiagnosticLevel::Warning => self.yellow_ext(is_redirecting),
            DiagnosticLevel::Info => self.blue_ext(is_redirecting),
            DiagnosticLevel::Note => self.cyan_ext(is_redirecting),
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
        fn $fn_name(base: &DiagnosticBase, filepath: &Path, f: &mut $W, is_redirecting: bool) -> $R {
            for message in base.messages.iter().filter(|m| m.span.is_none()) {
                writeln!(f, "{}: {}", message.level.to_string(is_redirecting).bold_ext(is_redirecting), message.message.bold_ext(is_redirecting))?;
            }

            let line_number = format!("{}", base.source_info.line);
            let line_number_width = line_number.len() + 2;
            let line_number = line_number.bold_ext(is_redirecting).magenta_ext(is_redirecting);

            if base.source_info.is_valid() {
                
                writeln!(f, "{}{}:{}:{}", "  --> ".magenta_ext(is_redirecting), filepath.display(), base.source_info.line, base.source_info.column + 1)?;
                writeln!(f, "{:width$}{}", ' ', "|".magenta_ext(is_redirecting), width=line_number_width)?;
                
                writeln!(f, " {} {} {}", line_number, "|".magenta_ext(is_redirecting), base.source_info.source.trim_end())?;
        
                let suggestions = DiagnosticBase::resolve_underline(base.messages.iter().filter(|m| m.span.is_some()).collect::<Vec<_>>());
        
                if !suggestions.is_empty() {
                    write!(f, "{:width$}{} ", ' ', "|".magenta_ext(is_redirecting), width=line_number_width)?;
                }
        
                {
                    let mut last_span = 0;
                    for (message, span) in suggestions.iter() {
                        let diff = ((span.start - last_span) as isize - 1).max(0) as usize;
                        write!(f, "{}", " ".repeat(diff))?;
                        write!(f, "{}", "^".repeat(span.len()).colorize(message.level, is_redirecting))?;
                        last_span = span.end;
                    }
                }
        
                for (depth, (message, span)) in suggestions.iter().rev().enumerate() {
                    let mut last_span = 0;
                    for _ in 0..depth {
                        let diff = ((span.start - last_span) as isize).max(0) as usize;
                        write!(f, "{:width$}{}{}{}", ' ', "|".magenta_ext(is_redirecting), " ".repeat(diff), "|".colorize(message.level, is_redirecting), width=line_number_width)?;
                        last_span = span.end;
                    }
                    
                    if depth == 0 {
                        writeln!(f, " {}", message.message.colorize(message.level, is_redirecting))?;
                    } else {
                        let diff = (span.start as isize - 1).max(0) as usize;
                        writeln!(f, "\n{:width$}{}{}{}", ' ', "|".magenta_ext(is_redirecting), " ".repeat(diff), message.message.colorize(message.level, is_redirecting), width=line_number_width)?;
                    }
                }

                if base.notes.is_empty() {
                    return Ok(());
                }
                
            } else {
                writeln!(f, "{}{}\n", "  --> ".magenta_ext(is_redirecting), filepath.display())?;
                
                if base.notes.is_empty() {
                    return Ok(());
                }

                writeln!(f, "{:width$}{}", ' ', "|".magenta_ext(is_redirecting), width=line_number_width)?;
            }
            
            writeln!(f, "{:width$}{} {}", ' ', "= note:".magenta_ext(is_redirecting), base.notes[0].message, width=line_number_width)?;
    
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

pub(crate) struct DiagnosticBuilder<'a> {
    inner: Option<DiagnosticBase>,
    base: RwLockWriteGuard<'a, DiagnosticInner>
}

impl<'a> DiagnosticBuilder<'a> {
    pub(crate) fn report<S: AsRef<str>>(&mut self, level: DiagnosticLevel, message: S, source_info: SourceManagerDiagnosticInfo, span: Option<Span>) -> &mut Self {
        self.inner = Some(DiagnosticBase::new(DiagnosticMessage::new(level, message.as_ref().to_owned(), span), source_info));
        self
    }

    pub(crate) fn add_note<S: AsRef<str>>(&mut self, message: S, span: Option<Span>) -> &mut Self {
        if let Some(ref mut inner) = self.inner {
            inner.add_note(DiagnosticMessage::new(DiagnosticLevel::Note, message.as_ref().to_owned(), span));
        }
        self
    }

    pub(crate) fn add_error<S: AsRef<str>>(&mut self, message: S, span: Option<Span>) -> &mut Self {
        if let Some(ref mut inner) = self.inner {
            inner.add_error(message.as_ref().to_owned(), span);
        } else {
            self.inner = Some(DiagnosticBase::new(DiagnosticMessage::new(DiagnosticLevel::Error, message.as_ref().to_owned(), span), SourceManagerDiagnosticInfo::default()));
        }
        self
    }

    pub(crate) fn add_warning<S: AsRef<str>>(&mut self, message: S, span: Option<Span>) -> &mut Self {
        if let Some(ref mut inner) = self.inner {
            inner.add_warning(message.as_ref().to_owned(), span);
        } else {
            self.inner = Some(DiagnosticBase::new(DiagnosticMessage::new(DiagnosticLevel::Warning, message.as_ref().to_owned(), span), SourceManagerDiagnosticInfo::default()));
        }
        self
    }

    pub(crate) fn add_info<S: AsRef<str>>(&mut self, message: S, span: Option<Span>) -> &mut Self {
        if let Some(ref mut inner) = self.inner {
            inner.add_info(message.as_ref().to_owned(), span);
        } else {
            self.inner = Some(DiagnosticBase::new(DiagnosticMessage::new(DiagnosticLevel::Info, message.as_ref().to_owned(), span), SourceManagerDiagnosticInfo::default()));
        }
        self
    }

    pub(crate) fn commit(&mut self) {
        let inner = self.inner.take().expect("DiagnosticBuilder::commit called without a message!");
        self.base.add_message(inner);
    }
}

impl Drop for DiagnosticBuilder<'_> {
    fn drop(&mut self) {
        if self.inner.is_some() {
            self.commit();
        }
    }
}

pub trait DiagnosticReporter {
    fn add_message(&mut self, message: DiagnosticBase);
    fn get_filepath(&self) -> &Path;
    fn has_error(&self) -> bool;
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

    fn has_error(&self) -> bool {
        self.messages.iter().any(|m| m.has_error)
    }
}

impl Display for DiagnosticBag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for message in self.iter() {
            print_message_fmt(message, self.get_filepath(), f, is_redirecting())?;
        }
        Ok(())
    }
}

struct StreamingDiagnosticBagInner {
    writer: Box<dyn std::io::Write>,
}

pub struct StreamingDiagnosticBag {
    inner: Arc<RwLock<StreamingDiagnosticBagInner>>,
    filepath: PathBuf,
    has_error: bool,
}

impl StreamingDiagnosticBag {
    pub fn new<P: AsRef<Path>>(writer: Box<dyn std::io::Write>, filepath: P) -> Self {
        Self {
            inner: Arc::new(RwLock::new(StreamingDiagnosticBagInner {
                writer,
            })),
            filepath: filepath.as_ref().to_path_buf(),
            has_error: false,
        }
    }

    fn iter(&self) -> impl Iterator<Item = &DiagnosticBase> {
        std::iter::empty()
    }
}

unsafe impl Send for StreamingDiagnosticBag {}
unsafe impl Sync for StreamingDiagnosticBag {}

impl DiagnosticReporter for StreamingDiagnosticBag {
    fn add_message(&mut self, message: DiagnosticBase) {
        let mut self_ = self.inner.write().unwrap();
        self.has_error = message.has_error;
        let path = self.get_filepath().to_path_buf();
        print_message_io(&message, &path, &mut self_.writer, is_redirecting()).unwrap();
    }

    fn get_filepath(&self) -> &Path {
        &self.filepath
    }

    fn has_error(&self) -> bool {
        self.has_error
    }
}

enum DiagnosticInner {
    Stream(StreamingDiagnosticBag),
    Bag(DiagnosticBag),
}

pub struct Diagnostic {
    inner: Arc<RwLock<DiagnosticInner>>,
} 

impl Display for StreamingDiagnosticBag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for message in self.iter() {
            print_message_fmt(message, self.get_filepath(), f, is_redirecting())?;
        }
        Ok(())
    }
}

impl Display for DiagnosticInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Stream(s) => s.fmt(f),
            Self::Bag(b) => b.fmt(f),
        }
    }
}

impl DiagnosticReporter for DiagnosticInner {
    fn add_message(&mut self, message: DiagnosticBase) {
        match self {
            Self::Stream(s) => s.add_message(message),
            Self::Bag(b) => b.add_message(message),
        }
    }

    fn get_filepath(&self) -> &Path {
        match self {
            Self::Stream(s) => s.get_filepath(),
            Self::Bag(b) => b.get_filepath(),
        }
    }

    fn has_error(&self) -> bool {
        match self {
            Self::Stream(s) => s.has_error(),
            Self::Bag(b) => b.has_error(),
        }
    }
}

impl From<StreamingDiagnosticBag> for Diagnostic {
    fn from(stream: StreamingDiagnosticBag) -> Self {
        Diagnostic {
            inner: Arc::new(RwLock::new(DiagnosticInner::Stream(stream))),
        }
    }
}

impl From<DiagnosticBag> for Diagnostic {
    fn from(bag: DiagnosticBag) -> Self {
        Diagnostic {
            inner: Arc::new(RwLock::new(DiagnosticInner::Bag(bag))),
        }
    }
}


impl Diagnostic {
    pub(crate) fn add_message(&self, message: DiagnosticBase) {
        let mut inner = self.inner.write().unwrap();
        inner.add_message(message);
    }

    pub(crate) fn get_filepath(&self) -> PathBuf {
        let inner = self.inner.read().unwrap();
        inner.get_filepath().to_path_buf()
    }

    pub(crate) fn builder(&self) -> DiagnosticBuilder {
        let inner = self.inner.write().unwrap();
        DiagnosticBuilder {
            inner: None,
            base: inner,
        }
    }
    pub(crate) fn has_error(&self) -> bool {
        let inner = self.inner.read().unwrap();
        inner.has_error()
    }
}

impl Display for Diagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let inner = self.inner.read().unwrap();
        inner.fmt(f)
    }
}
