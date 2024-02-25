mod eoc;
use std::{io::Result, path::Path};

use crate::eoc::utils::diagnostic::StreamingDiagnosticBag;

fn main() -> Result<()> {
    let filepath = Path::new("source.eoc");
    let diagnostic = StreamingDiagnosticBag::new(Box::new(std::io::stderr()), filepath);
    let mut lexer = eoc::lexer::Lexer::new_from_filepath(filepath, diagnostic)?;
    let (tokens, diagnostics) = lexer.lex();
    tokens.iter().for_each(|token| println!("{}", token));
    println!("{}", diagnostics);
    println!("Operators: {:#?}", lexer.get_custom_operators());
    println!("Keywords: {:#?}", lexer.get_custom_keywords());
    // let mut diagnostics = eoc::utils::diagnostic::DiagnosticBag::new(filepath);
    // diagnostics.builder()
    //     .add_warning("This is an error".to_string(), None)
    //     .add_note("This is a note".to_string(), None)
    //     .add_note("This is a note".to_string(), None)
    //     .commit();

    // println!("{}", diagnostics);
    return Ok(());
}
