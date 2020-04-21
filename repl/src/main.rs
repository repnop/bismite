#![allow(clippy::match_bool)]

mod eval;
mod repl;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use oxygen::ParseError;
use repl::{Repl, ReplError};

fn main() {
    let mut repl = Repl::new();

    loop {
        match repl.run() {
            Ok(Some(text)) => println!("{}", text),
            Ok(None) => {}
            Err(ReplError::MultiExpression) => {
                println!("Error: can't evaluate more than one expression at one time")
            }
            Err(ReplError::Readline(e)) => {
                println!("Error reading input: {}", e);
            }
            Err(ReplError::ParseError(source, e)) => match e {
                ParseError::BadBinOp => println!(
                    "Error: Multiple binary operations in the same expression require parenthesis"
                ),
                ParseError::Eof => println!("Error: got eof"),
                ParseError::BadToken { got, expected } => {
                    let file = SimpleFile::new("<stdin>", source);
                    let message = match &expected[..] {
                        [single] => format!("Expected {}, got {}", single, got),
                        _ => format!("Expected one of {}, got {}", expected.join(", "), got),
                    };
                    let diagnostic = Diagnostic::error()
                        .with_message("Unexpected token")
                        .with_labels(vec![Label::primary((), got.span)])
                        .with_notes(vec![message]);
                    let writer = StandardStream::stderr(ColorChoice::Always);
                    let config = codespan_reporting::term::Config::default();

                    codespan_reporting::term::emit(&mut writer.lock(), &config, &file, &diagnostic)
                        .unwrap();
                }
            },
        }
    }
}
