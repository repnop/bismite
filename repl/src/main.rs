#![allow(clippy::match_bool)]

mod hir_engine;
mod repl;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use parser::ParseError;
use repl::{Repl, ReplError, ReplErrorKind};
use structopt::StructOpt;

#[derive(StructOpt)]
struct Arguments {
    #[structopt(long = "clear-screen", short = "c")]
    clear_screen: bool,
}

fn main() {
    let args = Arguments::from_args();

    let mut repl = Repl::new();

    if args.clear_screen {
        println!("\x1B[2J\x1B[H");
    }

    loop {
        match repl.run() {
            Ok(Some(text)) => println!("{}", text),
            Ok(None) => {}
            Err(e) => print_err(e),
        }
    }
}

fn print_err(e: ReplError) {
    match e {
        ReplError { kind: ReplErrorKind::MultiExpression, .. } => {
            println!("Error: can't evaluate more than one expression at one time")
        }
        ReplError { kind: ReplErrorKind::Readline(e), .. } => {
            println!("Error reading input: {}", e);
        }
        ReplError { source, kind: ReplErrorKind::ParseError(e) } => match e {
            ParseError::BadBinOp => {
                println!("Error: Multiple binary operations in the same expression require parenthesis")
            }
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

                codespan_reporting::term::emit(&mut writer.lock(), &config, &file, &diagnostic).unwrap();
            }
        },
    }
}
