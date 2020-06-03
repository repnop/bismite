#![allow(clippy::match_bool)]

mod eval;
mod repl;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use eval::EvalError;
use parser::ParseError;
use repl::{Repl, ReplError, ReplErrorKind};

fn main() {
    let mut repl = Repl::new();

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
        ReplError {
            kind: ReplErrorKind::MultiExpression,
            ..
        } => println!("Error: can't evaluate more than one expression at one time"),
        ReplError {
            kind: ReplErrorKind::Readline(e),
            ..
        } => {
            println!("Error reading input: {}", e);
        }
        ReplError {
            source,
            kind: ReplErrorKind::ParseError(e),
        } => match e {
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
        ReplError {
            source,
            kind: ReplErrorKind::EvalError(e),
        } => match e {
            EvalError::NotImplementedYet(thing) => println!("Error: {} not supported yet", thing),
            EvalError::NotValidAssignee(span) => {
                let file = SimpleFile::new("<stdin>", source);
                let diagnostic = Diagnostic::error()
                    .with_message("Cannot assign to expression")
                    .with_labels(vec![Label::primary((), span)]);
                let writer = StandardStream::stderr(ColorChoice::Always);
                let config = codespan_reporting::term::Config::default();

                codespan_reporting::term::emit(&mut writer.lock(), &config, &file, &diagnostic)
                    .unwrap();
            }
            EvalError::VariableNotAssignable(span) => {
                let file = SimpleFile::new("<stdin>", source);
                let diagnostic = Diagnostic::error()
                    .with_message("Cannot assign to immutable binding")
                    .with_labels(vec![
                        Label::primary((), span).with_message("was not declared as mutable")
                    ]);
                let writer = StandardStream::stderr(ColorChoice::Always);
                let config = codespan_reporting::term::Config::default();

                codespan_reporting::term::emit(&mut writer.lock(), &config, &file, &diagnostic)
                    .unwrap();
            }
            EvalError::VariableNotFound(span) => {
                let file = SimpleFile::new("<stdin>", source);
                let diagnostic = Diagnostic::error()
                    .with_message("Unknown identifier")
                    .with_labels(vec![Label::primary((), span)]);
                let writer = StandardStream::stderr(ColorChoice::Always);
                let config = codespan_reporting::term::Config::default();

                codespan_reporting::term::emit(&mut writer.lock(), &config, &file, &diagnostic)
                    .unwrap();
            }
            EvalError::IncompatibleTypes((type1, span1), (type2, span2)) => {
                let file = SimpleFile::new("<stdin>", source);
                let diagnostic = Diagnostic::error()
                    .with_message("Incompatible types")
                    .with_labels(vec![
                        Label::primary((), span1).with_message(format!("Has type {}", type1)),
                        Label::primary((), span2).with_message(format!("Has type {}", type2)),
                    ]);
                let writer = StandardStream::stderr(ColorChoice::Always);
                let config = codespan_reporting::term::Config::default();

                codespan_reporting::term::emit(&mut writer.lock(), &config, &file, &diagnostic)
                    .unwrap();
            }
            EvalError::Multiple(errs) => {
                for err in errs {
                    print_err(ReplError::new(
                        source.clone(),
                        ReplErrorKind::EvalError(err),
                    ));
                }
            }
        },
    }
}
