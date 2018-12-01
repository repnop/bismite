mod ast;
mod parser;
mod token;
mod token_stream;
mod visit;

use codespan_reporting::{emit, termcolor::StandardStream, ColorArg, Diagnostic, Label, Severity};
use std::path::PathBuf;
use std::process::exit;
use std::str::FromStr;
use structopt::StructOpt;

macro_rules! err {
    ($e:expr) => {{
        eprintln!("{}", $e);
        exit(1)
    }};
}

/// Compiler command line arguments.
#[derive(StructOpt, Debug)]
struct Options {
    /// Input source file for compilation.
    #[structopt(parse(from_os_str))]
    input: PathBuf,
}

fn main() {
    let options = Options::from_args();

    let file_contents = match std::fs::read_to_string(&options.input) {
        Ok(s) => s,
        Err(_) => err!(format!(
            "{}: File does not exist.",
            options.input.to_str().unwrap()
        )),
    };

    let mut cm = codespan::CodeMap::new();

    cm.add_filemap(
        codespan::FileName::Real(options.input.to_path_buf()),
        file_contents.clone(),
    );

    let ts = token_stream::TokenStream::new(&file_contents).peekable();

    let writer = StandardStream::stdout(ColorArg::from_str("auto").unwrap().into());
    let res = parser::Parser(ts).begin_parse();

    if let Err(e) = res {
        emit(
            writer.lock(),
            &cm,
            &Diagnostic::new(Severity::Error, format!("{}", e)).with_label(Label::new_primary(
                match e {
                    parser::ParserError::InvalidToken(e) => e.span(),
                    parser::ParserError::UnexpectedToken(t, _) => t.span,
                    parser::ParserError::UnexpectedInGroup(t, _) => t.span,
                    _ => panic!("{:?}", e),
                },
            )),
        )
        .unwrap();
    } else if let Ok(ast) = res {
        use crate::visit::Visitor;

        let mut visitor = visit::SExprVisitor {};
        visitor.visit_decls(&ast);
    }
}
