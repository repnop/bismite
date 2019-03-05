mod ast;
mod error;
mod parser;
mod token;
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

    use crate::visit::Visitor;
    crate::visit::SExprVisitor.visit_decls(&parser::Parser::new(&file_contents).parse().unwrap());
}
