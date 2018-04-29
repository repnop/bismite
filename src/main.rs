extern crate codespan;
extern crate codespan_reporting;
extern crate structopt;
#[macro_use]
extern crate structopt_derive;

mod parser;
mod token;
mod token_stream;

use codespan_reporting::termcolor::StandardStream;
use codespan_reporting::{emit, ColorArg, Diagnostic, Label, Severity};

use structopt::StructOpt;

use std::path::PathBuf;
use std::process::exit;
use std::str::FromStr;

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
    let t = token::TokenKind::Ident(String::from("asdf"));
    let t2 = token::TokenKind::Ident(String::from("asdf2"));

    println!("{:?}\n\n\n\n\n", t == t2);

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

    println!("{:?}", parser::Parser(ts).begin_parse());
}
