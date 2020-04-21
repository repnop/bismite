use crate::eval::Environment;
use aster::AstNode;
use oxygen::{ParseError, Parser};
use rustyline::{error::ReadlineError, hint::HistoryHinter, CompletionType, Config, Editor};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum EvalMode {
    Ast,
    Eval,
}

pub enum LineReturn {
    Done(String),
    Empty,
    Error(rustyline::error::ReadlineError),
}

#[derive(Clone, Copy, Debug)]
pub enum PromptMode {
    Continuing,
    Fresh,
}

impl PromptMode {
    fn as_str(self) -> &'static str {
        match self {
            PromptMode::Fresh => ">> ",
            PromptMode::Continuing => ".. ",
        }
    }
}

pub enum ReplError {
    ParseError(String, ParseError),
    MultiExpression,
    Readline(rustyline::error::ReadlineError),
}

pub struct Repl {
    code: String,
    environment: Environment,
    editor: Editor<Helper>,
    prompt_mode: PromptMode,
}

impl Repl {
    pub fn new() -> Self {
        let mut editor = Editor::with_config(
            Config::builder()
                .completion_type(CompletionType::Circular)
                .build(),
        );
        editor.set_helper(Some(Helper::new()));

        Self {
            code: String::new(),
            environment: Environment::new(),
            editor,
            prompt_mode: PromptMode::Fresh,
        }
    }
    pub fn run(&mut self) -> Result<Option<String>, ReplError> {
        let line = match self.read_line(self.prompt_mode) {
            LineReturn::Done(s) => s,
            LineReturn::Empty => std::process::exit(0),
            LineReturn::Error(e) => return Err(ReplError::Readline(e)),
        };

        self.editor.add_history_entry(&*line);
        if self.eval_repl_command(&line) {
            return Ok(None);
        }

        self.code += &line;
        self.code += "\n";

        let old_code = self.code.clone();
        let eval_mode = self.eval_mode();

        let mut parser = Parser::new(&self.code);
        let mut nodes = Vec::new();
        let mut hit_expr = false;

        loop {
            let node = parser.guess();

            if let Err(ParseError::Eof) = &node {
                self.code = old_code;
                self.prompt_mode = PromptMode::Continuing;
                return Ok(None);
            }

            if hit_expr && matches!(&node, Ok(Some(AstNode::Expression(_)))) {
                self.reset();
                return Err(ReplError::MultiExpression);
            }

            match node {
                Ok(Some(node)) => {
                    hit_expr = matches!(&node, AstNode::Expression(_));
                    nodes.push(node);
                }
                Ok(None) => break,
                Err(e) => {
                    let code = self.code.clone();
                    self.reset();
                    return Err(ReplError::ParseError(code, e));
                }
            }
        }

        let mut eval_output = None;
        for node in nodes {
            match eval_mode {
                EvalMode::Eval => eval_output = self.environment.eval(node),
                EvalMode::Ast => eval_output = Some(format!("{:#?}", node)),
            }
        }

        self.reset();

        Ok(eval_output)
    }

    fn eval_repl_command(&mut self, s: &str) -> bool {
        if s.trim() == ".clear" {
            println!("\x1B[2J\x1B[H");
            self.code.clear();
        } else if s.trim() == ".help" {
            println!(
                r"Commands:
    .help                       Displays this help text
    .ast                        Display the following code as its AST form
    .clear                      Clear the current screen
    .varinfo <ident>            Display the variable information associated with the given identifier"
            );
        } else if s.starts_with(".varinfo") {
            let ident = match s.split(' ').nth(1) {
                Some(ident) => ident,
                None => {
                    println!("Must provide an identifier to .varinfo, e.g. .varinfo my_variable");
                    return true;
                }
            };
            let valid_ident = Parser::new(ident).identifier().is_ok();

            if !valid_ident {
                println!("Invalid identifier");
                return true;
            }

            match self.environment.variable_info(ident) {
                Some(var_info) => println!("{:?}", var_info),
                None => println!("Variable with identifier `{}` not found in scope", ident),
            }
        } else {
            return false;
        }

        true
    }

    fn eval_mode(&mut self) -> EvalMode {
        if self.code.starts_with(".ast") {
            self.code = self.code.trim_start_matches(".ast").to_string();
            EvalMode::Ast
        } else {
            EvalMode::Eval
        }
    }

    fn reset(&mut self) {
        self.code.clear();
        self.prompt_mode = PromptMode::Fresh;
    }

    fn read_line(&mut self, mode: PromptMode) -> LineReturn {
        match self.editor.readline(mode.as_str()) {
            Ok(s) => LineReturn::Done(s),
            Err(ReadlineError::Eof) | Err(ReadlineError::Interrupted) => LineReturn::Empty,
            Err(e) => LineReturn::Error(e),
        }
    }
}

struct Helper {
    hinter: HistoryHinter,
}

impl Helper {
    fn new() -> Self {
        Self {
            hinter: HistoryHinter {},
        }
    }
}

impl rustyline::Helper for Helper {}
impl rustyline::highlight::Highlighter for Helper {
    fn highlight_hint<'h>(&self, hint: &'h str) -> std::borrow::Cow<'h, str> {
        format!("\x1B[31;32m{}\x1B[0m", hint).into()
    }
}
impl rustyline::validate::Validator for Helper {}
impl rustyline::completion::Completer for Helper {
    type Candidate = rustyline::completion::Pair;
}
impl rustyline::hint::Hinter for Helper {
    fn hint(&self, line: &str, pos: usize, ctx: &rustyline::Context<'_>) -> Option<String> {
        self.hinter.hint(line, pos, ctx)
    }
}
