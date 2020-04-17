use crate::eval::Environment;
use oxygen::{ast::AstNode, ParseError, Parser};
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
    pub fn run(&mut self) -> Option<String> {
        let line = match self.read_line(self.prompt_mode) {
            LineReturn::Done(s) => s,
            LineReturn::Empty => std::process::exit(0),
            LineReturn::Error(e) => return Some(e.to_string()),
        };

        self.editor.add_history_entry(&*line);
        if self.eval_repl_command(&line) {
            return None;
        }

        self.code += &line;

        let eval_mode = self.eval_mode();
        let mut parser = Parser::new(&self.code);
        let mut nodes = Vec::new();
        let mut hit_expr = false;

        loop {
            let node = parser.guess();

            if let Err(ParseError::Eof) = &node {
                self.prompt_mode = PromptMode::Continuing;
                return None;
            }

            if hit_expr && matches!(&node, Ok(Some(AstNode::Expression(_)))) {
                self.code.clear();
                return Some("Error: Can only eval one expression per line".into());
            }

            match node {
                Ok(Some(node)) => {
                    hit_expr = matches!(&node, AstNode::Expression(_));
                    nodes.push(node);
                }
                Ok(None) => break,
                Err(e) => {
                    self.code.clear();
                    return Some(format!("{:?}", e));
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

        eval_output
    }

    fn eval_repl_command(&mut self, s: &str) -> bool {
        use std::io::Write;

        if s.trim() == ".clear" {
            print!("\x1B[2J\x1B[H");
            std::io::stdout().flush().unwrap();
            self.code.clear();

            true
        } else {
            false
        }
    }

    fn eval_mode(&mut self) -> EvalMode {
        if self.code.starts_with(".ast:") {
            self.code = self.code.trim_start_matches(".ast:").to_string();
            EvalMode::Ast
        } else {
            EvalMode::Eval
        }
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
