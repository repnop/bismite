use crate::eval::{hir_engine::HirEngine, Environment, EvalError};
use ast::AstNode;
use parser::{ParseError, Parser};
use rustyline::{error::ReadlineError, hint::HistoryHinter, CompletionType, Config, Editor};

const HELP_MSG: &str = r"Commands:
    .help                       Displays this help text
    .ast                        Display the following code as its AST form
    .clear                      Clear the current screen
    .varinfo <ident>            Display the variable information associated with the given identifier
    .typeinfo <path>            Display information about the type associated with the given path
    .loadfile <file_path>       Load the given file path as a module";

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum EvalMode {
    Ast,
    Eval,
    Hir,
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

pub struct ReplError {
    pub source: String,
    pub kind: ReplErrorKind,
}

impl ReplError {
    pub fn new(source: String, kind: ReplErrorKind) -> Self {
        Self { source, kind }
    }
}

pub enum ReplErrorKind {
    EvalError(EvalError),
    ParseError(ParseError),
    MultiExpression,
    Readline(rustyline::error::ReadlineError),
}

pub struct Repl {
    code: String,
    environment: Environment,
    editor: Editor<Helper>,
    prompt_mode: PromptMode,
    hir_engine: HirEngine,
}

impl Repl {
    pub fn new() -> Self {
        let mut editor = Editor::with_config(Config::builder().completion_type(CompletionType::Circular).build());
        editor.set_helper(Some(Helper::new()));
        let _ = editor.load_history("repl_history.bismite");

        Self {
            code: String::new(),
            environment: Environment::new(),
            editor,
            prompt_mode: PromptMode::Fresh,
            hir_engine: HirEngine::new(),
        }
    }
    pub fn run(&mut self) -> Result<Option<String>, ReplError> {
        let line = match self.read_line(self.prompt_mode) {
            LineReturn::Done(s) => s,
            LineReturn::Empty => {
                let _ = self.editor.save_history("repl_history.bismite");
                std::process::exit(0)
            }
            LineReturn::Error(e) => return Err(ReplError::new(String::new(), ReplErrorKind::Readline(e))),
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
                let code = self.code.clone();
                self.reset();
                return Err(ReplError::new(code, ReplErrorKind::MultiExpression));
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
                    return Err(ReplError::new(code, ReplErrorKind::ParseError(e)));
                }
            }
        }

        let mut eval_output = None;
        for node in nodes {
            match eval_mode {
                EvalMode::Eval => match node {
                    AstNode::Item(item) => match self.hir_engine.evaluate_item(&hir::Item::convert(&item)) {
                        Ok(_) => eval_output = None,
                        Err(e) => eval_output = Some(format!("{:?}", e)),
                    },
                    AstNode::Expression(e) => {
                        match self.hir_engine.evaluate_expression(&hir::Expression::convert(&e), None) {
                            Ok(e) => eval_output = Some(format!("{:?}", e)),
                            Err(e) => eval_output = Some(format!("{:?}", e)),
                        }
                    }
                    AstNode::Statement(ast::Statement { kind: ast::StatementKind::VariableBinding(vb), .. }) => {
                        match self.hir_engine.evaluate_local(&hir::Local::convert(&vb)) {
                            Ok(_) => eval_output = None,
                            Err(e) => eval_output = Some(format!("{:?}", e)),
                        }
                    }
                    _ => todo!("idfrhgb;oksernhbks"),
                },
                EvalMode::Ast => eval_output = Some(format!("{:#?}", node)),
                EvalMode::Hir => {
                    eval_output = Some(format!("{:#?}", {
                        match &node {
                            AstNode::Expression(_) => todo!(),
                            AstNode::Item(i) => hir::Item::convert(i),
                            AstNode::Statement(_) => todo!(),
                        }
                    }))
                }
            }
        }
        self.reset();

        Ok(eval_output)
    }

    fn eval_repl_command(&mut self, s: &str) -> bool {
        let command = match s.split(' ').next() {
            Some(cmd) => cmd,
            None => return false,
        };

        match command {
            ".clear" => {
                println!("\x1B[2J\x1B[H");
                self.code.clear();
            }
            ".help" => {
                println!("{}", HELP_MSG);
            }
            ".varinfo" => {
                let ident = match s.split(' ').nth(1) {
                    Some(ident) => ident,
                    None => {
                        println!("Must provide an identifier to .varinfo, e.g. .varinfo my_variable");
                        return true;
                    }
                };
                let valid_ident = Parser::new(ident).identifier();

                match valid_ident {
                    Ok(valid_ident) => {
                        let valid_ident = hir::Identifier::convert(&valid_ident);
                        match self.hir_engine.varinfo(valid_ident) {
                            Some(var_info) => println!("{:?}", var_info.debug(self.hir_engine.type_engine())),
                            None => {
                                println!("Variable with identifier `{}` not found in scope", valid_ident);
                                return true;
                            }
                        }
                    }
                    Err(_) => {
                        println!("Invalid identifier");
                        return true;
                    }
                }
            }
            ".typeinfo" => {
                let path = match s.split(' ').nth(1) {
                    Some(path) => path,
                    None => {
                        println!("Must provide a path to .typeinfo, e.g. .typeinfo MyType");
                        return true;
                    }
                };
                let path = match Parser::new(path).path() {
                    Ok(path) => hir::Path::convert(&path),
                    Err(e) => {
                        println!("Invalid path: {:?}", e);
                        return true;
                    }
                };

                match self.hir_engine.typeinfo(&path) {
                    Some(type_info) => println!("{:?}", type_info.debug(&self.hir_engine.type_engine())),
                    None => println!("Type with path `{}` not found in scope", path),
                }
            }
            ".loadfile" => {
                let file_path = match s.split(' ').nth(1) {
                    Some(path) => path,
                    None => {
                        println!("Must provide a file path to .loadfile");
                        return true;
                    }
                };

                let file_contents = match std::fs::read_to_string(&file_path) {
                    Ok(contents) => contents,
                    Err(e) => {
                        println!("Error reading file: {}", e);
                        return true;
                    }
                };

                let parsed = match Parser::new(&file_contents).module(true) {
                    Ok(mut module) => {
                        module.name = ast::Identifier {
                            value: std::path::Path::new(&file_path).file_stem().unwrap().to_string_lossy().to_string(),
                            span: codespan::Span::new(0, 0),
                        };

                        module
                    }
                    Err(e) => {
                        println!("Error parsing file: {:?}", e);
                        return true;
                    }
                };

                let module = hir::Module::convert(&parsed);
                let span = module.span;
                if let Err(e) = self.hir_engine.evaluate_item(&hir::Item { kind: hir::ItemKind::Module(module), span })
                {
                    println!("Error processing module: {:?}", e);
                }
            }
            _ => return false,
        }

        true
    }

    fn eval_mode(&mut self) -> EvalMode {
        let cmd = match self.code.split_whitespace().next() {
            Some(cmd) => cmd,
            None => return EvalMode::Eval,
        };

        match cmd.trim() {
            ".ast" => {
                self.code = self.code.trim_start_matches(".ast").to_string();
                EvalMode::Ast
            }
            ".hir" => {
                self.code = self.code.trim_start_matches(".hir").to_string();
                EvalMode::Hir
            }
            _ => EvalMode::Eval,
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
        Self { hinter: HistoryHinter {} }
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
