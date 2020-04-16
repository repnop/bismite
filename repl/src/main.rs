use oxygen::{
    ast::{AstNode, BinOp, Expression, ExpressionKind, Statement},
    ParseError, Parser,
};
use std::io::{stdin, stdout, BufRead, Write};

struct Env;

impl Env {
    fn eval(&mut self, node: AstNode) {
        match node {
            AstNode::Expression(e) => {
                let res = self.eval_expr(e);
                println!("{:?}", res);
            }
            AstNode::Statement(Statement::Expression(e)) => {
                self.eval_expr(e);
            }
            _ => todo!(),
        }
    }

    fn eval_expr(&mut self, expr: Expression) -> Expression {
        match expr.kind.clone() {
            ExpressionKind::Integer(_) => expr,
            ExpressionKind::BinaryOperation(e1, op, e2) => {
                let e1 = self.eval_expr(*e1);
                let e2 = self.eval_expr(*e2);

                match (e1.kind, op, e2.kind) {
                    (ExpressionKind::Integer(i1), op, ExpressionKind::Integer(i2)) => match op {
                        BinOp::Plus => Expression {
                            kind: ExpressionKind::Integer(i1 + i2),
                            span: expr.span,
                        },
                        BinOp::Minus => Expression {
                            kind: ExpressionKind::Integer(i1 - i2),
                            span: expr.span,
                        },
                        BinOp::Mult => Expression {
                            kind: ExpressionKind::Integer(i1 * i2),
                            span: expr.span,
                        },
                    },
                    _ => todo!(),
                }
            }
            _ => todo!(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Mode {
    Ast,
    Eval,
}

fn get_input() -> Option<String> {
    print!("> ");
    stdout().flush().unwrap();

    let mut s = String::new();
    let mut buf = Vec::new();

    let stdin = stdin();
    let mut stdin = stdin.lock();

    stdin.read_until(b'\n', &mut buf).unwrap();
    s.push_str(std::str::from_utf8(&buf).ok()?);
    buf.clear();

    if s.is_empty() {
        std::process::exit(0);
    } else if s.trim().is_empty() {
        println!();
        None
    } else {
        Some(s)
    }
}

fn main() {
    let mut code = String::new();
    'outer: loop {
        let mut env = Env;
        if let Some(input) = get_input() {
            code += &input;
            let (mode, mut parser) = if code.starts_with(".ast:") {
                (Mode::Ast, Parser::new(code.trim_start_matches(".ast:")))
            } else if code.trim() == ".clear" {
                print!("\x1B[2J\x1B[H");
                stdout().flush().unwrap();
                code.clear();
                continue;
            } else {
                (Mode::Eval, Parser::new(&code))
            };

            let mut nodes = Vec::new();
            let mut hit_expr = false;

            loop {
                let node = parser.guess();

                if let Err(ParseError::Eof) = &node {
                    print!("|");
                    continue 'outer;
                }

                if hit_expr && matches!(&node, Ok(Some(AstNode::Expression(_)))) {
                    println!("Can only eval one expression per line");
                    code.clear();
                    continue 'outer;
                }

                match node {
                    Ok(Some(node)) => {
                        hit_expr = matches!(&node, AstNode::Expression(_));
                        nodes.push(node);
                    }
                    Ok(None) => break,
                    Err(e) => {
                        println!("{:?}", e);
                        code.clear();
                        continue 'outer;
                    }
                }
            }

            for node in nodes {
                match mode {
                    Mode::Eval => env.eval(node),
                    Mode::Ast => println!("{:#?}", node),
                }
            }

            code.clear();
        }
    }
}
