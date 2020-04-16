use oxygen::{
    ast::{AstNode, BinOp, Expression, ExpressionKind},
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

#[derive(Clone, Copy, Debug)]
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
    loop {
        let mut env = Env;
        let mut keep_code = false;
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

            loop {
                match (mode, parser.guess()) {
                    (Mode::Eval, Ok(Some(node))) => env.eval(node),
                    (Mode::Ast, Ok(Some(node))) => println!("{:#?}", node),
                    (_, Ok(None)) => break,
                    (_, Err(ParseError::Eof)) => {
                        print!("|");
                        keep_code = true;
                        break;
                    }
                    (_, Err(e)) => {
                        println!("{:?}", e);
                        break;
                    }
                }
            }

            if !keep_code {
                code.clear();
            }
        }
    }
}
