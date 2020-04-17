use oxygen::ast::{AstNode, BinOp, Expression, ExpressionKind, Statement};

pub struct Environment;

impl Environment {
    pub fn new() -> Self {
        Self
    }

    pub fn eval(&mut self, node: AstNode) -> Option<String> {
        match node {
            AstNode::Expression(e) => {
                let res = self.eval_expr(e);
                Some(format!("{:?}", res))
            }
            AstNode::Statement(Statement::Expression(e)) => {
                self.eval_expr(e);
                None
            }
            _ => Some(format!("Not supported yet!")),
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
