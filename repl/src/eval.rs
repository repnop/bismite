use oxygen::ast::{AstNode, BinOp, Expression, ExpressionKind, Statement};
use std::collections::HashMap;

#[derive(Debug)]
pub enum EvalError {
    IncompatibleTypes,
    NotImplementedYet(&'static str),
    NotValidAssignee,
    VariableNotFound,
    VariableNotAssignable,
}

pub struct Environment {
    variables: HashMap<String, VariableInfo>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }

    pub fn variable_info(&self, ident: &str) -> Option<&VariableInfo> {
        self.variables.get(ident)
    }

    pub fn eval(&mut self, node: AstNode) -> Option<String> {
        match node {
            AstNode::Expression(e) => {
                let res = self.eval_expr(e);
                Some(match res {
                    Ok(res) => format!("{:?}", res),
                    Err(res) => format!("{:?}", res),
                })
            }
            AstNode::Statement(stmt) => match self.eval_stmt(stmt) {
                Ok(_) => None,
                Err(e) => Some(format!("{:?}", e)),
            },
            #[allow(unreachable_patterns)]
            _ => Some("Not supported yet!".to_string()),
        }
    }

    fn eval_expr(&mut self, expr: Expression) -> Result<Expression, EvalError> {
        let expr_span = expr.span;
        match expr.kind.clone() {
            ExpressionKind::Integer(_) => Ok(expr),
            ExpressionKind::Identifier(ident) => match self.variables.get(&ident.value) {
                Some(variable) => match variable.ty() {
                    Type::Integer(i) => Ok(Expression {
                        kind: ExpressionKind::Integer(*i),
                        span: ident.span,
                    }),
                    #[allow(unreachable_patterns)]
                    _ => Err(EvalError::NotImplementedYet("types to exprs")),
                },
                None => Err(EvalError::VariableNotFound),
            },
            ExpressionKind::BinaryOperation(e1, op, e2) => {
                let e1 = self.eval_expr(*e1)?;
                let e2 = self.eval_expr(*e2)?;

                match (e1.kind, op, e2.kind) {
                    (ExpressionKind::Integer(i1), op, ExpressionKind::Integer(i2)) => match op {
                        BinOp::Plus => Ok(Expression {
                            kind: ExpressionKind::Integer(i1 + i2),
                            span: expr.span,
                        }),
                        BinOp::Minus => Ok(Expression {
                            kind: ExpressionKind::Integer(i1 - i2),
                            span: expr.span,
                        }),
                        BinOp::Mult => Ok(Expression {
                            kind: ExpressionKind::Integer(i1 * i2),
                            span: expr.span,
                        }),
                        BinOp::Divide => Ok(Expression {
                            kind: ExpressionKind::Integer(i1 / i2),
                            span: expr.span,
                        }),
                    },
                    _ => Err(EvalError::NotImplementedYet("non-int ops")),
                }
            }
            ExpressionKind::Assignment(e1, e2) => {
                let e2 = self.eval_expr(*e2)?;

                match e1.kind {
                    ExpressionKind::Identifier(ident) => {
                        match self.variables.get_mut(&ident.value) {
                            Some(variable) => match variable.mutable() {
                                true => match (&mut variable.ty, e2.kind) {
                                    (Type::Integer(i1), ExpressionKind::Integer(i2)) => {
                                        *i1 = i2;
                                        Ok(Expression {
                                            kind: ExpressionKind::Unit,
                                            span: expr_span,
                                        })
                                    }
                                    _ => Err(EvalError::IncompatibleTypes),
                                },
                                false => Err(EvalError::VariableNotAssignable),
                            },
                            None => Err(EvalError::VariableNotFound),
                        }
                    }
                    _ => Err(EvalError::NotValidAssignee),
                }
            }
            _ => Err(EvalError::NotImplementedYet("non-binary-op exprs")),
        }
    }

    fn eval_stmt(&mut self, stmt: Statement) -> Result<(), EvalError> {
        match stmt {
            Statement::Expression(e) => {
                self.eval_expr(e)?;
            }
            Statement::VariableBinding(vb) => {
                let ty = match self.eval_expr(vb.value)?.kind {
                    ExpressionKind::Integer(i) => Type::Integer(i),
                    _ => return Err(EvalError::NotImplementedYet("more expr types")),
                };

                self.variables
                    .insert(vb.name.value, VariableInfo::new(ty, vb.mutable));
            }
        }

        Ok(())
    }
}

#[derive(Debug)]
pub enum Type {
    Integer(i128),
}

#[derive(Debug)]
pub struct VariableInfo {
    ty: Type,
    mutable: bool,
}

impl VariableInfo {
    pub fn new(ty: Type, mutable: bool) -> Self {
        Self { ty, mutable }
    }

    pub fn ty(&self) -> &Type {
        &self.ty
    }

    pub fn mutable(&self) -> bool {
        self.mutable
    }
}
