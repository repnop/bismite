pub mod hir_engine;

use ast::{AstNode, BinOp, Expression, ExpressionKind, Statement, StatementKind};
use codespan::Span;
use std::collections::HashMap;

#[derive(Debug)]
pub enum EvalError {
    IncompatibleTypes((String, Span), (String, Span)),
    NotImplementedYet(&'static str),
    NotValidAssignee(Span),
    VariableNotFound(Span),
    VariableNotAssignable(Span),
    Multiple(Vec<EvalError>),
}

#[derive(Default)]
pub struct Environment {
    variables: HashMap<String, VariableInfo>,
}

impl Environment {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn variable_info(&self, ident: &str) -> Option<&VariableInfo> {
        self.variables.get(ident)
    }

    pub fn eval(&mut self, node: AstNode) -> Result<Option<String>, EvalError> {
        Ok(match node {
            AstNode::Expression(e) => {
                let res = self.eval_expr(e)?;
                Some(format!("{:?}", res))
            }
            AstNode::Statement(stmt) => self.eval_stmt(stmt).map(|_| None)?,
            _ => return Err(EvalError::NotImplementedYet("whatever you typed")),
        })
    }

    fn eval_expr(&mut self, expr: Expression) -> Result<Expression, EvalError> {
        let expr_span = expr.span;
        match expr.kind.clone() {
            ExpressionKind::Integer(_) => Ok(expr),
            ExpressionKind::Path(path) => match self.variables.get(&path.segments[0].value) {
                Some(variable) => match variable.ty() {
                    Type::Integer(i) => Ok(Expression { kind: ExpressionKind::Integer(*i), span: path.span }),
                    Type::Boolean(b) => Ok(Expression { kind: ExpressionKind::Boolean(*b), span: path.span }),
                },
                None => Err(EvalError::VariableNotFound(path.span)),
            },
            ExpressionKind::BinaryOperation(e1, op, e2) => {
                let e1 = self.eval_expr(*e1)?;
                let e2 = self.eval_expr(*e2)?;

                match (e1.kind, op, e2.kind) {
                    (ExpressionKind::Integer(i1), op, ExpressionKind::Integer(i2)) => match op {
                        BinOp::Add => Ok(Expression { kind: ExpressionKind::Integer(i1 + i2), span: expr.span }),
                        BinOp::Subtract => Ok(Expression { kind: ExpressionKind::Integer(i1 - i2), span: expr.span }),
                        BinOp::Multiply => Ok(Expression { kind: ExpressionKind::Integer(i1 * i2), span: expr.span }),
                        BinOp::Divide => Ok(Expression { kind: ExpressionKind::Integer(i1 / i2), span: expr.span }),
                    },
                    _ => Err(EvalError::NotImplementedYet("non-int ops")),
                }
            }
            ExpressionKind::Assignment(e1, e2) => {
                let e2_span = e2.span;
                let e2 = self.eval_expr(*e2)?;

                match e1.kind {
                    ExpressionKind::Path(path) => match self.variables.get_mut(&path.segments[0].value) {
                        Some(variable) => {
                            let t2 = Type::try_from(&e2)?;
                            match variable.mutable() {
                                true => match (&mut variable.ty, t2) {
                                    (t1, t2) if t1.same_kind(&t2) => {
                                        *t1 = t2;
                                        Ok(Expression { kind: ExpressionKind::Unit, span: expr_span })
                                    }
                                    (t1, t2) => Err(EvalError::IncompatibleTypes(
                                        (t1.to_string(), e1.span),
                                        (t2.to_string(), e2_span),
                                    )),
                                },
                                false if variable.ty().same_kind(&t2) => {
                                    Err(EvalError::VariableNotAssignable(path.span))
                                }
                                false => Err(EvalError::Multiple(vec![
                                    EvalError::VariableNotAssignable(path.span),
                                    EvalError::IncompatibleTypes(
                                        (variable.ty().to_string(), e1.span),
                                        (t2.to_string(), e2_span),
                                    ),
                                ])),
                            }
                        }
                        None => Err(EvalError::VariableNotFound(path.span)),
                    },
                    _ => Err(EvalError::NotValidAssignee(e1.span)),
                }
            }
            ExpressionKind::Boolean(_) => Ok(expr),
            _ => Err(EvalError::NotImplementedYet("non-binary-op exprs")),
        }
    }

    fn eval_stmt(&mut self, stmt: Statement) -> Result<(), EvalError> {
        match stmt.kind {
            StatementKind::Expression(e) => {
                self.eval_expr(e)?;
            }
            StatementKind::VariableBinding(vb) => {
                let ty = match self.eval_expr(vb.value)?.kind {
                    ExpressionKind::Integer(i) => Type::Integer(i),
                    ExpressionKind::Boolean(b) => Type::Boolean(b),
                    _ => return Err(EvalError::NotImplementedYet("more expr types")),
                };

                self.variables.insert(vb.name.value, VariableInfo::new(ty, vb.mutable));
            }
        }

        Ok(())
    }
}

#[derive(Debug)]
pub enum Type {
    Integer(i128),
    Boolean(bool),
}

impl Type {
    pub fn to_string(&self) -> String {
        match self {
            Type::Integer(_) => String::from("int"),
            Type::Boolean(_) => String::from("bool"),
        }
    }

    pub fn same_kind(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Integer(_), Type::Integer(_)) => true,
            (Type::Boolean(_), Type::Boolean(_)) => true,
            _ => false,
        }
    }

    pub fn try_from(expression: &Expression) -> Result<Self, EvalError> {
        match &expression.kind {
            ExpressionKind::Integer(i) => Ok(Type::Integer(*i)),
            ExpressionKind::Boolean(b) => Ok(Type::Boolean(*b)),
            _ => Err(EvalError::NotImplementedYet("this type extraction from expr")),
        }
    }
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