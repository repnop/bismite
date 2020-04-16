use codespan::Span;

#[derive(Debug)]
pub enum AstNode {
    Statement(Statement),
    Expression(Expression),
}

#[derive(Debug)]
pub enum Statement {
    VariableBinding(VariableBinding),
    Assignment(Assignment),
    Expression(Expression),
}

#[derive(Debug)]
pub struct VariableBinding {
    pub name: String,
    pub ty: Type,
    pub value: Expression,
    pub span: Span,
}

#[derive(Debug)]
pub struct Assignment {
    pub ident: String,
    pub value: Expression,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    Integer(i128),
    Identifier(String),
    BinaryOperation(Box<Expression>, BinOp, Box<Expression>),
    FnCall(Box<Expression>, Vec<Expression>),
    Unary(),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BinOp {
    Plus,
    Minus,
    Mult,
}

pub enum UnaryOp {
    Minus,
}

#[derive(Debug)]
pub struct Type {
    pub kind: TypeKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum TypeKind {
    Named(String),
}
