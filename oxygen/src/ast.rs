use codespan::Span;

#[derive(Debug)]
pub enum AstNode {
    Statement(Statement),
    Expression(Expression),
}

#[derive(Debug)]
pub enum Statement {
    VariableBinding(VariableBinding),
    Expression(Expression),
}

#[derive(Debug)]
pub struct VariableBinding {
    pub name: Identifier,
    pub ty: Type,
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
    Assignment(Box<Expression>, Box<Expression>),
    BinaryOperation(Box<Expression>, BinOp, Box<Expression>),
    FieldAccess(Box<Expression>, Identifier),
    FnCall(Box<Expression>, Vec<Expression>),
    Identifier(String),
    Integer(i128),
    Unary(UnaryOp, Box<Expression>),
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub value: String,
    pub span: Span,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BinOp {
    Plus,
    Minus,
    Mult,
    Divide,
}

#[derive(Debug, Clone)]
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
