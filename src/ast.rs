use crate::{parser::GLOBAL_INTERNER, token::*};
use string_interner::Sym;

#[derive(Debug)]
pub enum Decls {
    Struct(StructDecl),
    Fn(FnDecl),
    Const(ConstDecl),
}

#[derive(Debug)]
pub struct StructDecl {
    pub ident: Ident,
    pub fields: Vec<FieldDecl>,
    pub span: ByteSpan,
}

#[derive(Debug)]
pub struct FieldDecl {
    pub ident: Ident,
    pub field_type: Type,
    pub span: ByteSpan,
}

#[derive(Debug)]
pub struct FnDecl {
    pub ident: Ident,
    pub arguments: Vec<FieldDecl>,
    pub return_type: Option<Type>,
    pub statements: Vec<StatementDecl>,
    pub span: ByteSpan,
}

#[derive(Debug)]
pub struct ConstDecl {
    pub ident: Ident,
    pub ty: Type,
    pub value: Literal,
}

#[derive(Debug)]
pub enum StatementDecl {
    VarDecl(VarDecl),
}

#[derive(Debug)]
pub struct VarDecl {
    pub ident: Ident,
    pub var_type: Option<Type>,
    pub value: Expression,
    pub span: ByteSpan,
}

#[derive(Debug, Clone, Copy)]
pub struct Ident {
    pub span: ByteSpan,
    pub id: Sym,
}

// TODO:
#[derive(Debug, Clone)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub span: ByteSpan,
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    Literal(Literal),
    Path(Path),
    Unary(UnaryOp, Box<Expression>),
    Binary(Box<Expression>, BinaryOp, Box<Expression>),
    FnCall(Box<Expression>, Vec<Expression>),
    //MemberAccess(Box<Expression>, Box<Expression>),
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Negate,
    Not,
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
    Add,
    Sub,
    Mult,
    Div,
    LShift,
    RShift,
    Xor,
    And,
    Or,
}

#[derive(Debug, Clone)]
pub struct Literal {
    pub span: ByteSpan,
    pub kind: LiteralKind,
}

#[derive(Debug, Clone)]
pub enum LiteralKind {
    Int(i128),
    Float(f64),
    RawStr(Sym),
}

#[derive(Debug, Clone)]
pub struct Type {
    pub span: ByteSpan,
    pub kind: TypeKind,
}

#[derive(Debug, Clone)]
pub enum TypeKind {
    Infer,
    Path(Path),
    Array(Box<Type>, usize),
    Literal(LiteralKind),
}

#[derive(Debug, Clone)]
pub struct Path {
    pub span: ByteSpan,
    pub segments: Vec<PathSegment>,
}

#[derive(Debug, Clone)]
pub struct PathSegment {
    pub ident: Ident,
}
