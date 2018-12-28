use crate::{parser::GLOBAL_INTERNER, token::*};
use string_interner::Sym;

#[derive(Debug)]
pub enum Decls<'a> {
    Struct(StructDecl),
    Fn(FnDecl<'a>),
    Const(ConstDecl<'a>),
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
pub struct FnDecl<'a> {
    pub ident: Ident,
    pub arguments: Vec<FieldDecl>,
    pub return_type: Option<Type>,
    pub statements: Vec<StatementDecl<'a>>,
    pub span: ByteSpan,
}

#[derive(Debug)]
pub struct ConstDecl<'a> {
    pub ident: Ident,
    pub ty: Type,
    pub value: Literal<'a>,
}

#[derive(Debug)]
pub enum StatementDecl<'a> {
    VarDecl(VarDecl<'a>),
}

#[derive(Debug)]
pub struct VarDecl<'a> {
    pub ident: Ident,
    pub var_type: Option<Type>,
    pub value: Expression<'a>,
    pub span: ByteSpan,
}

#[derive(Debug, Clone, Copy)]
pub struct Ident {
    pub span: ByteSpan,
    pub id: Sym,
}

// TODO:
#[derive(Debug, Clone)]
pub struct Expression<'a> {
    pub kind: ExpressionKind<'a>,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub enum ExpressionKind<'a> {
    Literal(Literal<'a>),
    Identifier(Token<'a>),
    Unary(Token<'a>, Box<Expression<'a>>),
    Binary(Box<Expression<'a>>, Token<'a>, Box<Expression<'a>>),
    FnCall(Token<'a>, Vec<Expression<'a>>),
    //MemberAccess(Box<Expression<'a>>, Box<Expression<'a>>),
}

#[derive(Debug, Clone)]
pub struct Literal<'a> {
    pub token: Token<'a>,
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
