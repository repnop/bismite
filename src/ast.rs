use crate::{parser::ParserError, parser::GLOBAL_INTERNER, token::*};
use derive_more::{Constructor, From};
use itertools::Itertools;
use std::convert::TryFrom;
use string_interner::Sym;

#[derive(Debug)]
pub enum Decls {
    Struct(StructDecl),
    Fn(FnDecl),
    Const(ConstDecl),
}

#[derive(Debug, Clone, Constructor)]
pub struct StructDecl {
    pub ident: Ident,
    pub fields: Vec<FieldDecl>,
    pub span: ByteSpan,
}

#[derive(Debug, Clone, Constructor)]
pub struct FieldDecl {
    pub ident: Ident,
    pub field_type: Type,
    pub span: ByteSpan,
}

pub type ParameterDecl = FieldDecl;

#[derive(Debug, Clone, Constructor)]
pub struct FnDecl {
    pub ident: Ident,
    pub parameters: Vec<FieldDecl>,
    pub return_type: Option<Type>,
    pub statements: Vec<StatementDecl>,
    pub span: ByteSpan,
}

#[derive(Debug, Clone, Constructor)]
pub struct ConstDecl {
    pub ident: Ident,
    pub ty: Type,
    pub value: Literal,
}

#[derive(Debug, Clone)]
pub enum StatementDecl {
    VarDecl(VarDecl),
}

#[derive(Debug, Clone, Constructor)]
pub struct VarDecl {
    pub ident: Ident,
    pub var_type: Option<Type>,
    pub value: Expression,
    pub span: ByteSpan,
}

#[derive(Debug, Clone, Copy, Constructor)]
pub struct Ident {
    pub span: ByteSpan,
    pub id: Sym,
}

impl ToString for Ident {
    fn to_string(&self) -> String {
        GLOBAL_INTERNER
            .read()
            .unwrap()
            .resolve(self.id)
            .unwrap()
            .to_string()
    }
}

// TODO:
#[derive(Debug, Clone, Constructor)]
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
    FnCall(Path, Vec<Expression>),
    FieldAccess(Box<Expression>, Ident),
    MethodCall(PathSegment, Vec<Expression>),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOp {
    Negate,
    Not,
}

impl ToString for UnaryOp {
    fn to_string(&self) -> String {
        match self {
            UnaryOp::Negate => "-",
            UnaryOp::Not => "!",
        }
        .to_string()
    }
}

impl<'a> TryFrom<Token<'a>> for UnaryOp {
    type Error = ParserError<'a>;

    fn try_from(kind: Token<'a>) -> Result<UnaryOp, ParserError<'a>> {
        Ok(match kind.kind {
            TokenKind::Minus => UnaryOp::Negate,
            TokenKind::Not => UnaryOp::Not,
            _ => return Err(ParserError::UnexpectedToken(kind)),
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mult,
    Div,
    LShift,
    RShift,
    BitAnd,
    BitOr,
    LogicalAnd,
    LogicalOr,
    Xor,
    Eq,
    Gt,
    Lt,
    GtEq,
    LtEq,
    NotEq,
    Access,
}

impl ToString for BinaryOp {
    fn to_string(&self) -> String {
        match self {
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Mult => "*",
            BinaryOp::Div => "/",
            BinaryOp::LShift => "<<",
            BinaryOp::RShift => ">>",
            BinaryOp::BitAnd => "&",
            BinaryOp::BitOr => "|",
            BinaryOp::LogicalAnd => "&&",
            BinaryOp::LogicalOr => "||",
            BinaryOp::Xor => "^",
            BinaryOp::Eq => "==",
            BinaryOp::Gt => ">",
            BinaryOp::Lt => "<",
            BinaryOp::GtEq => ">=",
            BinaryOp::LtEq => "<=",
            BinaryOp::NotEq => "!=",
            BinaryOp::Access => ".",
        }
        .to_string()
    }
}

impl<'a> TryFrom<Token<'a>> for BinaryOp {
    type Error = ParserError<'a>;

    fn try_from(kind: Token<'a>) -> Result<BinaryOp, ParserError<'a>> {
        Ok(match kind.kind {
            TokenKind::Plus => BinaryOp::Add,
            TokenKind::Minus => BinaryOp::Sub,
            TokenKind::Mult => BinaryOp::Mult,
            TokenKind::Div => BinaryOp::Div,
            TokenKind::LShift => BinaryOp::LShift,
            TokenKind::RShift => BinaryOp::RShift,
            TokenKind::Xor => BinaryOp::Xor,
            TokenKind::BitAnd => BinaryOp::BitAnd,
            TokenKind::BitOr => BinaryOp::BitOr,
            TokenKind::LogicalAnd => BinaryOp::LogicalAnd,
            TokenKind::LogicalOr => BinaryOp::LogicalOr,
            TokenKind::EqTo => BinaryOp::Eq,
            TokenKind::Gt => BinaryOp::Gt,
            TokenKind::Lt => BinaryOp::Lt,
            TokenKind::GtEq => BinaryOp::GtEq,
            TokenKind::LtEq => BinaryOp::LtEq,
            TokenKind::NotEq => BinaryOp::NotEq,
            TokenKind::Period => BinaryOp::Access,
            _ => return Err(ParserError::UnexpectedToken(kind)),
        })
    }
}

impl BinaryOp {
    pub fn precedence(self) -> u8 {
        use self::BinaryOp::*;
        match self {
            Access => 10,
            Mult => 9,
            Div => 9,
            Add => 8,
            Sub => 8,
            LShift => 7,
            RShift => 7,
            Xor => 5,
            BitAnd => 4,
            BitOr => 3,
            LogicalAnd => 2,
            LogicalOr => 1,
            Eq => 0,
            Lt => 0,
            Gt => 0,
            GtEq => 0,
            LtEq => 0,
            NotEq => 0,
        }
    }
}

#[derive(Debug, Clone, Constructor)]
pub struct Literal {
    pub kind: LiteralKind,
    pub span: ByteSpan,
}

#[derive(Debug, Clone, From)]
pub enum LiteralKind {
    Int(i128),
    Float(f64),
    RawStr(Sym),
    Array(Vec<Expression>),
    Ident(Ident),
}

#[derive(Debug, Clone, Constructor)]
pub struct Type {
    pub kind: TypeKind,
    pub span: ByteSpan,
}

#[derive(Debug, Clone)]
pub enum TypeKind {
    Infer,
    Path(Path),
    Array(Box<Type>, usize),
    Literal(LiteralKind),
}

#[derive(Debug, Clone, Constructor)]
pub struct Path {
    pub segments: Vec<PathSegment>,
    pub span: ByteSpan,
}

#[derive(Debug, Clone, Constructor)]
pub struct PathSegment {
    pub ident: Ident,
}
