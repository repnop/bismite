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

#[derive(Debug, Clone, Constructor)]
pub struct FnDecl {
    pub ident: Ident,
    pub arguments: Vec<FieldDecl>,
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
    MethodCall(PathSegment, Vec<Box<Expression>>),
}

#[derive(Debug, Clone, Copy)]
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

#[derive(Debug, Clone, Copy)]
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
            _ => return Err(ParserError::UnexpectedToken(kind)),
        })
    }
}

impl BinaryOp {
    pub fn precedence(self) -> u8 {
        use self::BinaryOp::*;
        match self {
            Add => 9,
            Sub => 9,
            Mult => 10,
            Div => 10,
            LShift => 8,
            RShift => 8,
            Xor => 6,
            BitAnd => 5,
            BitOr => 4,
            LogicalAnd => 3,
            LogicalOr => 2,
            Eq => 1,
            Gt => 1,
            Lt => 1,
            GtEq => 1,
            LtEq => 1,
            NotEq => 1,
        }
    }
}

#[derive(Debug, Clone, Constructor)]
pub struct Literal {
    pub span: ByteSpan,
    pub kind: LiteralKind,
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

#[derive(Debug, Clone, Constructor)]
pub struct Path {
    pub span: ByteSpan,
    pub segments: Vec<PathSegment>,
}

#[derive(Debug, Clone, Constructor)]
pub struct PathSegment {
    pub ident: Ident,
}
