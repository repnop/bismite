use crate::{parser::GLOBAL_INTERNER, token::*};
use derive_more::{Constructor, From};
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
    FnCall(Box<Expression>, Vec<Expression>),
    //MemberAccess(Box<Expression>, Box<Expression>),
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Negate,
    Not,
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
    Add,
    Sub,
    Mult,
    Div,
    LShift,
    RShift,
    BitXor,
    BitAnd,
    BitOr,
    LogicalXor,
    LogicalAnd,
    LogicalOr,
    Eq,
    Gt,
    Lt,
    GtEq,
    LtEq,
    NotEq,
}

impl BinaryOp {
    fn precedence(self) -> u8 {
        use self::BinaryOp::*;
        match self {
            Add => 9,
            Sub => 9,
            Mult => 10,
            Div => 10,
            LShift => 8,
            RShift => 8,
            LogicalXor | BitXor => 6,
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
