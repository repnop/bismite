use crate::token::*;

#[derive(Debug)]
pub struct Decls<'a> {
    pub structs: Vec<StructDecl<'a>>,
    pub fns: Vec<FnDecl<'a>>,
}

impl<'a> Decls<'a> {
    pub fn new() -> Decls<'a> {
        Decls {
            structs: Vec::new(),
            fns: Vec::new(),
        }
    }

    pub fn push_struct(&mut self, s: StructDecl<'a>) {
        self.structs.push(s);
    }

    pub fn push_fn(&mut self, s: FnDecl<'a>) {
        self.fns.push(s);
    }
}

#[derive(Debug)]
pub struct StructDecl<'a> {
    pub ident: Token<'a>,
    pub fields: Vec<FieldDecl<'a>>,
    pub span: ByteSpan,
}

#[derive(Debug)]
pub struct FieldDecl<'a> {
    pub ident: Token<'a>,
    pub field_type: Token<'a>,
    pub span: ByteSpan,
}

#[derive(Debug)]
pub struct FnDecl<'a> {
    pub ident: Token<'a>,
    pub arguments: Vec<FieldDecl<'a>>,
    pub return_type: Option<Token<'a>>,
    pub statements: Vec<StatementDecl<'a>>,
    pub span: ByteSpan,
}

#[derive(Debug)]
pub enum StatementDecl<'a> {
    VarDecl(VarDecl<'a>),
}

#[derive(Debug)]
pub struct VarDecl<'a> {
    pub ident: Token<'a>,
    pub var_type: Option<Type<'a>>,
    pub value: Expression<'a>,
    pub span: ByteSpan,
}

// TODO:
#[derive(Debug, Clone)]
pub struct Expression<'a> {
    pub kind: ExpressionKind<'a>,
    pub ty: Type<'a>,
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
    Integer(u128),
    //String(String),
}

#[derive(Debug, Clone)]
pub struct Type<'a> {
    pub span: ByteSpan,
    pub kind: TypeKind<'a>,
}

#[derive(Debug, Clone)]
pub enum TypeKind<'a> {
    Infer,
    Named(&'a str),
    Literal(Literal<'a>),
}
