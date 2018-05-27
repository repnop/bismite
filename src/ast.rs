use token::*;

#[derive(Debug)]
pub struct Decls<'a> {
    structs: Vec<StructDecl<'a>>,
    fns: Vec<FnDecl<'a>>,
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
    pub var_type: Option<Token<'a>>,
    pub value: Expression<'a>,
    pub span: ByteSpan,
}

#[derive(Debug, Clone)]
pub enum Expression<'a> {
    IntegerLiteral(Token<'a>),
    Identifier(Token<'a>),
    Unary(Token<'a>, Box<Expression<'a>>),
    Binary(Box<Expression<'a>>, Token<'a>, Box<Expression<'a>>),
    Other,
}
