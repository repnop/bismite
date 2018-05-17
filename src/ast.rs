use token::*;

#[derive(Debug)]
pub struct Decls {
    structs: Vec<StructDecl>,
    fns: Vec<FnDecl>,
}

impl Decls {
    pub fn new() -> Decls {
        Decls {
            structs: Vec::new(),
            fns: Vec::new(),
        }
    }

    pub fn push_struct(&mut self, s: StructDecl) {
        self.structs.push(s);
    }

    pub fn push_fn(&mut self, s: FnDecl) {
        self.fns.push(s);
    }
}

#[derive(Debug)]
pub struct StructDecl {
    pub ident: Token,
    pub fields: Vec<FieldDecl>,
    pub span: ByteSpan,
}

#[derive(Debug)]
pub struct FieldDecl {
    pub ident: Token,
    pub field_type: Token,
    pub span: ByteSpan,
}

#[derive(Debug)]
pub struct FnDecl {
    pub ident: Token,
    pub arguments: Vec<FieldDecl>,
    pub return_type: Option<Token>,
    pub statements: Vec<StatementDecl>,
    pub span: ByteSpan,
}

#[derive(Debug)]
pub enum StatementDecl {
    VarDecl(VarDecl),
}

#[derive(Debug)]
pub struct VarDecl {
    pub ident: Token,
    pub var_type: Option<Token>,
    pub value: Expression,
    pub span: ByteSpan,
}

#[derive(Debug)]
pub enum Expression {
    Literal(TokenKind),
    Other,
}
