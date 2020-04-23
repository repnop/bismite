use codespan::Span;

#[derive(Clone, Debug)]
pub enum AstNode {
    Statement(Statement),
    Expression(Expression),
    Item(Item),
}

#[derive(Clone, Debug)]
pub enum Item {
    Function(Function),
    Struct(Struct),
}

#[derive(Clone, Debug)]
pub struct Function {
    pub name: Identifier,
    pub parameters: Vec<TypeInstance>,
    pub return_ty: Option<Type>,
    pub body: Block,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct TypeInstance {
    pub name: Identifier,
    pub ty: Type,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Block {
    pub items: Vec<Item>,
    pub statements: Vec<Statement>,
    pub return_expr: Option<Expression>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Struct {
    pub name: Identifier,
    pub members: Vec<TypeInstance>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum Statement {
    VariableBinding(VariableBinding),
    Expression(Expression),
}

#[derive(Clone, Debug)]
pub struct VariableBinding {
    pub mutable: bool,
    pub name: Identifier,
    pub ty: Option<Type>,
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
    Block(Box<Block>),
    BinaryOperation(Box<Expression>, BinOp, Box<Expression>),
    Boolean(bool),
    FieldAccess(Box<Expression>, Identifier),
    FnCall(Box<Expression>, Vec<Expression>),
    Identifier(Identifier),
    Integer(i128),
    Unary(UnaryOp, Box<Expression>),
    Unit,
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

#[derive(Clone, Debug)]
pub struct Type {
    pub kind: TypeKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum TypeKind {
    Named(String),
}
