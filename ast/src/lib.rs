pub mod visit;

use codespan::Span;
pub use visit::Visitor;

#[derive(Clone, Debug)]
pub struct Geode {
    pub module: Module,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Module {
    pub name: Identifier,
    pub items: Vec<Item>,
    pub span: Span,
}

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
    Module(Module),
}

impl Item {
    pub fn span(&self) -> Span {
        match self {
            Item::Function(f) => f.span,
            Item::Struct(s) => s.span,
            Item::Module(m) => m.span,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Function {
    pub name: Identifier,
    pub parameters: Vec<FunctionParameter>,
    pub return_ty: Option<Type>,
    pub body: Block,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct FunctionParameter {
    pub name: Identifier,
    pub ty: Type,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct StructMember {
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
    pub members: Vec<StructMember>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Statement {
    pub kind: StatementKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum StatementKind {
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
    BinaryOperation(Box<Expression>, BinOp, Box<Expression>),
    Block(Box<Block>),
    Boolean(bool),
    FieldAccess(Box<Expression>, Identifier),
    FnCall(Box<Expression>, Vec<Expression>),
    Integer(i128),
    Path(Path),
    Struct(Box<StructExpr>),
    Unary(UnaryOp, Box<Expression>),
    Unit,
}

#[derive(Debug, Clone)]
pub struct StructExpr {
    pub name: Path,
    pub members: Vec<StructExprMember>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct StructExprMember {
    pub name: Identifier,
    pub expression: Expression,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub value: String,
    pub span: Span,
}

impl Identifier {
    pub fn dummy() -> Self {
        Self {
            value: String::new(),
            span: Span::new(0, 0),
        }
    }
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
    Bool,
    Integer,
    Named(Path),
}

#[derive(Clone, Debug)]
pub struct Path {
    pub segments: Vec<Identifier>,
    pub span: Span,
}
