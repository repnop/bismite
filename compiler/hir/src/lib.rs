pub mod arena;
pub mod ctx;
mod ty;
pub mod visit;

pub use ast::{BinOp, UnaryOp};
use codespan::Span;
use ctx::{FunctionId, ItemId, ModuleId, TypeId};
pub use string_interner::{DefaultStringInterner, Sym};
pub use ty::*;

#[derive(Clone, Debug)]
pub struct Module {
    pub name: Identifier,
    pub items: Vec<ItemId>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Item {
    pub kind: ItemKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum ItemKind {
    Module(ModuleId),
    Function(FunctionId),
    Struct(TypeId),
}

#[derive(Clone, Debug)]
pub struct Use {
    pub path: Path,
    pub span: Span,
}

#[derive(Clone, Hash, PartialEq, Eq, Debug, Default)]
pub struct Path {
    pub segments: Vec<Identifier>,
}

impl Path {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn from_identifier(ident: Identifier) -> Self {
        Self { segments: vec![ident] }
    }

    pub fn last(&self) -> Identifier {
        self.segments.last().cloned().unwrap()
    }

    pub fn first(&self) -> Identifier {
        self.segments.first().cloned().unwrap()
    }

    pub fn with_ident(&self, ident: Identifier) -> Self {
        let mut segments = self.segments.clone();
        segments.push(ident);

        Self { segments }
    }

    pub fn is_identifier(&self) -> Option<Identifier> {
        match self.segments.len() {
            1 => Some(self.first()),
            _ => None,
        }
    }

    pub fn pop(&mut self) -> Option<Identifier> {
        self.segments.pop()
    }

    pub fn join(&self, other: &Self) -> Self {
        Self {
            segments: {
                let mut segs = self.segments.clone();
                segs.extend_from_slice(&other.segments);
                segs
            },
        }
    }
}

#[derive(Debug, Clone, Copy, Eq)]
pub struct Identifier {
    pub name: Sym,
    pub span: Span,
}

impl Identifier {
    pub fn with_dummy_span(name: Sym) -> Self {
        Self { name, span: Span::new(0, 0) }
    }
}

impl PartialEq for Identifier {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl std::hash::Hash for Identifier {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

#[derive(Clone, Debug)]
pub struct Statement {
    pub kind: StatementKind,
    pub span: Span,
}

impl Statement {
    pub fn convert(statement: &ast::Statement) -> Self {
        Self { kind: StatementKind::convert(&statement.kind), span: statement.span }
    }
}

#[derive(Clone, Debug)]
pub enum StatementKind {
    Local(Local),
    Expression(Expression),
}

impl StatementKind {
    pub fn convert(sk: &ast::StatementKind) -> Self {
        match sk {
            ast::StatementKind::Expression(e) => StatementKind::Expression(Expression::convert(e)),
            ast::StatementKind::VariableBinding(vb) => StatementKind::Local(Local::convert(vb)),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Local {
    pub name: Identifier,
    pub mutable: bool,
    pub ty: Type,
    pub value: Expression,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub span: Span,
}

impl Expression {
    pub fn convert(expr: &ast::Expression) -> Self {
        let kind = ExpressionKind::convert(&expr.kind);
        let span = expr.span;

        Self { kind, span }
    }
}

#[derive(Clone, Debug)]
pub enum ExpressionKind {
    Assignment(Box<Expression>, Box<Expression>),
    BinaryOperation(Box<Expression>, BinOp, Box<Expression>),
    Block(Box<Block>),
    Boolean(bool),
    FieldAccess(Box<Expression>, Identifier),
    FnCall(Box<Expression>, Vec<Expression>),
    If(Box<IfExpr>),
    Integer(i128),
    Path(Path),
    Struct(StructExpr),
    Unary(UnaryOp, Box<Expression>),
    Unit,
}

impl ExpressionKind {
    pub fn convert(kind: &ast::ExpressionKind) -> Self {
        match kind {
            ast::ExpressionKind::Assignment(lhs, rhs) => {
                ExpressionKind::Assignment(Box::new(Expression::convert(lhs)), Box::new(Expression::convert(rhs)))
            }
            ast::ExpressionKind::BinaryOperation(e1, op, e2) => ExpressionKind::BinaryOperation(
                Box::new(Expression::convert(e1)),
                *op,
                Box::new(Expression::convert(e2)),
            ),
            ast::ExpressionKind::Block(b) => ExpressionKind::Block(Box::new(Block::convert(b))),
            ast::ExpressionKind::Boolean(b) => ExpressionKind::Boolean(*b),
            ast::ExpressionKind::FieldAccess(e, ident) => {
                ExpressionKind::FieldAccess(Box::new(Expression::convert(e)), Identifier::convert(ident))
            }
            ast::ExpressionKind::Integer(i) => ExpressionKind::Integer(*i),
            ast::ExpressionKind::Path(path) => ExpressionKind::Path(Path::convert(path)),
            ast::ExpressionKind::Struct(s) => ExpressionKind::Struct(StructExpr::convert(s)),
            ast::ExpressionKind::Unit => ExpressionKind::Unit,
            ast::ExpressionKind::FnCall(lhs, args) => ExpressionKind::FnCall(
                Box::new(Expression::convert(lhs)),
                args.iter().map(Expression::convert).collect(),
            ),
            ast::ExpressionKind::Unary(op, expr) => ExpressionKind::Unary(*op, Box::new(Expression::convert(expr))),
            ast::ExpressionKind::If(if_expr) => ExpressionKind::If(Box::new(IfExpr::convert(if_expr))),
        }
    }
}

#[derive(Debug, Clone)]
pub struct IfExpr {
    pub ifs: Vec<If>,
    pub r#else: Block,
    pub span: Span,
}

impl IfExpr {
    pub fn convert(if_expr: &ast::IfExpr) -> Self {
        Self {
            ifs: if_expr.ifs.iter().map(If::convert).collect(),
            r#else: if_expr.r#else.as_ref().map(Block::convert).unwrap_or_else(|| Block {
                statements: Vec::new(),
                return_expr: Expression { kind: ExpressionKind::Unit, span: Span::new(0, 0) },
                span: Span::new(0, 0),
            }),
            span: if_expr.span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct If {
    pub condition: Expression,
    pub body: Block,
    pub span: Span,
}

impl If {
    pub fn convert(if_: &ast::If) -> Self {
        Self { condition: Expression::convert(&if_.condition), body: Block::convert(&if_.body), span: if_.span }
    }
}

#[derive(Debug, Clone)]
pub struct StructExpr {
    pub name: Path,
    pub members: Vec<StructExprMember>,
    pub span: Span,
}

impl StructExpr {
    pub fn convert(struct_expr: &ast::StructExpr) -> Self {
        let name = Path::convert(&struct_expr.name);
        let members = struct_expr.members.iter().map(StructExprMember::convert).collect();
        let span = struct_expr.span;

        Self { name, members, span }
    }
}

#[derive(Debug, Clone)]
pub struct StructExprMember {
    pub name: Identifier,
    pub expression: Expression,
    pub span: Span,
}

impl StructExprMember {
    pub fn convert(se_member: &ast::StructExprMember) -> Self {
        let name = Identifier::convert(&se_member.name);
        let expression = Expression::convert(&se_member.expression);
        let span = se_member.span;

        Self { name, expression, span }
    }
}

#[derive(Clone, Debug)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub return_expr: Expression,
    pub span: Span,
}

impl Block {
    pub fn convert(block: &ast::Block) -> Self {
        Self {
            statements: block.statements.iter().map(Statement::convert).collect(),
            return_expr: block.return_expr.as_ref().map(Expression::convert).unwrap_or_else(|| Expression {
                kind: ExpressionKind::Unit,
                span: block.statements.last().map(|s| s.span).unwrap_or(block.span),
            }),
            span: block.span,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Function {
    pub name: Identifier,
    pub parameters: Vec<FunctionParameter>,
    pub return_type: TypeId,
    pub body: Block,
}

#[derive(Clone, Debug)]
pub struct FunctionParameter {
    pub name: Identifier,
    pub ty: TypeId,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Struct {
    pub name: Identifier,
    pub members: Vec<StructMember>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct StructMember {
    pub name: Identifier,
    pub ty: TypeId,
    pub span: Span,
}
