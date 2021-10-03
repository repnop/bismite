pub mod arena;
pub mod ctx;
mod ty;
//pub mod visit;

pub use ast::{BinOp, UnaryOp};
use codespan::Span;
use ctx::{ExpressionId, HirContext, ItemId, StatementId, TypeId};
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
    Module(Module),
    Function(Function),
    Struct(Struct),
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

    pub fn push(&mut self, ident: Identifier) {
        self.segments.push(ident)
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

    pub fn to_string(&self, ctx: &HirContext) -> String {
        let parts: Vec<_> = self.segments.iter().map(|i| i.as_str(ctx)).collect();
        parts.join("::")
    }
}

#[derive(Debug, Clone, Copy, Eq)]
pub struct Identifier {
    pub name: Sym,
    pub span: Span,
}

impl Identifier {
    pub fn as_str(self, ctx: &HirContext) -> &str {
        ctx.interner.resolve(self.name).unwrap()
    }
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

#[derive(Clone, Debug)]
pub enum StatementKind {
    Local(Local),
    Expression(ExpressionId),
}

#[derive(Clone, Debug)]
pub struct Local {
    pub name: Identifier,
    pub mutable: bool,
    pub ty: TypeId,
    pub value: ExpressionId,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum ExpressionKind {
    Assignment(ExpressionId, ExpressionId),
    BinaryOperation(ExpressionId, BinOp, ExpressionId),
    Block(Block),
    Boolean(bool),
    FieldAccess(ExpressionId, Identifier),
    FnCall(ExpressionId, Vec<ExpressionId>),
    If(IfExpr),
    Integer(i128),
    Path(Path),
    Struct(StructExpr),
    Unary(UnaryOp, ExpressionId),
    Unit,
}

#[derive(Debug, Clone)]
pub struct IfExpr {
    pub ifs: Vec<If>,
    pub r#else: Block,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct If {
    pub condition: ExpressionId,
    pub body: Block,
    pub span: Span,
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
    pub expression: ExpressionId,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Block {
    pub statements: Vec<StatementId>,
    pub return_expr: ExpressionId,
    pub span: Span,
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
