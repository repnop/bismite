mod ty;
pub mod visit;

pub use ast::BinOp;
use codespan::Span;
use std::{
    cell::RefCell,
    fmt::{self, Display, Formatter},
};
pub use string_interner::{DefaultStringInterner, Sym};
pub use ty::*;

std::thread_local! {
    static INTERNER: RefCell<DefaultStringInterner> = RefCell::new(DefaultStringInterner::new());
}

#[derive(Clone, Debug)]
pub struct Module {
    pub name: Identifier,
    pub items: Vec<Item>,
    pub span: Span,
}

impl Module {
    pub fn convert(module: &ast::Module) -> Self {
        Self {
            name: Identifier::convert(&module.name),
            items: module.items.iter().map(Item::convert).collect(),
            span: module.span,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Item {
    pub kind: ItemKind,
    pub span: Span,
}

impl Item {
    pub fn convert(item: &ast::Item) -> Self {
        Self { kind: ItemKind::convert(&item), span: item.span() }
    }
}

#[derive(Clone, Debug)]
pub enum ItemKind {
    Module(Module),
    Function(Function),
    Struct(Struct),
    Use(Use),
}

impl ItemKind {
    pub fn convert(item: &ast::Item) -> Self {
        match item {
            ast::Item::Function(f) => ItemKind::Function(Function::convert(f)),
            ast::Item::Module(f) => ItemKind::Module(Module::convert(f)),
            ast::Item::Struct(f) => ItemKind::Struct(Struct::convert(f)),
            ast::Item::Use(u) => ItemKind::Use(Use::convert(u)),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Use {
    pub path: Path,
    pub span: Span,
}

impl Use {
    pub fn convert(usage: &ast::Use) -> Self {
        Self { path: Path::convert(&usage.path), span: usage.span }
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug, Default)]
pub struct Path {
    pub segments: Vec<Identifier>,
}

impl std::fmt::Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        INTERNER.with(|i| {
            for (idx, ident) in self.segments.iter().enumerate() {
                write!(f, "{}", i.borrow_mut().resolve(ident.name).unwrap())?;

                if idx != self.segments.len() - 1 {
                    write!(f, "::")?;
                }
            }

            Ok(())
        })
    }
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

    pub fn convert(ast: &ast::Path) -> Self {
        Self { segments: ast.segments.iter().map(|ident| Identifier::convert(ident)).collect() }
    }

    pub fn canonicalize(&self) -> Self {
        let mut segments = Vec::with_capacity(self.segments.len());

        for &seg in &self.segments {
            if seg.string() == "super" {
                segments.pop();
            } else {
                segments.push(seg);
            }
        }

        Self { segments }
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

#[derive(Clone, Copy, Eq, Debug)]
pub struct Identifier {
    pub name: Sym,
    pub span: Span,
}

impl Identifier {
    pub fn string(self) -> String {
        INTERNER.with(|i| i.borrow().resolve(self.name).unwrap().to_owned())
    }
    pub fn convert(ast: &ast::Identifier) -> Self {
        Self { name: INTERNER.with(|i| i.borrow_mut().get_or_intern(&ast.value)), span: ast.span }
    }

    pub fn with_dummy_span(name: Sym) -> Self {
        Self { name, span: Span::new(0, 0) }
    }
}

impl std::fmt::Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        INTERNER.with(|i| write!(f, "{}", i.borrow_mut().resolve(self.name).unwrap()))
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

impl Local {
    pub fn convert(vb: &ast::VariableBinding) -> Self {
        let name = Identifier::convert(&vb.name);
        let mutable = vb.mutable;
        let ty = Type::convert_optional(&vb.ty);
        let value = Expression::convert(&vb.value);
        let span = vb.span;

        Self { name, mutable, ty, value, span }
    }
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
    Integer(i128),
    Path(Path),
    Struct(StructExpr),
    Unit,
}

impl ExpressionKind {
    pub fn convert(kind: &ast::ExpressionKind) -> Self {
        match kind {
            ast::ExpressionKind::Assignment(lhs, rhs) => {
                ExpressionKind::Assignment(Box::new(Expression::convert(lhs)), Box::new(Expression::convert(rhs)))
            }
            ast::ExpressionKind::BinaryOperation(e1, op, e2) => ExpressionKind::BinaryOperation(
                Box::new(Expression::convert(&e1)),
                *op,
                Box::new(Expression::convert(&e2)),
            ),
            ast::ExpressionKind::Block(b) => ExpressionKind::Block(Box::new(Block::convert(&b))),
            ast::ExpressionKind::Boolean(b) => ExpressionKind::Boolean(*b),
            ast::ExpressionKind::FieldAccess(e, ident) => {
                ExpressionKind::FieldAccess(Box::new(Expression::convert(e)), Identifier::convert(ident))
            }
            ast::ExpressionKind::Integer(i) => ExpressionKind::Integer(*i),
            ast::ExpressionKind::Path(path) => ExpressionKind::Path(Path::convert(path)),
            ast::ExpressionKind::Struct(s) => ExpressionKind::Struct(StructExpr::convert(s)),
            ast::ExpressionKind::Unit => ExpressionKind::Unit,
            ast::ExpressionKind::FnCall(lhs, args) => ExpressionKind::FnCall(
                Box::new(Expression::convert(&lhs)),
                args.iter().map(Expression::convert).collect(),
            ),
            e => todo!("convert {:?}", e),
        }
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
    pub items: Vec<Item>,
    pub statements: Vec<Statement>,
    pub return_expr: Expression,
}

impl Block {
    pub fn convert(block: &ast::Block) -> Self {
        Self {
            items: block.items.iter().map(Item::convert).collect(),
            statements: block.statements.iter().map(Statement::convert).collect(),
            return_expr: block.return_expr.as_ref().map(Expression::convert).unwrap_or_else(|| Expression {
                kind: ExpressionKind::Unit,
                span: block.statements.last().map(|s| s.span).unwrap_or(block.span),
            }),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Function {
    pub name: Identifier,
    pub parameters: Vec<FunctionParameter>,
    pub return_type: Type,
    pub body: Block,
}

impl Function {
    pub fn convert(f: &ast::Function) -> Self {
        Self {
            name: Identifier::convert(&f.name),
            parameters: f.parameters.iter().map(FunctionParameter::convert).collect(),
            return_type: f.return_ty.as_ref().map(Type::convert).unwrap_or_else(|| Type {
                kind: TypeKind::Unit,
                // FIXME
                span: Span::new(0, 0),
            }),
            body: Block::convert(&f.body),
        }
    }
}

#[derive(Clone, Debug)]
pub struct FunctionParameter {
    pub name: Identifier,
    pub ty: Type,
    pub span: Span,
}

impl FunctionParameter {
    pub fn convert(fp: &ast::FunctionParameter) -> Self {
        Self { name: Identifier::convert(&fp.name), ty: Type::convert(&fp.ty), span: fp.span }
    }
}

#[derive(Clone, Debug)]
pub struct Struct {
    pub name: Identifier,
    pub members: Vec<StructMember>,
    pub span: Span,
}

impl Struct {
    pub fn convert(strukt: &ast::Struct) -> Self {
        Self {
            name: Identifier::convert(&strukt.name),
            members: strukt.members.iter().map(StructMember::convert).collect(),
            span: strukt.span,
        }
    }
}

impl Display for Struct {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "struct {} {{", self.name)?;
        for member in &self.members {
            writeln!(f, "    {}", member)?;
        }
        write!(f, "}}")
    }
}

#[derive(Clone, Debug)]
pub struct StructMember {
    pub name: Identifier,
    pub ty: Type,
    pub span: Span,
}

impl StructMember {
    pub fn convert(member: &ast::StructMember) -> Self {
        Self { name: Identifier::convert(&member.name), ty: Type::convert(&member.ty), span: member.span }
    }
}

impl Display for StructMember {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.ty.kind)
    }
}
