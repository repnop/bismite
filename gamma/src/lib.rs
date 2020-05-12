mod database;
mod symbol_table;
mod ty;

use codespan::Span;
use database::id::*;
use string_interner::Sym;

#[derive(Clone, Debug)]
pub struct Module {
    pub symbols: ItemSymbolTableId,
    pub name: Sym,
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
    Struct(StructId),
}

#[derive(Clone, Debug)]
pub struct Path {
    pub segments: Vec<Identifier>,
}

#[derive(Clone, Debug)]
pub struct Identifier {
    pub name: Sym,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Statement {
    pub kind: StatementKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum StatementKind {
    Local(Local),
}

#[derive(Clone, Debug)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum ExpressionKind {
    Identifier(Identifier),
    Integer(i128),
    Boolean(bool),
    Unit,
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
pub struct Block {
    pub symbol_table: ItemSymbolTableId,
    pub statements: Vec<StatementId>,
}

#[derive(Clone, Debug)]
pub struct Function {
    pub name: Identifier,
    pub arguments: Vec<(Identifier, TypeId)>,
    pub return_type: TypeId,
    pub body: BlockId,
    pub symbol_table: ItemSymbolTableId,
}
