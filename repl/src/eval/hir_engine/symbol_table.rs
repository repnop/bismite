use hir::{Identifier, Item, Sym};
use std::collections::HashMap;

#[derive(Clone, Debug, Default)]
pub struct SymbolTable<M = ()> {
    parent: Option<Box<SymbolTable<M>>>,
    symbols: HashMap<Sym, SymbolKind<M>>,
}

#[derive(Clone, Debug)]
pub enum SymbolKind<M = ()> {
    Local(Local<M>),
    Item(Item),
}

#[derive(Clone, Debug)]
pub struct Local<M> {
    pub ident: Identifier,
    pub metadata: M,
}
