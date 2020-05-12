use crate::database::{
    id::{ItemId, ItemSymbolTableId, LocalId, ModuleId, StructId, SymbolId},
    GlobalContext,
};
use std::collections::HashMap;
use string_interner::Sym;

#[derive(Clone, Debug, Default)]
pub struct ItemSymbolTable {
    parent: Option<ItemSymbolTableId>,
    symbols: HashMap<Sym, ItemSymbolKind>,
}

impl ItemSymbolTable {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_parent(parent: ItemSymbolTableId) -> Self {
        Self {
            parent: Some(parent),
            symbols: Default::default(),
        }
    }

    pub fn resolve(&self, ctx: &GlobalContext, symbol: Sym) -> Option<ItemSymbolKind> {
        self.symbols
            .get(&symbol)
            .copied()
            .or_else(|| ctx[self.parent.as_ref()?].resolve(ctx, symbol))
    }
}

#[derive(Clone, Copy, Debug)]
pub enum ItemSymbolKind {
    Struct(StructId),
    Function(ItemId),
    Module(ModuleId),
}

impl ItemSymbolKind {
    pub fn is_struct(self) -> bool {
        match self {
            ItemSymbolKind::Struct(_) => true,
            _ => false,
        }
    }

    pub fn is_function(self) -> bool {
        match self {
            ItemSymbolKind::Function(_) => true,
            _ => false,
        }
    }

    pub fn is_module(self) -> bool {
        match self {
            ItemSymbolKind::Module(_) => true,
            _ => false,
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct ScopeSymbolTable {
    item_symbol_table: Option<ItemSymbolTableId>,
    locals: Vec<LocalId>,
}

impl ScopeSymbolTable {
    pub fn locals(&self) -> impl Iterator<Item = LocalId> {
        let locals = self.locals.clone();
        locals.into_iter()
    }
}

pub struct ScopedLocal<'a> {
    table: &'a ScopeSymbolTable,
    position: usize,
    pub id: LocalId,
}

impl<'a> ScopedLocal<'a> {
    fn new(table: &'a ScopeSymbolTable, position: usize, id: LocalId) -> Self {
        Self {
            table,
            position,
            id,
        }
    }
}
