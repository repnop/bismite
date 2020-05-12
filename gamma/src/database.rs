pub mod id;
pub mod lower;

use crate::{
    symbol_table::{ItemSymbolTable, ScopeSymbolTable},
    ty::Type,
    Expression, Function, Item, Local, Module,
};
use id::*;
use string_interner::DefaultStringInterner as StringInterner;

#[derive(Clone, Debug, Default)]
pub struct GlobalContext {
    modules: Vec<Module>,
    items: Vec<Item>,
    functions: Vec<Function>,
    locals: Vec<Local>,
    expressions: Vec<Expression>,
    types: Vec<Type>,
    item_symbol_tables: Vec<ItemSymbolTable>,
    scope_symbol_tables: Vec<ScopeSymbolTable>,
    interner: StringInterner,
}

impl GlobalContext {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_module(&mut self, module: Module) -> ModuleId {
        let idx = self.modules.len();
        self.modules.push(module);

        ModuleId { idx }
    }

    pub fn add_symbol_table(&mut self, symbol_table: ItemSymbolTable) -> ItemSymbolTableId {
        let idx = self.item_symbol_tables.len();
        self.item_symbol_tables.push(symbol_table);

        ItemSymbolTableId { idx }
    }

    pub fn root_symbol_table(&self) -> &ItemSymbolTable {
        &self.item_symbol_tables[0]
    }

    pub fn root_symbol_table_mut(&mut self) -> &mut ItemSymbolTable {
        &mut self.item_symbol_tables[0]
    }
}

macro_rules! index_with {
    ($($t:ty => $i:ident -> $t2:ty),+$(,)?) => {
        $(
            impl std::ops::Index<$t> for GlobalContext {
                type Output = $t2;

                fn index(&self, idx: $t) -> &Self::Output {
                    &(self.$i)[idx.idx]
                }
            }

            impl std::ops::Index<&$t> for GlobalContext {
                type Output = $t2;

                fn index(&self, idx: &$t) -> &Self::Output {
                    &(self.$i)[idx.idx]
                }
            }

            impl std::ops::IndexMut<$t> for GlobalContext {
                fn index_mut(&mut self, idx: $t) -> &mut Self::Output {
                    &mut (self.$i)[idx.idx]
                }
            }

            impl std::ops::IndexMut<&$t> for GlobalContext {
                fn index_mut(&mut self, idx: &$t) -> &mut Self::Output {
                    &mut (self.$i)[idx.idx]
                }
            }
        )+
    };
}

index_with! {
    ModuleId => modules -> Module,
    ItemId => items -> Item,
    FunctionId => functions -> Function,
    LocalId => locals -> Local,
    ExpressionId => expressions -> Expression,
    TypeId => types -> Type,
    ItemSymbolTableId => item_symbol_tables -> ItemSymbolTable,
}
