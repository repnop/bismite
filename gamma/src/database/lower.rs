use crate::{
    database::{id::*, GlobalContext},
    symbol_table::{ItemSymbolKind, ItemSymbolTable},
};
use aster::*;

pub enum LoweringError {
    Semantic(SemanticError),
    Type(TypeError),
}

pub enum SemanticError {}

pub enum TypeError {}

impl GlobalContext {
    pub fn lower_ast(&mut self, geode: &Geode) -> Result<ModuleId, LoweringError> {
        let id = self.lower_module(&geode.module)?;
        self.lower_internals()?;
        Ok(id)
    }

    pub fn lower_module(&mut self, module: &Module) -> Result<ModuleId, LoweringError> {
        let name = self.interner.get_or_intern(&module.name.value);

        let mut items = Vec::new();
        for item in &module.items {
            items.push(self.lower_item(item)?);
        }

        let symbols = self.add_symbol_table(ItemSymbolTable::new());

        Ok(self.add_module(crate::Module {
            name,
            items,
            symbols,
            span: module.span,
        }))
    }

    pub fn lower_item(&mut self, item: &Item) -> Result<ItemId, LoweringError> {
        todo!()
    }

    pub fn lower_fn(&mut self, function: &Function) -> Result<FunctionId, LoweringError> {
        todo!()
    }

    pub fn lower_internals(&mut self) -> Result<(), LoweringError> {
        todo!()
    }
}
