use hir::{Identifier, Item, Sym};
use std::collections::HashMap;
use typecheck::{TypeEngine, TypeId};

#[derive(Clone, Default)]
pub struct SymbolTable<M = ()> {
    pub parent: Option<Box<SymbolTable<M>>>,
    pub symbols: HashMap<Identifier, Local<M>>,
}

#[derive(Clone)]
pub struct Local<M> {
    pub name: Identifier,
    pub value: super::expr::Expression,
    pub ty: TypeId,
    pub mutable: bool,
    pub metadata: M,
}

impl<M> Local<M> {
    pub fn debug<'a>(&'a self, engine: &'a TypeEngine) -> LocalDebug<'a, M> {
        LocalDebug { local: self, engine }
    }
}

pub struct LocalDebug<'a, M> {
    local: &'a Local<M>,
    engine: &'a TypeEngine,
}

impl<M: std::fmt::Debug> std::fmt::Debug for LocalDebug<'_, M> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Binding `{}`:", self.local.name)?;
        writeln!(f, "   mutable? {}", self.local.mutable)?;
        writeln!(f, "      type: {:?}", self.engine.typeinfo(self.local.ty).debug(self.engine).add_indent(3))?;
        write!(f, "     value: {:?}", self.local.value.debug().add_indent(3))
    }
}
