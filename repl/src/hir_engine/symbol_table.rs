use hir::Identifier;
use std::collections::HashMap;
use typecheck::{TypeEngine, TypeId};

#[derive(Debug, Clone, Default)]
pub struct SymbolTable {
    parent: Option<Box<SymbolTable>>,
    symbols: HashMap<Identifier, Local>,
}

impl SymbolTable {
    pub fn with_parent(parent: &SymbolTable) -> SymbolTable {
        Self { parent: Some(Box::new(parent.clone())), symbols: HashMap::new() }
    }

    pub fn resolve_binding(&self, ident: Identifier) -> Option<Local> {
        match self.symbols.get(&ident) {
            Some(local) => Some(*local),
            None => match &self.parent {
                Some(parent) => parent.resolve_binding(ident),
                None => None,
            },
        }
    }

    pub fn new_binding(&mut self, local: Local) {
        self.symbols.insert(local.name, local);
    }

    pub fn bindings(&self) -> Box<dyn Iterator<Item = Local> + '_> {
        match &self.parent {
            Some(parent) => Box::new(parent.bindings().chain(self.symbols.values().copied())),
            None => Box::new(self.symbols.values().copied()),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Local {
    pub name: Identifier,
    pub value: super::expr::ExpressionId,
    pub ty: TypeId,
    pub mutable: bool,
}

impl Local {
    pub fn new(name: Identifier, value: super::expr::ExpressionId, ty: TypeId, mutable: bool) -> Self {
        Self { name, value, ty, mutable }
    }

    pub fn debug<'a>(&'a self, arena: &'a [super::expr::Expression], engine: &'a TypeEngine) -> LocalDebug<'a> {
        LocalDebug { local: self, arena, engine }
    }
}

pub struct LocalDebug<'a> {
    arena: &'a [super::expr::Expression],
    local: &'a Local,
    engine: &'a TypeEngine,
}

impl std::fmt::Debug for LocalDebug<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Binding `{}`:", self.local.name)?;
        writeln!(f, "   mutable? {}", self.local.mutable)?;
        writeln!(f, "      type: {:?}", self.engine.typeinfo(self.local.ty).debug(self.engine).add_indent(3))?;
        write!(f, "     value: {:?}", self.arena[self.local.value.0].debug(self.arena).add_indent(3))
    }
}
