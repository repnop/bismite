#[derive(Clone, Copy, Debug)]
pub struct HirId {
    pub(super) idx: usize,
}

#[derive(Clone, Copy, Debug)]
pub struct ModuleId {
    pub(super) idx: usize,
}

#[derive(Clone, Copy, Debug)]
pub struct ItemId {
    pub(super) idx: usize,
}

#[derive(Clone, Copy, Debug)]
pub struct FunctionId {
    pub(super) idx: usize,
}

#[derive(Clone, Copy, Debug)]
pub struct StructId {
    pub(super) idx: usize,
}

#[derive(Clone, Copy, Debug)]
pub struct ItemSymbolTableId {
    pub(super) idx: usize,
}

#[derive(Clone, Copy, Debug)]
pub struct ScopeSymbolTableId {
    pub(super) idx: usize,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct SymbolId {
    pub(super) idx: usize,
}

#[derive(Clone, Copy, Debug)]
pub struct LocalId {
    pub(super) idx: usize,
}

#[derive(Clone, Copy, Debug)]
pub struct StatementId {
    pub(super) idx: usize,
}

#[derive(Clone, Copy, Debug)]
pub struct BlockId {
    pub(super) idx: usize,
}

#[derive(Clone, Copy, Debug)]
pub struct TypeId {
    pub(super) idx: usize,
}

#[derive(Clone, Copy, Debug)]
pub struct ExpressionId {
    pub(super) idx: usize,
}
