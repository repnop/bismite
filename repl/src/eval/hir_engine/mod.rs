mod symbol_table;

use codespan::Span;
use hir::{
    Expression, ExpressionKind, Item, ItemKind, Local, Path, Statement, StatementKind, Struct,
};
use std::collections::HashMap;
use std::fmt::{self, Debug, Formatter};
use symbol_table::{SymbolKind, SymbolTable};
use typecheck::{TypeEngine, TypeError, TypeId, TypeInfo};

pub enum HirEngineError {
    TypeError(TypeError),
}

impl Debug for HirEngineError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            HirEngineError::TypeError(e) => write!(f, "{:?}", e),
        }
    }
}

impl From<TypeError> for HirEngineError {
    fn from(e: TypeError) -> Self {
        HirEngineError::TypeError(e)
    }
}

#[derive(Default)]
pub struct HirEngine {
    type_engine: TypeEngine,
    symbol_table: SymbolTable,
    current_path: Path,
}

impl HirEngine {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn type_engine(&self) -> &TypeEngine {
        &self.type_engine
    }

    pub fn evaluate_item(&mut self, item: &Item) -> Result<(), HirEngineError> {
        match &item.kind {
            ItemKind::Struct(s) => self
                .type_engine
                .register_struct(&self.current_path, s)
                .map_err(Into::into),
            ItemKind::Module(module) => {
                self.current_path = self.current_path.with_ident(module.name);

                // TODO: clear stuff on errors
                for item in &module.items {
                    self.evaluate_item(item)?;
                }

                self.current_path.pop();

                Ok(())
            }
            _ => todo!("item eval"),
        }
    }

    pub fn evaluate_statement(&mut self, statement: &Statement) -> Result<(), HirEngineError> {
        match &statement.kind {
            StatementKind::Local(local) => self.evaluate_local(local),
            StatementKind::Expression(expr) => self.evaluate_expression(expr, None).map(drop),
        }
    }

    pub fn evaluate_local(&mut self, local: &Local) -> Result<(), HirEngineError> {
        todo!("evaluate_local")
    }

    pub fn evaluate_expression(
        &mut self,
        expr: &Expression,
        expected_type: Option<TypeId>,
    ) -> Result<Expression, HirEngineError> {
        let expected_type = expected_type.unwrap_or(self.type_engine.fresh_infer());
        let id = self.type_engine.typecheck_expression(expr, expected_type)?;
        println!("{:?}", self.type_engine.typeinfo(id));

        Ok(Expression {
            kind: ExpressionKind::Unit,
            span: Span::new(0, 0),
        })
    }

    pub fn typeinfo(&self, path: &Path) -> Option<TypeInfo> {
        let id = self.type_engine.typeid_from_path(path)?;
        Some(self.type_engine.typeinfo(id).clone())
    }
}
