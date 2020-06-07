mod expr;
mod symbol_table;

use hir::{
    visit::Visitor, BinOp, Block, Expression, ExpressionKind, Identifier, Item, ItemKind, Local, Path, Statement,
    StatementKind,
};
use std::{
    collections::HashMap,
    fmt::{self, Debug, Formatter},
};
use symbol_table::SymbolTable;
use typecheck::{Context, TypeEngine, TypeError, TypeId, TypeInfo};

pub enum HirEngineError {
    NotMutable(Identifier),
    TypeError(Box<TypeError>, Box<TypeEngine>),
    UnknownImport(Path),
    UnknownIdentifier(Identifier),
}

impl Debug for HirEngineError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            HirEngineError::NotMutable(ident) => write!(f, "Local `{}` was not declared mutable", ident),
            HirEngineError::TypeError(e, engine) => write!(f, "{:?}", e.debug(engine)),
            HirEngineError::UnknownImport(path) => write!(f, "UnknownImport({})", path),
            HirEngineError::UnknownIdentifier(ident) => write!(f, "UnknownIdentifier({})", ident),
        }
    }
}

#[derive(Default)]
pub struct HirEngine {
    type_engine: TypeEngine,
    symbol_table: SymbolTable,
    current_path: Path,
    aliases: HashMap<Path, HashMap<Path, Path>>,
    values: Vec<expr::Expression>,
}

impl HirEngine {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn type_engine(&self) -> &TypeEngine {
        &self.type_engine
    }

    pub fn expr_arena(&self) -> &[expr::Expression] {
        &self.values
    }

    pub fn new_expr(&mut self, expr: expr::Expression) -> expr::ExpressionId {
        let id = expr::ExpressionId(self.values.len());
        self.values.push(expr);
        id
    }

    pub fn evaluate_item(&mut self, item: &Item) -> Result<(), HirEngineError> {
        match &item.kind {
            ItemKind::Struct(s) => {
                let ctx = self.mk_context();
                self.type_engine.register_struct(&ctx, &self.current_path, s).map_err(|e| self.mk_type_error(e))
            }
            ItemKind::Module(module) => {
                self.current_path = self.current_path.with_ident(module.name);

                let aliases = self.aliases.entry(self.current_path.clone()).or_default();
                UseCollector::new(aliases).visit_module(&module);

                // TODO: clear stuff on errors
                for item in &module.items {
                    self.evaluate_item(item)?;
                }

                self.current_path.pop();

                Ok(())
            }
            ItemKind::Use(usage) => {
                let ctx = self.mk_context();
                if self.type_engine.typeid_from_path(&ctx, &usage.path).is_none() {
                    return Err(HirEngineError::UnknownImport(usage.path.clone()));
                }

                self.aliases
                    .entry(self.current_path.clone())
                    .or_default()
                    .insert(Path::from_identifier(usage.path.last()), usage.path.clone());

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
        let ctx = self.mk_context();
        let type_id = self.type_engine.from_hir_type(&ctx, &local.ty).map_err(|e| self.mk_type_error(e))?;
        let expr = self.evaluate_expression(&local.value, Some(type_id))?;
        let expr = self.new_expr(expr);

        self.symbol_table.new_binding(symbol_table::Local::new(local.name, expr, type_id, local.mutable));

        Ok(())
    }

    pub fn evaluate_expression(
        &mut self,
        expr: &Expression,
        expected_type: Option<TypeId>,
    ) -> Result<expr::Expression, HirEngineError> {
        let expected_type = expected_type.unwrap_or_else(|| self.type_engine.fresh_infer());
        let ctx = self.mk_context();

        self.type_engine.typecheck_expression(&ctx, expr, expected_type).map_err(|e| self.mk_type_error(e))?;

        Ok(match &expr.kind {
            ExpressionKind::Block(block) => self.evaluate_block(block, Some(expected_type))?,
            ExpressionKind::BinaryOperation(lhs, op, rhs) => {
                let lhs = self.evaluate_expression(lhs, None)?;
                let rhs = self.evaluate_expression(rhs, None)?;

                match (lhs, rhs) {
                    (expr::Expression::Integer(lhs), expr::Expression::Integer(rhs)) => match op {
                        BinOp::Add => expr::Expression::Integer(lhs + rhs),
                        BinOp::Subtract => expr::Expression::Integer(lhs - rhs),
                        BinOp::Multiply => expr::Expression::Integer(lhs * rhs),
                        BinOp::Divide => expr::Expression::Integer(lhs / rhs),
                        BinOp::LogicalAnd => todo!(),
                    },
                    _ => todo!("actual eval stuff"),
                }
            }
            ExpressionKind::Integer(i) => expr::Expression::Integer(*i),
            ExpressionKind::Boolean(b) => expr::Expression::Bool(*b),
            ExpressionKind::Path(path) => match path.is_identifier() {
                Some(ident) => match self.symbol_table.resolve_binding(ident) {
                    Some(local) => self.values[local.value.0].clone(),
                    None => return Err(HirEngineError::UnknownIdentifier(ident)),
                },
                _ => todo!("path"),
            },
            ExpressionKind::Struct(s) => expr::Expression::Struct(
                s.name.clone(),
                s.members
                    .iter()
                    .map(|member| {
                        let expr = self.evaluate_expression(&member.expression, None)?;
                        let expr = self.new_expr(expr);

                        Ok((member.name, expr))
                    })
                    .collect::<Result<_, _>>()?,
            ),
            ExpressionKind::FieldAccess(lhs, ident) => {
                let s = self.evaluate_expression(lhs, None)?;

                match s {
                    expr::Expression::Struct(_, members) => self.values[members.get(ident).unwrap().0].clone(),
                    _ => unreachable!(),
                }
            }
            ExpressionKind::Assignment(lhs, rhs) => {
                let rhs = self.evaluate_expression(rhs, None)?;
                *self.get_place(lhs)? = rhs;

                expr::Expression::Unit
            }
            ExpressionKind::Unit => expr::Expression::Unit,
        })
    }

    pub fn evaluate_block(
        &mut self,
        block: &Block,
        expected_type: Option<TypeId>,
    ) -> Result<expr::Expression, HirEngineError> {
        let old_symtab = self.symbol_table.clone();

        self.symbol_table = SymbolTable::with_parent(&old_symtab);

        let res = (|| {
            for statement in &block.statements {
                self.evaluate_statement(statement)?;
            }

            self.evaluate_expression(&block.return_expr, expected_type)
        })();

        self.symbol_table = old_symtab;

        res
    }

    fn get_place(&mut self, expr: &Expression) -> Result<&mut expr::Expression, HirEngineError> {
        match &expr.kind {
            ExpressionKind::FieldAccess(lhs, field) => {
                let lhs = self.get_place(lhs)?;

                match lhs {
                    expr::Expression::Struct(_, members) => {
                        let id = members.get_mut(field).unwrap().0;
                        Ok(&mut self.values[id])
                    }
                    _ => unreachable!(),
                }
            }
            ExpressionKind::Path(path) => match path.is_identifier() {
                Some(ident) => match self.symbol_table.resolve_binding(ident) {
                    Some(e) if e.mutable => Ok(&mut self.values[e.value.0]),
                    Some(_) => Err(HirEngineError::NotMutable(ident)),
                    None => unreachable!(),
                },
                None => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    pub fn typeinfo(&self, path: &Path) -> Option<TypeInfo> {
        let ctx = self.mk_context();
        let id = self.type_engine.typeid_from_path(&ctx, path)?;
        Some(self.type_engine.typeinfo(id).clone())
    }

    pub fn varinfo(&self, ident: Identifier) -> Option<symbol_table::Local> {
        self.symbol_table.resolve_binding(ident)
    }

    fn mk_type_error(&self, error: TypeError) -> HirEngineError {
        HirEngineError::TypeError(Box::new(error), Box::new(self.type_engine.clone()))
    }

    fn mk_context(&self) -> Context<'static> {
        Context {
            aliases: self.aliases.get(&self.current_path).cloned().unwrap_or_default(),
            bindings: self.symbol_table.bindings().map(|local| (local.name, local.ty)).collect(),
            parent: None,
        }
    }
}

struct UseCollector<'a> {
    aliases: &'a mut HashMap<Path, Path>,
}

impl<'a> UseCollector<'a> {
    fn new(aliases: &'a mut HashMap<Path, Path>) -> Self {
        Self { aliases }
    }
}

impl Visitor for UseCollector<'_> {
    fn visit_use(&mut self, usage: &hir::Use) {
        let path = usage.path.clone();
        self.aliases.insert(Path::from_identifier(path.last()), path);
    }

    fn visit_item(&mut self, item: &Item) {
        if let ItemKind::Use(usage) = &item.kind {
            self.visit_use(usage);
        }
    }
}
