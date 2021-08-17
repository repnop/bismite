mod expr;
mod symbol_table;

use hir::{
    visit::Visitor, BinOp, Block, Expression, ExpressionKind, Identifier, Item, ItemKind, Local, Path, Statement,
    StatementKind, UnaryOp,
};
use std::{
    collections::HashMap,
    fmt::{self, Debug, Formatter},
};
use symbol_table::SymbolTable;
use typecheck::{Context, TypeEngine, TypeError, TypeId, TypeInfo};

pub enum HirEngineError {
    NotMutable(Identifier),
    RecursionLimitReached,
    TypeError(Box<TypeError>, Box<TypeEngine>),
    UnknownImport(Path),
    UnknownIdentifier(Identifier),
}

impl Debug for HirEngineError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            HirEngineError::NotMutable(ident) => write!(f, "Local `{}` was not declared mutable", ident),
            HirEngineError::RecursionLimitReached => write!(f, "Reached recursion limit while evaluating expression"),
            HirEngineError::TypeError(e, engine) => write!(f, "{:?}", e.debug(engine)),
            HirEngineError::UnknownImport(path) => write!(f, "UnknownImport({})", path),
            HirEngineError::UnknownIdentifier(ident) => write!(f, "UnknownIdentifier({})", ident),
        }
    }
}

pub struct HirEngine {
    type_engine: TypeEngine,
    symbol_table: SymbolTable,
    current_path: Path,
    aliases: HashMap<Path, HashMap<Path, Path>>,
    functions: HashMap<Path, (hir::Function, TypeId, usize)>,
    values: Vec<expr::Expression>,
    do_typechecking: bool,
    expr_eval_count: usize,
    expr_eval_limit: usize,
    unnamable_count: usize,
}

impl Default for HirEngine {
    fn default() -> Self {
        Self {
            type_engine: Default::default(),
            symbol_table: Default::default(),
            current_path: Default::default(),
            aliases: Default::default(),
            functions: Default::default(),
            values: Default::default(),
            do_typechecking: true,
            expr_eval_count: 0,
            expr_eval_limit: 100,
            unnamable_count: 0,
        }
    }
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
        self.type_engine.typecheck_item(&self.mk_context(), item).map_err(|e| self.mk_type_error(e))?;

        match &item.kind {
            // already inserted in typechecker
            ItemKind::Struct(_) => Ok(()),
            ItemKind::Module(module) => {
                self.current_path = self.current_path.with_ident(module.name);

                //let aliases = self.aliases.entry(self.current_path.clone()).or_default();
                //ast_lowering::visitors::UseCollector::new(aliases).visit_module(&module);

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
            ItemKind::Function(f) => {
                let ctx = self.mk_context();
                let path = self.current_path.with_ident(f.name);
                let id = self.type_engine.typeid_from_path(&ctx, &path).unwrap();
                self.functions.insert(path, (f.clone(), id, self.unnamable_count));

                Ok(())
            }
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
        let expected = self.type_engine.from_hir_type(&ctx, &local.ty).map_err(|e| self.mk_type_error(e))?;
        let expr = self.evaluate_expression(&local.value, Some(expected))?;
        let expr = self.new_expr(expr);

        self.symbol_table.new_binding(symbol_table::Local::new(local.name, expr, expected, local.mutable));

        Ok(())
    }

    pub fn evaluate_expression(
        &mut self,
        expr: &Expression,
        expected_type: Option<TypeId>,
    ) -> Result<expr::Expression, HirEngineError> {
        let expected_type = expected_type.unwrap_or_else(|| self.type_engine.fresh_infer());
        let ctx = self.mk_context();

        self.expr_eval_count += 1;
        if self.expr_eval_count > self.expr_eval_limit {
            self.do_typechecking = true;
            return Err(HirEngineError::RecursionLimitReached);
        }

        if self.do_typechecking {
            self.type_engine.typecheck_expression(&ctx, expr, expected_type).map_err(|e| self.mk_type_error(e))?;
        }

        self.do_typechecking = false;

        let res = (|| {
            Ok(match &expr.kind {
                ExpressionKind::Block(block) => self.evaluate_block(block, Some(expected_type), None)?,
                ExpressionKind::BinaryOperation(lhs, op, rhs) => {
                    let lhs = self.evaluate_expression(lhs, None)?;
                    let rhs = self.evaluate_expression(rhs, None)?;

                    match op {
                        op if op.is_arith_op() => match (lhs, rhs) {
                            (expr::Expression::Integer(lhs), expr::Expression::Integer(rhs)) => match op {
                                BinOp::Add => expr::Expression::Integer(lhs + rhs),
                                BinOp::Subtract => expr::Expression::Integer(lhs - rhs),
                                BinOp::Multiply => expr::Expression::Integer(lhs * rhs),
                                BinOp::Divide => expr::Expression::Integer(lhs / rhs),
                                _ => unreachable!(),
                            },
                            _ => todo!("actual eval stuff"),
                        },
                        BinOp::Equal => expr::Expression::Bool(self.expressions_are_equal(&lhs, &rhs)),
                        _ => unreachable!(),
                    }
                }
                ExpressionKind::Boolean(b) => expr::Expression::Bool(*b),
                ExpressionKind::FnCall(lhs, args) => self.evaluate_fn_call(lhs, args, Some(expected_type))?,
                ExpressionKind::If(if_expr) => {
                    for if_expr in &if_expr.ifs {
                        let condition = self.evaluate_expression(&if_expr.condition, None)?;

                        match condition {
                            expr::Expression::Bool(true) => {
                                return self.evaluate_block(&if_expr.body, Some(expected_type), None);
                            }
                            expr::Expression::Bool(false) => continue,
                            _ => unreachable!(),
                        }
                    }

                    self.evaluate_block(&if_expr.r#else, Some(expected_type), None)?
                }
                ExpressionKind::Integer(i) => expr::Expression::Integer(*i),
                ExpressionKind::Path(path) => match path.is_identifier() {
                    Some(ident) => match self.symbol_table.resolve_binding(ident) {
                        Some(local) => self.values[local.value.0].clone(),
                        None => match self
                            .aliases
                            .get(&self.current_path)
                            .and_then(|map| {
                                let real_path = map.get(path)?;
                                Some((real_path, self.functions.get(real_path)?))
                            })
                            .or_else(|| Some((path, self.functions.get(path)?)))
                        {
                            Some((real_path, _)) => expr::Expression::Function(real_path.clone()),
                            None => return Err(HirEngineError::UnknownIdentifier(ident)),
                        },
                    },
                    _ => match self.functions.get(path) {
                        Some(_) => expr::Expression::Function(path.clone()),
                        _ => todo!("path stuff"),
                    },
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
                ExpressionKind::Unary(op, expr) => {
                    let expr = self.evaluate_expression(expr, None)?;

                    match (op, expr) {
                        (UnaryOp::Minus, expr::Expression::Integer(i)) => expr::Expression::Integer(-i),
                        (UnaryOp::Minus, expr::Expression::Bool(b)) => expr::Expression::Bool(!b),
                        _ => unreachable!(),
                    }
                }
            })
        })();

        self.do_typechecking = true;
        self.expr_eval_count -= 1;

        res
    }

    pub fn evaluate_fn_call(
        &mut self,
        callable: &Expression,
        args: &[Expression],
        expected_type: Option<TypeId>,
    ) -> Result<expr::Expression, HirEngineError> {
        match self.evaluate_expression(callable, None)? {
            expr::Expression::Function(path) => {
                self.do_typechecking = false;
                let (f, fn_id, unnamable) = self.functions.get(&path).unwrap().clone();
                let parameters = match self.type_engine.typeinfo(fn_id) {
                    TypeInfo::Function { parameters, .. } => parameters.clone(),
                    _ => unreachable!(),
                };
                let iter = parameters.iter().zip(args.iter());

                let mut new_symbols = SymbolTable::new();
                let old_aliases = self.aliases.clone();

                for (param, arg) in iter {
                    let expr = self.evaluate_expression(arg, Some(param.1));

                    if expr.is_err() {
                        self.do_typechecking = true;
                    }

                    let expr = self.new_expr(expr?);

                    new_symbols.new_binding(symbol_table::Local::new(param.0, expr, param.1, false));
                }

                let old_symtab = self.symbol_table.clone();
                let old_path = self.current_path.clone();
                self.symbol_table = new_symbols;

                self.current_path = self.current_path.with_ident(f.name);

                let res = self.evaluate_block(&f.body, expected_type, Some(unnamable));

                self.current_path.pop();
                self.aliases = old_aliases;
                self.symbol_table = old_symtab;
                self.do_typechecking = true;

                res
            }
            _ => unreachable!(),
        }
    }

    pub fn evaluate_block(
        &mut self,
        block: &Block,
        expected_type: Option<TypeId>,
        existing_unnamable: Option<usize>,
    ) -> Result<expr::Expression, HirEngineError> {
        let old_symtab = self.symbol_table.clone();
        let old_aliases = self.aliases.clone();

        self.symbol_table = SymbolTable::with_parent(&old_symtab);
        self.current_path = self.current_path.with_ident(Identifier::new(&match existing_unnamable {
            Some(n) => n.to_string(),
            None => {
                let n = self.unnamable_count.to_string();
                self.unnamable_count += 1;
                n
            }
        }));

        let res = (|| {
            for statement in &block.statements {
                self.evaluate_statement(statement)?;
            }

            self.evaluate_expression(&block.return_expr, expected_type)
        })();

        self.current_path.pop();
        self.symbol_table = old_symtab;
        self.aliases = old_aliases;

        res
    }

    fn expressions_are_equal(&self, lhs: &expr::Expression, rhs: &expr::Expression) -> bool {
        match (lhs, rhs) {
            (expr::Expression::Integer(lhs), expr::Expression::Integer(rhs)) => lhs == rhs,
            (expr::Expression::Bool(lhs), expr::Expression::Bool(rhs)) => lhs == rhs,
            (expr::Expression::Unit, expr::Expression::Unit) => true,
            (expr::Expression::Struct(_, members), expr::Expression::Struct(_, members2)) => {
                for (ident, expr) in members.iter() {
                    let expr1 = &self.expr_arena()[expr.0];
                    let expr2 = &self.expr_arena()[members2.get(ident).unwrap().0];
                    if !self.expressions_are_equal(expr1, expr2) {
                        return false;
                    }
                }

                true
            }
            (expr::Expression::Function(p), expr::Expression::Function(p2)) => p == p2,
            _ => unreachable!(),
        }
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
            bindings: self
                .symbol_table
                .bindings()
                .map(|local| (local.name, typecheck::BindingInfo { mutable: local.mutable, typeid: local.ty }))
                .collect(),
            parent: None,
        }
    }
}
