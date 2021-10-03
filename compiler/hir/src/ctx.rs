use std::collections::HashMap;

use crate::{
    arena::{Arena, Key},
    Block, Expression, ExpressionKind, Function, FunctionParameter, Identifier, If, IfExpr, Item, ItemKind, Local,
    Module, Path, Statement, StatementKind, Struct, StructExpr, StructExprMember, StructMember, Type, TypeKind,
};
use string_interner::DefaultStringInterner;

#[derive(Debug)]
struct UseStack {
    uses: Vec<HashMap<Identifier, Path>>,
}

impl UseStack {
    fn new_with(uses: impl Iterator<Item = Path>) -> Self {
        let mut this = Self { uses: Vec::with_capacity(4) };
        this.enter(uses);
        this
    }

    fn enter(&mut self, uses: impl Iterator<Item = Path>) {
        let mut map = HashMap::new();

        for r#use in uses {
            // TODO: check for dup ident
            map.insert(r#use.last(), r#use);
        }

        self.uses.push(map);
    }

    fn resolve(&self, ident: Identifier) -> Option<Path> {
        for use_map in self.uses.iter().rev() {
            if let Some(path) = use_map.get(&ident) {
                return Some(path.clone());
            }
        }

        None
    }

    fn exit(&mut self) {
        self.uses.pop();
    }
}

#[derive(Debug, Default)]
pub struct HirContext {
    pub interner: DefaultStringInterner,
    pub items: Arena<Item>,
    pub types: Arena<Type>,
    pub expressions: Arena<Expression>,
    pub statements: Arena<Statement>,
    // TODO: probably need to expose more info to track down import errors
    use_stack: Vec<UseStack>,
    current_path: Path,
}

impl HirContext {
    pub fn intern(&mut self, identifier: ast::Identifier) -> Identifier {
        let sym = self.interner.get_or_intern(identifier.value);

        Identifier { name: sym, span: identifier.span }
    }

    pub fn lower_path(&mut self, path: ast::Path) -> Path {
        let mut path = Path { segments: path.segments.into_iter().map(|s| self.intern(s)).collect() };

        if let Some(stack) = self.use_stack.last() {
            if let Some(mut resolved) = stack.resolve(path.first()) {
                resolved.segments.extend(path.segments.into_iter().skip(1));
                path = Path { segments: resolved.segments };
            }
        }

        path
    }

    pub fn lower_module(&mut self, module: ast::Module) -> ItemId {
        let span = module.span;
        let (uses, items): (Vec<_>, Vec<_>) = module.items.into_iter().partition(|i| matches!(i, ast::Item::Use(_)));

        let uses = UseStack::new_with(uses.into_iter().map(|u| match u {
            ast::Item::Use(u) => self.lower_path(u.path),
            _ => unreachable!(),
        }));
        self.use_stack.push(uses);

        let name = self.intern(module.name);
        self.current_path.push(name);

        let module = Module { name, items: items.into_iter().map(|i| self.lower_item(i)).collect(), span };

        self.current_path.pop();

        ItemId(self.items.insert(Item { kind: ItemKind::Module(module), span }))
    }

    pub fn lower_item(&mut self, item: ast::Item) -> ItemId {
        match item {
            ast::Item::Function(f) => self.lower_function(f),
            ast::Item::Module(f) => self.lower_module(f),
            ast::Item::Struct(f) => self.lower_struct(f),
            _ => unreachable!("uses should be filtered out by this point"),
        }
    }

    pub fn lower_function(&mut self, afunc: ast::Function) -> ItemId {
        // TODO: imports in function bodies

        let func = Function {
            name: self.intern(afunc.name),
            parameters: afunc.parameters.into_iter().map(|p| self.lower_fn_parameter(p)).collect(),
            return_type: afunc.return_ty.map(|ty| self.lower_type(ty)).unwrap_or_else(|| self.unit_typeid()),
            body: self.lower_block(afunc.body),
        };

        ItemId(self.items.insert(Item { kind: ItemKind::Function(func), span: afunc.span }))
    }

    pub fn lower_fn_parameter(&mut self, fn_param: ast::FunctionParameter) -> FunctionParameter {
        FunctionParameter { name: self.intern(fn_param.name), ty: self.lower_type(fn_param.ty), span: fn_param.span }
    }

    pub fn lower_statement(&mut self, statement: ast::Statement) -> StatementId {
        let stmt = Statement { kind: self.lower_statement_kind(statement.kind), span: statement.span };
        StatementId(self.statements.insert(stmt))
    }

    pub fn lower_statement_kind(&mut self, kind: ast::StatementKind) -> StatementKind {
        match kind {
            ast::StatementKind::Expression(e) => StatementKind::Expression(self.lower_expr(e)),
            ast::StatementKind::VariableBinding(vb) => StatementKind::Local(self.lower_local(vb)),
        }
    }

    pub fn lower_local(&mut self, local: ast::VariableBinding) -> Local {
        let span = local.span;
        Local {
            name: self.intern(local.name),
            value: self.lower_expr(local.value),
            mutable: local.mutable,
            // FIXME: right span?
            ty: local.ty.map(|ty| self.lower_type(ty)).unwrap_or_else(|| self.fresh_infer(span)),
            span,
        }
    }

    pub fn lower_expr(&mut self, expr: ast::Expression) -> ExpressionId {
        let expr = Expression { kind: self.lower_expr_kind(expr.kind), span: expr.span };
        ExpressionId(self.expressions.insert(expr))
    }

    pub fn lower_expr_kind(&mut self, expr_kind: ast::ExpressionKind) -> ExpressionKind {
        match expr_kind {
            ast::ExpressionKind::Assignment(lhs, rhs) => {
                ExpressionKind::Assignment(self.lower_expr(*lhs), self.lower_expr(*rhs))
            }
            ast::ExpressionKind::BinaryOperation(e1, op, e2) => {
                ExpressionKind::BinaryOperation(self.lower_expr(*e1), op, self.lower_expr(*e2))
            }
            ast::ExpressionKind::Block(b) => ExpressionKind::Block(self.lower_block(*b)),
            ast::ExpressionKind::Boolean(b) => ExpressionKind::Boolean(b),
            ast::ExpressionKind::FieldAccess(e, ident) => {
                ExpressionKind::FieldAccess(self.lower_expr(*e), self.intern(ident))
            }
            ast::ExpressionKind::Integer(i) => ExpressionKind::Integer(i),
            ast::ExpressionKind::Path(path) => ExpressionKind::Path(self.lower_path(path)),
            ast::ExpressionKind::Struct(s) => ExpressionKind::Struct(self.lower_struct_expr(*s)),
            ast::ExpressionKind::Unit => ExpressionKind::Unit,
            ast::ExpressionKind::FnCall(lhs, args) => {
                ExpressionKind::FnCall(self.lower_expr(*lhs), args.into_iter().map(|e| self.lower_expr(e)).collect())
            }
            ast::ExpressionKind::Unary(op, expr) => ExpressionKind::Unary(op, self.lower_expr(*expr)),
            ast::ExpressionKind::If(if_expr) => ExpressionKind::If(self.lower_if_expr(*if_expr)),
        }
    }

    pub fn lower_struct_expr(&mut self, struct_expr: ast::StructExpr) -> StructExpr {
        StructExpr {
            name: self.lower_path(struct_expr.name),
            members: struct_expr.members.into_iter().map(|m| self.lower_struct_expr_member(m)).collect(),
            span: struct_expr.span,
        }
    }

    pub fn lower_struct_expr_member(&mut self, expr_member: ast::StructExprMember) -> StructExprMember {
        StructExprMember {
            name: self.intern(expr_member.name),
            expression: self.lower_expr(expr_member.expression),
            span: expr_member.span,
        }
    }

    pub fn lower_if_expr(&mut self, if_expr: ast::IfExpr) -> IfExpr {
        IfExpr {
            ifs: if_expr.ifs.into_iter().map(|i| self.lower_if(i)).collect(),
            // TODO: better way to do this?
            r#else: if_expr.r#else.map(|b| self.lower_block(b)).unwrap_or_else(|| Block {
                statements: Vec::new(),
                return_expr: self.dummy_unit_expr(),
                span: codespan::Span::new(0, 0),
            }),
            span: if_expr.span,
        }
    }

    pub fn lower_if(&mut self, if_: ast::If) -> If {
        If { condition: self.lower_expr(if_.condition), body: self.lower_block(if_.body), span: if_.span }
    }

    pub fn lower_block(&mut self, block: ast::Block) -> Block {
        let expr_span = block.statements.last().map(|s| s.span).unwrap_or(block.span);
        Block {
            statements: block.statements.into_iter().map(|s| self.lower_statement(s)).collect(),
            return_expr: block.return_expr.map(|e| self.lower_expr(e)).unwrap_or_else(|| {
                ExpressionId(self.expressions.insert(Expression { kind: ExpressionKind::Unit, span: expr_span }))
            }),
            span: block.span,
        }
    }

    pub fn lower_struct(&mut self, strukt: ast::Struct) -> ItemId {
        let name = self.intern(strukt.name);
        let strukt = Item {
            kind: ItemKind::Struct(Struct {
                name,
                members: strukt.members.into_iter().map(|m| self.lower_struct_member(m)).collect(),
                span: strukt.span,
            }),
            span: strukt.span,
        };

        ItemId(self.items.insert(strukt))
    }

    pub fn lower_struct_member(&mut self, struct_member: ast::StructMember) -> StructMember {
        StructMember {
            name: self.intern(struct_member.name),
            ty: self.lower_type(struct_member.ty),
            span: struct_member.span,
        }
    }

    pub fn lower_type(&mut self, ty: ast::Type) -> TypeId {
        match ty.kind {
            ast::TypeKind::Bool => self.bool_typeid(),
            ast::TypeKind::Integer => self.int_typeid(),
            ast::TypeKind::Named(path) => self.lower_type_path(path, ty.span),
        }
    }

    fn lower_type_path(&mut self, ty_path: ast::Path, span: codespan::Span) -> TypeId {
        let ty = Type { kind: TypeKind::Path(self.lower_path(ty_path)), span };
        TypeId(self.types.insert(ty))
    }

    fn unit_typeid(&self) -> TypeId {
        todo!()
    }

    fn bool_typeid(&self) -> TypeId {
        todo!()
    }

    fn int_typeid(&self) -> TypeId {
        todo!()
    }

    fn dummy_unit_expr(&self) -> ExpressionId {
        todo!()
    }

    fn fresh_infer(&mut self, span: codespan::Span) -> TypeId {
        TypeId(self.types.insert(Type { kind: TypeKind::Infer, span }))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TypeId(Key<Type>);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ItemId(Key<Item>);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ModuleId(Key<Module>);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FunctionId(Key<Function>);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct StructId(Key<Struct>);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ExpressionId(Key<Expression>);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct StatementId(Key<Statement>);
