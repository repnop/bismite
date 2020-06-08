use hir::{
    BinOp, Block, Expression, ExpressionKind, Function, Identifier, Item, ItemKind, Path, Statement, StatementKind,
    Struct, StructExpr, Type, TypeKind, UnaryOp,
};
use std::{
    collections::HashMap,
    fmt::{self, Debug, Formatter},
};

pub type Result<T> = std::result::Result<T, TypeError>;
pub type TypeId = usize;

#[derive(Debug, Copy, Clone)]
pub struct BindingInfo {
    pub mutable: bool,
    pub typeid: TypeId,
}

#[derive(Default)]
pub struct Context<'a> {
    pub aliases: HashMap<Path, Path>,
    pub bindings: HashMap<Identifier, BindingInfo>,
    pub parent: Option<&'a Context<'a>>,
}

impl<'a> Context<'a> {
    pub fn new() -> Context<'static> {
        Context { aliases: HashMap::new(), bindings: HashMap::new(), parent: None }
    }

    pub fn new_child<'b: 'a>(&'b self) -> Context<'a> {
        Self { aliases: HashMap::new(), bindings: HashMap::new(), parent: Some(self) }
    }

    pub fn new_path_alias(&mut self, original: Path, alias: Path) {
        self.aliases.insert(original, alias);
    }

    pub fn new_binding(&mut self, ident: Identifier, binding: BindingInfo) {
        self.bindings.insert(ident, binding);
    }

    pub fn resolve_path_alias(&self, path: &Path) -> Option<&Path> {
        match self.aliases.get(path) {
            Some(path) => Some(path),
            None => match &self.parent {
                Some(parent) => parent.resolve_path_alias(path),
                None => None,
            },
        }
    }

    pub fn resolve_binding(&self, ident: Identifier) -> Option<BindingInfo> {
        match self.bindings.get(&ident) {
            Some(binding) => Some(*binding),
            None => match &self.parent {
                Some(parent) => parent.resolve_binding(ident),
                None => None,
            },
        }
    }
}

pub enum TypeError {
    CannotInferType,
    MismatchedTypes { wanted: TypeInfo, have: TypeInfo },
    NoField(TypeInfo, Identifier),
    NotCallable(TypeInfo),
    NotEnoughArgs,
    NotMutable(Identifier),
    NotValidRhs,
    TooManyArgs,
    UnknownBinOp { lhs: TypeInfo, op: BinOp, rhs: TypeInfo },
    UnknownIdentifier(Identifier),
    UnknownType(Path),
    UnknownUnaryOp { op: UnaryOp, info: TypeInfo },
}

impl TypeError {
    pub fn debug<'a>(&'a self, engine: &'a TypeEngine) -> TypeErrorDebug<'a> {
        TypeErrorDebug { error: self, engine }
    }
}

pub struct TypeErrorDebug<'a> {
    error: &'a TypeError,
    engine: &'a TypeEngine,
}

impl Debug for TypeErrorDebug<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match &self.error {
            TypeError::NoField(info, field) => write!(f, "No field `{}` on type {}", field, info.name(self.engine)),
            TypeError::UnknownType(id) => write!(f, "Unknown type: `{}`", id.to_string()),
            TypeError::MismatchedTypes { wanted, have } => write!(
                f,
                "Type mismatch: expected `{}`, but found `{}`",
                wanted.name(self.engine),
                have.name(self.engine)
            ),
            TypeError::UnknownBinOp { lhs, op, rhs } => {
                write!(f, "No implmentation for `{}` {} `{}`", lhs.name(self.engine), op, rhs.name(self.engine))
            }
            TypeError::UnknownIdentifier(ident) => write!(f, "(TypeError) Unknown identifier `{}`", ident),
            TypeError::NotValidRhs => write!(f, "Not a valid right hand side expression"),
            TypeError::NotCallable(info) => write!(f, "Type `{}` is not a function", info.name(self.engine)),
            TypeError::TooManyArgs => write!(f, "Too many arguments <todo: fn stuff>"),
            TypeError::NotEnoughArgs => write!(f, "Too few arguments <todo: fn stuff>"),
            TypeError::NotMutable(ident) => write!(f, "Local `{}` was not declared as mutable", ident),
            TypeError::UnknownUnaryOp { op, info } => {
                write!(f, "No implmentation for {}(`{}`)", op, info.name(self.engine))
            }
            TypeError::CannotInferType => write!(f, "Cannot infer type"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeEngine {
    types: Vec<TypeInfo>,
    name_map: HashMap<Path, TypeId>,
    current_path: Path,
    unnamable_count: usize,
}

impl TypeEngine {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn typeinfo(&self, id: TypeId) -> &TypeInfo {
        match &self.types[id] {
            TypeInfo::Ref(r) => self.typeinfo(*r),
            info => info,
        }
    }

    pub fn unify(&mut self, ctx: &Context<'_>, want: TypeId, have: TypeId) -> Result<TypeId> {
        match (self.types[want].clone(), self.types[have].clone()) {
            (TypeInfo::Bool, TypeInfo::Bool) => Ok(self.bool()),
            (TypeInfo::Integer, TypeInfo::Integer) => Ok(self.integer()),
            (TypeInfo::Unit, TypeInfo::Unit) => Ok(self.unit()),
            (
                TypeInfo::Struct { full_path: full_path1, members: members1, .. },
                TypeInfo::Struct { full_path: full_path2, members: members2, .. },
            ) => {
                let full_path1 = ctx.resolve_path_alias(&full_path1).unwrap_or(&full_path1);
                let full_path2 = ctx.resolve_path_alias(&full_path2).unwrap_or(&full_path2);

                if full_path1 != full_path2 {
                    return Err(TypeError::MismatchedTypes {
                        wanted: self.types[want].clone(),
                        have: self.types[have].clone(),
                    });
                }

                for (i, &a) in members1.iter() {
                    let b = members2[i];

                    self.unify(ctx, a, b)?;
                }

                Ok(want)
            }
            (
                TypeInfo::Function { parameters: parameters1, return_type: return_type1 },
                TypeInfo::Function { parameters: parameters2, return_type: return_type2 },
            ) => {
                match parameters1.len().cmp(&parameters2.len()) {
                    std::cmp::Ordering::Less => return Err(TypeError::TooManyArgs),
                    std::cmp::Ordering::Greater => return Err(TypeError::NotEnoughArgs),
                    _ => {}
                }

                self.unify(ctx, return_type1, return_type2)?;

                Ok(want)
            }
            (TypeInfo::Infer, TypeInfo::Infer) => Err(TypeError::CannotInferType),
            (TypeInfo::Infer, _) => {
                self.types[want] = TypeInfo::Ref(have);
                Ok(have)
            }
            (_, TypeInfo::Infer) => {
                self.types[have] = TypeInfo::Ref(want);
                Ok(want)
            }
            (TypeInfo::Ref(a), _) => self.unify(ctx, a, have),
            (_, TypeInfo::Ref(b)) => self.unify(ctx, want, b),
            (a, b) => Err(TypeError::MismatchedTypes { wanted: a, have: b }),
        }
    }

    pub fn fresh_infer(&mut self) -> TypeId {
        self.types.push(TypeInfo::Infer);
        self.types.len() - 1
    }

    pub fn typecheck_expression(&mut self, ctx: &Context<'_>, expr: &Expression, expected: TypeId) -> Result<TypeId> {
        match &expr.kind {
            ExpressionKind::Integer(_) => self.unify(ctx, expected, self.integer()),
            ExpressionKind::Boolean(_) => self.unify(ctx, expected, self.bool()),
            ExpressionKind::Block(block) => self.typecheck_block(ctx, block, expected),
            ExpressionKind::Unit => self.unify(ctx, expected, self.unit()),
            ExpressionKind::FnCall(lhs, args) => {
                let infer = self.fresh_infer();
                let fn_id = self.typecheck_expression(ctx, lhs, infer)?;

                match self.typeinfo(fn_id).clone() {
                    TypeInfo::Function { parameters, return_type } => {
                        match parameters.len().cmp(&args.len()) {
                            std::cmp::Ordering::Less => return Err(TypeError::TooManyArgs),
                            std::cmp::Ordering::Greater => return Err(TypeError::NotEnoughArgs),
                            _ => {}
                        }

                        for ((_, id), arg) in parameters.iter().zip(args.iter()) {
                            self.typecheck_expression(ctx, arg, *id)?;
                        }

                        Ok(return_type)
                    }
                    _ => Err(TypeError::NotCallable(self.typeinfo(fn_id).clone())),
                }
            }
            ExpressionKind::Struct(struct_expr) => {
                let have = self.gen_struct_typeinfo(ctx, struct_expr)?;
                let want = match self.resolve_two_way(ctx, &struct_expr.name) {
                    Some(id) => id,
                    None => return Err(TypeError::UnknownType(struct_expr.name.clone())),
                };

                self.unify(ctx, want, have)?;
                self.unify(ctx, want, expected)
            }
            ExpressionKind::Path(path) => match path.is_identifier() {
                Some(ident) => match ctx.resolve_binding(ident) {
                    Some(binding) => Ok(binding.typeid),
                    None => match self.typeid_from_path(ctx, path) {
                        Some(id) => match self.typeinfo(id) {
                            TypeInfo::Function { .. } => Ok(self.unify(ctx, expected, id)?),
                            info => Err(TypeError::NotCallable(info.clone())),
                        },
                        None => Err(TypeError::UnknownIdentifier(ident)),
                    },
                },
                None => self.name_map.get(&path).copied().ok_or_else(|| TypeError::UnknownType(path.clone())),
            },
            ExpressionKind::FieldAccess(lhs, ident) => {
                let infer = self.fresh_infer();
                let lhs_id = self.typecheck_expression(ctx, lhs, infer)?;
                let type_info = self.typeinfo(lhs_id);

                match type_info {
                    TypeInfo::Struct { members, .. } if members.get(ident).is_some() => {
                        Ok(members.get(ident).copied().unwrap())
                    }
                    _ => Err(TypeError::NoField(type_info.clone(), *ident)),
                }
            }
            ExpressionKind::Assignment(lhs, rhs) => {
                let lhs_id = match &lhs.kind {
                    ExpressionKind::FieldAccess(_, _) => {
                        let infer = self.fresh_infer();
                        self.typecheck_expression(ctx, lhs, infer)?
                    }
                    ExpressionKind::Path(path) => match path.is_identifier() {
                        Some(ident) => match ctx.resolve_binding(ident) {
                            Some(binding) if binding.mutable => binding.typeid,
                            Some(_) => return Err(TypeError::NotMutable(ident)),
                            None => return Err(TypeError::UnknownIdentifier(ident)),
                        },
                        None => return Err(TypeError::NotValidRhs),
                    },
                    _ => return Err(TypeError::NotValidRhs),
                };

                let infer = self.fresh_infer();
                let rhs_id = self.typecheck_expression(ctx, rhs, infer)?;

                self.unify(ctx, lhs_id, rhs_id)?;

                Ok(self.unit())
            }
            ExpressionKind::BinaryOperation(original_lhs, op, original_rhs) => {
                let op = *op;

                let infer = self.fresh_infer();
                let lhs_id = self.typecheck_expression(ctx, original_lhs, infer)?;

                let infer = self.fresh_infer();
                let rhs_id = self.typecheck_expression(ctx, original_rhs, infer)?;

                match (self.typeinfo(lhs_id), self.typeinfo(rhs_id)) {
                    (TypeInfo::Integer, TypeInfo::Integer) if op.is_arith_op() => {
                        Ok(self.unify(ctx, expected, self.integer())?)
                    }
                    (TypeInfo::Bool, TypeInfo::Bool) if op.is_logic_op() => {
                        Ok(self.unify(ctx, expected, self.bool())?)
                    }
                    (_, _) if op == BinOp::Equal => {
                        self.unify(ctx, lhs_id, rhs_id)?;
                        Ok(self.bool())
                    }
                    (_, _) => Err(TypeError::UnknownBinOp {
                        lhs: self.types[lhs_id].clone(),
                        op,
                        rhs: self.types[rhs_id].clone(),
                    }),
                }
            }
            ExpressionKind::If(if_expr) => {
                let expected = {
                    let infer = self.fresh_infer();
                    // FIXME: probably less confusing to not early return
                    // here and check each `if` and then `else` individually
                    // to report their types
                    let typeid = self.typecheck_block(ctx, &if_expr.r#else, infer)?;
                    self.unify(ctx, expected, typeid)?
                };

                for if_ in &if_expr.ifs {
                    self.typecheck_expression(ctx, &if_.condition, self.bool())?;
                    self.typecheck_block(ctx, &if_.body, expected)?;
                }

                Ok(expected)
            }
            ExpressionKind::Unary(op, expr) => {
                let expr = self.typecheck_expression(ctx, expr, expected)?;

                match self.typeinfo(expr) {
                    TypeInfo::Integer | TypeInfo::Bool => Ok(expr),
                    info => Err(TypeError::UnknownUnaryOp { op: *op, info: info.clone() }),
                }
            }
        }
    }

    pub fn register_path_type(&mut self, path: &Path, type_id: TypeId) -> Result<()> {
        // TODO: collisions?
        self.name_map.insert(path.clone(), type_id);

        Ok(())
    }

    pub fn typecheck_struct(&mut self, ctx: &Context<'_>, strukt: &Struct) -> Result<TypeId> {
        let struct_path = self.current_path.with_ident(strukt.name);

        let type_info = TypeInfo::Struct {
            members: {
                strukt
                    .members
                    .iter()
                    .map(|m| {
                        let id = match &m.ty.kind {
                            hir::TypeKind::Integer => self.integer(),
                            hir::TypeKind::Bool => self.bool(),
                            hir::TypeKind::Path(path) => match self.resolve_two_way(ctx, path) {
                                Some(id) => id,
                                None => return Err(TypeError::UnknownType(path.clone())),
                            },
                            _ => todo!("more type stuff"),
                        };

                        Ok((m.name, id))
                    })
                    .collect::<Result<_>>()?
            },
            full_path: struct_path.clone(),
        };

        self.name_map.insert(struct_path, self.types.len());
        self.types.push(type_info);

        Ok(self.types.len() - 1)
    }

    pub fn typecheck_function(&mut self, ctx: &Context<'_>, function: &Function) -> Result<TypeId> {
        let mut ctx = Context { aliases: ctx.aliases.clone(), bindings: HashMap::new(), parent: None };

        let mut parameters = Vec::new();

        for fp in &function.parameters {
            let parameter_id = self.from_hir_type(&ctx, &fp.ty)?;
            ctx.bindings.insert(fp.name, BindingInfo { mutable: false, typeid: parameter_id });
            parameters.push((fp.name, parameter_id));
        }

        let return_type = self.from_hir_type(&ctx, &function.return_type)?;

        let fn_id = self.types.len();
        self.name_map.insert(self.current_path.with_ident(function.name), fn_id);
        ctx.aliases.insert(Path::from_identifier(function.name), self.current_path.with_ident(function.name));

        let type_info = TypeInfo::Function { parameters, return_type };
        self.types.push(type_info);

        self.current_path = self.current_path.with_ident(function.name);
        self.typecheck_block(&ctx, &function.body, return_type)?;
        self.current_path.pop();

        Ok(fn_id)
    }

    pub fn typecheck_block(&mut self, ctx: &Context<'_>, block: &Block, expected: TypeId) -> Result<TypeId> {
        let mut child_ctx = ctx.new_child();

        let unnamable = Identifier::new(&self.unnamable_count.to_string());
        self.unnamable_count += 1;
        self.current_path = self.current_path.with_ident(unnamable);

        for item in &block.items {
            self.typecheck_item(ctx, item)?;

            match &item.kind {
                ItemKind::Struct(s) => {
                    child_ctx.aliases.insert(Path::from_identifier(s.name), self.current_path.with_ident(s.name));
                }
                ItemKind::Function(f) => {
                    child_ctx.aliases.insert(Path::from_identifier(f.name), self.current_path.with_ident(f.name));
                }
                ItemKind::Module(m) => {
                    child_ctx.aliases.insert(Path::from_identifier(m.name), self.current_path.with_ident(m.name));
                }
                _ => {}
            }
        }

        for statement in &block.statements {
            if let Some((name, id)) = self.typecheck_statement(&child_ctx, statement)? {
                child_ctx.bindings.insert(name, id);
            }
        }

        let res = self.typecheck_expression(&child_ctx, &block.return_expr, expected);
        self.current_path.pop();

        res
    }

    pub fn typecheck_statement(
        &mut self,
        ctx: &Context<'_>,
        statement: &Statement,
    ) -> Result<Option<(Identifier, BindingInfo)>> {
        match &statement.kind {
            StatementKind::Expression(e) => {
                let infer = self.fresh_infer();
                self.typecheck_expression(ctx, &e, infer)?;
                Ok(None)
            }
            StatementKind::Local(local) => {
                let typeid = self.from_hir_type(ctx, &local.ty)?;
                self.typecheck_expression(ctx, &local.value, typeid)?;

                Ok(Some((local.name, BindingInfo { mutable: local.mutable, typeid })))
            }
        }
    }

    pub fn typecheck_item(&mut self, ctx: &Context<'_>, item: &Item) -> Result<()> {
        match &item.kind {
            ItemKind::Module(module) => {
                self.current_path = self.current_path.with_ident(module.name);
                let ctx = ctx.new_child();
                for item in &module.items {
                    self.typecheck_item(&ctx, item)?;
                }
                self.current_path.pop();
            }
            ItemKind::Struct(strukt) => {
                self.typecheck_struct(ctx, strukt)?;
            }
            ItemKind::Function(f) => {
                self.typecheck_function(ctx, f)?;
            }
            ItemKind::Use(u) => {
                self.typeid_from_path(ctx, &u.path).ok_or_else(|| TypeError::UnknownType(u.path.clone()))?;
            }
        }

        Ok(())
    }

    pub fn typeid_from_path(&self, ctx: &Context<'_>, path: &Path) -> Option<TypeId> {
        self.name_map.get(ctx.resolve_path_alias(path).unwrap_or(path)).copied()
    }

    pub fn from_hir_type(&mut self, ctx: &Context<'_>, ty: &Type) -> Result<TypeId> {
        match &ty.kind {
            TypeKind::Integer => Ok(self.integer()),
            TypeKind::Bool => Ok(self.bool()),
            TypeKind::Path(path) => {
                self.typeid_from_path(ctx, path).ok_or_else(|| TypeError::UnknownType(path.clone()))
            }
            TypeKind::Infer => Ok(self.fresh_infer()),
            TypeKind::Unit => Ok(self.unit()),
        }
    }

    fn gen_struct_typeinfo(&mut self, ctx: &Context<'_>, se: &StructExpr) -> Result<TypeId> {
        let type_info = TypeInfo::Struct {
            full_path: se.name.clone(),
            members: se
                .members
                .iter()
                .map(|member| {
                    let id = self.fresh_infer();
                    Ok((member.name, self.typecheck_expression(ctx, &member.expression, id)?))
                })
                .collect::<Result<_>>()?,
        };

        self.types.push(type_info);
        Ok(self.types.len() - 1)
    }

    fn resolve_two_way(&self, ctx: &Context<'_>, path: &Path) -> Option<TypeId> {
        self.name_map.get(ctx.resolve_path_alias(path).unwrap_or(path)).copied()
    }

    fn integer(&self) -> TypeId {
        0
    }

    fn bool(&self) -> TypeId {
        1
    }

    fn unit(&self) -> TypeId {
        2
    }
}

impl Default for TypeEngine {
    fn default() -> Self {
        Self {
            types: vec![TypeInfo::Integer, TypeInfo::Bool, TypeInfo::Unit],
            name_map: HashMap::new(),
            current_path: Path::new(),
            unnamable_count: 0,
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypeInfo {
    Bool,
    Function { parameters: Vec<(Identifier, TypeId)>, return_type: TypeId },
    Infer,
    Integer,
    Ref(TypeId),
    Struct { full_path: Path, members: HashMap<Identifier, TypeId> },
    Unit,
}

impl TypeInfo {
    pub fn debug<'a>(&'a self, engine: &'a TypeEngine) -> TypeInfoDebug<'a> {
        TypeInfoDebug { info: self, engine, indent_level: 0 }
    }

    pub fn name(&self, engine: &TypeEngine) -> String {
        match self {
            TypeInfo::Bool => String::from("Bool"),
            TypeInfo::Integer => String::from("Int"),
            TypeInfo::Unit => String::from("Unit"),
            TypeInfo::Struct { full_path, .. } => full_path.to_string(),
            TypeInfo::Ref(r) => engine.typeinfo(*r).name(engine),
            TypeInfo::Function { .. } => format!("{:?}", self.debug(engine)),
            info => unreachable!("{:?}", info),
        }
    }
}

pub struct TypeInfoDebug<'a> {
    info: &'a TypeInfo,
    engine: &'a TypeEngine,
    indent_level: usize,
}

impl<'a> TypeInfoDebug<'a> {
    pub fn add_indent(mut self, level: usize) -> Self {
        self.indent_level += level;
        self
    }
}

impl Debug for TypeInfoDebug<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match &self.info {
            TypeInfo::Bool => write!(f, "Bool"),
            TypeInfo::Function { parameters, return_type } => {
                write!(f, "fn(")?;

                if let Some((ident, id)) = parameters.first() {
                    write!(f, "{}: {}", ident, self.engine.typeinfo(*id).name(self.engine))?;
                }

                for (ident, id) in parameters.iter().skip(1) {
                    write!(f, ", {}: {}", ident, self.engine.typeinfo(*id).name(self.engine))?;
                }

                write!(f, ") -> {}", self.engine.typeinfo(*return_type).name(self.engine))
            }
            TypeInfo::Integer => write!(f, "Int"),
            TypeInfo::Struct { full_path, members, .. } => {
                writeln!(f, "{} {{", full_path)?;
                for (ident, &ty) in members {
                    writeln!(
                        f,
                        "{:<width$}{}: {:?}, ",
                        "",
                        ident,
                        self.engine.typeinfo(ty).debug(self.engine).add_indent(1),
                        width = (self.indent_level + 1) * 4
                    )?;
                }
                write!(f, "{:<width$}}}", "", width = self.indent_level * 4)
            }
            TypeInfo::Infer => write!(f, "_"),
            TypeInfo::Unit => write!(f, "Unit"),
            TypeInfo::Ref(id) => write!(f, "{:?}", self.engine.typeinfo(*id).debug(self.engine)),
        }
    }
}
