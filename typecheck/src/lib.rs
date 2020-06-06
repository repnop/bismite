use hir::{
    BinOp, Expression, ExpressionKind, Identifier, Item, ItemKind, Path, Struct, StructExpr, Sym, Type, TypeKind,
};
use std::{
    collections::HashMap,
    fmt::{self, Debug, Formatter},
};

pub type Result<T> = std::result::Result<T, TypeError>;
pub type TypeId = usize;

type Aliases = HashMap<Path, Path>;

pub struct Context {
    pub aliases: Aliases,
    pub bindings: HashMap<Identifier, TypeId>,
}

pub enum TypeError {
    MismatchedTypes { wanted: TypeInfo, have: TypeInfo },
    NoField(TypeInfo, Identifier),
    NotValidRhs,
    UnknownBinOp { lhs: TypeInfo, op: BinOp, rhs: TypeInfo },
    UnknownIdentifier(Identifier),
    UnknownType(Path),
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
            TypeError::UnknownType(id) => write!(f, "Unknown type: {}", id.to_string()),
            TypeError::MismatchedTypes { wanted, have } => write!(
                f,
                "Type mismatch: expected {:?}, but found {:?}",
                wanted.debug(self.engine),
                have.debug(self.engine)
            ),
            TypeError::UnknownBinOp { lhs, op, rhs } => {
                write!(f, "No implmentation for {} {} {}", lhs.name(self.engine), op, rhs.name(self.engine))
            }
            TypeError::UnknownIdentifier(ident) => write!(f, "UnknownIdentifier({})", ident),
            TypeError::NotValidRhs => write!(f, "Not a valid right hand side expression"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeEngine {
    types: Vec<TypeInfo>,
    name_map: HashMap<Path, TypeId>,
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

    pub fn unify(&mut self, ctx: &Context, a: TypeId, b: TypeId) -> Result<TypeId> {
        match (self.types[a].clone(), self.types[b].clone()) {
            (TypeInfo::Bool, TypeInfo::Bool) => Ok(self.bool()),
            (TypeInfo::Integer, TypeInfo::Integer) => Ok(self.integer()),
            (
                TypeInfo::Struct { full_path: full_path1, members: members1, .. },
                TypeInfo::Struct { full_path: full_path2, members: members2, .. },
            ) => {
                let full_path1 = ctx.aliases.get(&full_path1).unwrap_or(&full_path1);
                let full_path2 = ctx.aliases.get(&full_path2).unwrap_or(&full_path2);

                if full_path1 != full_path2 {
                    return Err(TypeError::MismatchedTypes {
                        wanted: self.types[a].clone(),
                        have: self.types[b].clone(),
                    });
                }

                for (i, &a) in members1.iter() {
                    let b = members2[i];

                    self.unify(ctx, a, b)?;
                }

                Ok(a)
            }
            (TypeInfo::Infer, _) => {
                self.types[a] = TypeInfo::Ref(b);
                Ok(b)
            }
            (_, TypeInfo::Infer) => {
                self.types[b] = TypeInfo::Ref(a);
                Ok(a)
            }
            (TypeInfo::Ref(a), _) => self.unify(ctx, a, b),
            (_, TypeInfo::Ref(b)) => self.unify(ctx, a, b),
            (a, b) => Err(TypeError::MismatchedTypes { wanted: a, have: b }),
        }
    }

    pub fn fresh_infer(&mut self) -> TypeId {
        self.types.push(TypeInfo::Infer);
        self.types.len() - 1
    }

    pub fn typecheck_expression(&mut self, ctx: &Context, expr: &Expression, expected: TypeId) -> Result<TypeId> {
        match &expr.kind {
            ExpressionKind::Integer(_) => self.unify(ctx, self.integer(), expected),
            ExpressionKind::Boolean(_) => self.unify(ctx, self.bool(), expected),
            ExpressionKind::Struct(struct_expr) => {
                let have = self.gen_struct_typeinfo(ctx, struct_expr)?;
                let want = match self.resolve_two_way(ctx, &struct_expr.name) {
                    Some(id) => id,
                    None => return Err(TypeError::UnknownType(struct_expr.name.clone())),
                };

                self.unify(ctx, want, have)?;
                self.unify(ctx, want, expected)
            }
            ExpressionKind::Unit => self.unify(ctx, self.unit(), expected),
            ExpressionKind::Path(path) => match path.is_identifier() {
                Some(ident) => match ctx.bindings.get(&ident) {
                    Some(id) => Ok(*id),
                    None => Err(TypeError::UnknownIdentifier(ident)),
                },
                None => todo!("non ident path check"),
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
                        Some(ident) => match ctx.bindings.get(&ident) {
                            Some(&id) => id,
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
                match (&original_lhs.kind, op, &original_rhs.kind) {
                    (ExpressionKind::Integer(_), op, ExpressionKind::Integer(_)) if op.is_arith_op() => {
                        Ok(self.integer())
                    }
                    (ExpressionKind::Boolean(_), op, ExpressionKind::Boolean(_)) if op.is_logic_op() => Ok(self.bool()),
                    (ExpressionKind::Path(path), &op, _) => match path.is_identifier() {
                        Some(ident) => match ctx.bindings.get(&ident) {
                            Some(&id) => {
                                let infer = self.fresh_infer();
                                let rhs_id = self.typecheck_expression(ctx, original_rhs, infer)?;
                                let lhs = self.typeinfo(id);

                                match (lhs, self.typeinfo(rhs_id)) {
                                    (TypeInfo::Integer, TypeInfo::Integer) if op.is_arith_op() => Ok(self.integer()),
                                    (TypeInfo::Bool, TypeInfo::Bool) if op.is_logic_op() => Ok(self.bool()),
                                    (lhs, rhs) => {
                                        Err(TypeError::UnknownBinOp { lhs: lhs.clone(), op, rhs: rhs.clone() })
                                    }
                                }
                            }
                            None => Err(TypeError::UnknownIdentifier(ident)),
                        },
                        None => todo!("other path checking"),
                    },
                    (ExpressionKind::BinaryOperation(_, _, _), &op, _) => {
                        let infer = self.fresh_infer();
                        let lhs_id = self.typecheck_expression(ctx, original_lhs, infer)?;

                        let infer = self.fresh_infer();
                        let rhs_id = self.typecheck_expression(ctx, original_rhs, infer)?;

                        match (self.typeinfo(lhs_id), self.typeinfo(rhs_id)) {
                            (TypeInfo::Integer, TypeInfo::Integer) if op.is_arith_op() => Ok(self.integer()),
                            (TypeInfo::Bool, TypeInfo::Bool) if op.is_logic_op() => Ok(self.bool()),
                            (lhs, rhs) => Err(TypeError::UnknownBinOp { lhs: lhs.clone(), op, rhs: rhs.clone() }),
                        }
                    }
                    (_, &op, ExpressionKind::BinaryOperation(_, _, _)) => {
                        let infer = self.fresh_infer();
                        let lhs_id = self.typecheck_expression(ctx, original_lhs, infer)?;

                        let infer = self.fresh_infer();
                        let rhs_id = self.typecheck_expression(ctx, original_rhs, infer)?;

                        match (self.typeinfo(lhs_id), self.typeinfo(rhs_id)) {
                            (TypeInfo::Integer, TypeInfo::Integer) if op.is_arith_op() => Ok(self.integer()),
                            (TypeInfo::Bool, TypeInfo::Bool) if op.is_logic_op() => Ok(self.bool()),
                            (lhs, rhs) => Err(TypeError::UnknownBinOp { lhs: lhs.clone(), op, rhs: rhs.clone() }),
                        }
                    }
                    (_, &op, _) => {
                        let infer = self.fresh_infer();
                        let lhs = self.typecheck_expression(ctx, original_lhs, infer)?;

                        let infer = self.fresh_infer();
                        let rhs = self.typecheck_expression(ctx, original_rhs, infer)?;
                        Err(TypeError::UnknownBinOp { lhs: self.types[lhs].clone(), op, rhs: self.types[rhs].clone() })
                    }
                }
            }
        }
    }

    pub fn register_path_type(&mut self, path: &Path, type_id: TypeId) -> Result<()> {
        // TODO: collisions?
        self.name_map.insert(path.clone(), type_id);

        Ok(())
    }

    pub fn register_struct(&mut self, ctx: &Context, path: &Path, strukt: &Struct) -> Result<()> {
        let struct_path = path.with_ident(strukt.name);

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

        Ok(())
    }

    pub fn typeid_from_path(&self, ctx: &Context, path: &Path) -> Option<TypeId> {
        self.name_map.get(ctx.aliases.get(path).unwrap_or(path)).copied()
    }

    pub fn from_hir_type(&mut self, ctx: &Context, ty: &Type) -> Result<TypeId> {
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

    fn gen_struct_typeinfo(&mut self, ctx: &Context, se: &StructExpr) -> Result<TypeId> {
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

    fn resolve_two_way(&self, ctx: &Context, path: &Path) -> Option<TypeId> {
        self.name_map.get(ctx.aliases.get(path).unwrap_or(path)).copied()
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
        Self { types: vec![TypeInfo::Integer, TypeInfo::Bool, TypeInfo::Unit], name_map: HashMap::new() }
    }
}

#[derive(Debug, Clone)]
pub enum TypeInfo {
    Bool,
    Integer,
    Struct { full_path: Path, members: HashMap<Identifier, TypeId> },
    Infer,
    Ref(TypeId),
    Unit,
}

impl TypeInfo {
    pub fn debug<'a>(&'a self, engine: &'a TypeEngine) -> TypeInfoDebug<'a> {
        TypeInfoDebug { info: self, engine, indent_level: 0 }
    }

    pub fn name(&self, engine: &TypeEngine) -> String {
        match self {
            TypeInfo::Bool => String::from("Bool"),
            TypeInfo::Integer => String::from("Integer"),
            TypeInfo::Unit => String::from("Unit"),
            TypeInfo::Struct { full_path, .. } => full_path.to_string(),
            TypeInfo::Ref(r) => engine.typeinfo(*r).name(engine),
            _ => unreachable!(),
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
