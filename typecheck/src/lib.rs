use hir::{Expression, ExpressionKind, Identifier, Item, ItemKind, Path, Struct, StructExpr, Sym};
use std::{
    collections::HashMap,
    fmt::{self, Debug, Formatter},
};

pub type Result<T> = std::result::Result<T, TypeError>;
pub type TypeId = usize;

pub enum TypeError {
    UnknownType(Path),
    MismatchedTypes { wanted: TypeInfo, have: TypeInfo },
}

impl Debug for TypeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            TypeError::UnknownType(id) => write!(f, "Unknown type: {}", id.to_string()),
            TypeError::MismatchedTypes { wanted, have } => write!(
                f,
                "Type mismatch: expected {:?}, but found {:?}",
                wanted, have
            ),
        }
    }
}

pub struct TypeEngine {
    types: Vec<TypeInfo>,
    name_map: HashMap<Path, TypeId>,
}

impl TypeEngine {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn typeinfo(&self, id: TypeId) -> &TypeInfo {
        &self.types[id]
    }

    pub fn unify(&mut self, a: TypeId, b: TypeId) -> Result<TypeId> {
        match (self.types[a].clone(), self.types[b].clone()) {
            (TypeInfo::Bool, TypeInfo::Bool) => Ok(self.bool()),
            (TypeInfo::Integer, TypeInfo::Integer) => Ok(self.integer()),
            (
                TypeInfo::Struct {
                    full_path: full_path1,
                    members: members1,
                    ..
                },
                TypeInfo::Struct {
                    full_path: full_path2,
                    members: members2,
                    ..
                },
            ) => {
                // members should be forced to be equal by here if they're the same type
                if full_path1 != full_path2 {
                    return Err(TypeError::MismatchedTypes {
                        wanted: self.types[a].clone(),
                        have: self.types[b].clone(),
                    });
                }

                for (i, &a) in members1.iter() {
                    let b = members2[i];

                    self.unify(a, b)?;
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
            (TypeInfo::Ref(a), _) => self.unify(a, b),
            (_, TypeInfo::Ref(b)) => self.unify(a, b),
            (a, b) => Err(TypeError::MismatchedTypes { wanted: a, have: b }),
        }
    }

    pub fn fresh_infer(&mut self) -> TypeId {
        self.types.push(TypeInfo::Infer);
        self.types.len() - 1
    }

    pub fn typecheck_expression(&mut self, expr: &Expression, expected: TypeId) -> Result<TypeId> {
        match &expr.kind {
            ExpressionKind::Integer(_) => self.unify(self.integer(), expected),
            ExpressionKind::Boolean(_) => self.unify(self.bool(), expected),
            ExpressionKind::Struct(struct_expr) => {
                let have = self.gen_struct_typeinfo(&struct_expr)?;
                let want = match self.name_map.get(&struct_expr.name) {
                    Some(id) => *id,
                    None => return Err(TypeError::UnknownType(struct_expr.name.clone())),
                };

                self.unify(want, have)?;
                self.unify(want, expected)
            }
            ExpressionKind::Unit => self.unify(self.unit(), expected),
            ExpressionKind::Path(_) => todo!("path typecheck"),
        }
    }

    pub fn register_path_type(&mut self, path: &Path, type_id: TypeId) -> Result<()> {
        // TODO: collisions?
        self.name_map.insert(path.clone(), type_id);

        Ok(())
    }

    pub fn register_struct(&mut self, path: &Path, strukt: &Struct) -> Result<()> {
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
                            hir::TypeKind::Path(path) => match self.name_map.get(&path) {
                                Some(id) => *id,
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

    pub fn typeid_from_path(&self, path: &Path) -> Option<TypeId> {
        self.name_map.get(path).copied()
    }

    fn gen_struct_typeinfo(&mut self, se: &StructExpr) -> Result<TypeId> {
        let type_info = TypeInfo::Struct {
            full_path: se.name.clone(),
            members: se
                .members
                .iter()
                .map(|member| {
                    let id = self.fresh_infer();
                    Ok((
                        member.name,
                        self.typecheck_expression(&member.expression, id)?,
                    ))
                })
                .collect::<Result<_>>()?,
        };

        self.types.push(type_info);
        Ok(self.types.len() - 1)
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
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypeInfo {
    Bool,
    Integer,
    Struct {
        full_path: Path,
        members: HashMap<Identifier, TypeId>,
    },
    Infer,
    Ref(TypeId),
    Unit,
}

impl TypeInfo {
    pub fn debug<'a>(&'a self, engine: &'a TypeEngine) -> TypeInfoDebug<'a> {
        TypeInfoDebug { info: self, engine }
    }
}

pub struct TypeInfoDebug<'a> {
    info: &'a TypeInfo,
    engine: &'a TypeEngine,
}

impl Debug for TypeInfoDebug<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match &self.info {
            TypeInfo::Bool => write!(f, "Bool"),
            TypeInfo::Integer => write!(f, "Int"),
            TypeInfo::Struct {
                full_path, members, ..
            } => {
                writeln!(f, "{} {{", full_path)?;
                for (ident, &ty) in members {
                    writeln!(
                        f,
                        "    {}: {:?},",
                        ident.string(),
                        self.engine.typeinfo(ty).debug(self.engine)
                    )?;
                }
                write!(f, "}}")
            }
            TypeInfo::Infer => write!(f, "_"),
            TypeInfo::Unit => write!(f, "Unit"),
            TypeInfo::Ref(_) => write!(f, "<type reference>"),
        }
    }
}
