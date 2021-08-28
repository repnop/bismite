use crate::{Path, Struct};
use codespan::Span;
use std::fmt::{self, Display, Formatter};

#[derive(Clone, Debug)]
pub struct Type {
    pub kind: TypeKind,
    pub span: Span,
}

impl Type {
    pub fn convert(ty: &ast::Type) -> Self {
        Self { kind: TypeKind::convert(&ty.kind), span: ty.span }
    }

    pub fn convert_optional(ty: &Option<ast::Type>) -> Self {
        match ty {
            Some(ty) => Self::convert(ty),
            None => Type { kind: TypeKind::Infer, span: Span::new(0, 0) },
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TypeKind {
    Integer,
    Bool,
    Path(Path),
    Struct(Struct),
    Unit,
    Infer,
}

impl TypeKind {
    pub fn convert(ast: &ast::TypeKind) -> Self {
        match ast {
            ast::TypeKind::Bool => TypeKind::Bool,
            ast::TypeKind::Integer => TypeKind::Integer,
            ast::TypeKind::Named(path) => TypeKind::Path(Path::convert(path)),
        }
    }
}

impl Display for TypeKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            TypeKind::Integer => write!(f, "Int"),
            TypeKind::Bool => write!(f, "Bool"),
            TypeKind::Path(p) => write!(f, "{}", p),
            TypeKind::Unit => write!(f, "Unit"),
            TypeKind::Infer => write!(f, "_"),
        }
    }
}
