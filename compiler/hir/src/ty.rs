use crate::Path;
use codespan::Span;

#[derive(Clone, Debug)]
pub struct Type {
    pub kind: TypeKind,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeKind {
    Integer,
    Bool,
    Path(Path),
    Unit,
    Infer,
}
