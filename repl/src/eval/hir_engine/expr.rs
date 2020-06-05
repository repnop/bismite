use hir::Identifier;
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub enum Expression {
    Integer(i128),
    Bool(bool),
    Struct(HashMap<Identifier, Expression>),
}
