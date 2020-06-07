use hir::{Identifier, Path};
use std::collections::HashMap;

#[derive(Debug, Copy, Clone)]
pub struct ExpressionId(pub usize);

#[derive(Clone)]
pub enum Expression {
    Integer(i128),
    Bool(bool),
    Struct(Path, HashMap<Identifier, ExpressionId>),
    Function(Path),
    Unit,
}

impl Expression {
    pub fn is_unit(&self) -> bool {
        match self {
            Expression::Unit => true,
            _ => false,
        }
    }

    pub fn debug<'a>(&'a self, arena: &'a [Expression]) -> ExpressionDebug<'a> {
        ExpressionDebug { expr: self, arena, indent_level: 0 }
    }
}

pub struct ExpressionDebug<'a> {
    arena: &'a [Expression],
    expr: &'a Expression,
    indent_level: usize,
}

impl<'a> ExpressionDebug<'a> {
    pub fn add_indent(mut self, level: usize) -> Self {
        self.indent_level += level;
        self
    }
}

impl std::fmt::Debug for ExpressionDebug<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.expr {
            Expression::Integer(i) => write!(f, "{}", i),
            Expression::Bool(b) => write!(f, "{}", b),
            Expression::Struct(s, members) => {
                writeln!(f, "{} {{", s)?;
                for (ident, value) in members.iter() {
                    writeln!(
                        f,
                        "{:<width$}{}: {:?}, ",
                        "",
                        ident,
                        self.arena[value.0].debug(self.arena).add_indent(1),
                        width = (self.indent_level + 1) * 4
                    )?;
                }
                write!(f, "{:<width$}}}", "", width = self.indent_level * 4)
            }
            Expression::Unit => write!(f, "Unit"),
            Expression::Function(path) => write!(f, "{}", path),
        }
    }
}
