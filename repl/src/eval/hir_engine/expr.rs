use hir::{Identifier, Path};
use std::collections::HashMap;

#[derive(Clone)]
pub enum Expression {
    Integer(i128),
    Bool(bool),
    Struct(Path, HashMap<Identifier, Expression>),
    Unit,
}

impl Expression {
    pub fn debug(&self) -> ExpressionDebug<'_> {
        ExpressionDebug { expr: self, indent_level: 0 }
    }
}

pub struct ExpressionDebug<'a> {
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
                        value.debug().add_indent(1),
                        width = (self.indent_level + 1) * 4
                    )?;
                }
                write!(f, "{:<width$}}}", "", width = self.indent_level * 4)
            }
            Expression::Unit => write!(f, "Unit"),
        }
    }
}
