mod path_expansion;

use ast::{visit, Visitor};

pub fn expand(mut ast: ast::Geode) -> Result<ast::Geode, ()> {
    let ast = expand_macros(ast)?;
    expand_paths(ast)
}

pub fn expand_paths(mut ast: ast::Geode) -> Result<ast::Geode, ()> {}

// TODO: stuff when macros exist
pub fn expand_macros(mut ast: ast::Geode) -> Result<ast::Geode, ()> {
    Ok(ast)
}
