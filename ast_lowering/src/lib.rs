mod visitors;

use ast::Visitor;
use hir::{Block, Expression, ExpressionKind, Function, Identifier, Item, ItemKind, Module, Path};
use std::collections::{HashMap, HashSet};
use visitors::UseCollector;

#[derive(Debug, Default, Clone)]
pub struct LoweringContext {
    current_path: Path,
    scopes: Vec<Scope>,
    scope_storage: Vec<Vec<Scope>>,
}

impl LoweringContext {
    pub fn lower_module(&mut self, module: &ast::Module) -> Module {
        self.with_scope(Scope::default(), |this| {
            let mut hir_module = Module::new(Identifier::convert(&module.name), Vec::new(), module.span);

            let mut collector = UseCollector::new(this);
            collector.visit_module(module);

            for item in &module.items {
                if let ast::Item::Use(_) = item {
                    continue;
                }

                hir_module.items.push(this.lower_item(item));
            }

            hir_module
        })
    }

    pub fn lower_item(&mut self, item: &ast::Item) -> Item {
        match item {
            ast::Item::Function(f) => Item { kind: ItemKind::Function(self.lower_function(f)), span: f.span },
            ast::Item::Module(m) => self.with_no_parents(|this| {
                let items = m.items.iter().map(|item| this.lower_item(item)).collect();
                Item { kind: ItemKind::Module(Module::new(Identifier::convert(&m.name), items, m.span)), span: m.span }
            }),
            ast::Item::Struct(s) => todo!("lower struct"),
            ast::Item::Use(_) => panic!("attempting to lower a `use`"),
        }
    }

    pub fn lower_function(&mut self, function: &ast::Function) -> Function {
        Function {
            name: Identifier::convert(&function.name),
            parameters: vec![],
            body: self.lower_block(&function.body),
            return_type: hir::Type { kind: hir::TypeKind::Unit, span: function.span },
        }
    }

    pub fn lower_block(&mut self, block: &ast::Block) -> Block {
        let mut scope = Scope::default();

        for usage in &block.uses {
            let path = hir::Path::convert(&usage.path);
            scope.imports.insert(path.last(), path);
        }

        self.with_scope(scope, |this| {
            let statements = block.statements.iter().map(|s| this.lower_statement(s)).collect();
            let return_expr = block
                .return_expr
                .as_ref()
                .map(|e| this.lower_expression(e))
                .unwrap_or_else(|| Expression { kind: ExpressionKind::Unit, span: block.span });
            Block { statements, return_expr, span: block.span }
        })
    }

    pub fn lower_expression(&mut self, expression: &ast::Expression) -> Expression {}

    fn with_scope<T>(&mut self, scope: Scope, mut f: impl FnMut(&mut Self) -> T) -> T {
        self.scopes.push(scope);
        let val = f(self);
        self.scopes.pop();

        val
    }

    fn with_fresh_scope<T>(&mut self, mut f: impl FnMut(&mut Self) -> T) -> T {
        self.scopes.push(Scope::default());
        let val = f(self);
        self.scopes.pop();

        val
    }

    fn with_no_parents<T>(&mut self, mut f: impl FnMut(&mut Self) -> T) -> T {
        self.scopes.push(Scope::default());
        let val = f(self);
        self.scopes.pop();

        val
    }

    // Two step process to check for if an identifier is an import or binding:
    //
    // 1. Check latest scope to see if its a binding that would shadow an import
    // 2. Check latest scope imports
    // 3. If neither of the above, repeat for the parent scope if it exists
    fn get_ident_kind(&self, ident: Identifier) -> IdentKind {
        self.scopes.iter().rev().filter_map(|scope| scope.ident_kind(ident)).next().expect("ident doesn't exist rip")
    }
}

#[derive(Debug, Default, Clone)]
struct Scope {
    pub(crate) imports: HashMap<Identifier, Path>,
    pub(crate) identifiers: HashSet<Identifier>,
}

impl Scope {
    fn ident_kind(&self, ident: Identifier) -> Option<IdentKind> {
        if self.identifiers.get(&ident).is_some() {
            Some(IdentKind::Binding)
        } else if let Some(path) = self.imports.get(&ident) {
            Some(IdentKind::Import(path.clone()))
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
enum IdentKind {
    Binding,
    Import(Path),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let code = "use foo::Bar; fn main() { }";
        let ast = parser::Parser::new(code).geode().unwrap();
        let mut lowering_context = LoweringContext::default();
        dbg!(lowering_context.lower_module(&ast.module));
        panic!()
    }
}
