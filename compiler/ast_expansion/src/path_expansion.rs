use std::collections::HashMap;

use ast::{visit, Identifier, Path, Visitor};

pub struct PathExpander {
    scopes: Vec<(HashMap<Identifier, Path>, bool)>,
}

impl PathExpander {
    pub fn expand(ast: &mut ast::Geode) {
        let mut this = Self { scopes: Vec::new() };
        this.visit_geode(ast);
    }

    fn latest(&mut self) -> &mut HashMap<Identifier, Path> {
        self.scopes.last_mut().map(|(m, _)| m).unwrap()
    }

    fn lookup(&self, ident: &Identifier) -> Option<Path> {
        let mut done = false;

        for (scope, module_boundary) in self.scopes.iter().rev() {
            match (done, module_boundary) {
                (true, true) => break,
                (false, true) => done = true,
                (_, _) => {}
            }

            if let Some(path) = scope.get(ident) {
                return Some(path.clone());
            }
        }

        None
    }
}

impl Visitor for PathExpander {
    fn visit_module(&mut self, module: &mut ast::Module) {
        self.scopes.push((HashMap::new(), true));
        visit::walk::module(self, module);
    }

    fn visit_path(&mut self, path: &mut Path) {
        if let Some(full) = self.lookup(path.segments.first().unwrap()) {
            path.segments = full.segments.clone().into_iter().chain(path.segments.drain(1..)).collect();
        }
    }

    fn visit_use(&mut self, u: &mut ast::Use) {
        self.latest().insert(u.path.segments.last().unwrap().clone(), u.path.clone());
    }

    fn visit_block(&mut self, block: &mut ast::Block) {
        self.scopes.push((HashMap::new(), false));
        visit::walk::block(self, block);
    }
}
