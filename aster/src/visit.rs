use crate::*;

pub trait Visitor: Sized {
    fn visit_module(&mut self, module: &Module) {
        walk::module(self, module);
    }

    fn visit_item(&mut self, item: &Item) {
        walk::item(self, item);
    }

    fn visit_function(&mut self, function: &Function) {}

    fn visit_identifier(&mut self, identifier: &Identifier) {
        // Nothing else to do
    }
}

pub mod walk {
    use super::*;

    #[doc(hidden)]
    #[macro_export]
    macro_rules! _list {
        ($v:ident, $v_method:ident, $list:expr) => {
            for item in $list {
                $v.$v_method(item);
            }
        };
    }

    pub use _list as list;

    pub fn module<V: Visitor>(visitor: &mut V, module: &Module) {
        visitor.visit_identifier(&module.name);
        list!(visitor, visit_item, &module.items);
    }

    pub fn item<V: Visitor>(visitor: &mut V, item: &Item) {
        match item {
            Item::Function(f) => visitor.visit_function(f),
            Item::Module(m) => visitor.visit_module(m),
            Item::Struct(s) => visitor.visit_struct(s),
        }
    }
}
