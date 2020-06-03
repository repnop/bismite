use crate::*;

pub trait Visitor: Sized {
    fn visit_module(&mut self, module: &Module) {
        walk::module(self, module);
    }

    fn visit_item(&mut self, item: &Item) {
        walk::item(self, item);
    }

    fn visit_function(&mut self, function: &Function) {
        walk::function(self, function);
    }

    fn visit_identifier(&mut self, _: &Identifier) {
        // Nothing else to do
    }

    fn visit_struct(&mut self, strukt: &Struct) {
        walk::structure(self, strukt);
    }

    fn visit_struct_member(&mut self, struct_member: &StructMember) {
        walk::struct_member(self, struct_member);
    }

    fn visit_path(&mut self, path: &Path) {
        walk::path(self, path);
    }

    fn visit_type(&mut self, ty: &Type) {
        walk::ty(self, ty);
    }

    fn visit_function_parameter(&mut self, function_param: &FunctionParameter) {
        todo!()
    }

    fn visit_block(&mut self, block: &Block) {
        todo!()
    }

    fn visit_node(&mut self, node: &AstNode) {
        walk::node(self, node)
    }

    fn visit_statement(&mut self, statement: &Statement) {
        todo!()
    }

    fn visit_expression(&mut self, expression: &Expression) {
        todo!()
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
        list!(visitor, visit_item, &module.items);
    }

    pub fn item<V: Visitor>(visitor: &mut V, item: &Item) {
        match item {
            Item::Function(f) => visitor.visit_function(f),
            Item::Module(m) => visitor.visit_module(m),
            Item::Struct(s) => visitor.visit_struct(s),
        }
    }

    pub fn node<V: Visitor>(visitor: &mut V, node: &AstNode) {
        match node {
            AstNode::Expression(e) => visitor.visit_expression(e),
            AstNode::Item(i) => visitor.visit_item(i),
            AstNode::Statement(s) => visitor.visit_statement(s),
        }
    }

    pub fn structure<V: Visitor>(visitor: &mut V, strukt: &Struct) {
        list!(visitor, visit_struct_member, &strukt.members);
    }

    pub fn struct_member<V: Visitor>(visitor: &mut V, struct_member: &StructMember) {
        visitor.visit_identifier(&struct_member.name);
        visitor.visit_type(&struct_member.ty);
    }

    pub fn path<V: Visitor>(visitor: &mut V, path: &Path) {
        list!(visitor, visit_identifier, &path.segments);
    }

    pub fn ty<V: Visitor>(visitor: &mut V, ty: &Type) {
        match &ty.kind {
            TypeKind::Integer | TypeKind::Bool => todo!("hmm"),
            TypeKind::Named(path) => visitor.visit_path(path),
        }
    }

    pub fn function<V: Visitor>(visitor: &mut V, function: &Function) {
        list!(visitor, visit_function_parameter, &function.parameters);
        if let Some(ty) = &function.return_ty {
            visitor.visit_type(ty);
        }
        visitor.visit_block(&function.body);
    }
}
