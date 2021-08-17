use crate::*;

pub trait Visitor: Sized {
    fn visit_module(&mut self, module: &mut Module) {
        walk::module(self, module);
    }

    fn visit_item(&mut self, item: &mut Item) {
        walk::item(self, item);
    }

    fn visit_function(&mut self, function: &mut Function) {
        walk::function(self, function);
    }

    fn visit_identifier(&mut self, _: &mut Identifier) {
        // Nothing else to do
    }

    fn visit_struct(&mut self, strukt: &mut Struct) {
        walk::structure(self, strukt);
    }

    fn visit_struct_member(&mut self, struct_member: &mut StructMember) {
        walk::struct_member(self, struct_member);
    }

    fn visit_path(&mut self, path: &mut Path) {
        walk::path(self, path);
    }

    fn visit_type(&mut self, ty: &mut Type) {
        walk::ty(self, ty);
    }

    fn visit_function_parameter(&mut self, function_parameter: &mut FunctionParameter) {
        walk::function_parameter(self, function_parameter)
    }

    fn visit_block(&mut self, block: &mut Block) {
        walk::block(self, block);
    }

    fn visit_node(&mut self, node: &mut AstNode) {
        walk::node(self, node)
    }

    fn visit_statement(&mut self, _: &mut Statement) {
        todo!()
    }

    fn visit_expression(&mut self, _: &mut Expression) {
        todo!()
    }

    fn visit_use(&mut self, _: &mut Use) {
        todo!()
    }

    fn visit_geode(&mut self, geode: &mut Geode) {
        walk::module(self, &mut geode.module)
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

    pub fn module<V: Visitor>(visitor: &mut V, module: &mut Module) {
        list!(visitor, visit_item, &mut module.items);
    }

    pub fn item<V: Visitor>(visitor: &mut V, item: &mut Item) {
        match item {
            Item::Function(f) => visitor.visit_function(f),
            Item::Module(m) => visitor.visit_module(m),
            Item::Struct(s) => visitor.visit_struct(s),
            Item::Use(u) => visitor.visit_use(u),
        }
    }

    pub fn node<V: Visitor>(visitor: &mut V, node: &mut AstNode) {
        match node {
            AstNode::Expression(e) => visitor.visit_expression(e),
            AstNode::Item(i) => visitor.visit_item(i),
            AstNode::Statement(s) => visitor.visit_statement(s),
        }
    }

    pub fn block<V: Visitor>(visitor: &mut V, block: &mut Block) {
        list!(visitor, visit_use, &mut block.uses);
        list!(visitor, visit_statement, &mut block.statements);

        if let Some(expr) = &mut block.return_expr {
            visitor.visit_expression(expr);
        }
    }

    pub fn structure<V: Visitor>(visitor: &mut V, strukt: &mut Struct) {
        list!(visitor, visit_struct_member, &mut strukt.members);
    }

    pub fn struct_member<V: Visitor>(visitor: &mut V, struct_member: &mut StructMember) {
        visitor.visit_identifier(&mut struct_member.name);
        visitor.visit_type(&mut struct_member.ty);
    }

    pub fn function_parameter<V: Visitor>(visitor: &mut V, function_parameter: &mut FunctionParameter) {
        visitor.visit_identifier(&mut function_parameter.name);
        visitor.visit_type(&mut function_parameter.ty);
    }

    pub fn path<V: Visitor>(visitor: &mut V, path: &mut Path) {
        list!(visitor, visit_identifier, &mut path.segments);
    }

    pub fn ty<V: Visitor>(visitor: &mut V, ty: &mut Type) {
        match &mut ty.kind {
            TypeKind::Integer | TypeKind::Bool => todo!("hmm"),
            TypeKind::Named(path) => visitor.visit_path(path),
        }
    }

    pub fn function<V: Visitor>(visitor: &mut V, function: &mut Function) {
        list!(visitor, visit_function_parameter, &mut function.parameters);
        if let Some(ty) = &mut function.return_ty {
            visitor.visit_type(ty);
        }
        visitor.visit_block(&mut function.body);
    }

    pub fn usage<V: Visitor>(visitor: &mut V, usage: &mut Use) {
        visitor.visit_path(&mut usage.path);
    }
}
