use std::collections::HashMap;

use crate::{
    arena::{Arena, Key},
    Function, FunctionParameter, Identifier, Item, ItemKind, Module, Path, Struct, StructMember, Type, TypeKind,
};
use string_interner::DefaultStringInterner;

struct UseStack {
    uses: Vec<HashMap<Identifier, Path>>,
}

impl UseStack {
    fn new_with(uses: impl Iterator<Item = Path>) -> Self {
        let mut this = Self { uses: Vec::with_capacity(4) };
        this.enter(uses);
        this
    }

    fn enter(&mut self, uses: impl Iterator<Item = Path>) {
        let mut map = HashMap::new();

        for r#use in uses {
            // TODO: check for dup ident
            map.insert(r#use.last(), r#use);
        }

        self.uses.push(map);
    }

    fn resolve(&self, ident: Identifier) -> Option<Path> {
        for use_map in self.uses.iter().rev() {
            if let Some(path) = use_map.get(&ident) {
                return Some(path.clone());
            }
        }

        None
    }

    fn exit(&mut self) {
        self.uses.pop();
    }
}

#[derive(Default)]
pub struct HirContext {
    interner: DefaultStringInterner,
    items: Arena<Item>,
    types: Arena<Type>,
    type_path_map: HashMap<Path, TypeId>,
    modules: Arena<Module>,
    functions: Arena<Function>,
    // TODO: probably need to expose more info to track down import errors
    use_stack: Vec<UseStack>,
    current_path: Path,
}

impl HirContext {
    pub fn intern(&mut self, identifier: ast::Identifier) -> Identifier {
        let sym = self.interner.get_or_intern(identifier.value);

        Identifier { name: sym, span: identifier.span }
    }

    pub fn lower_path(&mut self, path: ast::Path) -> Path {
        let mut path = Path { segments: path.segments.into_iter().map(|s| self.intern(s)).collect() };

        if let Some(stack) = self.use_stack.last() {
            if let Some(mut resolved) = stack.resolve(path.first()) {
                resolved.segments.extend(path.segments.into_iter().skip(1));
                path = Path { segments: resolved.segments };
            }
        }

        path
    }

    pub fn lower_module(&mut self, module: ast::Module) -> ModuleId {
        let (uses, items): (Vec<_>, Vec<_>) = module.items.into_iter().partition(|i| matches!(i, ast::Item::Use(_)));

        self.use_stack.push(UseStack::new_with(uses.into_iter().map(|u| match u {
            ast::Item::Use(u) => self.lower_path(u.path),
            _ => unreachable!(),
        })));

        let module = Module {
            name: self.intern(module.name),
            items: items.into_iter().map(|i| self.lower_item(i)).collect(),
            span: module.span,
        };

        ModuleId(self.modules.insert(module))
    }

    pub fn lower_item(&mut self, item: ast::Item) -> ItemId {
        let span = item.span();
        let item = Item {
            kind: match item {
                ast::Item::Function(f) => ItemKind::Function(self.lower_function(f)),
                ast::Item::Module(f) => ItemKind::Module(self.lower_module(f)),
                ast::Item::Struct(f) => ItemKind::Struct(self.lower_struct(f)),
                _ => unreachable!("uses should be filtered out by this point"),
            },
            span,
        };

        ItemId(self.items.insert(item))
    }

    pub fn lower_function(&mut self, func: ast::Function) -> FunctionId {
        let func = Function {
            name: self.intern(func.name),
            parameters: func.parameters.into_iter().map(|p| self.lower_fn_parameter(p)).collect(),
            return_type: func.return_ty.map(|ty| self.lower_type(ty)).unwrap_or(self.unit_typeid()),
            body: self.lower_block(func.body),
        };

        FunctionId(self.functions.insert(func))
    }

    pub fn lower_fn_parameter(&mut self, fn_param: ast::FunctionParameter) -> FunctionParameter {
        FunctionParameter { name: self.intern(fn_param.name), ty: self.lower_type(fn_param.ty), span: fn_param.span }
    }

    pub fn lower_struct(&mut self, strukt: ast::Struct) -> TypeId {
        let name = self.intern(strukt.name);
        let path = self.current_path.with_ident(name);
        let entry = self.type_path_map.get_mut(&path);

        let strukt_ty = Type {
            kind: TypeKind::Struct(Struct {
                name,
                members: strukt.members.into_iter().map(|m| self.lower_struct_member(m)).collect(),
                span: strukt.span,
            }),
            span: strukt.span,
        };

        match entry {
            Some(typeid) => match &mut self.types[typeid.0] {
                ty @ Type { kind: TypeKind::Infer, .. } => {
                    *ty = strukt_ty;
                    *typeid
                }
                _ => *typeid,
            },
            None => {
                let id = TypeId(self.types.insert(strukt_ty));
                self.type_path_map.insert(path, id);

                id
            }
        }
    }

    pub fn lower_struct_member(&mut self, fn_param: ast::StructMember) -> StructMember {
        FunctionParameter { name: self.intern(fn_param.name), ty: self.lower_type(fn_param.ty), span: fn_param.span }
    }

    pub fn lower_type(&mut self, ty: ast::Type) -> TypeId {
        match ty.kind {
            ast::TypeKind::Bool => self.bool_typeid(),
            ast::TypeKind::Integer => self.int_typeid(),
            ast::TypeKind::Named(path) => self.lower_type_path(path, ty.span),
        }
    }

    fn lower_type_path(&mut self, ty_path: ast::Path, span: codespan::Span) -> TypeId {
        // This can get swapped out later in the `self.types` to the actual type description
        *self.type_path_map.entry(self.lower_path(ty_path)).or_insert_with(|| self.fresh_infer(span))
    }

    fn unit_typeid(&self) -> TypeId {
        todo!()
    }

    fn bool_typeid(&self) -> TypeId {
        todo!()
    }

    fn int_typeid(&self) -> TypeId {
        todo!()
    }

    fn fresh_infer(&mut self, span: codespan::Span) -> TypeId {
        TypeId(self.types.insert(Type { kind: TypeKind::Infer, span }))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TypeId(Key<Type>);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ItemId(Key<Item>);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ModuleId(Key<Module>);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FunctionId(Key<Function>);
