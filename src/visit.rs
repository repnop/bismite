use crate::{ast::*, token::*};

pub trait Visitor {
    fn visit_decls(&mut self, d: &[Decls]) {
        print!("(");
        for decl in d {
            match decl {
                Decls::Struct(s) => self.visit_struct(s),
                Decls::Fn(f) => self.visit_fn(f),
                Decls::Const(c) => self.visit_const(c),
            }
        }

        println!(")");
    }

    fn visit_ident(&mut self, t: &Ident);
    fn visit_struct(&mut self, s: &StructDecl);
    fn visit_fn(&mut self, f: &FnDecl);
    fn visit_field(&mut self, f: &FieldDecl);
    fn visit_statement(&mut self, s: &StatementDecl);
    fn visit_var(&mut self, v: &VarDecl);
    fn visit_expr(&mut self, e: &Expression);
    fn visit_const(&mut self, c: &ConstDecl);
    fn visit_ty(&mut self, ty: &Type);
    fn visit_literal(&mut self, lit: &Literal);
}

pub struct SExprVisitor;

impl Visitor for SExprVisitor {
    fn visit_ident(&mut self, t: &Ident) {
        print!("{}", t.to_string());
    }

    fn visit_struct(&mut self, s: &StructDecl) {
        print!("(struct ");
        self.visit_ident(&s.ident);
        print!(" ");

        for field in &s.fields {
            self.visit_field(field);
        }

        print!(")");
    }

    fn visit_fn(&mut self, f: &FnDecl) {
        print!("(fn ");
        self.visit_ident(&f.ident);

        for field in &f.arguments {
            self.visit_field(field);
        }

        for stmt in &f.statements {
            self.visit_statement(stmt);
        }

        print!(")");
    }

    fn visit_field(&mut self, f: &FieldDecl) {
        print!("(field ");
        self.visit_ident(&f.ident);
        print!(" ");
        //self.visit_ident(&f.field_type);
        print!(")");
    }

    fn visit_statement(&mut self, s: &StatementDecl) {
        print!("(stmt ");
        match s {
            StatementDecl::VarDecl(vd) => self.visit_var(vd),
        }
        print!(")");
    }

    fn visit_var(&mut self, v: &VarDecl) {
        print!("(var ");
        self.visit_ident(&v.ident);

        print!(" ");

        if let Some(ty) = &v.var_type {
            self.visit_ty(ty);
        }

        self.visit_expr(&v.value);
        print!(")");
    }

    fn visit_ty(&mut self, ty: &Type) {
        match ty.kind {
            TypeKind::Infer => print!("<inferred>"),
            // TypeKind::Named(s) => print!("{}", s),
            TypeKind::Literal(_) => {}
            _ => {}
        }
    }

    fn visit_expr(&mut self, e: &Expression) {
        use crate::ast::ExpressionKind::*;

        print!("(expr ");
        match &e.kind {
            Literal(i) => self.visit_literal(i),
            Unary(op, expr) => {
                print!("{} ", op.to_string());
                self.visit_expr(expr);
            }
            Binary(left, op, right) => {
                print!("{} ", op.to_string());
                self.visit_expr(left);
                self.visit_expr(right);
            }
            /*FnCall(i, args) => {
                self.visit_ident(i);

                for expr in args {
                    self.visit_expr(expr);
                }
            }*/
            _ => unimplemented!(),
        };
        print!(")");
    }

    fn visit_const(&mut self, c: &ConstDecl) {
        print!("(const ");
        self.visit_ident(&c.ident);
        self.visit_ty(&c.ty);
        print!(")");
    }

    fn visit_literal(&mut self, lit: &Literal) {
        match &lit.kind {
            LiteralKind::Int(i) => print!("{}", i),
            LiteralKind::Float(f) => print!("{}", f),
            LiteralKind::RawStr(s) => print!(
                "\"{}\"",
                crate::parser::GLOBAL_INTERNER
                    .read()
                    .unwrap()
                    .resolve(*s)
                    .unwrap()
            ),
            LiteralKind::Array(es) => {
                print!("[");

                for expr in &es[..es.len()] {
                    self.visit_expr(expr);
                    print!(", ");
                }

                if let Some(e) = es.last() {
                    self.visit_expr(e);
                }

                print!("]");
            }
            LiteralKind::Ident(i) => print!("{}", i.to_string()),
        }
    }
}
