use crate::{ast::*, token::*};

pub trait Visitor {
    fn visit_decls(&mut self, d: &Decls) {
        print!("(");
        for s in &d.structs {
            self.visit_struct(s);
        }

        for f in &d.fns {
            self.visit_fn(f);
        }
        println!(")");
    }

    fn visit_name(&mut self, t: &Token);
    fn visit_struct(&mut self, s: &StructDecl);
    fn visit_fn(&mut self, f: &FnDecl);
    fn visit_field(&mut self, f: &FieldDecl);
    fn visit_statement(&mut self, s: &StatementDecl);
    fn visit_var(&mut self, v: &VarDecl);
    fn visit_expr(&mut self, e: &Expression);
}

pub struct SExprVisitor;

impl Visitor for SExprVisitor {
    fn visit_name(&mut self, t: &Token) {
        print!("{}", t.lit);
    }

    fn visit_struct(&mut self, s: &StructDecl) {
        print!("(struct {} ", s.ident.lit);

        for field in &s.fields {
            self.visit_field(field);
        }

        print!(")");
    }

    fn visit_fn(&mut self, f: &FnDecl) {
        print!("(fn ");
        self.visit_name(&f.ident);

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
        self.visit_name(&f.ident);
        print!(" ");
        self.visit_name(&f.field_type);
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
        self.visit_name(&v.ident);

        print!(" ");

        match &v.var_type {
            Some(ty) => match ty.kind {
                TypeKind::Infer => print!("<inferred>"),
                TypeKind::Named(s) => print!("{}", s),
                TypeKind::Literal(_) => {}
            },
            None => {}
        }

        self.visit_expr(&v.value);
        print!(")");
    }

    fn visit_expr(&mut self, e: &Expression) {
        use crate::ast::ExpressionKind::*;

        print!("(expr ");
        match &e.kind {
            Literal(i) => self.visit_name(&i.token),
            Identifier(i) => self.visit_name(i),
            Unary(op, expr) => {
                self.visit_name(op);
                self.visit_expr(expr);
            }
            Binary(left, op, right) => {
                self.visit_name(op);
                self.visit_expr(left);
                self.visit_expr(right);
            }
            FnCall(i, args) => {
                self.visit_name(i);

                for expr in args {
                    self.visit_expr(expr);
                }
            }
        };
        print!(")");
    }
}
