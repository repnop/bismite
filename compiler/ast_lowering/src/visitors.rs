use super::IdentKind;
use ast::Visitor;
use hir::{Expression, ExpressionKind, Path};

pub struct UseCollector<'a> {
    ctx: &'a mut super::LoweringContext,
}

impl<'a> UseCollector<'a> {
    pub fn new(ctx: &'a mut super::LoweringContext) -> Self {
        Self { ctx }
    }
}

impl Visitor for UseCollector<'_> {
    fn visit_use(&mut self, usage: &mut ast::Use) {
        let path = hir::Path::convert(&usage.path);
        self.ctx.scopes.last_mut().expect("no import scope").imports.insert(path.last(), path);
    }

    fn visit_item(&mut self, item: &mut ast::Item) {
        if let ast::Item::Use(usage) = &mut item {
            self.visit_use(usage);
        }
    }
}

pub struct ExprPathReplacer {
    can_replace_idents: bool,
}

impl ExprPathReplacer {
    pub fn new() -> Self {
        Self { can_replace_idents: true }
    }

    pub fn replace(&mut self, ctx: &mut super::LoweringContext, expr: &mut Expression) {
        match &mut expr.kind {
            ExpressionKind::Unit | ExpressionKind::Integer(_) | ExpressionKind::Boolean(_) => {}
            ExpressionKind::Path(p) => match ctx.get_ident_kind(p.first()) {
                IdentKind::Binding => {}
                IdentKind::Import(mut full_path) => match p.is_identifier() {
                    Some(_) if self.can_replace_idents => *p = full_path,
                    None => {
                        for segment in &p.segments[1..] {
                            full_path.segments.push(*segment);
                        }
                        *p = full_path;
                    }
                    _ => {}
                },
            },
            ExpressionKind::BinaryOperation(lhs, _, rhs) => {
                self.replace(ctx, &mut *lhs);
                self.replace(ctx, &mut *rhs);
            }
            ExpressionKind::FieldAccess(lhs, _) => {
                self.replace(ctx, &mut *lhs);
            }
            ExpressionKind::If(ifs) => {}
            _ => todo!(),
        }
    }
}
