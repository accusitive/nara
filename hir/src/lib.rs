use std::{collections::HashMap, marker::PhantomData, ops::Deref};

use bumpalo::Bump;
use parser::{
    SimpleSpan, Spanned,
    ast::{Expression, FunctionItem, Value},
};
#[derive(Debug)]
pub struct HirExpression<'hir> {
    pub kind: HirExpressionKind<'hir>,
    pub span: SimpleSpan,
    pub id: HirId,
}

#[derive(Debug)]
pub enum HirExpressionKind<'hir> {
    NewRegion,
    FreeRegion(&'hir HirExpression<'hir>),
    Allocate(Spanned<Value>, &'hir HirExpression<'hir>),
    Local(HirId),
    Block(Vec<&'hir HirExpression<'hir>>),
    Dereference(&'hir HirExpression<'hir>),
    LetIn(&'hir HirExpression<'hir>, &'hir HirExpression<'hir>),
}
#[derive(Debug)]
pub struct Hir<'hir, 'src> {
    pub arena: &'hir Bump,
    pub next_id: usize,
    pub functions: HashMap<&'src str, &'hir HirExpression<'hir>>,
    pub expression: HashMap<HirId, &'hir HirExpression<'hir>>,
    pub scopes: Vec<Scope<'src>>,
}
#[derive(Debug)]
pub struct Scope<'src> {
    pub bindings: Vec<Binding<'src>>,
}
impl<'src> Scope<'src> {
    pub fn new() -> Self {
        Self { bindings: vec![] }
    }
}
#[derive(Debug)]
pub struct Binding<'src> {
    name: &'src str,
    expr: HirId,
}
impl<'hir, 'src> Hir<'hir, 'src> {
    pub fn lower_function(&mut self, function: &FunctionItem<'src>) {
        let body = self.lower_expression(&function.body);
        self.functions
            .insert(function.name.value, self.arena.alloc(body));
    }
    fn resolve_name(&self, name: &str) -> Option<HirId> {
        for scope in self.scopes.iter().rev() {
            for binding in &scope.bindings {
                if binding.name == name {
                    return Some(binding.expr);
                }
            }
        }
        None
    }
    pub fn lower_expression(
        &mut self,
        expression: &Spanned<Expression<'src>>,
    ) -> &'hir HirExpression<'hir> {
        let id = self.hir_id();

        let kind: HirExpressionKind = match &expression.value {
            Expression::Value(spanned) => todo!(),
            Expression::Block(spanneds) => HirExpressionKind::Block(
                spanneds.iter().map(|e| self.lower_expression(e)).collect(),
            ),
            Expression::Path(spanned) => {
                let id = self
                    .resolve_name(spanned.value)
                    .expect("could not resolve name");

                HirExpressionKind::Local(id)
            }
            Expression::LetIn {
                binding,
                init,
                next,
            } => {
                let lowered_init = self.lower_expression(init);
                self.scopes.push(Scope {
                    bindings: vec![Binding {
                        name: binding.value,
                        expr: lowered_init.id,
                    }],
                });
                let lowered_next = self.lower_expression(&next.deref());
                self.scopes.pop();

                HirExpressionKind::LetIn(lowered_init, lowered_next)
            }
            Expression::NewRegion => HirExpressionKind::NewRegion,
            Expression::FreeRegion(spanned) => {
                HirExpressionKind::FreeRegion(self.lower_expression(&spanned))
            }
            Expression::Allocate { value, region } => {
                HirExpressionKind::Allocate(value.deref().clone(), self.lower_expression(region))
            }
            Expression::Reference(spanned) => todo!(),
            Expression::Dereference(spanned) => {
                HirExpressionKind::Dereference(self.lower_expression(&spanned))
            }
        };
        let e = HirExpression {
            kind,
            span: expression.span,
            id,
        };
        let a = self.arena.alloc(e);
        self.expression.insert(id, a);

        a
    }
    fn hir_id(&mut self) -> HirId {
        let id = self.next_id;
        self.next_id += 1;

        HirId(id)
    }
}
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct HirId(usize);
