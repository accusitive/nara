#![feature(if_let_guard)]
use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    ops::{Deref, Range},
};

use ariadne::{Label, Report, Source};
use hir::{Hir, HirExpression, HirId};
use parser::{
    SimpleSpan, Spanned,
    ast::{Expression, FunctionItem, Item, TranslationUnit},
};
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct TyId(usize);
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Region {
    Variable(usize),
    Global,
}
impl Display for Region {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Region::Variable(v) => write!(f, "'{}", v),
            Region::Global => write!(f, "'global"),
        }
    }
}
#[derive(Debug)]
pub struct Check {
    pub next_id: usize,
    pub ty: HashMap<HirId, Type>,
    pub reg: HashMap<HirId, Region>,
    pub effect: HashMap<HirId, Effect>,
    pub ty_constraints: Vec<(Type, Type)>,
    pub reg_constraints: Vec<(Region, Region)>,
    pub cache: (&'static str, Source),

    pub dead: HashMap<Region, SimpleSpan>,
}
#[derive(Debug, Clone)]
pub enum Effect {
    Bottom,
    Fresh(Region),
    Free(Region),
    Alloc(Region),
    Sequence(Box<Effect>, Box<Effect>),
}
#[derive(Debug, Clone)]
pub enum Type {
    Variable(TyId),
    Int,
    Unit,
}

impl<'hir, 'src> Check {
    pub fn new(c: (&'static str, Source)) -> Self {
        Self {
            next_id: 0,
            ty: HashMap::new(),
            reg: HashMap::new(),
            effect: HashMap::new(),

            ty_constraints: vec![],
            reg_constraints: vec![],
            cache: c,

            dead: HashMap::new(),
        }
    }
    fn next_id(&mut self) -> usize {
        let id = self.next_id;
        self.next_id += 1;
        id
    }
    pub fn check_translation_unit(&mut self, hir: &Hir<'hir, 'src>) {
        for f in hir.functions.values() {
            self.generate_constraints(f).unwrap();
        }
        let region_sub = self.generate_region_substitutions();
        let ty_sub = self.generate_type_substitutions();

        for r in self.reg.values_mut() {
            *r = Self::apply_region(&region_sub, &r);
        }
        for e in self.effect.values_mut() {
            *e = Self::apply_effect(&region_sub, e);
        }
        for t in self.ty.values_mut() {
            *t = Self::apply_type(&ty_sub, t);
        }
        for f in hir.functions.values() {
            match self.check_expression(f) {
                Ok(_) => {}
                Err(e) => {
                    e.eprint(self.cache.clone()).unwrap();
                    panic!()
                }
            }
        }
    }
    pub fn generate_constraints(&mut self, expression: &HirExpression<'hir>) -> Result<(), ()> {
        if self.ty.contains_key(&expression.id) {
            return Ok(());
        }
        let expr_ty_id = TyId(self.next_id());
        let expr_reg_id = Region::Variable(self.next_id());

        self.ty.insert(expression.id, Type::Variable(expr_ty_id));
        self.reg.insert(expression.id, expr_reg_id);

        match &expression.kind {
            hir::HirExpressionKind::NewRegion => {
                self.ty_constraints
                    .push((Type::Variable(expr_ty_id), Type::Unit));

                let effect = Effect::Sequence(
                    Box::new(Effect::Fresh(expr_reg_id)),
                    Box::new(Effect::Alloc(expr_reg_id)), // 1 byte allocation for the "handle"
                );

                self.effect.insert(expression.id, effect);
            }
            hir::HirExpressionKind::FreeRegion(at) => {
                self.generate_constraints(&at)?;
                self.ty_constraints
                    .push((Type::Variable(expr_ty_id), Type::Unit));
                self.reg_constraints.push((expr_reg_id, Region::Global));

                let at_effect = self.effect[&at.id].clone();
                let at_region = self.reg[&at.id];

                let effect =
                    Effect::Sequence(Box::new(at_effect), Box::new(Effect::Free(at_region)));
                self.effect.insert(expression.id, effect);
            }
            hir::HirExpressionKind::Allocate(value, at) => {
                self.generate_constraints(at)?;

                // Hardcode value type to int for now. can deal with this later
                self.ty_constraints
                    .push((Type::Variable(expr_ty_id), Type::Int));

                let at_effect = self.effect[&at.id].clone();
                let at_region = self.reg[&at.id];

                let effect =
                    Effect::Sequence(Box::new(at_effect), Box::new(Effect::Alloc(at_region)));

                self.effect.insert(expression.id, effect);

                self.reg_constraints.push((expr_reg_id, at_region));
            }
            hir::HirExpressionKind::Local(hir_id) => {
                self.ty_constraints
                    .push((Type::Variable(expr_ty_id), self.ty[hir_id].clone()));
                self.reg_constraints.push((expr_reg_id, self.reg[hir_id]));
                self.effect.insert(expression.id, Effect::Bottom);
            }
            hir::HirExpressionKind::Block(hir_expressions) => {
                let mut effect = Effect::Bottom;
                let mut last = None;
                for expr in hir_expressions {
                    self.generate_constraints(&expr)?;
                    let sub_effect = self.effect[&expr.id].clone();
                    effect = Effect::Sequence(Box::new(effect), Box::new(sub_effect));

                    last = Some(expr);
                }
                if let Some(last) = last {
                    self.ty_constraints
                        .push((Type::Variable(expr_ty_id), self.ty[&last.id].clone()));
                    self.reg_constraints.push((expr_reg_id, self.reg[&last.id]));
                } else {
                    self.ty_constraints
                        .push((Type::Variable(expr_ty_id), Type::Unit));
                    self.reg_constraints.push((expr_reg_id, Region::Global));
                }
                self.effect.insert(expression.id, effect);
            }
            hir::HirExpressionKind::Dereference(hir_expression) => todo!(),
            // the form `let x = y in z` has been abstracted, every "path" expression, (like y) has been resolved to HirExpressionkind::Local
            hir::HirExpressionKind::LetIn(init, next) => {
                self.generate_constraints(init)?;
                self.generate_constraints(next)?;

                let next_ty_id = self.ty[&next.id].clone();
                let next_reg_id = self.reg[&next.id];

                self.ty_constraints
                    .push((Type::Variable(expr_ty_id), next_ty_id));
                self.reg_constraints.push((expr_reg_id, next_reg_id));

                let init_effect = self.effect[&init.id].clone();
                let next_effect = self.effect[&next.id].clone();
                let effect = Effect::Sequence(Box::new(init_effect), Box::new(next_effect));

                self.effect.insert(expression.id, effect);
            }
        }
        Ok(())
    }
    pub fn generate_region_substitutions(&mut self) -> HashMap<usize, Region> {
        let mut sub = HashMap::new();
        for (lhs, rhs) in &self.reg_constraints {
            sub = Self::unify_region(sub, &lhs, &rhs);
        }
        dbg!(&sub);
        sub
    }
    pub fn generate_type_substitutions(&mut self) -> HashMap<usize, Type> {
        let mut sub = HashMap::new();
        for (lhs, rhs) in &self.ty_constraints {
            sub = Self::unify_ty(sub, &lhs, &rhs);
        }
        dbg!(&sub);
        sub
    }
    pub fn apply_region(sub: &HashMap<usize, Region>, region: &Region) -> Region {
        match region {
            Region::Variable(var) => match sub.get(var) {
                Some(r) => *r,
                None => Region::Variable(*var),
            },
            Region::Global => Region::Global,
        }
    }
    pub fn apply_type(sub: &HashMap<usize, Type>, r#type: &Type) -> Type {
        match r#type {
            Type::Variable(ty_id) if let Some(s) = sub.get(&ty_id.0) => s.clone(),
            _ => r#type.clone(),
        }
    }
    pub fn apply_effect(sub: &HashMap<usize, Region>, r#effect: &Effect) -> Effect {
        match r#effect {
            Effect::Bottom => Effect::Bottom,
            Effect::Fresh(region) => Effect::Fresh(Self::apply_region(sub, region)),
            Effect::Free(region) => Effect::Free(Self::apply_region(sub, region)),
            Effect::Alloc(region) => Effect::Alloc(Self::apply_region(sub, region)),
            Effect::Sequence(first, second) => Effect::Sequence(
                Box::new(Self::apply_effect(sub, first)),
                Box::new(Self::apply_effect(sub, second)),
            ),
        }
    }
    pub fn unify_region(
        mut sub: HashMap<usize, Region>,
        lhs: &Region,
        rhs: &Region,
    ) -> HashMap<usize, Region> {
        let lhs = Self::apply_region(&sub, lhs);
        let rhs = Self::apply_region(&sub, rhs);

        match (lhs, rhs) {
            // (R
            (Region::Variable(v), other) | (other, Region::Variable(v)) => {
                sub.insert(v, other);
            }
            _ => todo!(),
        }
        sub
    }
    pub fn unify_ty(mut sub: HashMap<usize, Type>, lhs: &Type, rhs: &Type) -> HashMap<usize, Type> {
        let lhs = Self::apply_type(&sub, lhs);
        let rhs = Self::apply_type(&sub, rhs);

        match (lhs, rhs) {
            (Type::Variable(v), other) | (other, Type::Variable(v)) => {
                sub.insert(v.0, other);
            }
            _ => todo!(),
        }
        sub
    }
    // pub fn is_expr_alive(&mut self, expression: &HirExpression<'hir>) -> bool {
    //     let reg = &self.reg[&expression.id];
    //     let eff = &self.effect[&expression.id];

    //     !self.is_region_free(reg, eff)
    // }
    // Check for memory errors
    pub fn check_expression<'a, 'b>(
        &'a mut self,
        expr: &'hir HirExpression<'hir>,
    ) -> CheckResult<'b, ()> {
        // if !self.is_expr_alive(expr) {
        //     return Err(Report::build(ariadne::ReportKind::Error, ("test.sw", 0..0))
        //         .with_label(Label::new(("test.sw", expr.span.into())).with_message("dead"))
        //         .finish());
        // }

        match &expr.kind {
            hir::HirExpressionKind::NewRegion => {
                // creating a new region is always safe
            }
            hir::HirExpressionKind::FreeRegion(at) => {
                self.check_expression(&at)?;
                let r = self.reg[&at.id];
                self.dead.insert(r, expr.span);
            }
            hir::HirExpressionKind::Allocate(spanned, at) => {
                self.check_expression(&at)?;
                if let Some(freed_at) = self.dead.get(&self.reg[&at.id]) {
                    return Err(Report::build(ariadne::ReportKind::Error, ("test.sw", 0..0))
                        .with_label(Label::new(("test.sw", at.span.into())).with_priority(1).with_message("trying to allocate at this expression which has region that has been freed"))
                        .with_label(Label::new(("test.sw", (*freed_at).into())).with_priority(0).with_message("Freed here"))
                        .finish());
                }
            }
            hir::HirExpressionKind::Local(hir_id) => {}
            hir::HirExpressionKind::Block(block_items) => {
                for expr in block_items {
                    self.check_expression(&expr)?;
                }
            }
            hir::HirExpressionKind::Dereference(hir_expression) => {}
            hir::HirExpressionKind::LetIn(init, next) => {
                self.check_expression(&init)?;
                self.check_expression(&next)?;
            }
        }
        Ok(())
    }
    // pub fn is_region_free(&self, region: &Region, effect: &Effect) -> bool {
    //     match effect {
    //         Effect::Bottom => false,
    //         Effect::Fresh(_) => false,
    //         Effect::Alloc(_) => false,
    //         Effect::Free(r) => r == region,
    //         Effect::Sequence(effect, effect1) => {
    //             self.is_region_free(region, effect) || self.is_region_free(region, effect1)
    //         }
    //     }
    // }
}
pub type CheckResult<'a, T> = Result<T, Report<'a, (&'static str, Range<usize>)>>;
impl Display for Effect {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Effect::Bottom => write!(f, "⊥"),

            Effect::Sequence(effect, effect1) => match (effect.deref(), effect1.deref()) {
                (Effect::Bottom, Effect::Bottom) => write!(f, ""),

                (Effect::Bottom, e @ _) | (e @ _, Effect::Bottom) => write!(f, "{}", e),

                (e0 @ _, e1 @ _) => write!(f, "{}, {}", e0, e1),
            },

            Effect::Alloc(r) => write!(f, "alloc {}", r),

            Effect::Fresh(r) => write!(f, "fresh {}", r),
            Effect::Free(r) => write!(f, "free {}", r),
        }
    }
}
