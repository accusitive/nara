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
    pub eff: HashMap<HirId, Effect>,

    pub ty_constraints: Vec<(Type, Type)>,
    pub reg_constraints: Vec<(Region, Region)>,
    pub live: Vec<(Region, Effect)>,

    pub cache: (&'static str, Source),
}
#[derive(Debug, Clone)]
pub enum Effect {
    Bottom,
    Fresh(Region),
    Free(Region),
    Alloc(Region),
    Sequence(Box<Effect>, Box<Effect>),
}
impl Effect {
    pub fn is_region_dead(&self, region: &Region) -> bool {
        match self {
            Effect::Free(r) if region == r => true,
            Effect::Sequence(head, tail) => {
                head.is_region_dead(region) || tail.is_region_dead(region)
            }
            _ => false,
        }
    }
}
#[derive(Debug, Clone)]
pub enum Type {
    Reference(Box<Type>),
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
            eff: HashMap::new(),

            ty_constraints: vec![],
            reg_constraints: vec![],
            live: vec![],

            cache: c,
        }
    }
    fn next_id(&mut self) -> usize {
        let id = self.next_id;
        self.next_id += 1;
        id
    }
    pub fn generate_all_constraints(&mut self, hir: &Hir<'hir, 'src>) {
        for f in hir.functions.values() {
            let base = Effect::Bottom;
            self.generate_constraints(&base, f).unwrap();
        }
    }
    pub fn unify_all_constraints(&mut self) {
        let region_sub = self.generate_region_substitutions();
        let ty_sub = self.generate_type_substitutions();

        for r in self.reg.values_mut() {
            *r = Self::apply_region(&region_sub, &r);
        }
        for e in self.eff.values_mut() {
            *e = Self::apply_effect(&region_sub, e);
        }
        for t in self.ty.values_mut() {
            *t = Self::apply_type(&ty_sub, t);
        }
        for (r, e) in self.live.iter_mut() {
            *r = Self::apply_region(&region_sub, &r);
            *e = Self::apply_effect(&region_sub, e);
        }

        self.check_liveness();
    }
    pub fn check_liveness(&mut self) {
        for (region, effect) in &self.live {
            dbg!(&region, &effect);
            if effect.is_region_dead(region) {
                
                panic!();
            }
        }
    }
    pub fn generate_constraints(
        &mut self,
        incoming: &Effect,
        expression: &HirExpression<'hir>,
    ) -> Result<Effect, ()> {
        let ty_var = TyId(self.next_id());
        let reg_var = Region::Variable(self.next_id());

        self.ty.insert(expression.id, Type::Variable(ty_var));
        self.reg.insert(expression.id, reg_var);

        let effect = match &expression.kind {
            hir::HirExpressionKind::NewRegion => {
                let total_effect = incoming.clone(); // just for consistency

                self.ty_constraints
                    .push((Type::Variable(ty_var), Type::Unit));

                let sub_effect = Effect::Sequence(
                    Box::new(Effect::Fresh(reg_var)),
                    Box::new(Effect::Alloc(reg_var)), // 1 byte allocation for the "handle"
                );

                self.eff.insert(expression.id, sub_effect.clone());
                Effect::Sequence(Box::new(total_effect), Box::new(sub_effect))
            }
            hir::HirExpressionKind::FreeRegion(at) => {
                let total_effect = self.generate_constraints(incoming, &at)?;

                self.ty_constraints
                    .push((Type::Variable(ty_var), Type::Unit));
                self.reg_constraints.push((reg_var, Region::Global));

                let at_effect = self.eff[&at.id].clone();
                let at_region = self.reg[&at.id];
                self.live.push((at_region, total_effect.clone()));

                let sub_effect =
                    Effect::Sequence(Box::new(at_effect), Box::new(Effect::Free(at_region)));

                self.eff.insert(expression.id, sub_effect.clone());
                Effect::Sequence(Box::new(total_effect), Box::new(sub_effect))
            }
            hir::HirExpressionKind::Allocate(value, at) => {
                let e = self.generate_constraints(incoming, at)?;

                // Hardcode value type to int for now. can deal with this later
                self.ty_constraints
                    .push((Type::Variable(ty_var), Type::Int));

                let at_effect = self.eff[&at.id].clone();
                let at_region = self.reg[&at.id];

                let sub_effect =
                    Effect::Sequence(Box::new(at_effect), Box::new(Effect::Alloc(at_region)));

                self.reg_constraints.push((reg_var, at_region));

                self.eff.insert(expression.id, sub_effect.clone());
                e
            }
            hir::HirExpressionKind::Local(hir_id) => {
                self.ty_constraints
                    .push((Type::Variable(ty_var), self.ty[hir_id].clone()));
                self.reg_constraints.push((reg_var, self.reg[hir_id]));
                self.eff.insert(expression.id, Effect::Bottom);

                incoming.clone()
            }
            hir::HirExpressionKind::Block(hir_expressions) => {
                let mut sub_effect = Effect::Bottom;
                let mut last = None;

                let mut e = incoming.clone();
                for expr in hir_expressions {
                    e = self.generate_constraints(&e, &expr)?;
                    let expr_sub_effect = self.eff[&expr.id].clone();
                    sub_effect = Effect::Sequence(Box::new(sub_effect), Box::new(expr_sub_effect));

                    last = Some(expr);
                }
                if let Some(last) = last {
                    self.ty_constraints
                        .push((Type::Variable(ty_var), self.ty[&last.id].clone()));
                    self.reg_constraints.push((reg_var, self.reg[&last.id]));
                } else {
                    self.ty_constraints
                        .push((Type::Variable(ty_var), Type::Unit));
                    self.reg_constraints.push((reg_var, Region::Global));
                }
                self.eff.insert(expression.id, sub_effect.clone());
                e
            }

            hir::HirExpressionKind::LetIn(init, next) => {
                let total_effect = self.generate_constraints(incoming, init)?;
                let total_effect = self.generate_constraints(&total_effect, next)?;

                let next_ty_id = self.ty[&next.id].clone();
                let next_reg_id = self.reg[&next.id];

                self.ty_constraints
                    .push((Type::Variable(ty_var), next_ty_id));
                self.reg_constraints.push((reg_var, next_reg_id));

                let init_effect = self.eff[&init.id].clone();
                let next_effect = self.eff[&next.id].clone();

                let sub_effect = Effect::Sequence(Box::new(init_effect), Box::new(next_effect));

                self.eff.insert(expression.id, sub_effect.clone());
                total_effect
            }
            hir::HirExpressionKind::Dereference(hir_expression) => {
                let total_effect = self.generate_constraints(incoming, &hir_expression)?;

                // inner must be reference(e) where e is expr_ty_id
                self.ty_constraints.push((
                    self.ty[&hir_expression.id].clone(),
                    Type::Reference(Box::new(Type::Variable(ty_var))),
                ));

                self.reg_constraints
                    .push((reg_var, self.reg[&hir_expression.id]));

                let sub_effect = self.eff[&hir_expression.id].clone();

                self.live.push((reg_var, total_effect.clone()));
                self.eff.insert(expression.id, sub_effect.clone());
                total_effect
            }
            hir::HirExpressionKind::Reference(hir_expression) => {
                let total_effect = self.generate_constraints(incoming, &hir_expression)?;

                self.ty_constraints.push((
                    Type::Variable(ty_var),
                    Type::Reference(Box::new(self.ty[&hir_expression.id].clone())),
                ));
                self.reg_constraints
                    .push((reg_var, self.reg[&hir_expression.id].clone()));

                let sub_effect = self.eff[&hir_expression.id].clone();
                self.eff.insert(expression.id, sub_effect.clone());

                total_effect
            }
        };

        Ok(effect)
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
            sub = self.unify_ty(sub, &lhs, &rhs);
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
            Type::Reference(inner_type) => {
                Type::Reference(Box::new(Self::apply_type(sub, inner_type)))
            }
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
            (Region::Variable(v), other) | (other, Region::Variable(v)) => {
                sub.insert(v, other);
            }
            _ => todo!(),
        }
        sub
    }
    pub fn unify_ty(
        &self,
        mut sub: HashMap<usize, Type>,
        lhs: &Type,
        rhs: &Type,
    ) -> HashMap<usize, Type> {
        let lhs = Self::apply_type(&sub, lhs);
        let rhs = Self::apply_type(&sub, rhs);

        match (lhs, rhs) {
            (Type::Variable(v), other) | (other, Type::Variable(v)) => {
                sub.insert(v.0, other);
            }
            (Type::Reference(l), Type::Reference(r)) => {
                sub = self.unify_ty(sub, &l, &r);
            }
            // nop
            (Type::Int, Type::Int) | (Type::Unit, Type::Unit) => {}
            x => todo!("{:#?}", x),
        }
        sub
    }
    // unused, but good to keep around; its annoying boilerplate
    // pub fn walk<'a>(&mut self, expr: &'hir HirExpression<'hir>) -> CheckResult<'a, ()> {

    //     match &expr.kind {
    //         hir::HirExpressionKind::NewRegion => {}
    //         hir::HirExpressionKind::FreeRegion(at) => {
    //             self.walk(&at)?;
    //         }
    //         hir::HirExpressionKind::Allocate(value, at) => {
    //             self.walk(&at)?;
    //         }
    //         hir::HirExpressionKind::Local(hir_id) => {}
    //         hir::HirExpressionKind::Block(block_items) => {
    //             for expr in block_items {
    //                 self.walk(&expr)?;
    //             }
    //         }
    //         hir::HirExpressionKind::Dereference(hir_expression) => {
    //             self.walk(&hir_expression)?;
    //         }
    //         hir::HirExpressionKind::LetIn(init, next) => {
    //             self.walk(&init)?;
    //             self.walk(&next)?;
    //         }
    //         hir::HirExpressionKind::Reference(hir_expression) => {
    //             self.walk(&hir_expression)?;
    //         }
    //     }
    //     Ok(())
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
