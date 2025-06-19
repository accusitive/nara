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

    pub cache: (&'static str, Source),
    // pub dead: HashMap<Region, SimpleSpan>,
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
            self.generate_constraints(f).unwrap();
        }
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
    }
    pub fn check_translation_unit(
        &mut self,
        hir: &Hir<'hir, 'src>,
    ) -> Vec<Report<'_, (&str, Range<usize>)>> {
        hir.functions
            .values()
            .map(|f| self.check_expression(*f))
            .filter(|e| e.is_err())
            .map(|e| e.unwrap_err())
            .collect::<Vec<_>>()
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

                self.eff.insert(expression.id, effect);
            }
            hir::HirExpressionKind::FreeRegion(at) => {
                self.generate_constraints(&at)?;
                self.ty_constraints
                    .push((Type::Variable(expr_ty_id), Type::Unit));
                self.reg_constraints.push((expr_reg_id, Region::Global));

                let at_effect = self.eff[&at.id].clone();
                let at_region = self.reg[&at.id];

                let effect =
                    Effect::Sequence(Box::new(at_effect), Box::new(Effect::Free(at_region)));
                self.eff.insert(expression.id, effect);
            }
            hir::HirExpressionKind::Allocate(value, at) => {
                self.generate_constraints(at)?;

                // Hardcode value type to int for now. can deal with this later
                self.ty_constraints
                    .push((Type::Variable(expr_ty_id), Type::Int));

                let at_effect = self.eff[&at.id].clone();
                let at_region = self.reg[&at.id];

                let effect =
                    Effect::Sequence(Box::new(at_effect), Box::new(Effect::Alloc(at_region)));

                self.eff.insert(expression.id, effect);

                self.reg_constraints.push((expr_reg_id, at_region));
            }
            hir::HirExpressionKind::Local(hir_id) => {
                self.ty_constraints
                    .push((Type::Variable(expr_ty_id), self.ty[hir_id].clone()));
                self.reg_constraints.push((expr_reg_id, self.reg[hir_id]));
                self.eff.insert(expression.id, Effect::Bottom);
            }
            hir::HirExpressionKind::Block(hir_expressions) => {
                let mut effect = Effect::Bottom;
                let mut last = None;
                for expr in hir_expressions {
                    self.generate_constraints(&expr)?;
                    let sub_effect = self.eff[&expr.id].clone();
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
                self.eff.insert(expression.id, effect);
            }

            hir::HirExpressionKind::LetIn(init, next) => {
                self.generate_constraints(init)?;
                self.generate_constraints(next)?;

                let next_ty_id = self.ty[&next.id].clone();
                let next_reg_id = self.reg[&next.id];

                self.ty_constraints
                    .push((Type::Variable(expr_ty_id), next_ty_id));
                self.reg_constraints.push((expr_reg_id, next_reg_id));

                let init_effect = self.eff[&init.id].clone();
                let next_effect = self.eff[&next.id].clone();
                let effect = Effect::Sequence(Box::new(init_effect), Box::new(next_effect));

                self.eff.insert(expression.id, effect);
            }
            hir::HirExpressionKind::Dereference(hir_expression) => {
                self.generate_constraints(&hir_expression)?;

                // inner must be reference(e) where e is expr_ty_id
                self.ty_constraints.push((
                    self.ty[&hir_expression.id].clone(),
                    Type::Reference(Box::new(Type::Variable(expr_ty_id))),
                ));

                self.reg_constraints
                    .push((expr_reg_id, self.reg[&hir_expression.id]));

                self.eff
                    .insert(expression.id, self.eff[&hir_expression.id].clone());
            }
            hir::HirExpressionKind::Reference(hir_expression) => {
                self.generate_constraints(&hir_expression)?;

                self.ty_constraints.push((
                    Type::Variable(expr_ty_id),
                    Type::Reference(Box::new(self.ty[&hir_expression.id].clone())),
                ));
                self.reg_constraints
                    .push((expr_reg_id, self.reg[&hir_expression.id].clone()));

                self.eff
                    .insert(expression.id, self.eff[&hir_expression.id].clone());
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
    pub fn unify_ty(mut sub: HashMap<usize, Type>, lhs: &Type, rhs: &Type) -> HashMap<usize, Type> {
        let lhs = Self::apply_type(&sub, lhs);
        let rhs = Self::apply_type(&sub, rhs);

        match (lhs, rhs) {
            (Type::Variable(v), other) | (other, Type::Variable(v)) => {
                sub.insert(v.0, other);
            }
            (Type::Reference(l), Type::Reference(r)) => {
                sub = Self::unify_ty(sub, &l, &r);
            }
            // nop
            (Type::Int, Type::Int) | (Type::Unit, Type::Unit) => {}
            x => todo!("{:#?}", x),
        }
        sub
    }
    fn is_dead(&self, e: &Effect, r: &Region) -> bool {
        match e {
            Effect::Free(region) if region == r => true,
            Effect::Sequence(head, tail) => self.is_dead(&head, r) || self.is_dead(&tail, r),
            _ => false,
        }
    }
    fn check_effects<'a>(
        &self,
        freed: &mut HashSet<&'a Region>,
        e: &'a Effect,
    ) -> Result<(), &'static str> {
        match e {
            Effect::Bottom => Ok(()),
            Effect::Fresh(_) => Ok(()),
            Effect::Free(region) => {
                if !freed.insert(region) {
                    Err("double free")
                } else {
                    Ok(())
                }
            }
            Effect::Alloc(region) => {
                if freed.contains(region) {
                    Err("Allocate after free")
                } else {
                    Ok(())
                }
            }
            Effect::Sequence(head, tail) => {
                self.check_effects(freed, &head)?;
                self.check_effects(freed, &tail)?;

                Ok(())
            }
        }
    }
    // Recursively walk the expression tree and check for memory errors
    pub fn check_expression<'a>(
        &mut self,
        expr: &'hir HirExpression<'hir>,
    ) -> CheckResult<'a, ()> {
        {
            let mut freed_set = HashSet::new();
            self.check_effects(&mut freed_set, &self.eff[&expr.id])
                .unwrap();
        }

        match &expr.kind {
            hir::HirExpressionKind::NewRegion => {}
            hir::HirExpressionKind::FreeRegion(at) => {
                self.check_expression(&at)?;
            }
            hir::HirExpressionKind::Allocate(value, at) => {
                self.check_expression(&at)?;
            }
            hir::HirExpressionKind::Local(hir_id) => {}
            hir::HirExpressionKind::Block(block_items) => {
                for expr in block_items {
                    self.check_expression(&expr)?;
                }
            }
            hir::HirExpressionKind::Dereference(hir_expression) => {
                self.check_expression(&hir_expression)?;
                {
                    let mut freed_set = HashSet::new();
                    self.check_effects(&mut freed_set, &self.eff[&hir_expression.id])
                        .unwrap();
                    let r = &self.reg[&hir_expression.id];
                    if freed_set.contains(r) {
                        panic!();
                    }
                    dbg!(freed_set);
                    panic!();
                }

                // let region = self.reg[&hir_expression.id];
                // let effect = &self.eff[&hir_expression.id];
                // if self.is_dead(effect, &region) {
                //     panic!()
                // }
            }
            hir::HirExpressionKind::LetIn(init, next) => {
                self.check_expression(&init)?;
                self.check_expression(&next)?;
            }
            hir::HirExpressionKind::Reference(hir_expression) => {
                self.check_expression(&hir_expression)?;
            }
        }
        Ok(())
    }
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
