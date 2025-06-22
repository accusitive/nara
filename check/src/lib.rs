#![feature(if_let_guard)]
use std::{
    collections::HashMap,
    fmt::Display,
    ops::{Deref, Range},
};

use ariadne::{Config, Label, Report, Source};
use hir::{Hir, HirExpression, HirId};
use parser::SimpleSpan;
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
pub struct Check<'a> {
    pub next_id: usize,

    pub ty: HashMap<HirId, Type>,
    pub place_map: HashMap<HirId, Region>,
    pub effect_map: HashMap<HirId, Effect>,

    pub ty_constraints: Vec<(Type, Type)>,
    pub reg_constraints: Vec<(Region, Region)>,
    pub live_constraints: Vec<LivenessConstraint>,

    pub cache: (&'static str, Source),
    pub config: Config,

    pub diagnostics: Vec<Report<'a, (&'a str, Range<usize>)>>,
}
#[derive(Debug)]
pub struct LivenessConstraint {
    effect: Effect,
    region: Region,
    span: SimpleSpan,
    kind: LivenessConstraintKind,
}
#[derive(Debug)]
pub enum LivenessConstraintKind {
    TakeRef,
    UseRef,
    Alloc,
    Free,
    Dangling,
}
#[derive(Debug, Clone)]
pub enum Effect {
    Bottom,
    Fresh(Region),
    Free(Region, SimpleSpan),
    Alloc(Region),
    Sequence(Box<Effect>, Box<Effect>),
}
#[derive(Debug)]
pub enum LivenessResult<'a> {
    Live,
    Dead(&'a SimpleSpan),
}
impl Effect {
    pub fn is_region_dead(&self, region: &Region) -> LivenessResult {
        match self {
            Effect::Free(r, s) if region == r => LivenessResult::Dead(s),
            Effect::Sequence(head, tail) => match head.is_region_dead(region) {
                LivenessResult::Live => tail.is_region_dead(region),
                d @ LivenessResult::Dead(..) => d,
            },
            _ => LivenessResult::Live,
        }
    }
}
#[derive(Debug, Clone)]
pub enum Type {
    Reference(Box<Type>),
    InferenceVariable(TyId),
    Int,
    Unit,
}

impl<'hir, 'src> Check<'_> {
    pub fn new(cache: (&'static str, Source), config: Config) -> Self {
        Self {
            next_id: 0,
            ty: HashMap::new(),
            place_map: HashMap::new(),
            effect_map: HashMap::new(),

            ty_constraints: vec![],
            reg_constraints: vec![],
            live_constraints: vec![],

            cache,
            config,
            diagnostics: vec![],
        }
    }
    fn next_id(&mut self) -> usize {
        let id = self.next_id;
        self.next_id += 1;
        id
    }
    pub fn generate_all_constraints(&mut self, hir: &Hir<'hir, 'src>) {
        for expr in hir.functions.values() {
            self.generate_constraints(&Effect::Bottom, expr).unwrap();

            self.live_constraints.push(LivenessConstraint {
                effect: self.effect_map[&expr.id].clone(),
                region: self.place_map[&expr.id],
                span: expr.span,
                kind: LivenessConstraintKind::Dangling,
            });
        }
    }
    pub fn unify_all_constraints(&mut self) {
        let region_sub = self.generate_region_substitutions();
        let ty_sub = self.generate_type_substitutions();

        for r in self.place_map.values_mut() {
            *r = Self::apply_region(&region_sub, r);
        }
        for e in self.effect_map.values_mut() {
            *e = Self::apply_effect(&region_sub, e);
        }
        for LivenessConstraint { region, effect, .. } in self.live_constraints.iter_mut() {
            *region = Self::apply_region(&region_sub, region);
            *effect = Self::apply_effect(&region_sub, effect);
        }

        for t in self.ty.values_mut() {
            *t = Self::apply_type(&ty_sub, t);
        }
    }
    pub fn check_liveness(&mut self) {
        for LivenessConstraint {
            region,
            effect,
            span,
            kind,
        } in &self.live_constraints
        {
            // dbg!(&region, &effect);
            match effect.is_region_dead(region) {
                LivenessResult::Live => {}
                LivenessResult::Dead(simple_span) => {
                    let label_content = match kind {
                        LivenessConstraintKind::TakeRef => "Cannot take reference to freed memory",
                        LivenessConstraintKind::UseRef => {
                            "Cannot dereference a pointer belonging in freed memory"
                        }
                        LivenessConstraintKind::Alloc => {
                            "Cannot allocate into a region which has been freed"
                        }
                        LivenessConstraintKind::Free => {
                            "Cannot free a region that has already been freed"
                        }
                        LivenessConstraintKind::Dangling => {
                            "Cannot return a reference to a region that has been freed"
                        },
                    };
                    self.diagnostics.push(
                        Report::build(
                            ariadne::ReportKind::Error,
                            (self.cache.0, span.into_range()),
                        )
                        .with_message("memory safety issues")
                        .with_label(
                            Label::new((self.cache.0, span.into_range()))
                                .with_message(label_content),
                        )
                        .with_label(
                            Label::new((self.cache.0, simple_span.into_range()))
                                .with_message("region freed here"),
                        )
                        .with_config(self.config)
                        .finish(),
                    );
                }
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

        self.ty
            .insert(expression.id, Type::InferenceVariable(ty_var));
        self.place_map.insert(expression.id, reg_var);

        let effect = match &expression.kind {
            hir::HirExpressionKind::NewRegion => {
                let total_effect = incoming.clone(); // just for consistency

                self.ty_constraints
                    .push((Type::InferenceVariable(ty_var), Type::Unit));

                let sub_effect = Effect::Sequence(
                    Box::new(Effect::Fresh(reg_var)),
                    Box::new(Effect::Alloc(reg_var)), // 1 byte allocation for the "handle"
                );

                self.effect_map.insert(expression.id, sub_effect.clone());
                Effect::Sequence(Box::new(total_effect), Box::new(sub_effect))
            }
            hir::HirExpressionKind::FreeRegion(at) => {
                let total_effect = self.generate_constraints(incoming, &at)?;

                self.ty_constraints
                    .push((Type::InferenceVariable(ty_var), Type::Unit));
                self.reg_constraints.push((reg_var, Region::Global));

                let at_effect = self.effect_map[&at.id].clone();
                let at_region = self.place_map[&at.id];

                self.live_constraints.push(LivenessConstraint {
                    region: at_region,
                    effect: total_effect.clone(),
                    span: at.span,
                    kind: LivenessConstraintKind::Free,
                });

                let sub_effect = Effect::Sequence(
                    Box::new(at_effect),
                    Box::new(Effect::Free(at_region, expression.span)),
                );

                self.effect_map.insert(expression.id, sub_effect.clone());
                Effect::Sequence(Box::new(total_effect), Box::new(sub_effect))
            }
            hir::HirExpressionKind::Allocate(value, at) => {
                let total_effect = self.generate_constraints(incoming, at)?;

                // Hardcode value type to int for now. can deal with this later
                self.ty_constraints
                    .push((Type::InferenceVariable(ty_var), Type::Int));

                let at_effect = self.effect_map[&at.id].clone();
                let at_region = self.place_map[&at.id];

                let sub_effect =
                    Effect::Sequence(Box::new(at_effect), Box::new(Effect::Alloc(at_region)));

                self.live_constraints.push(LivenessConstraint {
                    region: at_region,
                    effect: total_effect.clone(),
                    span: at.span,
                    kind: LivenessConstraintKind::Alloc,
                });
                self.reg_constraints.push((reg_var, at_region));

                self.effect_map.insert(expression.id, sub_effect.clone());
                Effect::Sequence(Box::new(total_effect), Box::new(sub_effect))
            }
            hir::HirExpressionKind::Local(hir_id) => {
                self.ty_constraints
                    .push((Type::InferenceVariable(ty_var), self.ty[hir_id].clone()));
                self.reg_constraints.push((reg_var, self.place_map[hir_id]));
                self.effect_map.insert(expression.id, Effect::Bottom);

                incoming.clone()
            }
            hir::HirExpressionKind::Block(hir_expressions) => {
                let mut total_effect = incoming.clone();

                let mut sub_effect = Effect::Bottom;
                let mut last = None;

                for expr in hir_expressions {
                    total_effect = self.generate_constraints(&total_effect, &expr)?;
                    let expr_sub_effect = self.effect_map[&expr.id].clone();
                    sub_effect = Effect::Sequence(Box::new(sub_effect), Box::new(expr_sub_effect));

                    last = Some(expr);
                }
                if let Some(last) = last {
                    self.ty_constraints
                        .push((Type::InferenceVariable(ty_var), self.ty[&last.id].clone()));
                    self.reg_constraints.push((reg_var, self.place_map[&last.id]));
                } else {
                    self.ty_constraints
                        .push((Type::InferenceVariable(ty_var), Type::Unit));
                    self.reg_constraints.push((reg_var, Region::Global));
                }
                self.effect_map.insert(expression.id, sub_effect.clone());
                total_effect
            }

            hir::HirExpressionKind::LetIn(init, next) => {
                let total_effect = self.generate_constraints(incoming, init)?;
                let total_effect = self.generate_constraints(&total_effect, next)?;

                let next_ty_id = self.ty[&next.id].clone();
                let next_reg_id = self.place_map[&next.id];

                self.ty_constraints
                    .push((Type::InferenceVariable(ty_var), next_ty_id));
                self.reg_constraints.push((reg_var, next_reg_id));

                let init_effect = self.effect_map[&init.id].clone();
                let next_effect = self.effect_map[&next.id].clone();

                let sub_effect = Effect::Sequence(Box::new(init_effect), Box::new(next_effect));

                self.effect_map.insert(expression.id, sub_effect.clone());
                total_effect
            }
            hir::HirExpressionKind::Dereference(target) => {
                let total_effect = self.generate_constraints(incoming, &target)?;

                // inner must be reference(e) where e is expr_ty_id
                self.ty_constraints.push((
                    self.ty[&target.id].clone(),
                    Type::Reference(Box::new(Type::InferenceVariable(ty_var))),
                ));

                self.reg_constraints.push((reg_var, self.place_map[&target.id]));

                let sub_effect = self.effect_map[&target.id].clone();

                self.live_constraints.push(LivenessConstraint {
                    region: reg_var,
                    effect: total_effect.clone(),
                    span: target.span,
                    kind: LivenessConstraintKind::UseRef,
                });
                self.effect_map.insert(expression.id, sub_effect.clone());
                total_effect
            }
            hir::HirExpressionKind::Reference(target) => {
                let total_effect = self.generate_constraints(incoming, &target)?;

                self.ty_constraints.push((
                    Type::InferenceVariable(ty_var),
                    Type::Reference(Box::new(self.ty[&target.id].clone())),
                ));
                self.reg_constraints
                    .push((reg_var, self.place_map[&target.id].clone()));

                let at_region = self.place_map[&target.id];

                self.live_constraints.push(LivenessConstraint {
                    region: at_region,
                    effect: total_effect.clone(),
                    span: target.span,
                    kind: LivenessConstraintKind::TakeRef,
                });

                let sub_effect = self.effect_map[&target.id].clone();
                self.effect_map.insert(expression.id, sub_effect.clone());

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
        sub
    }
    pub fn generate_type_substitutions(&mut self) -> HashMap<usize, Type> {
        let mut sub = HashMap::new();
        for (lhs, rhs) in &self.ty_constraints {
            sub = self.unify_ty(sub, &lhs, &rhs);
        }
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
            Type::InferenceVariable(ty_id) if let Some(s) = sub.get(&ty_id.0) => s.clone(),
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
            Effect::Free(region, s) => Effect::Free(Self::apply_region(sub, region), *s),
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
            (Type::InferenceVariable(v), other) | (other, Type::InferenceVariable(v)) => {
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
}
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
            Effect::Free(r, _) => write!(f, "free {}", r),
        }
    }
}
