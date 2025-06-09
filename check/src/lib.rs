use std::{collections::HashMap, fmt::Display, ops::Deref};

use hir::{Hir, HirExpression, HirId};
use parser::{
    Spanned,
    ast::{Expression, FunctionItem, Item, TranslationUnit},
};
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct TyId(usize);
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Region {
    Variable(usize),
    Global,
}

#[derive(Debug)]
pub struct Check<'hir, 'src> {
    next_id: usize,
    ty: HashMap<HirId, TyId>,
    reg: HashMap<HirId, Region>,
    effect: HashMap<HirId, Effect>,

    ty_constraints: Vec<(Type, Type)>,
    reg_constraints: Vec<(Region, Region)>,
    hir: Hir<'hir, 'src>,
}
#[derive(Debug, Clone)]
pub enum Effect {
    Bottom,
    Fresh(Region),
    Free(Region),
    Alloc(Region),
    Sequence(Box<Effect>, Box<Effect>),
}
#[derive(Debug)]
pub enum Type {
    Variable(TyId),
    Int,
    Unit,
}

impl<'hir, 'src> Check<'hir, 'src> {
    pub fn new(hir: Hir<'hir, 'src>) -> Self {
        Self {
            next_id: 0,
            ty: HashMap::new(),
            reg: HashMap::new(),
            effect: HashMap::new(),

            ty_constraints: vec![],
            reg_constraints: vec![],
            hir,
        }
    }
    fn next_id(&mut self) -> usize {
        let id = self.next_id;
        self.next_id += 1;
        id
    }
    pub fn check_translation_unit(&mut self) {
        // sad that i have to clone here :(
        for f in self.hir.functions.clone().values() {
            self.generate_constraints(f).unwrap();
        }
    }
    pub fn generate_constraints(&mut self, expression: &HirExpression<'hir>) -> Result<(), ()> {
        if self.ty.contains_key(&expression.id) {
            return Ok(());
        }
        let expr_ty_id = TyId(self.next_id());
        let expr_reg_id = Region::Variable(self.next_id());
        self.ty.insert(expression.id, expr_ty_id);
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

                self.reg_constraints.push((expr_reg_id, at_region))
            }
            hir::HirExpressionKind::Local(hir_id) => {
                self.ty_constraints
                    .push((Type::Variable(expr_ty_id), Type::Variable(self.ty[hir_id])));
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
                    self.ty_constraints.push((
                        Type::Variable(expr_ty_id),
                        Type::Variable(self.ty[&last.id]),
                    ));
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

                let next_ty_id = self.ty[&next.id];
                let next_reg_id = self.reg[&next.id];

                self.ty_constraints
                    .push((Type::Variable(expr_ty_id), Type::Variable(next_ty_id)));
                self.reg_constraints.push((expr_reg_id, next_reg_id));

                let init_effect = self.effect[&init.id].clone();
                let next_effect = self.effect[&next.id].clone();
                let effect = Effect::Sequence(Box::new(init_effect), Box::new(next_effect));
                
                self.effect.insert(expression.id, effect);
            }
        }
        Ok(())
    }
}
// #[derive(Debug, Clone)]
// pub enum Effect {
//     Bottom,
//     Fresh,
//     Free,
//     Compose(Box<Self>, Box<Self>),
//     Alloc(Size),
// }
// #[derive(Debug, Clone)]
// pub enum CheckerError {
//     DoubleFree,
//     UseAfterFree,
// }
// impl Effect {
//     pub fn is_free(&self) -> bool {
//         match self {
//             Effect::Free => true,
//             Effect::Compose(effect, effect1) => effect.is_free() || effect1.is_free(),
//             _ => false,
//         }
//     }
//     pub fn compose(first: Self, second: Self) -> Result<Self, CheckerError> {
//         if first.is_free() && matches!(second, Self::Free) {
//             Err(CheckerError::DoubleFree)
//         } else {
//             Ok(Self::Compose(Box::new(first), Box::new(second)))
//         }
//     }
// }
// #[derive(Debug, Clone)]
// pub enum Size {
//     N(usize),
//     Unbounded,
// }
// impl Display for Effect {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             Effect::Bottom => write!(f, "⊥"),
//             Effect::Fresh => write!(f, "fresh"),
//             Effect::Compose(effect, effect1) => match (effect.deref(), effect1.deref()) {
//                 (Effect::Bottom, Effect::Bottom) => write!(f, ""),
//                 (Effect::Bottom, e @ _) | (e @ _, Effect::Bottom) => write!(f, "{}", e),
//                 (e0 @ _, e1 @ _) => write!(f, "{} x {}", e0, e1),
//             },
//             Effect::Alloc(Size::Unbounded) => write!(f, "alloc(?)",),
//             Effect::Alloc(Size::N(n)) => write!(f, "alloc({n})",),
//             Effect::Free => write!(f, "free"),
//         }
//     }
// }
