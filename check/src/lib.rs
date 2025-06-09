use std::{collections::HashMap, fmt::Display, ops::Deref};

use parser::{
    Spanned,
    ast::{Expression, FunctionItem, Item, TranslationUnit},
};

pub struct Check<'src> {
    pub context: HashMap<&'src str, Effect>,
}

impl<'src> Check<'src> {
    pub fn check_translation_unit(&mut self, tu: &Spanned<TranslationUnit<'src>>) {
        for item in &tu.value.items {
            self.check_item(item)
        }
    }
    pub fn check_item(&mut self, item: &Spanned<Item<'src>>) {
        match &item.value {
            Item::Function(function_item) => {
                self.check_function_item(function_item);
            }
        }
    }
    pub fn check_function_item(&mut self, item: &FunctionItem<'src>) {
        let effect = self.check_expression(&item.body);
        println!("{}", effect.unwrap());
    }
    pub fn check_expression(
        &mut self,
        expression: &Spanned<Expression<'src>>,
    ) -> Result<Effect, CheckerError> {
        match &expression.value {
            Expression::Value(spanned) => Ok(Effect::Bottom),
            Expression::Block(spanneds) => {
                let mut effect = Effect::Bottom;
                let mut i = spanneds.iter();
                while let Some(next) = i.next() {
                    effect = Effect::compose(effect, self.check_expression(next)?)?;
                }
                Ok(effect)
            }
            Expression::Path(spanned) => Ok(Effect::Bottom),
            Expression::LetIn {
                binding,
                init,
                next,
            } => {
                let init_effect = self.check_expression(init.as_ref())?;
                assert!(self.context.insert(binding.value, init_effect).is_none());

                let effect = self.check_expression(&next)?;
                let init_effect = self.context.remove(binding.value).unwrap();

                Ok(Effect::Compose(Box::new(init_effect), Box::new(effect)))
            }
            Expression::NewRegion => {
                Ok(Effect::compose(Effect::Fresh, Effect::Alloc(Size::N(1))).unwrap())
            }
            Expression::FreeRegion(spanned) => Ok(Effect::Free),
            Expression::Allocate { value, region } => {
                let inner = self.check_expression(region)?;
                Ok(Effect::compose(inner, Effect::Alloc(Size::Unbounded)).unwrap())
            }
            Expression::Reference(spanned) => Ok(Effect::Bottom),
            Expression::Dereference(expression) => {
                let v = self.check_expression(&expression)?;
                if v.is_free() {
                    Ok(Effect::Bottom)
                } else {
                    Err(CheckerError::UseAfterFree)
                }
            }
        }
    }
}
#[derive(Debug, Clone)]
pub enum Effect {
    Bottom,
    Fresh,
    Free,
    Compose(Box<Self>, Box<Self>),
    Alloc(Size),
}
#[derive(Debug, Clone)]
pub enum CheckerError {
    DoubleFree,
    UseAfterFree,
}
impl Effect {
    pub fn is_free(&self) -> bool {
        match self {
            Effect::Free => true,
            Effect::Compose(effect, effect1) => effect.is_free() || effect1.is_free(),
            _ => false,
        }
    }
    pub fn compose(first: Self, second: Self) -> Result<Self, CheckerError> {
        if first.is_free() && matches!(second, Self::Free) {
            Err(CheckerError::DoubleFree)
        } else {
            Ok(Self::Compose(Box::new(first), Box::new(second)))
        }
    }
}
#[derive(Debug, Clone)]
pub enum Size {
    N(usize),
    Unbounded,
}
impl Display for Effect {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Effect::Bottom => write!(f, "⊥"),
            Effect::Fresh => write!(f, "fresh"),
            Effect::Compose(effect, effect1) => match (effect.deref(), effect1.deref()) {
                (Effect::Bottom, Effect::Bottom) => write!(f, ""),
                (Effect::Bottom, e @ _) | (e @ _, Effect::Bottom) => write!(f, "{}", e),
                (e0 @ _, e1 @ _) => write!(f, "{} x {}", e0, e1),
            },
            Effect::Alloc(Size::Unbounded) => write!(f, "alloc(?)",),
            Effect::Alloc(Size::N(n)) => write!(f, "alloc({n})",),
            Effect::Free => write!(f, "free"),
        }
    }
}
