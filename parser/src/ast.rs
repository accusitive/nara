use crate::Spanned;
pub type Identifier<'src> = &'src str;

#[derive(Debug)]
pub struct TranslationUnit<'src> {
    pub items: Vec<Spanned<Item<'src>>>,
}
#[derive(Debug)]
pub enum Item<'src> {
    Function(FunctionItem<'src>),
}
#[derive(Debug)]
pub struct FunctionItem<'src> {
    pub name: Spanned<Identifier<'src>>,
    pub signature: Spanned<FunctionSignature<'src>>
}
#[derive(Debug)]
pub struct FunctionParameter<'src> {
    pub name: Spanned<Identifier<'src>>,
    pub ty: Spanned<Ty>,
}
#[derive(Debug)]
pub struct FunctionSignature<'src> {
    pub parameters: Vec<Spanned<FunctionParameter<'src>>>,
    pub ret: Spanned<Ty>,
}
#[derive(Debug)]
pub enum Ty {
    Int,
    Unit,
    BOol,
    Ref(Box<Spanned<Self>>),
    Function {
        params: Vec<Spanned<Self>>,
        ret: Box<Spanned<Self>>,
    },
}
