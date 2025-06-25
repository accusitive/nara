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
    pub signature: Spanned<FunctionSignature<'src>>,
    pub body: Spanned<Expression<'src>>,
}
#[derive(Debug)]
pub struct FunctionParameter<'src> {
    pub name: Spanned<Identifier<'src>>,
    pub ty: Spanned<Ty<'src>>,
}
#[derive(Debug)]
pub struct FunctionSignature<'src> {
    pub parameters: Vec<Spanned<FunctionParameter<'src>>>,
    pub ret: Spanned<Ty<'src>>,
    pub type_parameters: Vec<Spanned<TypeParameter<'src>>>,
}
#[derive(Debug)]
pub enum Ty<'a> {
    Int,
    Unit,
    Bool,

    Ref(Box<Spanned<Self>>),
    Function {
        params: Vec<Spanned<Self>>,
        ret: Box<Spanned<Self>>,
    },
    Forall(Vec<Spanned<TypeParameter<'a>>>, Box<Spanned<Self>>),
    Tuple(Vec<Spanned<Ty<'a>>>),
}
#[derive(Debug)]
pub enum TypeParameter<'src> {
    Region(Spanned<Identifier<'src>>),
    Type(Spanned<Identifier<'src>>),
}
#[derive(Debug, Clone)]
pub enum Value {
    Number(i64),
}
#[derive(Debug)]
pub enum Expression<'src> {
    Value(Spanned<Value>),
    Block(Vec<Spanned<Self>>),
    Path(Spanned<Identifier<'src>>),
    LetIn {
        binding: Spanned<Identifier<'src>>,
        init: Box<Spanned<Self>>,
        next: Box<Spanned<Self>>,
    },
    NewRegion,
    FreeRegion(Box<Spanned<Self>>),
    Allocate {
        value: Box<Spanned<Value>>,
        region: Box<Spanned<Self>>,
    },
    Reference(Box<Spanned<Self>>),
    Dereference(Box<Spanned<Self>>),
}
