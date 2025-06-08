use chumsky::{input::BorrowInput, prelude::*};

use crate::{
    Spanned,
    ast::{FunctionItem, FunctionParameter, FunctionSignature, Item, TranslationUnit, Ty},
    lexer::Token,
};
macro_rules! open_param {
    () => {
        just(crate::lexer::Token::Punctuation(
            crate::lexer::Punctuation::LeftParen,
        ))
    };
}
macro_rules! close_param {
    () => {
        just(crate::lexer::Token::Punctuation(
            crate::lexer::Punctuation::RightParen,
        ))
    };
}
macro_rules! comma {
    () => {
        just(crate::lexer::Token::Punctuation(
            crate::lexer::Punctuation::Comma,
        ))
    };
}
macro_rules! skinny_arrow {
    () => {
        just(crate::lexer::Token::Punctuation(
            crate::lexer::Punctuation::Arrow,
        ))
    };
}
macro_rules! colon {
    () => {
        just(crate::lexer::Token::Punctuation(
            crate::lexer::Punctuation::Colon,
        ))
    };
}
macro_rules! e {
    ($t: expr, $e: expr) => {
        Spanned::e($t, $e)
    };
    ($t: expr, $e: expr,) => {
        Spanned::e($t, $e)
    };
}
pub fn parser<'src, I: BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan>>()
-> impl Parser<'src, I, Spanned<TranslationUnit<'src>>, chumsky::extra::Err<Rich<'src, Token<'src>>>>
+ Clone {
    let identifier = select_ref!( Token::Identifier(x) => x).map_with(|s, e| Spanned::e(*s, e));
    let ty = recursive(|ty| {
        let int = just(Token::Keyword(crate::lexer::Keyword::I32))
            .map_with(|_, e| Spanned::e(Ty::Int, e));
        let function_type = ty
            .clone()
            .separated_by(comma!())
            .collect::<Vec<_>>()
            .delimited_by(open_param!(), close_param!())
            .then_ignore(skinny_arrow!())
            .then(ty.clone())
            .map_with(|(params, ret), e| {
                e!(
                    Ty::Function {
                        params,
                        ret: Box::new(ret),
                    },
                    e
                )
            });

        choice((int, function_type))
    });
    let parameter = identifier
        .clone()
        .then_ignore(colon!())
        .then(ty.clone())
        .map_with(|(name, ty), e| e!(FunctionParameter { name, ty }, e));

    let parameters = parameter
        .separated_by(comma!())
        .collect::<Vec<_>>()
        .delimited_by(open_param!(), close_param!());

    let signature = parameters
        .then_ignore(skinny_arrow!())
        .then(ty)
        .map_with(|(parameters, ret), e| e!(FunctionSignature { parameters, ret }, e));

    let fn_item = just(Token::Keyword(crate::lexer::Keyword::Fn))
        .ignore_then(identifier)
        .then(signature)
        .map_with(|(name, signature), e| {
            e!(
                Item::Function(FunctionItem {
                    name: name,
                    signature
                }),
                e
            )
        });
    let items = choice((fn_item,)).repeated().collect::<Vec<_>>();
    items.map_with(|items, e| Spanned::e(TranslationUnit { items }, e))
}
