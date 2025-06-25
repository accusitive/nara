use chumsky::{
    input::{BorrowInput, MapExtra},
    prelude::*,
};

use crate::{
    Spanned,
    ast::{
        Expression, FunctionItem, FunctionParameter, FunctionSignature, Item, TranslationUnit, Ty,
        TypeParameter, Value,
    },
    lexer::{Keyword, Punctuation, Token},
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
macro_rules! open_bracket {
    () => {
        just(crate::lexer::Token::Punctuation(
            crate::lexer::Punctuation::LeftBracket,
        ))
    };
}
macro_rules! close_bracket {
    () => {
        just(crate::lexer::Token::Punctuation(
            crate::lexer::Punctuation::RightBracket,
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
    let region = just(Token::Punctuation(Punctuation::Tick)).ignore_then(identifier);

    let ty = recursive(|ty| {
        let int = just(Token::Keyword(crate::lexer::Keyword::I32)).map_with(|_, e| e!(Ty::Int, e));

        let type_parameter = choice((
            identifier.map_with(|i, e| e!(TypeParameter::Type(i), e)),
            region.map_with(|i, e| e!(TypeParameter::Region(i), e)),
        ));
        let type_parameters = type_parameter.separated_by(comma!()).collect::<Vec<_>>();

        let forall = just(Token::Keyword(Keyword::For))
            .ignore_then(type_parameters.delimited_by(
                just(Token::Punctuation(Punctuation::LeftAngle)),
                just(Token::Punctuation(Punctuation::RightAngle)),
            ))
            .then(ty.clone())
            .map_with(|(type_parameters, t), e| e!(Ty::Forall(type_parameters, Box::new(t)), e));

        // let at = ty
        //     .clone()
        //     .then(just(Token::Punctuation(Punctuation::At)))
        //     .ignore_then(region);
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
        // let tuple = ty
        //     .clone()
        //     .separated_by(comma!())
        //     .collect::<Vec<_>>()
        //     .delimited_by(open_param!(), close_param!())
        //     .map_with(|params, e| e!(Ty::Tuple(params), e));

        choice((int, function_type, forall))
    });

    let expression = recursive(|expr| {
        let int = select_ref!( Token::Literal(crate::lexer::LiteralTok::Integer(i)) => i).map_with(
            |s, e: &mut MapExtra<'_, '_, I, chumsky::extra::Err<Rich<'src, Token<'src>>>>| {
                e!(Value::Number(*s), e)
            },
        );
        // let tuple = expr
        //     .clone()
        //     .separated_by(comma!())
        //     .collect::<Vec<_>>()
        //     .delimited_by(open_param!(), close_param!())
        //     .map_with(|params, e| e!(Value::Tuple(params), e));
        let value = choice((int,));

        let path = identifier
            .clone()
            .map_with(|v, e| e!(Expression::Path(v), e));
        let ref_create = just(Token::Punctuation(Punctuation::Ampersand))
            .ignore_then(expr.clone())
            .map_with(|p, e| e!(Expression::Reference(Box::new(p)), e));
        let deref = just(Token::Punctuation(Punctuation::Star))
            .ignore_then(expr.clone())
            .map_with(|p, e| e!(Expression::Dereference(Box::new(p)), e));
        let at = value
            .then_ignore(just(Token::Keyword(Keyword::At)))
            .then(path)
            .map_with(|(value, region), e| {
                e!(
                    Expression::Allocate {
                        value: Box::new(value),
                        region: Box::new(region)
                    },
                    e
                )
            });

        let let_in_expr = just(Token::Keyword(Keyword::Let))
            .ignore_then(identifier.clone())
            .then_ignore(just(Token::Punctuation(Punctuation::Equal)))
            .then(expr.clone())
            .then_ignore(just(Token::Keyword(Keyword::In)))
            .then(expr.clone())
            .map_with(|((binding, init), next), e| {
                e!(
                    Expression::LetIn {
                        binding,
                        init: Box::new(init),
                        next: Box::new(next)
                    },
                    e
                )
            });
        let newrgn =
            just(Token::Keyword(Keyword::Newrgn)).map_with(|_, e| e!(Expression::NewRegion, e));
        let freergn = just(Token::Keyword(Keyword::Freergn))
            .ignore_then(expr.clone())
            .map_with(|x, e| e!(Expression::FreeRegion(Box::new(x)), e));

        let base_expr = choice((path, let_in_expr, newrgn, freergn, at, ref_create, deref));

        let block = base_expr
            .clone()
            .separated_by(just(Token::Punctuation(Punctuation::Semicolon)))
            .collect::<Vec<_>>()
            .delimited_by(open_bracket!(), close_bracket!())
            .map_with(|exprs, e| e!(Expression::Block(exprs), e));

        // let tuple = expr
        //     .clone()
        //     .separated_by(comma!())
        //     .collect::<Vec<_>>()
        //     .delimited_by(open_param!(), close_param!())
        //     .map_with(|params, e| e!(Expression::Tuple(params), e));

        choice((block, base_expr))
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

    let type_parameter = choice((
        identifier.map_with(|i, e| e!(TypeParameter::Type(i), e)),
        just(Token::Punctuation(Punctuation::Tick))
            .ignore_then(identifier)
            .map_with(|i, e| e!(TypeParameter::Type(i), e)),
    ));
    let type_parameters = type_parameter
        .separated_by(comma!())
        .collect::<Vec<_>>()
        .delimited_by(
            just(Token::Punctuation(Punctuation::LeftAngle)),
            just(Token::Punctuation(Punctuation::RightAngle)),
        )
        .or_not()
        .map(|x| x.unwrap_or_default());

    let signature = type_parameters
        .then(parameters)
        .then_ignore(skinny_arrow!())
        .then(ty)
        .map_with(|((type_parameters, parameters), ret), e| {
            e!(
                FunctionSignature {
                    parameters,
                    ret,
                    type_parameters
                },
                e
            )
        });

    let fn_item = just(Token::Keyword(crate::lexer::Keyword::Fn))
        .ignore_then(identifier)
        .then(signature)
        .then(expression.delimited_by(open_bracket!(), close_bracket!()))
        .map_with(|((name, signature), body), e| {
            e!(
                Item::Function(FunctionItem {
                    name: name,
                    signature,
                    body
                }),
                e
            )
        });
    let items = choice((fn_item,)).repeated().collect::<Vec<_>>();
    items.map_with(|items, e| Spanned::e(TranslationUnit { items }, e))
}
