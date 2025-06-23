use std::fmt::Display;

use chumsky::{input::BorrowInput, prelude::*, text::ascii::ident};

use crate::{Span, Spanned};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Keyword {
    I32,
    Void,
    Let,
    Fn,
    In,
    At,
    For,
    Newrgn,
    Freergn
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Punctuation {
    LeftAngle,
    RightAngle,

    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,

    Comma,
    Colon,
    ColonColon,

    Arrow,
    Dot,
    Equal,
    Semicolon,
    Plus,
    Star,
    Ampersand,
    Tick,
    At
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LiteralTok<'a> {
    Integer(i64),
    Char(&'a str),
    String(&'a str),
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token<'a> {
    Keyword(Keyword),
    Identifier(&'a str),
    Punctuation(Punctuation),
    Literal(LiteralTok<'a>),
}

pub fn lexer<'src>() -> impl Parser<
    'src,
    &'src str,
    Vec<Spanned<Token<'src>>>,
    chumsky::extra::Err<Rich<'src, char, SimpleSpan>>,
> {
    let kw = ident().map(|ident| match ident {
        "let" => Token::Keyword(Keyword::Let),
        "i32" => Token::Keyword(Keyword::I32),
        "for" => Token::Keyword(Keyword::For),

        "void" => Token::Keyword(Keyword::Void),
        "newrgn" => Token::Keyword(Keyword::Newrgn),
        "freergn" => Token::Keyword(Keyword::Freergn),

        "in" => Token::Keyword(Keyword::In),
        "at" => Token::Keyword(Keyword::At),

        "fn" => Token::Keyword(Keyword::Fn),

        _ => Token::Identifier(ident),
    });
    let punc = choice((
        // two-wide
        just("->").map(|_| Punctuation::Arrow),
        just("::").map(|_| Punctuation::ColonColon),
        just('<').map(|_| Punctuation::LeftAngle),
        just('>').map(|_| Punctuation::RightAngle),
        just('(').map(|_| Punctuation::LeftParen),
        just(')').map(|_| Punctuation::RightParen),
        just('{').map(|_| Punctuation::LeftBracket),
        just('}').map(|_| Punctuation::RightBracket),
        just(':').map(|_| Punctuation::Colon),
        just(',').map(|_| Punctuation::Comma),
        just('.').map(|_| Punctuation::Dot),
        just('=').map(|_| Punctuation::Equal),
        just(';').map(|_| Punctuation::Semicolon),
        just('+').map(|_| Punctuation::Plus),
        just('*').map(|_| Punctuation::Star),
        just('&').map(|_| Punctuation::Ampersand),
        just('\'').map(|_| Punctuation::Tick),
        just('@').map(|_| Punctuation::At),

    ))
    .map(|p| Token::Punctuation(p));

    let int_literal = text::int::<_, _>(10)
        .from_str()
        .unwrapped()
        .map(|i| Token::Literal(LiteralTok::Integer(i)));

    let string = none_of('"')
        .repeated()
        .to_slice()
        .delimited_by(just('"'), just('"'));

    let char_string = just('c')
        .ignore_then(string)
        .map(|content| Token::Literal(LiteralTok::Char(content)));

    let string = string.map(|content| Token::Literal(LiteralTok::String(content)));

    let token = choice((punc, int_literal, string, char_string, kw));
    let comment = just("//")
        .then(any().and_is(just('\n').not()).repeated())
        .padded();

    token
        .map_with(|tok, e| Spanned::new(tok, e.span()))
        .padded_by(comment.repeated())
        .padded()
        .repeated()
        .collect()
}

pub fn make_input<'src>(
    eoi: SimpleSpan,
    toks: &'src [Spanned<Token<'src>>],
) -> impl BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan> {
    toks.map(eoi, |Spanned { value, span }| (value, span))
}

impl<'a> Display for Token<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Keyword(keyword) => write!(f, "{:?}", keyword),
            Token::Identifier(s) => write!(f, "{}", s),
            Token::Punctuation(punctuation) => write!(f, "{:?}", punctuation),
            Token::Literal(literal_tok) => write!(f, "{:?}", literal_tok),
        }
    }
}
