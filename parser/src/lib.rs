use std::fmt::{Debug, Display, Pointer};

use ariadne::{Color, Label, Report, ReportKind, Source};
use ast::TranslationUnit;
use chumsky::{
    Parser,
    extra::ParserExtra,
    input::{Input, MapExtra},
    span::Span,
};
use lexer::Token;

pub mod ast;
pub mod lexer;
pub mod parser;
pub use chumsky::span::SimpleSpan;
pub struct Spanned<T> {
    pub value: T,
    pub span: SimpleSpan,
}
impl<T: Clone> Clone for Spanned<T> {
    fn clone(&self) -> Self {
        Self {
            value: self.value.clone(),
            span: self.span.clone(),
        }
    }
}

impl<T> Spanned<T> {
    fn e<'a, I: Input<'a, Span = SimpleSpan>, E: ParserExtra<'a, I>>(
        value: T,
        span: &mut MapExtra<'a, '_, I, E>,
    ) -> Self {
        Spanned {
            span: span.span(),
            value: value,
        }
    }
    pub fn new(value: T, span: SimpleSpan) -> Self {
        Spanned {
            span: span,
            value: value,
        }
    }
}

pub fn lex<'src>(src: &'src str) -> Option<Vec<Spanned<Token<'src>>>> {
    let (tokens, errs) = lexer::lexer().parse(src).into_output_errors();
    if let Some(toks) = tokens {
        return Some(toks);
    }

    errs.into_iter().for_each(|e| {
        Report::build(ReportKind::Error, ((), e.span().into_range()))
            .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
            .with_message(e.to_string())
            .with_label(
                Label::new(((), e.span().into_range()))
                    .with_message(e.reason().to_string())
                    .with_color(Color::Red),
            )
            .finish()
            .print(Source::from(&src))
            .unwrap()
    });
    None
}
pub fn parse_string<'src>(
    tokens: &'src [Spanned<Token<'src>>],
    src: &'src str,
) -> Option<Spanned<TranslationUnit<'src>>> {
    let input = lexer::make_input(SimpleSpan::new((), 0..src.len()), tokens);
    let (ast, errs) = crate::parser::parser().parse(input).into_output_errors();
    if let Some(ast) = ast {
        return Some(ast);
    }

    errs.into_iter().for_each(|e| {
        Report::build(ReportKind::Error, ((), e.span().into_range()))
            .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
            .with_message("Error while parsing")
            .with_label(
                Label::new(((), e.span().into_range()))
                    .with_message(e.reason())
                    .with_color(Color::Red),
            )
            .finish()
            .print(Source::from(&src))
            .unwrap()
    });

    None
}

impl<T: Debug> Debug for Spanned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.value, f)
    }
}
impl<T: Display> Display for Spanned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.value, f)
    }
}
