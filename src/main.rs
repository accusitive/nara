use std::collections::HashMap;

use ariadne::{Color, Config, Label, Source};
use bumpalo::Bump;
use check::Check;
use hir::Scope;
use parser::{lex, parse_string};

fn main() {
    let src = include_str!("../input.nara");
    let tokens = lex(src).unwrap();
    let ast = parse_string(&tokens, src).unwrap();
    dbg!(&ast);

    let mut hir = hir::Hir {
        arena: &Bump::new(),
        next_id: 0,
        functions: HashMap::new(),
        expression: HashMap::new(),
        scopes: vec![Scope::new()],
    };
    for item in ast.value.items {
        match item.value {
            parser::ast::Item::Function(function_item) => {
                hir.lower_function(&function_item);
            }
        }
    }
    let mut cache = ("test.nara", Source::from(src.to_string()));
    let cfg = Config::new()
        .with_compact(false)
        .with_cross_gap(false)
        .with_label_attach(ariadne::LabelAttach::End)
        .with_char_set(ariadne::CharSet::Ascii);

    let mut ck = Check::new(cache.clone(), cfg);

    ck.generate_all_constraints(&hir);
    ck.unify_all_constraints();

    let color = |n: usize| match n % 6 {
        0 => Color::Red,
        1 => Color::Green,
        2 => Color::Blue,
        3 => Color::Cyan,
        4 => Color::Magenta,
        5 => Color::Yellow,
        _ => Color::White,
    };

    {
        let mut labels = vec![];
        for (id, t) in &ck.ty {
            let e = hir.expression[&id];
            labels.push(
                Label::new(("test.nara", e.span.into_range()))
                    .with_message(format!("type: {:?}", t))
                    .with_color(color(labels.len())),
            )
        }

        ariadne::Report::build(
            ariadne::ReportKind::Custom(&format!("ty debugging:"), ariadne::Color::Blue),
            ("test.nara", 0..0),
        )
        .with_config(cfg)
        .with_labels(labels)
        .finish()
        .eprint(&mut cache)
        .unwrap();
    }
    {
        let mut labels = vec![];
        for (id, r) in &ck.place_map {
            let e = hir.expression[&id];
            labels.push(
                Label::new(("test.nara", e.span.into_range()))
                    .with_message(format!("region: {}", r))
                    .with_color(color(labels.len())),
            )
        }

        ariadne::Report::build(
            ariadne::ReportKind::Custom(&format!("region debugging:"), ariadne::Color::Blue),
            ("test.nara", 0..0),
        )
        .with_config(cfg)
        .with_labels(labels)
        .finish()
        .eprint(&mut cache)
        .unwrap();
    }
    {
        let mut labels = vec![];
        for (id, effect) in &ck.effect_map {
            let e = hir.expression[&id];
            labels.push(
                Label::new(("test.nara", e.span.into_range()))
                    .with_message(format!("effect: {}", effect))
                    .with_color(color(labels.len())),
            )
        }

        ariadne::Report::build(
            ariadne::ReportKind::Custom(&format!("effect debugging:"), ariadne::Color::Blue),
            ("test.nara", 0..0),
        )
        .with_config(cfg)
        .with_labels(labels)
        .finish()
        .eprint(&mut cache)
        .unwrap();
    }
    ck.check_liveness();
    for diagnostic in ck.diagnostics {
        diagnostic.eprint(&mut cache).unwrap();
    }
}
mod tests;
