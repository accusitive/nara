use std::collections::HashMap;

use bumpalo::Bump;
use check::Check;
use hir::Scope;
use parser::{lex, parse_string};

fn main() {
    let src = include_str!("../input.nara");
    let tokens = lex(src).unwrap();
    let ast = parse_string(&tokens, src).unwrap();
    dbg!(&ast);

    let mut hir = hir::Hir{
        arena: &Bump::new(),
        next_id: 0,
        functions: HashMap::new(),
        expression: HashMap::new(),
        scopes: vec![Scope::new()]
    };
    for item in ast.value.items {
        match item.value {
            parser::ast::Item::Function(function_item) => {
                hir.lower_function(&function_item);
            },
        }
    }
    dbg!(&hir);
    let mut ck = Check::new(hir);
    ck.check_translation_unit();
    // let mut ck = Check {
    //     context: HashMap::new(),
    // };
    // ck.check_translation_unit(&ast);
    dbg!(&ck);
}
