use std::collections::HashMap;

use ariadne::{Config, Source};
use bumpalo::Bump;
use check::Check;
use hir::Scope;
use parser::{lex, parse_string};

fn run_liveness_check_panic_test(src: &str) {
    let tokens = lex(src).unwrap();
    let ast = parse_string(&tokens, src).unwrap();

    let mut hir = hir::Hir {
        arena: &Bump::new(),
        next_id: 0,
        functions: HashMap::new(),
        expression: HashMap::new(),
        scopes: vec![Scope::new()],
    };

    for item in ast.value.items {
        if let parser::ast::Item::Function(function_item) = item.value {
            hir.lower_function(&function_item);
        }
    }

    let cache = ("test.nara", Source::from(src.to_string()));
    let mut ck = Check::new(cache.clone(), Config::default());

    ck.generate_all_constraints(&hir);
    ck.unify_all_constraints();
    ck.check_liveness();

    assert!(ck.diagnostics.is_empty());
}

#[test]
#[should_panic]
fn double_free() {
    run_liveness_check_panic_test(include_str!("../testcases/double_free.nara"));
}

#[test]
#[should_panic]
fn use_after_free() {
    run_liveness_check_panic_test(include_str!("../testcases/use_after_free.nara"));
}

#[test]
#[should_panic]
fn dangling() {
    run_liveness_check_panic_test(include_str!("../testcases/dangling.nara"));
}

#[test]
fn safe_dereference() {
    run_liveness_check_panic_test(include_str!("../testcases/safe_dereference.nara"));
}
