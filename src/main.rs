use parser::{lex, parse_string};

fn main() {
    let src = include_str!("../input.nara");
    let tokens = lex(src).unwrap();
    let ast = parse_string(&tokens, src).unwrap();
    dbg!(&ast);
}
