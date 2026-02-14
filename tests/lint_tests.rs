#[test]
fn warns_on_panic_unwrap() {
    let source = "value = parse(\"x\")!\nvalue";
    let tokens = scalf::lexer::lex(source).expect("lex should succeed");
    let mut parser = scalf::parser::Parser::new(tokens);
    let program = parser.parse_program().expect("parse should succeed");
    let warnings = scalf::lint::lint_program(&program);
    assert!(warnings.iter().any(|w| w.code == "LINT0001"));
}
