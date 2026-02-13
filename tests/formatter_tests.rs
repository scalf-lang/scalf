fn format_source(source: &str) -> String {
    let tokens = rask::lexer::lex(source).expect("lex should succeed");
    let mut parser = rask::parser::Parser::new(tokens);
    let program = parser.parse_program().expect("parse should succeed");
    rask::formatter::format_program(&program)
}

#[test]
fn formats_test_and_assert_blocks() {
    let formatted = format_source("test \"x\" { assert 1==1 }");
    assert_eq!(formatted, "test \"x\" {\n  assert 1 == 1\n}\n");
}
