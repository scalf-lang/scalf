fn format_source(source: &str) -> String {
    let tokens = scalf::lexer::lex(source).expect("lex should succeed");
    let mut parser = scalf::parser::Parser::new(tokens);
    let program = parser.parse_program().expect("parse should succeed");
    scalf::formatter::format_program(&program)
}

#[test]
fn formats_test_and_assert_blocks() {
    let formatted = format_source("test \"x\" { assert 1==1 }");
    assert_eq!(formatted, "test \"x\" {\n  assert 1 == 1\n}\n");
}

#[test]
fn formats_control_flow_blocks() {
    let formatted = format_source(
        "if true { x=1 } else { x=2 }\nwhile x<10 { x=x+1 }\nfor i=0; i<3; i=i+1 { print(i) }",
    );
    assert_eq!(
        formatted,
        "if true {\n  x = 1\n} else {\n  x = 2\n}\nwhile x < 10 {\n  x = x + 1\n}\nfor i = 0; i < 3; i = i + 1 {\n  print(i)\n}\n"
    );
}
