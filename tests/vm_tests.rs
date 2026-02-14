use scalf::runtime::value::Value;
use scalf::vm::{compile_program, run_program, Instruction};

fn parse_program(source: &str) -> scalf::parser::ast::Program {
    let tokens = scalf::lexer::lex(source).expect("lex should succeed");
    let mut parser = scalf::parser::Parser::new(tokens);
    parser.parse_program().expect("parse should succeed")
}

#[test]
fn vm_runs_arithmetic_and_assignment() {
    let program = parse_program("x = 1\nx = x + 2\nx");
    let value = run_program(&program).expect("vm execution should succeed");
    assert_eq!(value, Value::Int(3));
}

#[test]
fn vm_constant_folds_numeric_expressions() {
    let program = parse_program("1 + 2 * 3");
    let chunk = compile_program(&program).expect("compile should succeed");

    assert_eq!(chunk.constants, vec![Value::Int(7)]);
    assert_eq!(chunk.instructions.len(), 2);
    assert!(matches!(chunk.instructions[0], Instruction::LoadConst(0)));
    assert!(matches!(chunk.instructions[1], Instruction::Return));
    assert!(!chunk
        .instructions
        .iter()
        .any(|inst| matches!(inst, Instruction::Add | Instruction::Multiply)));
}

#[test]
fn vm_reports_unsupported_function_definitions() {
    let program = parse_program("def add(a: int, b: int) -> int { return a + b }\nadd(1, 2)");
    let err = compile_program(&program).expect_err("compile should fail on function defs");
    assert!(err.message.contains("function definitions"));
}
