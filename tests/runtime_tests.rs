use std::time::{SystemTime, UNIX_EPOCH};

use rask::runtime::value::Value;

fn run(source: &str) -> Value {
    let tokens = rask::lexer::lex(source).expect("lex should succeed");
    let mut parser = rask::parser::Parser::new(tokens);
    let program = parser.parse_program().expect("parse should succeed");
    let mut runtime = rask::runtime::Runtime::new();
    runtime.run_program(&program).expect("runtime should succeed")
}

#[test]
fn evaluates_string_and_list_methods() {
    let value = run(
        "def double(x: int) -> int { return x * 2 }\n\
         nums = [1, 2]\n\
         nums.push(3)\n\
         upper = \"  hi \".trim().uppercase()\n\
         mapped = nums.map(double)\n\
         mapped[2]",
    );
    assert_eq!(value, Value::Int(6));
}

#[test]
fn evaluates_map_methods_and_indexing() {
    let value = run(
        "m = {name: \"rask\"}\n\
         m.set(\"version\", 1)\n\
         m[\"version\"]",
    );
    assert_eq!(value, Value::Int(1));
}

#[test]
fn evaluates_json_roundtrip() {
    let value = run(
        "data = {name: \"rask\", nums: [1, 2, 3]}\n\
         encoded = json.stringify(data)\n\
         decoded = json.parse(encoded)\n\
         decoded[\"name\"]",
    );
    assert_eq!(value, Value::String("rask".to_string()));
}

#[test]
fn evaluates_use_alias_for_module() {
    let value = run("use std.math as m\nm.max(2, 5)");
    assert_eq!(value, Value::Float(5.0));
}

#[test]
fn evaluates_fs_module() {
    let mut path = std::env::temp_dir();
    let stamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("clock should be valid")
        .as_nanos();
    path.push(format!("rask_phase3_{}.txt", stamp));
    let path_str = path.to_string_lossy().replace('\\', "\\\\");

    let script = format!(
        "fs.write(\"{}\", \"hello\")\n\
         content = fs.read(\"{}\")\n\
         fs.delete(\"{}\")\n\
         content",
        path_str, path_str, path_str
    );

    let value = run(&script);
    assert_eq!(value, Value::String("hello".to_string()));
    assert!(!path.exists(), "temporary file should be deleted");
}

