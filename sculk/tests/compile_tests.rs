//! Integration tests for Sculk compiler

use sculk::*;

#[test]
fn test_compiler_creation() {
    let compiler = Compiler::new();
    assert_eq!(compiler.opt_level, 0);
}

#[test]
fn test_ir_module_creation() {
    let module = ir::Module::new("test".to_string());
    assert_eq!(module.name, "test");
    assert_eq!(module.functions.len(), 0);
}

#[test]
fn test_ir_function_creation() {
    let func = ir::Function::new("main".to_string(), vec![], ir::Type::Int);
    assert_eq!(func.name, "main");
    assert_eq!(func.return_type, ir::Type::Int);
}

#[test]
fn test_ir_builder() {
    use ir::builder::*;

    let mut builder = ModuleBuilder::new("test".to_string());

    let func = ir::Function::new(
        "add".to_string(),
        vec![
            ir::Parameter {
                name: "a".to_string(),
                ty: ir::Type::Int,
            },
            ir::Parameter {
                name: "b".to_string(),
                ty: ir::Type::Int,
            },
        ],
        ir::Type::Int,
    );

    builder.add_function(func);
    let module = builder.build();

    assert_eq!(module.functions.len(), 1);
    assert_eq!(module.functions[0].name, "add");
}

#[cfg(feature = "cranelift-backend")]
#[test]
fn test_cranelift_backend() {
    use backend::cranelift::CraneliftBackend;
    use backend::Backend;

    let backend = CraneliftBackend::new();
    assert!(backend.is_ok());

    let backend = backend.unwrap();
    assert_eq!(backend.name(), "cranelift");
}

#[cfg(feature = "cranelift-backend")]
#[test]
fn test_compile_source_and_run_main() {
    use backend::cranelift::CraneliftBackend;

    let source = r#"
def main() {
    print("Hello from native SCALF!")
    return 0
}
"#;

    let compiler = Compiler::new();
    let module = compiler
        .compile_source(source, "hello_native")
        .expect("source should lower to IR");

    let backend = CraneliftBackend::new().expect("backend should initialize");
    let exit_code = backend
        .run_main(&module)
        .expect("jit execution should succeed");

    assert_eq!(exit_code, 0);
}
