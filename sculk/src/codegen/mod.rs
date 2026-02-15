//! Code generation orchestration
//!
//! This module handles the overall compilation pipeline:
//! AST -> IR -> Optimized IR -> Native Code

pub mod lowering;

use crate::ir::Module;
use crate::CompileError;
use scalf::parser::ast::Program;

/// Main codegen entry point.
pub struct CodeGenerator {
    /// Whether to run optimizations.
    pub optimize: bool,
}

impl CodeGenerator {
    /// Create a new code generator.
    pub fn new() -> Self {
        Self { optimize: true }
    }

    /// Compile AST to IR.
    pub fn compile(&self, ast: &Program, module_name: &str) -> Result<Module, CompileError> {
        let mut lowering = lowering::Lowering::new();
        lowering.lower_program(ast, module_name)
    }
}

impl Default for CodeGenerator {
    fn default() -> Self {
        Self::new()
    }
}
