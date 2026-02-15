//! Sculk - Native code generation backend for SCALF
//!
//! **STATUS: EXPERIMENTAL - DO NOT USE IN PRODUCTION**
//!
//! Sculk compiles SCALF code to native machine code for maximum performance.
//! This is a long-term project - the main SCALF compiler uses bytecode and
//! is stable and production-ready.

#![warn(missing_docs)]

pub mod backend;
pub mod codegen;
pub mod ir;
pub mod optimize;

use scalf::parser::ast::Program;

/// Sculk compiler version
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

/// Main compiler interface
pub struct Compiler {
    /// Optimization level (0-3)
    pub opt_level: u8,
    /// Target architecture
    pub target: String,
}

impl Compiler {
    /// Create a new compiler with default settings
    pub fn new() -> Self {
        Self {
            opt_level: 0,
            target: std::env::consts::ARCH.to_string(),
        }
    }

    /// Compile SCALF source text into Sculk IR.
    pub fn compile_source(
        &self,
        source: &str,
        module_name: &str,
    ) -> Result<ir::Module, CompileError> {
        let tokens = scalf::lexer::lex(source)
            .map_err(|err| CompileError::FrontendError(format!("lex error: {}", err)))?;
        let mut parser = scalf::parser::Parser::new(tokens);
        let program = parser
            .parse_program()
            .map_err(|err| CompileError::FrontendError(format!("parse error: {}", err)))?;
        self.compile_program(&program, module_name)
    }

    /// Compile parsed SCALF program into Sculk IR.
    pub fn compile_program(
        &self,
        program: &Program,
        module_name: &str,
    ) -> Result<ir::Module, CompileError> {
        let mut lowering = codegen::lowering::Lowering::new();
        lowering.lower_program(program, module_name)
    }

    /// Compile SCALF AST to IR.
    pub fn compile_ast(&self, ast: &Program) -> Result<ir::Module, CompileError> {
        self.compile_program(ast, "main")
    }
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}

/// Compilation errors
#[derive(Debug)]
pub enum CompileError {
    /// Frontend (lex/parse) error
    FrontendError(String),
    /// Feature not yet implemented
    NotImplemented(&'static str),
    /// IR validation failed
    InvalidIR(String),
    /// Backend error
    BackendError(String),
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompileError::FrontendError(msg) => write!(f, "Frontend error: {}", msg),
            CompileError::NotImplemented(msg) => write!(f, "Not implemented: {}", msg),
            CompileError::InvalidIR(msg) => write!(f, "Invalid IR: {}", msg),
            CompileError::BackendError(msg) => write!(f, "Backend error: {}", msg),
        }
    }
}

impl std::error::Error for CompileError {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compiler_creation() {
        let compiler = Compiler::new();
        assert_eq!(compiler.opt_level, 0);
    }
}
