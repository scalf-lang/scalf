//! Custom x86-64 backend implementation (Far Future)
//!
//! A from-scratch x86-64 code generator.
//! This is the hardest option but gives maximum control.
//!
//! Only attempt this after Cranelift is working!

use crate::backend::Backend;
use crate::ir::Module;
use crate::CompileError;

/// Custom x86-64 code generator
pub struct X86Backend;

impl X86Backend {
    /// Create a new custom x86-64 backend
    pub fn new() -> Self {
        Self
    }
}

impl Default for X86Backend {
    fn default() -> Self {
        Self::new()
    }
}

impl Backend for X86Backend {
    fn generate(&self, _module: &Module) -> Result<Vec<u8>, CompileError> {
        Err(CompileError::NotImplemented(
            "Custom x86-64 backend not yet implemented (and may never be)",
        ))
    }

    fn name(&self) -> &'static str {
        "x86_64-custom"
    }

    fn supported_targets(&self) -> &[&str] {
        &["x86_64"]
    }
}

// Future: Instruction encoding, register allocation, etc.
// This is a HUGE undertaking. Use Cranelift instead.
