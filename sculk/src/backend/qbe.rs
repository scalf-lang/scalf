//! QBE backend implementation (Future)
//!
//! QBE (pronounced "cube") is a small compiler backend.
//! It's simpler than Cranelift but still produces decent code.

use crate::backend::Backend;
use crate::ir::Module;
use crate::CompileError;

/// QBE code generator
pub struct QBEBackend;

impl QBEBackend {
    /// Create a new QBE backend
    pub fn new() -> Self {
        Self
    }
}

impl Default for QBEBackend {
    fn default() -> Self {
        Self::new()
    }
}

impl Backend for QBEBackend {
    fn generate(&self, _module: &Module) -> Result<Vec<u8>, CompileError> {
        Err(CompileError::NotImplemented(
            "QBE backend not yet implemented",
        ))
    }

    fn name(&self) -> &'static str {
        "qbe"
    }

    fn supported_targets(&self) -> &[&str] {
        &["x86_64", "aarch64"]
    }
}
