//! Code generation backends
//!
//! Multiple backends can be implemented:
//! - Cranelift (production-ready, recommended)
//! - QBE (simple, lightweight)
//! - Custom x86-64 (learning/control)

#[cfg(feature = "cranelift-backend")]
pub mod cranelift;

#[cfg(feature = "qbe-backend")]
pub mod qbe;

#[cfg(feature = "custom-backend")]
pub mod x86_64;

use crate::ir::Module;
use crate::CompileError;

/// Code generation backend trait
pub trait Backend {
    /// Generate native code from IR
    fn generate(&self, module: &Module) -> Result<Vec<u8>, CompileError>;

    /// Get backend name
    fn name(&self) -> &'static str;

    /// Get supported target architectures
    fn supported_targets(&self) -> &[&str];
}

/// Compiled binary output
pub struct CompiledBinary {
    /// Machine code bytes
    pub code: Vec<u8>,
    /// Target architecture
    pub target: String,
    /// Entry point offset
    pub entry_point: usize,
}
