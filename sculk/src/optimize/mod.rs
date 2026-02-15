//! IR optimization passes
//!
//! Optimizations are applied to IR before code generation.
//! Each pass transforms the IR to make it faster/smaller.

use crate::ir::Module;
use crate::CompileError;

/// Optimization pass trait
pub trait Pass {
    /// Run the optimization pass on a module
    fn run(&self, module: &mut Module) -> Result<(), CompileError>;

    /// Get pass name
    fn name(&self) -> &'static str;
}

/// Pass manager - runs optimization passes in order
pub struct PassManager {
    passes: Vec<Box<dyn Pass>>,
}

impl PassManager {
    /// Create a new pass manager
    pub fn new() -> Self {
        Self { passes: Vec::new() }
    }

    /// Add an optimization pass
    pub fn add_pass(&mut self, pass: Box<dyn Pass>) {
        self.passes.push(pass);
    }

    /// Run all passes on a module
    pub fn run(&self, module: &mut Module) -> Result<(), CompileError> {
        for pass in &self.passes {
            pass.run(module)?;
        }
        Ok(())
    }
}

impl Default for PassManager {
    fn default() -> Self {
        Self::new()
    }
}

/// Constant folding pass
pub struct ConstantFolding;

impl Pass for ConstantFolding {
    fn run(&self, _module: &mut Module) -> Result<(), CompileError> {
        // TODO: Fold constant expressions
        // Example: 2 + 3 → 5
        Ok(())
    }

    fn name(&self) -> &'static str {
        "constant-folding"
    }
}

/// Dead code elimination pass
pub struct DeadCodeElimination;

impl Pass for DeadCodeElimination {
    fn run(&self, _module: &mut Module) -> Result<(), CompileError> {
        // TODO: Remove unused variables and unreachable code
        Ok(())
    }

    fn name(&self) -> &'static str {
        "dead-code-elimination"
    }
}

/// Function inlining pass
pub struct Inlining;

impl Pass for Inlining {
    fn run(&self, _module: &mut Module) -> Result<(), CompileError> {
        // TODO: Inline small functions
        Ok(())
    }

    fn name(&self) -> &'static str {
        "inlining"
    }
}
