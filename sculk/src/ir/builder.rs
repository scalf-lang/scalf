//! IR builder utilities
//!
//! Helpers for constructing IR programmatically

use super::*;

/// IR builder for constructing modules
pub struct ModuleBuilder {
    module: Module,
}

impl ModuleBuilder {
    /// Create a new module builder
    pub fn new(name: String) -> Self {
        Self {
            module: Module::new(name),
        }
    }

    /// Add a function to the module
    pub fn add_function(&mut self, func: Function) {
        self.module.functions.push(func);
    }

    /// Finish building and return the module
    pub fn build(self) -> Module {
        self.module
    }
}

/// IR builder for constructing functions
pub struct FunctionBuilder {
    function: Function,
    current_block: Option<usize>,
}

impl FunctionBuilder {
    /// Create a new function builder
    pub fn new(name: String, params: Vec<Parameter>, return_type: Type) -> Self {
        Self {
            function: Function::new(name, params, return_type),
            current_block: None,
        }
    }

    /// Create a new basic block
    pub fn create_block(&mut self, label: String) -> usize {
        let block = BasicBlock::new(label);
        self.function.blocks.push(block);
        self.function.blocks.len() - 1
    }

    /// Switch to a block
    pub fn switch_to_block(&mut self, block_id: usize) {
        self.current_block = Some(block_id);
    }

    /// Add an instruction to the current block
    pub fn add_instruction(&mut self, inst: Instruction) {
        if let Some(block_id) = self.current_block {
            self.function.blocks[block_id].instructions.push(inst);
        }
    }

    /// Set the terminator for the current block
    pub fn set_terminator(&mut self, term: Terminator) {
        if let Some(block_id) = self.current_block {
            self.function.blocks[block_id].terminator = term;
        }
    }

    /// Finish building and return the function
    pub fn build(self) -> Function {
        self.function
    }
}
