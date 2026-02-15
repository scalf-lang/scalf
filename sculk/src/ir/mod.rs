//! Intermediate Representation for Sculk
//!
//! The IR is a low-level, typed representation of SCALF programs
//! that is easier to optimize and compile than the AST.

pub mod builder;
pub mod printer;

/// A compiled module
#[derive(Debug, Clone)]
pub struct Module {
    /// Module name
    pub name: String,
    /// Functions defined in this module
    pub functions: Vec<Function>,
    /// Global constants
    pub constants: Vec<Constant>,
}

/// A function definition
#[derive(Debug, Clone)]
pub struct Function {
    /// Function name
    pub name: String,
    /// Parameters
    pub params: Vec<Parameter>,
    /// Return type
    pub return_type: Type,
    /// Basic blocks
    pub blocks: Vec<BasicBlock>,
}

/// Function parameter
#[derive(Debug, Clone)]
pub struct Parameter {
    /// Parameter name
    pub name: String,
    /// Parameter type
    pub ty: Type,
}

/// A basic block (straight-line code with no branches except at the end)
#[derive(Debug, Clone)]
pub struct BasicBlock {
    /// Block label
    pub label: String,
    /// Instructions in this block
    pub instructions: Vec<Instruction>,
    /// Block terminator
    pub terminator: Terminator,
}

/// IR instruction
#[derive(Debug, Clone)]
pub enum Instruction {
    /// Assign a value to a variable
    Assign { dest: String, value: Value },
    /// Binary operation
    BinOp {
        dest: String,
        op: BinOp,
        left: Value,
        right: Value,
    },
    /// Function call
    Call {
        dest: Option<String>,
        func: String,
        args: Vec<Value>,
    },
    /// Load from memory
    Load { dest: String, addr: Value },
    /// Store to memory
    Store { addr: Value, value: Value },
}

/// Block terminator (control flow)
#[derive(Debug, Clone)]
pub enum Terminator {
    /// Return from function
    Return(Option<Value>),
    /// Unconditional branch
    Branch { target: String },
    /// Conditional branch
    CondBranch {
        cond: Value,
        then_block: String,
        else_block: String,
    },
    /// Unreachable code
    Unreachable,
}

/// Binary operation
#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    Or,
}

/// IR value
#[derive(Debug, Clone)]
pub enum Value {
    /// Variable reference
    Var(String),
    /// Integer constant
    Int(i64),
    /// Float constant
    Float(f64),
    /// String constant
    String(String),
    /// Boolean constant
    Bool(bool),
    /// Null value
    Null,
}

/// IR type
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    /// Void (no value)
    Void,
    /// Integer
    Int,
    /// Float
    Float,
    /// Boolean
    Bool,
    /// String
    String,
    /// Pointer to type
    Ptr(Box<Type>),
    /// Function type
    Func { params: Vec<Type>, ret: Box<Type> },
}

/// Global constant
#[derive(Debug, Clone)]
pub struct Constant {
    /// Constant name
    pub name: String,
    /// Constant type
    pub ty: Type,
    /// Constant value
    pub value: Value,
}

impl Module {
    /// Create a new empty module
    pub fn new(name: String) -> Self {
        Self {
            name,
            functions: Vec::new(),
            constants: Vec::new(),
        }
    }
}

impl Function {
    /// Create a new function
    pub fn new(name: String, params: Vec<Parameter>, return_type: Type) -> Self {
        Self {
            name,
            params,
            return_type,
            blocks: Vec::new(),
        }
    }
}

impl BasicBlock {
    /// Create a new basic block
    pub fn new(label: String) -> Self {
        Self {
            label,
            instructions: Vec::new(),
            terminator: Terminator::Unreachable,
        }
    }
}
