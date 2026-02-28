use std::collections::HashMap;
use std::fmt;

use crate::parser::ast::{BinaryOp, Expr, Program, Stmt, UnaryOp};
use crate::runtime::value::Value;

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    LoadConst(usize),
    LoadGlobal(String),
    StoreGlobal(String),
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    And,
    Negate,
    Not,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Print,
    Pop,
    Return,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BytecodeChunk {
    pub constants: Vec<Value>,
    pub instructions: Vec<Instruction>,
}

impl BytecodeChunk {
    pub fn new() -> Self {
        Self {
            constants: Vec::new(),
            instructions: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct VmCompileError {
    pub message: String,
}

impl VmCompileError {
    fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }
}

impl fmt::Display for VmCompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "vm compile error: {}", self.message)
    }
}

impl std::error::Error for VmCompileError {}

#[derive(Debug, Clone)]
pub struct VmRuntimeError {
    pub message: String,
}

impl VmRuntimeError {
    fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }
}

impl fmt::Display for VmRuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "vm runtime error: {}", self.message)
    }
}

impl std::error::Error for VmRuntimeError {}

const SCLC_MAGIC: &[u8; 4] = b"SCLC";
const SCLC_VERSION: u8 = 1;

pub fn compile_program(program: &Program) -> Result<BytecodeChunk, VmCompileError> {
    let mut compiler = Compiler::new();
    compiler.compile_program(program)?;
    Ok(compiler.chunk)
}

pub fn compile_program_to_bytes(program: &Program) -> Result<Vec<u8>, VmCompileError> {
    let chunk = compile_program(program)?;
    encode_chunk(&chunk)
}

pub fn run_program(program: &Program) -> Result<Value, VmRuntimeError> {
    let chunk = compile_program(program).map_err(|err| VmRuntimeError::new(err.message))?;
    execute_chunk(&chunk)
}

pub fn execute_chunk(chunk: &BytecodeChunk) -> Result<Value, VmRuntimeError> {
    let mut vm = Vm::new();
    vm.execute(chunk)
}

pub fn run_chunk_bytes(bytes: &[u8]) -> Result<Value, VmRuntimeError> {
    let chunk = decode_chunk(bytes)?;
    execute_chunk(&chunk)
}

pub fn encode_chunk(chunk: &BytecodeChunk) -> Result<Vec<u8>, VmCompileError> {
    let mut bytes = Vec::new();
    bytes.extend_from_slice(SCLC_MAGIC);
    bytes.push(SCLC_VERSION);

    encode_count(&mut bytes, chunk.constants.len(), "constant count")?;
    for constant in &chunk.constants {
        encode_constant(&mut bytes, constant)?;
    }

    encode_count(&mut bytes, chunk.instructions.len(), "instruction count")?;
    for instruction in &chunk.instructions {
        encode_instruction(&mut bytes, instruction)?;
    }

    Ok(bytes)
}

pub fn decode_chunk(bytes: &[u8]) -> Result<BytecodeChunk, VmRuntimeError> {
    if bytes.len() < 5 {
        return Err(VmRuntimeError::new("invalid .sclc payload: too short"));
    }
    if &bytes[0..4] != SCLC_MAGIC {
        return Err(VmRuntimeError::new("invalid .sclc payload: bad magic"));
    }
    if bytes[4] != SCLC_VERSION {
        return Err(VmRuntimeError::new(format!(
            "unsupported .sclc version {}",
            bytes[4]
        )));
    }

    let mut cursor = 5;

    let constant_count = read_u32(bytes, &mut cursor, "constant count")? as usize;
    let mut constants = Vec::with_capacity(constant_count);
    for _ in 0..constant_count {
        constants.push(decode_constant(bytes, &mut cursor)?);
    }

    let instruction_count = read_u32(bytes, &mut cursor, "instruction count")? as usize;
    let mut instructions = Vec::with_capacity(instruction_count);
    for _ in 0..instruction_count {
        instructions.push(decode_instruction(bytes, &mut cursor)?);
    }

    if cursor != bytes.len() {
        return Err(VmRuntimeError::new("invalid .sclc payload: trailing bytes"));
    }

    Ok(BytecodeChunk {
        constants,
        instructions,
    })
}

fn encode_count(output: &mut Vec<u8>, value: usize, label: &str) -> Result<(), VmCompileError> {
    let narrowed = u32::try_from(value)
        .map_err(|_| VmCompileError::new(format!("{} exceeds .sclc encoding limit", label)))?;
    output.extend_from_slice(&narrowed.to_le_bytes());
    Ok(())
}

fn encode_string(output: &mut Vec<u8>, value: &str) -> Result<(), VmCompileError> {
    encode_count(output, value.len(), "string length")?;
    output.extend_from_slice(value.as_bytes());
    Ok(())
}

fn encode_constant(output: &mut Vec<u8>, value: &Value) -> Result<(), VmCompileError> {
    match value {
        Value::Int(v) => {
            output.push(0);
            output.extend_from_slice(&v.to_le_bytes());
        }
        Value::Float(v) => {
            output.push(1);
            output.extend_from_slice(&v.to_le_bytes());
        }
        Value::String(v) => {
            output.push(2);
            encode_string(output, v)?;
        }
        Value::Bool(v) => {
            output.push(3);
            output.push(u8::from(*v));
        }
        Value::Nil => {
            output.push(4);
        }
        _ => {
            return Err(VmCompileError::new(format!(
                "cannot encode VM constant type '{}' into .sclc",
                value.type_name()
            )));
        }
    }
    Ok(())
}

fn encode_instruction(
    output: &mut Vec<u8>,
    instruction: &Instruction,
) -> Result<(), VmCompileError> {
    match instruction {
        Instruction::LoadConst(index) => {
            output.push(0);
            encode_count(output, *index, "constant index")?;
        }
        Instruction::LoadGlobal(name) => {
            output.push(1);
            encode_string(output, name)?;
        }
        Instruction::StoreGlobal(name) => {
            output.push(2);
            encode_string(output, name)?;
        }
        Instruction::Add => output.push(3),
        Instruction::Subtract => output.push(4),
        Instruction::Multiply => output.push(5),
        Instruction::Divide => output.push(6),
        Instruction::Modulo => output.push(7),
        Instruction::And => output.push(8),
        Instruction::Negate => output.push(9),
        Instruction::Not => output.push(10),
        Instruction::Equal => output.push(11),
        Instruction::NotEqual => output.push(12),
        Instruction::Less => output.push(13),
        Instruction::LessEqual => output.push(14),
        Instruction::Greater => output.push(15),
        Instruction::GreaterEqual => output.push(16),
        Instruction::Print => output.push(17),
        Instruction::Pop => output.push(18),
        Instruction::Return => output.push(19),
    }
    Ok(())
}

fn read_u8(bytes: &[u8], cursor: &mut usize, label: &str) -> Result<u8, VmRuntimeError> {
    if *cursor >= bytes.len() {
        return Err(VmRuntimeError::new(format!(
            "invalid .sclc payload: missing {}",
            label
        )));
    }
    let value = bytes[*cursor];
    *cursor += 1;
    Ok(value)
}

fn read_u32(bytes: &[u8], cursor: &mut usize, label: &str) -> Result<u32, VmRuntimeError> {
    if bytes.len().saturating_sub(*cursor) < 4 {
        return Err(VmRuntimeError::new(format!(
            "invalid .sclc payload: missing {}",
            label
        )));
    }
    let mut buf = [0_u8; 4];
    buf.copy_from_slice(&bytes[*cursor..*cursor + 4]);
    *cursor += 4;
    Ok(u32::from_le_bytes(buf))
}

fn read_i64(bytes: &[u8], cursor: &mut usize, label: &str) -> Result<i64, VmRuntimeError> {
    if bytes.len().saturating_sub(*cursor) < 8 {
        return Err(VmRuntimeError::new(format!(
            "invalid .sclc payload: missing {}",
            label
        )));
    }
    let mut buf = [0_u8; 8];
    buf.copy_from_slice(&bytes[*cursor..*cursor + 8]);
    *cursor += 8;
    Ok(i64::from_le_bytes(buf))
}

fn read_f64(bytes: &[u8], cursor: &mut usize, label: &str) -> Result<f64, VmRuntimeError> {
    if bytes.len().saturating_sub(*cursor) < 8 {
        return Err(VmRuntimeError::new(format!(
            "invalid .sclc payload: missing {}",
            label
        )));
    }
    let mut buf = [0_u8; 8];
    buf.copy_from_slice(&bytes[*cursor..*cursor + 8]);
    *cursor += 8;
    Ok(f64::from_le_bytes(buf))
}

fn read_string(bytes: &[u8], cursor: &mut usize, label: &str) -> Result<String, VmRuntimeError> {
    let len = read_u32(bytes, cursor, label)? as usize;
    if bytes.len().saturating_sub(*cursor) < len {
        return Err(VmRuntimeError::new(format!(
            "invalid .sclc payload: truncated {}",
            label
        )));
    }
    let raw = &bytes[*cursor..*cursor + len];
    *cursor += len;
    std::str::from_utf8(raw)
        .map(|value| value.to_string())
        .map_err(|err| VmRuntimeError::new(format!("invalid .sclc utf-8 string: {}", err)))
}

fn decode_constant(bytes: &[u8], cursor: &mut usize) -> Result<Value, VmRuntimeError> {
    let tag = read_u8(bytes, cursor, "constant tag")?;
    match tag {
        0 => Ok(Value::Int(read_i64(bytes, cursor, "int constant")?)),
        1 => Ok(Value::Float(read_f64(bytes, cursor, "float constant")?)),
        2 => Ok(Value::String(read_string(
            bytes,
            cursor,
            "string constant",
        )?)),
        3 => {
            let flag = read_u8(bytes, cursor, "bool constant")?;
            match flag {
                0 => Ok(Value::Bool(false)),
                1 => Ok(Value::Bool(true)),
                _ => Err(VmRuntimeError::new("invalid .sclc bool constant value")),
            }
        }
        4 => Ok(Value::Nil),
        _ => Err(VmRuntimeError::new(format!(
            "invalid .sclc constant tag {}",
            tag
        ))),
    }
}

fn decode_instruction(bytes: &[u8], cursor: &mut usize) -> Result<Instruction, VmRuntimeError> {
    let opcode = read_u8(bytes, cursor, "instruction opcode")?;
    match opcode {
        0 => Ok(Instruction::LoadConst(
            read_u32(bytes, cursor, "constant index")? as usize,
        )),
        1 => Ok(Instruction::LoadGlobal(read_string(
            bytes,
            cursor,
            "global name",
        )?)),
        2 => Ok(Instruction::StoreGlobal(read_string(
            bytes,
            cursor,
            "global name",
        )?)),
        3 => Ok(Instruction::Add),
        4 => Ok(Instruction::Subtract),
        5 => Ok(Instruction::Multiply),
        6 => Ok(Instruction::Divide),
        7 => Ok(Instruction::Modulo),
        8 => Ok(Instruction::And),
        9 => Ok(Instruction::Negate),
        10 => Ok(Instruction::Not),
        11 => Ok(Instruction::Equal),
        12 => Ok(Instruction::NotEqual),
        13 => Ok(Instruction::Less),
        14 => Ok(Instruction::LessEqual),
        15 => Ok(Instruction::Greater),
        16 => Ok(Instruction::GreaterEqual),
        17 => Ok(Instruction::Print),
        18 => Ok(Instruction::Pop),
        19 => Ok(Instruction::Return),
        _ => Err(VmRuntimeError::new(format!(
            "invalid .sclc opcode {}",
            opcode
        ))),
    }
}

struct Compiler {
    chunk: BytecodeChunk,
}

impl Compiler {
    fn new() -> Self {
        Self {
            chunk: BytecodeChunk::new(),
        }
    }

    fn compile_program(&mut self, program: &Program) -> Result<(), VmCompileError> {
        let statement_count = program.statements.len();
        for (index, statement) in program.statements.iter().enumerate() {
            let is_last = index + 1 == statement_count;
            self.compile_statement(statement)?;
            if !is_last && statement_produces_value(statement) {
                self.emit(Instruction::Pop);
            }
        }
        self.emit(Instruction::Return);
        Ok(())
    }

    fn compile_statement(&mut self, statement: &Stmt) -> Result<(), VmCompileError> {
        match statement {
            Stmt::VarDecl {
                name, initializer, ..
            } => {
                self.compile_expr(initializer)?;
                self.emit(Instruction::StoreGlobal(name.clone()));
                Ok(())
            }
            Stmt::Print { expr } => {
                self.compile_expr(expr)?;
                self.emit(Instruction::Print);
                Ok(())
            }
            Stmt::Expr(expr) => self.compile_expr(expr),
            Stmt::If { .. } => Err(VmCompileError::new(
                "if statements are not yet supported by the VM",
            )),
            Stmt::While { .. } => Err(VmCompileError::new(
                "while statements are not yet supported by the VM",
            )),
            Stmt::For { .. } => Err(VmCompileError::new(
                "for loops are not yet supported by the VM",
            )),
            Stmt::ForIn { .. } => Err(VmCompileError::new(
                "for-in loops are not yet supported by the VM",
            )),
            Stmt::Use { .. } => Err(VmCompileError::new(
                "use statements are not yet supported by the VM",
            )),
            Stmt::DestructureDecl { .. } => Err(VmCompileError::new(
                "destructuring declarations are not yet supported by the VM",
            )),
            Stmt::FunctionDef { .. } => Err(VmCompileError::new(
                "function definitions are not yet supported by the VM",
            )),
            Stmt::Return { .. } => Err(VmCompileError::new(
                "return statements are not yet supported by the VM",
            )),
            Stmt::Test { .. } => Err(VmCompileError::new(
                "test blocks are not yet supported by the VM",
            )),
            Stmt::Assert { .. } => Err(VmCompileError::new(
                "assert statements are not yet supported by the VM",
            )),
        }
    }

    fn compile_expr(&mut self, expr: &Expr) -> Result<(), VmCompileError> {
        if let Some(value) = const_eval(expr)? {
            self.emit_constant(value);
            return Ok(());
        }

        match expr {
            Expr::Int(value) => self.emit_constant(Value::Int(*value)),
            Expr::Float(value) => self.emit_constant(Value::Float(*value)),
            Expr::String { value, .. } => self.emit_constant(Value::String(value.clone())),
            Expr::Bool(value) => self.emit_constant(Value::Bool(*value)),
            Expr::Nil => self.emit_constant(Value::Nil),
            Expr::Variable(name) => self.emit(Instruction::LoadGlobal(name.clone())),
            Expr::Assign { name, value } => {
                self.compile_expr(value)?;
                self.emit(Instruction::StoreGlobal(name.clone()));
            }
            Expr::Grouping(inner) => self.compile_expr(inner)?,
            Expr::Unary { op, rhs } => {
                self.compile_expr(rhs)?;
                self.emit(match op {
                    UnaryOp::Negate => Instruction::Negate,
                    UnaryOp::Not => Instruction::Not,
                });
            }
            Expr::Binary { lhs, op, rhs } => {
                self.compile_expr(lhs)?;
                self.compile_expr(rhs)?;
                self.emit(match op {
                    BinaryOp::Add => Instruction::Add,
                    BinaryOp::Subtract => Instruction::Subtract,
                    BinaryOp::Multiply => Instruction::Multiply,
                    BinaryOp::Divide => Instruction::Divide,
                    BinaryOp::Modulo => Instruction::Modulo,
                    BinaryOp::And => Instruction::And,
                    BinaryOp::Equal => Instruction::Equal,
                    BinaryOp::NotEqual => Instruction::NotEqual,
                    BinaryOp::Less => Instruction::Less,
                    BinaryOp::LessEqual => Instruction::LessEqual,
                    BinaryOp::Greater => Instruction::Greater,
                    BinaryOp::GreaterEqual => Instruction::GreaterEqual,
                });
            }
            Expr::Call { .. } => {
                return Err(VmCompileError::new(
                    "function calls are not yet supported by the VM",
                ))
            }
            Expr::Member { .. } => {
                return Err(VmCompileError::new(
                    "member access is not yet supported by the VM",
                ))
            }
            Expr::Coalesce { .. } => {
                return Err(VmCompileError::new(
                    "coalesce expressions are not yet supported by the VM",
                ))
            }
            Expr::OrReturn { .. } => {
                return Err(VmCompileError::new(
                    "or-return expressions are not yet supported by the VM",
                ))
            }
            Expr::PanicUnwrap(_) => {
                return Err(VmCompileError::new(
                    "panic unwrap is not yet supported by the VM",
                ))
            }
            Expr::Match { .. } => {
                return Err(VmCompileError::new(
                    "match expressions are not yet supported by the VM",
                ))
            }
            Expr::ListLiteral(_) => {
                return Err(VmCompileError::new(
                    "list literals are not yet supported by the VM",
                ))
            }
            Expr::ListComprehension { .. } => {
                return Err(VmCompileError::new(
                    "list comprehensions are not yet supported by the VM",
                ))
            }
            Expr::MapLiteral(_) => {
                return Err(VmCompileError::new(
                    "map literals are not yet supported by the VM",
                ))
            }
            Expr::Index { .. } => {
                return Err(VmCompileError::new(
                    "index expressions are not yet supported by the VM",
                ))
            }
            Expr::Function { .. } => {
                return Err(VmCompileError::new(
                    "inline function expressions are not yet supported by the VM",
                ))
            }
        }
        Ok(())
    }

    fn emit_constant(&mut self, value: Value) {
        let index = self.chunk.constants.len();
        self.chunk.constants.push(value);
        self.chunk.instructions.push(Instruction::LoadConst(index));
    }

    fn emit(&mut self, instruction: Instruction) {
        self.chunk.instructions.push(instruction);
    }
}

struct Vm {
    stack: Vec<Value>,
    globals: HashMap<String, Value>,
    ip: usize,
}

impl Vm {
    fn new() -> Self {
        Self {
            stack: Vec::new(),
            globals: HashMap::new(),
            ip: 0,
        }
    }

    fn execute(&mut self, chunk: &BytecodeChunk) -> Result<Value, VmRuntimeError> {
        self.ip = 0;
        while let Some(instruction) = chunk.instructions.get(self.ip) {
            self.ip += 1;
            match instruction {
                Instruction::LoadConst(index) => {
                    let constant = chunk
                        .constants
                        .get(*index)
                        .ok_or_else(|| VmRuntimeError::new("constant index out of bounds"))?;
                    self.stack.push(constant.clone());
                }
                Instruction::LoadGlobal(name) => {
                    let value = self.globals.get(name).cloned().ok_or_else(|| {
                        VmRuntimeError::new(format!("unknown variable '{}'", name))
                    })?;
                    self.stack.push(value);
                }
                Instruction::StoreGlobal(name) => {
                    let value = self
                        .stack
                        .last()
                        .cloned()
                        .ok_or_else(|| VmRuntimeError::new("stack underflow on store"))?;
                    self.globals.insert(name.clone(), value);
                }
                Instruction::Add => self.binary_numeric_or_string(|a, b| a + b)?,
                Instruction::Subtract => self.binary_numeric(|a, b| a - b)?,
                Instruction::Multiply => self.binary_numeric(|a, b| a * b)?,
                Instruction::Divide => self.binary_numeric(|a, b| a / b)?,
                Instruction::Modulo => self.binary_numeric(|a, b| a % b)?,
                Instruction::And => {
                    let rhs = self.pop()?;
                    let lhs = self.pop()?;
                    self.stack
                        .push(Value::Bool(lhs.is_truthy() && rhs.is_truthy()));
                }
                Instruction::Negate => {
                    let value = self.pop()?;
                    match value {
                        Value::Int(v) => self.stack.push(Value::Int(-v)),
                        Value::Float(v) => self.stack.push(Value::Float(-v)),
                        other => {
                            return Err(VmRuntimeError::new(format!(
                                "cannot negate '{}'",
                                other.type_name()
                            )))
                        }
                    }
                }
                Instruction::Not => {
                    let value = self.pop()?;
                    self.stack.push(Value::Bool(!value.is_truthy()));
                }
                Instruction::Equal => self.binary_compare(|a, b| a == b)?,
                Instruction::NotEqual => self.binary_compare(|a, b| a != b)?,
                Instruction::Less => self.binary_order(|a, b| a < b)?,
                Instruction::LessEqual => self.binary_order(|a, b| a <= b)?,
                Instruction::Greater => self.binary_order(|a, b| a > b)?,
                Instruction::GreaterEqual => self.binary_order(|a, b| a >= b)?,
                Instruction::Print => {
                    let value = self
                        .stack
                        .last()
                        .ok_or_else(|| VmRuntimeError::new("stack underflow on print"))?;
                    println!("{}", value);
                }
                Instruction::Pop => {
                    let _ = self.pop()?;
                }
                Instruction::Return => {
                    return Ok(self.stack.pop().unwrap_or(Value::Nil));
                }
            }
        }
        Ok(self.stack.pop().unwrap_or(Value::Nil))
    }

    fn pop(&mut self) -> Result<Value, VmRuntimeError> {
        self.stack
            .pop()
            .ok_or_else(|| VmRuntimeError::new("stack underflow"))
    }

    fn binary_numeric(&mut self, op: impl Fn(f64, f64) -> f64) -> Result<(), VmRuntimeError> {
        let rhs = self.pop()?;
        let lhs = self.pop()?;
        let a = to_f64(&lhs).ok_or_else(|| {
            VmRuntimeError::new(format!("expected numeric lhs, got '{}'", lhs.type_name()))
        })?;
        let b = to_f64(&rhs).ok_or_else(|| {
            VmRuntimeError::new(format!("expected numeric rhs, got '{}'", rhs.type_name()))
        })?;
        let result = op(a, b);
        if matches!(lhs, Value::Int(_)) && matches!(rhs, Value::Int(_)) && result.fract() == 0.0 {
            self.stack.push(Value::Int(result as i64));
        } else {
            self.stack.push(Value::Float(result));
        }
        Ok(())
    }

    fn binary_numeric_or_string(
        &mut self,
        op: impl Fn(f64, f64) -> f64,
    ) -> Result<(), VmRuntimeError> {
        let rhs = self.pop()?;
        let lhs = self.pop()?;
        match (&lhs, &rhs) {
            (Value::String(a), Value::String(b)) => {
                self.stack.push(Value::String(format!("{}{}", a, b)));
                return Ok(());
            }
            _ => {}
        }

        let a = to_f64(&lhs).ok_or_else(|| {
            VmRuntimeError::new(format!("expected numeric lhs, got '{}'", lhs.type_name()))
        })?;
        let b = to_f64(&rhs).ok_or_else(|| {
            VmRuntimeError::new(format!("expected numeric rhs, got '{}'", rhs.type_name()))
        })?;
        let result = op(a, b);
        if matches!(lhs, Value::Int(_)) && matches!(rhs, Value::Int(_)) && result.fract() == 0.0 {
            self.stack.push(Value::Int(result as i64));
        } else {
            self.stack.push(Value::Float(result));
        }
        Ok(())
    }

    fn binary_compare(
        &mut self,
        compare: impl Fn(&Value, &Value) -> bool,
    ) -> Result<(), VmRuntimeError> {
        let rhs = self.pop()?;
        let lhs = self.pop()?;
        self.stack.push(Value::Bool(compare(&lhs, &rhs)));
        Ok(())
    }

    fn binary_order(&mut self, compare: impl Fn(f64, f64) -> bool) -> Result<(), VmRuntimeError> {
        let rhs = self.pop()?;
        let lhs = self.pop()?;
        let a = to_f64(&lhs).ok_or_else(|| {
            VmRuntimeError::new(format!("expected numeric lhs, got '{}'", lhs.type_name()))
        })?;
        let b = to_f64(&rhs).ok_or_else(|| {
            VmRuntimeError::new(format!("expected numeric rhs, got '{}'", rhs.type_name()))
        })?;
        self.stack.push(Value::Bool(compare(a, b)));
        Ok(())
    }
}

fn statement_produces_value(statement: &Stmt) -> bool {
    matches!(
        statement,
        Stmt::VarDecl { .. } | Stmt::Expr(_) | Stmt::Print { .. }
    )
}

fn const_eval(expr: &Expr) -> Result<Option<Value>, VmCompileError> {
    match expr {
        Expr::Int(v) => Ok(Some(Value::Int(*v))),
        Expr::Float(v) => Ok(Some(Value::Float(*v))),
        Expr::String { value, .. } => Ok(Some(Value::String(value.clone()))),
        Expr::Bool(v) => Ok(Some(Value::Bool(*v))),
        Expr::Nil => Ok(Some(Value::Nil)),
        Expr::Grouping(inner) => const_eval(inner),
        Expr::Unary { op, rhs } => {
            let Some(rhs) = const_eval(rhs)? else {
                return Ok(None);
            };
            match op {
                UnaryOp::Negate => match rhs {
                    Value::Int(v) => Ok(Some(Value::Int(-v))),
                    Value::Float(v) => Ok(Some(Value::Float(-v))),
                    _ => Ok(None),
                },
                UnaryOp::Not => Ok(Some(Value::Bool(!rhs.is_truthy()))),
            }
        }
        Expr::Binary { lhs, op, rhs } => {
            let Some(lhs) = const_eval(lhs)? else {
                return Ok(None);
            };
            let Some(rhs) = const_eval(rhs)? else {
                return Ok(None);
            };
            eval_const_binary(op, lhs, rhs)
        }
        _ => Ok(None),
    }
}

fn eval_const_binary(
    op: &BinaryOp,
    lhs: Value,
    rhs: Value,
) -> Result<Option<Value>, VmCompileError> {
    let value = match op {
        BinaryOp::Add => match (lhs, rhs) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a + b),
            (Value::Float(a), Value::Float(b)) => Value::Float(a + b),
            (Value::Int(a), Value::Float(b)) => Value::Float(a as f64 + b),
            (Value::Float(a), Value::Int(b)) => Value::Float(a + b as f64),
            (Value::String(a), Value::String(b)) => Value::String(format!("{}{}", a, b)),
            _ => return Ok(None),
        },
        BinaryOp::Subtract => const_numeric_binary(lhs, rhs, |a, b| a - b)?,
        BinaryOp::Multiply => const_numeric_binary(lhs, rhs, |a, b| a * b)?,
        BinaryOp::Divide => const_numeric_binary(lhs, rhs, |a, b| a / b)?,
        BinaryOp::Modulo => const_numeric_binary(lhs, rhs, |a, b| a % b)?,
        BinaryOp::And => Value::Bool(lhs.is_truthy() && rhs.is_truthy()),
        BinaryOp::Equal => Value::Bool(lhs == rhs),
        BinaryOp::NotEqual => Value::Bool(lhs != rhs),
        BinaryOp::Less => Value::Bool(const_numeric_order(lhs, rhs, |a, b| a < b)?),
        BinaryOp::LessEqual => Value::Bool(const_numeric_order(lhs, rhs, |a, b| a <= b)?),
        BinaryOp::Greater => Value::Bool(const_numeric_order(lhs, rhs, |a, b| a > b)?),
        BinaryOp::GreaterEqual => Value::Bool(const_numeric_order(lhs, rhs, |a, b| a >= b)?),
    };
    Ok(Some(value))
}

fn const_numeric_binary(
    lhs: Value,
    rhs: Value,
    op: impl Fn(f64, f64) -> f64,
) -> Result<Value, VmCompileError> {
    let a =
        to_f64(&lhs).ok_or_else(|| VmCompileError::new("constant folding expected numeric lhs"))?;
    let b =
        to_f64(&rhs).ok_or_else(|| VmCompileError::new("constant folding expected numeric rhs"))?;
    let result = op(a, b);
    if matches!(lhs, Value::Int(_)) && matches!(rhs, Value::Int(_)) && result.fract() == 0.0 {
        Ok(Value::Int(result as i64))
    } else {
        Ok(Value::Float(result))
    }
}

fn const_numeric_order(
    lhs: Value,
    rhs: Value,
    compare: impl Fn(f64, f64) -> bool,
) -> Result<bool, VmCompileError> {
    let a =
        to_f64(&lhs).ok_or_else(|| VmCompileError::new("constant folding expected numeric lhs"))?;
    let b =
        to_f64(&rhs).ok_or_else(|| VmCompileError::new("constant folding expected numeric rhs"))?;
    Ok(compare(a, b))
}

fn to_f64(value: &Value) -> Option<f64> {
    match value {
        Value::Int(v) => Some(*v as f64),
        Value::Float(v) => Some(*v),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn encode_decode_roundtrip() {
        let chunk = BytecodeChunk {
            constants: vec![
                Value::Int(7),
                Value::Float(1.5),
                Value::String("ok".to_string()),
                Value::Bool(true),
                Value::Nil,
            ],
            instructions: vec![
                Instruction::LoadConst(0),
                Instruction::LoadConst(2),
                Instruction::Print,
                Instruction::Return,
            ],
        };

        let encoded = encode_chunk(&chunk).expect("encoding should succeed");
        let decoded = decode_chunk(&encoded).expect("decoding should succeed");
        assert_eq!(decoded, chunk);
    }

    #[test]
    fn decode_rejects_bad_magic() {
        let bytes = b"BAD!\x01";
        let err = decode_chunk(bytes).expect_err("bad magic should fail");
        assert!(err.to_string().contains("bad magic"));
    }
}
