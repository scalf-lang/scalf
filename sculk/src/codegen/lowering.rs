//! AST to IR lowering
//!
//! This module converts SCALF AST nodes to Sculk IR.

use crate::ir::*;
use crate::CompileError;
use scalf::parser::ast::{BinaryOp as AstBinaryOp, Expr, Program, Stmt};

/// Lower SCALF AST to Sculk IR.
pub struct Lowering {
    /// Next temporary variable ID.
    next_temp: usize,
}

impl Lowering {
    /// Create a new lowering context.
    pub fn new() -> Self {
        Self { next_temp: 0 }
    }

    /// Generate a unique temporary variable name.
    pub fn new_temp(&mut self) -> String {
        let temp = format!("t{}", self.next_temp);
        self.next_temp += 1;
        temp
    }

    /// Lower a parsed SCALF program.
    pub fn lower_program(
        &mut self,
        ast: &Program,
        module_name: &str,
    ) -> Result<Module, CompileError> {
        let mut module = Module::new(module_name.to_string());
        let mut top_level = Vec::new();

        for stmt in &ast.statements {
            match stmt {
                Stmt::FunctionDef {
                    name, params, body, ..
                } => {
                    let function = self.lower_function(name, params, body)?;
                    module.functions.push(function);
                }
                other => top_level.push(other.clone()),
            }
        }

        // Allow script-style programs without explicit `def main`.
        if !top_level.is_empty() {
            let main = self.lower_function("main", &[], &top_level)?;
            module.functions.push(main);
        }

        Ok(module)
    }

    fn lower_function(
        &mut self,
        name: &str,
        params: &[scalf::parser::ast::Param],
        body: &[Stmt],
    ) -> Result<Function, CompileError> {
        if !params.is_empty() {
            return Err(CompileError::NotImplemented(
                "function parameters are not implemented in sculk yet",
            ));
        }

        let return_type = self.infer_return_type(name, body);
        let mut function = Function::new(name.to_string(), vec![], return_type.clone());
        let mut entry = BasicBlock::new("entry".to_string());

        let mut has_terminator = false;
        for stmt in body {
            if has_terminator {
                break;
            }
            match stmt {
                Stmt::Print { expr } => {
                    let arg = self.lower_expr_value(expr, &mut entry)?;
                    entry.instructions.push(Instruction::Call {
                        dest: None,
                        func: "print".to_string(),
                        args: vec![arg],
                    });
                }
                Stmt::Return { value } => {
                    let lowered = match value {
                        Some(expr) => Some(self.lower_expr_value(expr, &mut entry)?),
                        None => None,
                    };
                    entry.terminator = Terminator::Return(lowered);
                    has_terminator = true;
                }
                Stmt::VarDecl {
                    name, initializer, ..
                } => {
                    let value = self.lower_expr_value(initializer, &mut entry)?;
                    entry.instructions.push(Instruction::Assign {
                        dest: name.clone(),
                        value,
                    });
                }
                Stmt::Expr(expr) => {
                    self.lower_expression_statement(expr, &mut entry)?;
                }
                _ => {
                    return Err(CompileError::NotImplemented(
                        "this statement kind is not implemented in sculk lowering yet",
                    ));
                }
            }
        }

        if !has_terminator {
            entry.terminator = match return_type {
                Type::Int => Terminator::Return(Some(Value::Int(0))),
                _ => Terminator::Return(None),
            };
        }

        function.blocks.push(entry);
        Ok(function)
    }

    fn infer_return_type(&self, name: &str, body: &[Stmt]) -> Type {
        for stmt in body {
            if let Stmt::Return { value } = stmt {
                return match value {
                    None | Some(Expr::Nil) => Type::Void,
                    _ => Type::Int,
                };
            }
        }

        if name == "main" {
            Type::Int
        } else {
            Type::Void
        }
    }

    fn lower_expression_statement(
        &mut self,
        expr: &Expr,
        block: &mut BasicBlock,
    ) -> Result<(), CompileError> {
        match expr {
            Expr::Call { callee, args } => {
                let func_name =
                    self.expr_to_function_name(callee)
                        .ok_or(CompileError::NotImplemented(
                            "only direct function calls are supported in expression statements",
                        ))?;
                let mut lowered_args = Vec::with_capacity(args.len());
                for arg in args {
                    lowered_args.push(self.lower_expr_value(arg, block)?);
                }
                block.instructions.push(Instruction::Call {
                    dest: None,
                    func: func_name,
                    args: lowered_args,
                });
                Ok(())
            }
            Expr::Assign { name, value } => {
                let lowered = self.lower_expr_value(value, block)?;
                block.instructions.push(Instruction::Assign {
                    dest: name.clone(),
                    value: lowered,
                });
                Ok(())
            }
            _ => {
                let _ = self.lower_expr_value(expr, block)?;
                Ok(())
            }
        }
    }

    fn lower_expr_value(
        &mut self,
        expr: &Expr,
        block: &mut BasicBlock,
    ) -> Result<Value, CompileError> {
        match expr {
            Expr::Int(value) => Ok(Value::Int(*value)),
            Expr::Float(value) => Ok(Value::Float(*value)),
            Expr::String { value, .. } => Ok(Value::String(value.clone())),
            Expr::Bool(value) => Ok(Value::Bool(*value)),
            Expr::Nil => Ok(Value::Null),
            Expr::Variable(name) => Ok(Value::Var(name.clone())),
            Expr::Grouping(inner) => self.lower_expr_value(inner, block),
            Expr::Binary { lhs, op, rhs } => {
                let left = self.lower_expr_value(lhs, block)?;
                let right = self.lower_expr_value(rhs, block)?;
                let dest = self.new_temp();
                let op = map_binary_op(*op)?;
                block.instructions.push(Instruction::BinOp {
                    dest: dest.clone(),
                    op,
                    left,
                    right,
                });
                Ok(Value::Var(dest))
            }
            Expr::Call { callee, args } => {
                let func_name =
                    self.expr_to_function_name(callee)
                        .ok_or(CompileError::NotImplemented(
                            "only direct function calls are supported in expression position",
                        ))?;
                let mut lowered_args = Vec::with_capacity(args.len());
                for arg in args {
                    lowered_args.push(self.lower_expr_value(arg, block)?);
                }
                let dest = self.new_temp();
                block.instructions.push(Instruction::Call {
                    dest: Some(dest.clone()),
                    func: func_name,
                    args: lowered_args,
                });
                Ok(Value::Var(dest))
            }
            _ => Err(CompileError::NotImplemented(
                "this expression kind is not implemented in sculk lowering yet",
            )),
        }
    }

    fn expr_to_function_name(&self, callee: &Expr) -> Option<String> {
        match callee {
            Expr::Variable(name) => Some(name.clone()),
            _ => None,
        }
    }
}

impl Default for Lowering {
    fn default() -> Self {
        Self::new()
    }
}

fn map_binary_op(op: AstBinaryOp) -> Result<BinOp, CompileError> {
    let mapped = match op {
        AstBinaryOp::Add => BinOp::Add,
        AstBinaryOp::Subtract => BinOp::Sub,
        AstBinaryOp::Multiply => BinOp::Mul,
        AstBinaryOp::Divide => BinOp::Div,
        AstBinaryOp::Modulo => BinOp::Mod,
        AstBinaryOp::And => BinOp::And,
        AstBinaryOp::Equal => BinOp::Eq,
        AstBinaryOp::NotEqual => BinOp::Ne,
        AstBinaryOp::Less => BinOp::Lt,
        AstBinaryOp::LessEqual => BinOp::Le,
        AstBinaryOp::Greater => BinOp::Gt,
        AstBinaryOp::GreaterEqual => BinOp::Ge,
    };
    Ok(mapped)
}
