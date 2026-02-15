//! Cranelift backend implementation
//!
//! Uses Cranelift for production-quality code generation.

#[cfg(feature = "cranelift-backend")]
use std::collections::HashMap;
#[cfg(feature = "cranelift-backend")]
use std::ffi::{c_char, CStr};

#[cfg(feature = "cranelift-backend")]
use cranelift::codegen::isa::OwnedTargetIsa;
#[cfg(feature = "cranelift-backend")]
use cranelift::prelude::{
    settings, types, AbiParam, Block, Configurable, FunctionBuilder, FunctionBuilderContext,
    InstBuilder, IntCC, TrapCode, Value as ClifValue,
};
#[cfg(feature = "cranelift-backend")]
use cranelift_jit::{JITBuilder, JITModule};
#[cfg(feature = "cranelift-backend")]
use cranelift_module::{DataDescription, DataId, FuncId, Linkage, Module as ClifModule};
#[cfg(feature = "cranelift-backend")]
use cranelift_object::{ObjectBuilder, ObjectModule};

use crate::backend::Backend;
use crate::ir::{BinOp, Function as IrFunction, Instruction, Module, Terminator, Type, Value};
use crate::CompileError;

/// Cranelift code generator.
pub struct CraneliftBackend {
    /// Target ISA (instruction set architecture).
    target: String,
}

impl CraneliftBackend {
    /// Create a new Cranelift backend.
    pub fn new() -> Result<Self, CompileError> {
        Ok(Self {
            target: std::env::consts::ARCH.to_string(),
        })
    }

    /// Compile IR to object bytes.
    #[cfg(feature = "cranelift-backend")]
    fn compile_module(&self, module: &Module) -> Result<Vec<u8>, CompileError> {
        let isa = build_native_isa()?;
        let builder = ObjectBuilder::new(isa, "sculk", cranelift_module::default_libcall_names())
            .map_err(module_error)?;
        let mut object_module = ObjectModule::new(builder);

        let _compiled = compile_into_module(&mut object_module, module)?;
        let product = object_module.finish();
        product.emit().map_err(|err| {
            CompileError::BackendError(format!("failed to emit object bytes: {}", err))
        })
    }

    /// JIT-compile and run `main`, returning the process-like exit code.
    #[cfg(feature = "cranelift-backend")]
    pub fn run_main(&self, module: &Module) -> Result<i64, CompileError> {
        let mut jit_builder =
            JITBuilder::new(cranelift_module::default_libcall_names()).map_err(module_error)?;
        jit_builder.symbol("__sculk_print_cstr", sculk_print_cstr as *const u8);
        jit_builder.symbol("__sculk_print_i64", sculk_print_i64 as *const u8);

        let mut jit_module = JITModule::new(jit_builder);
        let compiled = compile_into_module(&mut jit_module, module)?;
        jit_module.finalize_definitions().map_err(module_error)?;

        let main_id =
            compiled.functions.get("main").copied().ok_or_else(|| {
                CompileError::InvalidIR("module has no `main` function".to_string())
            })?;

        let code = jit_module.get_finalized_function(main_id);
        // SAFETY: We declare and define `main` with signature `fn() -> i64` in this backend.
        let main_fn: extern "C" fn() -> i64 = unsafe { std::mem::transmute(code) };
        Ok(main_fn())
    }

    /// Get configured target architecture name.
    pub fn target(&self) -> &str {
        &self.target
    }
}

impl Default for CraneliftBackend {
    fn default() -> Self {
        Self::new().unwrap_or_else(|err| panic!("failed to create CraneliftBackend: {}", err))
    }
}

impl Backend for CraneliftBackend {
    fn generate(&self, module: &Module) -> Result<Vec<u8>, CompileError> {
        #[cfg(feature = "cranelift-backend")]
        {
            self.compile_module(module)
        }

        #[cfg(not(feature = "cranelift-backend"))]
        {
            let _ = module;
            Err(CompileError::BackendError(
                "Cranelift backend not enabled. Compile with --features cranelift-backend"
                    .to_string(),
            ))
        }
    }

    fn name(&self) -> &'static str {
        "cranelift"
    }

    fn supported_targets(&self) -> &[&str] {
        &["x86_64", "aarch64"]
    }
}

#[cfg(feature = "cranelift-backend")]
struct RuntimeImports {
    print_cstr: FuncId,
    print_i64: FuncId,
}

#[cfg(feature = "cranelift-backend")]
struct CompiledFunctions {
    functions: HashMap<String, FuncId>,
}

#[cfg(feature = "cranelift-backend")]
struct StringPool {
    next_id: usize,
    ids: HashMap<String, DataId>,
}

#[cfg(feature = "cranelift-backend")]
impl StringPool {
    fn new() -> Self {
        Self {
            next_id: 0,
            ids: HashMap::new(),
        }
    }

    fn data_id_for<M: ClifModule>(
        &mut self,
        module: &mut M,
        text: &str,
    ) -> Result<DataId, CompileError> {
        if let Some(id) = self.ids.get(text) {
            return Ok(*id);
        }

        let name = format!("__sculk_str_{}", self.next_id);
        self.next_id += 1;

        let mut data = DataDescription::new();
        let mut bytes = text.as_bytes().to_vec();
        bytes.push(0);
        data.define(bytes.into_boxed_slice());

        let data_id = module
            .declare_data(&name, Linkage::Local, false, false)
            .map_err(module_error)?;
        module.define_data(data_id, &data).map_err(module_error)?;
        self.ids.insert(text.to_string(), data_id);
        Ok(data_id)
    }
}

#[cfg(feature = "cranelift-backend")]
fn compile_into_module<M: ClifModule>(
    module: &mut M,
    ir_module: &Module,
) -> Result<CompiledFunctions, CompileError> {
    let runtime = declare_runtime_imports(module)?;
    let mut functions = HashMap::new();

    for function in &ir_module.functions {
        let signature = make_signature(module, function)?;
        let func_id = module
            .declare_function(&function.name, Linkage::Export, &signature)
            .map_err(module_error)?;
        functions.insert(function.name.clone(), func_id);
    }

    let mut string_pool = StringPool::new();

    for function in &ir_module.functions {
        let func_id = *functions.get(&function.name).ok_or_else(|| {
            CompileError::InvalidIR(format!("missing function id for {}", function.name))
        })?;

        let mut context = module.make_context();
        context.func.signature = make_signature(module, function)?;

        {
            let mut builder_context = FunctionBuilderContext::new();
            let mut builder = FunctionBuilder::new(&mut context.func, &mut builder_context);
            lower_function(
                module,
                function,
                &runtime,
                &functions,
                &mut string_pool,
                &mut builder,
            )?;
            builder.seal_all_blocks();
            builder.finalize();
        }

        module
            .define_function(func_id, &mut context)
            .map_err(module_error)?;
    }

    Ok(CompiledFunctions { functions })
}

#[cfg(feature = "cranelift-backend")]
fn lower_function<M: ClifModule>(
    module: &mut M,
    ir_function: &IrFunction,
    runtime: &RuntimeImports,
    function_ids: &HashMap<String, FuncId>,
    string_pool: &mut StringPool,
    builder: &mut FunctionBuilder,
) -> Result<(), CompileError> {
    if ir_function.blocks.is_empty() {
        return Err(CompileError::InvalidIR(format!(
            "function '{}' has no basic blocks",
            ir_function.name
        )));
    }

    let mut block_ids: HashMap<String, Block> = HashMap::new();
    for block in &ir_function.blocks {
        let id = builder.create_block();
        block_ids.insert(block.label.clone(), id);
    }

    let entry = *block_ids
        .get(&ir_function.blocks[0].label)
        .ok_or_else(|| CompileError::InvalidIR("missing entry block".to_string()))?;
    builder.append_block_params_for_function_params(entry);

    let mut variables: HashMap<String, ClifValue> = HashMap::new();
    for (index, param) in ir_function.params.iter().enumerate() {
        let Some(value) = builder.block_params(entry).get(index) else {
            return Err(CompileError::InvalidIR(format!(
                "missing block parameter {} for function {}",
                index, ir_function.name
            )));
        };
        variables.insert(param.name.clone(), *value);
    }

    let print_cstr_ref = module.declare_func_in_func(runtime.print_cstr, builder.func);
    let print_i64_ref = module.declare_func_in_func(runtime.print_i64, builder.func);

    let mut function_refs = HashMap::new();
    for (name, id) in function_ids {
        let func_ref = module.declare_func_in_func(*id, builder.func);
        function_refs.insert(name.clone(), func_ref);
    }

    for block in &ir_function.blocks {
        let clif_block = *block_ids
            .get(&block.label)
            .ok_or_else(|| CompileError::InvalidIR(format!("missing block {}", block.label)))?;

        builder.switch_to_block(clif_block);

        for instruction in &block.instructions {
            lower_instruction(
                module,
                instruction,
                &mut variables,
                string_pool,
                &function_refs,
                print_cstr_ref,
                print_i64_ref,
                builder,
            )?;
        }

        lower_terminator(
            module,
            &block.terminator,
            &mut variables,
            string_pool,
            &block_ids,
            builder,
        )?;
    }

    Ok(())
}

#[cfg(feature = "cranelift-backend")]
fn lower_instruction<M: ClifModule>(
    module: &mut M,
    instruction: &Instruction,
    variables: &mut HashMap<String, ClifValue>,
    string_pool: &mut StringPool,
    function_refs: &HashMap<String, cranelift::codegen::ir::FuncRef>,
    print_cstr_ref: cranelift::codegen::ir::FuncRef,
    print_i64_ref: cranelift::codegen::ir::FuncRef,
    builder: &mut FunctionBuilder,
) -> Result<(), CompileError> {
    match instruction {
        Instruction::Assign { dest, value } => {
            let lowered = lower_value(module, value, variables, string_pool, builder)?;
            variables.insert(dest.clone(), lowered);
            Ok(())
        }
        Instruction::BinOp {
            dest,
            op,
            left,
            right,
        } => {
            let lhs = lower_value(module, left, variables, string_pool, builder)?;
            let rhs = lower_value(module, right, variables, string_pool, builder)?;
            let result = lower_binop(*op, lhs, rhs, builder);
            variables.insert(dest.clone(), result);
            Ok(())
        }
        Instruction::Call { dest, func, args } => {
            if func == "print" {
                if args.len() != 1 {
                    return Err(CompileError::InvalidIR(
                        "print expects exactly one argument".to_string(),
                    ));
                }

                match &args[0] {
                    Value::String(_) => {
                        let arg = lower_value(module, &args[0], variables, string_pool, builder)?;
                        builder.ins().call(print_cstr_ref, &[arg]);
                    }
                    _ => {
                        let arg = lower_value(module, &args[0], variables, string_pool, builder)?;
                        let arg = value_to_i64(arg, builder);
                        builder.ins().call(print_i64_ref, &[arg]);
                    }
                }

                if let Some(dest) = dest {
                    let zero = builder.ins().iconst(types::I64, 0);
                    variables.insert(dest.clone(), zero);
                }
                return Ok(());
            }

            let func_ref = function_refs.get(func).copied().ok_or_else(|| {
                CompileError::InvalidIR(format!("unknown call target '{}'", func))
            })?;

            let mut lowered_args = Vec::with_capacity(args.len());
            for arg in args {
                let value = lower_value(module, arg, variables, string_pool, builder)?;
                lowered_args.push(value);
            }

            let call = builder.ins().call(func_ref, &lowered_args);
            if let Some(dest_name) = dest {
                let results = builder.inst_results(call);
                if let Some(first) = results.first().copied() {
                    variables.insert(dest_name.clone(), first);
                } else {
                    let zero = builder.ins().iconst(types::I64, 0);
                    variables.insert(dest_name.clone(), zero);
                }
            }
            Ok(())
        }
        Instruction::Load { .. } | Instruction::Store { .. } => Err(CompileError::NotImplemented(
            "load/store are not implemented in Cranelift backend yet",
        )),
    }
}

#[cfg(feature = "cranelift-backend")]
fn lower_terminator<M: ClifModule>(
    module: &mut M,
    terminator: &Terminator,
    variables: &mut HashMap<String, ClifValue>,
    string_pool: &mut StringPool,
    block_ids: &HashMap<String, Block>,
    builder: &mut FunctionBuilder,
) -> Result<(), CompileError> {
    match terminator {
        Terminator::Return(value) => {
            if let Some(value) = value {
                let lowered = lower_value(module, value, variables, string_pool, builder)?;
                builder.ins().return_(&[lowered]);
            } else {
                builder.ins().return_(&[]);
            }
            Ok(())
        }
        Terminator::Branch { target } => {
            let target = *block_ids.get(target).ok_or_else(|| {
                CompileError::InvalidIR(format!("unknown branch target '{}'", target))
            })?;
            builder.ins().jump(target, &[]);
            Ok(())
        }
        Terminator::CondBranch {
            cond,
            then_block,
            else_block,
        } => {
            let then_block = *block_ids.get(then_block).ok_or_else(|| {
                CompileError::InvalidIR(format!("unknown branch target '{}'", then_block))
            })?;
            let else_block = *block_ids.get(else_block).ok_or_else(|| {
                CompileError::InvalidIR(format!("unknown branch target '{}'", else_block))
            })?;

            let cond_value = lower_value(module, cond, variables, string_pool, builder)?;
            let cond_i64 = value_to_i64(cond_value, builder);
            let cond_bool = builder.ins().icmp_imm(IntCC::NotEqual, cond_i64, 0);
            builder
                .ins()
                .brif(cond_bool, then_block, &[], else_block, &[]);
            Ok(())
        }
        Terminator::Unreachable => {
            builder.ins().trap(TrapCode::unwrap_user(1));
            Ok(())
        }
    }
}

#[cfg(feature = "cranelift-backend")]
fn lower_value<M: ClifModule>(
    module: &mut M,
    value: &Value,
    variables: &HashMap<String, ClifValue>,
    string_pool: &mut StringPool,
    builder: &mut FunctionBuilder,
) -> Result<ClifValue, CompileError> {
    match value {
        Value::Var(name) => variables.get(name).copied().ok_or_else(|| {
            CompileError::InvalidIR(format!("unknown variable '{}' referenced in backend", name))
        }),
        Value::Int(value) => Ok(builder.ins().iconst(types::I64, *value)),
        Value::Float(value) => Ok(builder.ins().f64const(*value)),
        Value::Bool(value) => Ok(builder.ins().iconst(types::I64, i64::from(*value))),
        Value::Null => Ok(builder.ins().iconst(types::I64, 0)),
        Value::String(text) => {
            let data_id = string_pool.data_id_for(module, text)?;
            let global_value = module.declare_data_in_func(data_id, builder.func);
            let ptr_ty = module.target_config().pointer_type();
            Ok(builder.ins().global_value(ptr_ty, global_value))
        }
    }
}

#[cfg(feature = "cranelift-backend")]
fn lower_binop(
    op: BinOp,
    lhs: ClifValue,
    rhs: ClifValue,
    builder: &mut FunctionBuilder,
) -> ClifValue {
    match op {
        BinOp::Add => builder.ins().iadd(lhs, rhs),
        BinOp::Sub => builder.ins().isub(lhs, rhs),
        BinOp::Mul => builder.ins().imul(lhs, rhs),
        BinOp::Div => builder.ins().sdiv(lhs, rhs),
        BinOp::Mod => builder.ins().srem(lhs, rhs),
        BinOp::Eq => cmp_to_i64(builder.ins().icmp(IntCC::Equal, lhs, rhs), builder),
        BinOp::Ne => cmp_to_i64(builder.ins().icmp(IntCC::NotEqual, lhs, rhs), builder),
        BinOp::Lt => cmp_to_i64(builder.ins().icmp(IntCC::SignedLessThan, lhs, rhs), builder),
        BinOp::Le => cmp_to_i64(
            builder.ins().icmp(IntCC::SignedLessThanOrEqual, lhs, rhs),
            builder,
        ),
        BinOp::Gt => cmp_to_i64(
            builder.ins().icmp(IntCC::SignedGreaterThan, lhs, rhs),
            builder,
        ),
        BinOp::Ge => cmp_to_i64(
            builder
                .ins()
                .icmp(IntCC::SignedGreaterThanOrEqual, lhs, rhs),
            builder,
        ),
        BinOp::And => {
            let lhs_i64 = value_to_i64(lhs, builder);
            let rhs_i64 = value_to_i64(rhs, builder);
            let lhs_bool = builder.ins().icmp_imm(IntCC::NotEqual, lhs_i64, 0);
            let rhs_bool = builder.ins().icmp_imm(IntCC::NotEqual, rhs_i64, 0);
            let l = cmp_to_i64(lhs_bool, builder);
            let r = cmp_to_i64(rhs_bool, builder);
            builder.ins().band(l, r)
        }
        BinOp::Or => {
            let lhs_i64 = value_to_i64(lhs, builder);
            let rhs_i64 = value_to_i64(rhs, builder);
            let lhs_bool = builder.ins().icmp_imm(IntCC::NotEqual, lhs_i64, 0);
            let rhs_bool = builder.ins().icmp_imm(IntCC::NotEqual, rhs_i64, 0);
            let l = cmp_to_i64(lhs_bool, builder);
            let r = cmp_to_i64(rhs_bool, builder);
            builder.ins().bor(l, r)
        }
    }
}

#[cfg(feature = "cranelift-backend")]
fn value_to_i64(value: ClifValue, builder: &mut FunctionBuilder) -> ClifValue {
    let ty = builder.func.dfg.value_type(value);
    if ty == types::I64 {
        value
    } else if ty.is_int() {
        builder.ins().sextend(types::I64, value)
    } else {
        value
    }
}

#[cfg(feature = "cranelift-backend")]
fn cmp_to_i64(value: ClifValue, builder: &mut FunctionBuilder) -> ClifValue {
    value_to_i64(value, builder)
}

#[cfg(feature = "cranelift-backend")]
fn make_signature<M: ClifModule>(
    module: &M,
    function: &IrFunction,
) -> Result<cranelift::prelude::Signature, CompileError> {
    let mut signature = module.make_signature();

    for param in &function.params {
        let ty = map_type_to_clif(&param.ty)?;
        signature.params.push(AbiParam::new(ty));
    }

    if function.return_type != Type::Void {
        let return_type = map_type_to_clif(&function.return_type)?;
        signature.returns.push(AbiParam::new(return_type));
    }

    Ok(signature)
}

#[cfg(feature = "cranelift-backend")]
fn declare_runtime_imports<M: ClifModule>(module: &mut M) -> Result<RuntimeImports, CompileError> {
    let pointer_ty = module.target_config().pointer_type();

    let mut print_cstr_sig = module.make_signature();
    print_cstr_sig.params.push(AbiParam::new(pointer_ty));
    let print_cstr = module
        .declare_function("__sculk_print_cstr", Linkage::Import, &print_cstr_sig)
        .map_err(module_error)?;

    let mut print_i64_sig = module.make_signature();
    print_i64_sig.params.push(AbiParam::new(types::I64));
    let print_i64 = module
        .declare_function("__sculk_print_i64", Linkage::Import, &print_i64_sig)
        .map_err(module_error)?;

    Ok(RuntimeImports {
        print_cstr,
        print_i64,
    })
}

#[cfg(feature = "cranelift-backend")]
fn map_type_to_clif(ty: &Type) -> Result<cranelift::prelude::Type, CompileError> {
    match ty {
        Type::Int => Ok(types::I64),
        Type::Bool => Ok(types::I64),
        Type::Float => Ok(types::F64),
        Type::Void => Err(CompileError::InvalidIR(
            "void cannot be used as a concrete value type".to_string(),
        )),
        Type::String | Type::Ptr(_) | Type::Func { .. } => Err(CompileError::NotImplemented(
            "this IR type is not yet supported by the Cranelift backend",
        )),
    }
}

#[cfg(feature = "cranelift-backend")]
fn build_native_isa() -> Result<OwnedTargetIsa, CompileError> {
    let mut flags = settings::builder();
    flags.set("is_pic", "true").map_err(|err| {
        CompileError::BackendError(format!("failed to set Cranelift flag: {}", err))
    })?;

    let isa_builder = cranelift_native::builder().map_err(|msg| {
        CompileError::BackendError(format!(
            "host machine is not supported by Cranelift: {}",
            msg
        ))
    })?;

    isa_builder
        .finish(settings::Flags::new(flags))
        .map_err(module_error)
}

#[cfg(feature = "cranelift-backend")]
fn module_error(err: impl std::fmt::Display) -> CompileError {
    CompileError::BackendError(err.to_string())
}

#[cfg(feature = "cranelift-backend")]
extern "C" fn sculk_print_cstr(ptr: *const c_char) {
    if ptr.is_null() {
        println!();
        return;
    }

    // SAFETY: Called from generated code with a pointer to a nul-terminated static string.
    let text = unsafe { CStr::from_ptr(ptr) };
    println!("{}", text.to_string_lossy());
}

#[cfg(feature = "cranelift-backend")]
extern "C" fn sculk_print_i64(value: i64) {
    println!("{}", value);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_backend_creation() {
        let backend = CraneliftBackend::new();
        assert!(backend.is_ok());
    }
}
