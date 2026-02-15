# Getting Started with Sculk Development

**Welcome to Sculk!** This guide will help you start contributing to SCALF's native compiler.

## Prerequisites

- Rust 1.75+ installed
- Basic understanding of compilers (lexing, parsing, IR)
- Familiarity with SCALF language syntax
- Patience (native compilation is complex!)

## Quick Start

```bash
# Navigate to sculk directory
cd sculk

# Run tests
cargo test

# Build the library
cargo build

# Build with Cranelift backend
cargo build --features cranelift-backend

# Run benchmarks (when implemented)
cargo bench
```

## Project Structure

```
sculk/
├── src/
│   ├── lib.rs           # Main entry point - START HERE
│   ├── ir/              # Intermediate Representation
│   ├── codegen/         # AST → IR compilation
│   ├── backend/         # IR → Native code
│   └── optimize/        # IR optimization passes
├── tests/               # Integration tests
├── examples/            # Example SCALF programs
└── runtime/             # Minimal runtime library
```

## Development Workflow

### Phase 0: Setup (You Are Here)

**Goal**: Understand the codebase structure

1. Read `README.md` - understand the architecture
2. Read `src/lib.rs` - main compiler interface
3. Read `src/ir/mod.rs` - IR definition
4. Run `cargo test` - make sure everything compiles

### Phase 1: IR Design

**Goal**: Define what the intermediate representation looks like

1. **Study** `src/ir/mod.rs` - current IR design
2. **Extend** IR with missing features:
   - Arrays/lists
   - Closures
   - Pattern matching
   - Error handling
3. **Test** IR construction with `ir::builder`

Example:
```rust
use sculk::ir::builder::*;
use sculk::ir::*;

// Build a simple function
let mut func_builder = FunctionBuilder::new(
    "add".to_string(),
    vec![
        Parameter { name: "a".to_string(), ty: Type::Int },
        Parameter { name: "b".to_string(), ty: Type::Int },
    ],
    Type::Int,
);

let entry = func_builder.create_block("entry".to_string());
func_builder.switch_to_block(entry);

// a + b
func_builder.add_instruction(Instruction::BinOp {
    dest: "result".to_string(),
    op: BinOp::Add,
    left: Value::Var("a".to_string()),
    right: Value::Var("b".to_string()),
});

// return result
func_builder.set_terminator(Terminator::Return(Some(Value::Var("result".to_string()))));

let function = func_builder.build();
println!("{}", function);
```

### Phase 2: AST → IR Lowering

**Goal**: Convert SCALF AST to Sculk IR

1. **Connect** to main SCALF compiler (in `../src/parser/ast.rs`)
2. **Implement** `codegen/lowering.rs`:
   - Walk AST nodes
   - Generate IR instructions
   - Handle control flow (if/while/for)
   - Handle function calls
3. **Test** with simple SCALF programs

This is where you'll spend most of Phase 1-2 months.

### Phase 3: Cranelift Integration

**Goal**: Compile IR to native code using Cranelift

1. **Study** Cranelift documentation: https://cranelift.dev/
2. **Implement** `backend/cranelift.rs`:
   - Create Cranelift module
   - Translate IR to Cranelift IR
   - Compile functions
   - Link into executable
3. **Test** with "hello world" example

Example structure:
```rust
// Pseudo-code for Cranelift integration
fn compile_function(ir_func: &ir::Function) -> Result<CompiledCode> {
    let mut builder_context = FunctionBuilderContext::new();
    let mut func = Function::new();
    
    // Translate IR blocks to Cranelift blocks
    for block in &ir_func.blocks {
        translate_block(block, &mut func)?;
    }
    
    // Compile to machine code
    let code = cranelift_compile(func)?;
    Ok(code)
}
```

### Phase 4: Optimization

**Goal**: Make generated code faster

1. **Implement** optimization passes in `optimize/`:
   - Constant folding
   - Dead code elimination
   - Common subexpression elimination
   - Function inlining
2. **Benchmark** before/after optimizations
3. **Tune** based on real SCALF programs

### Phase 5: Testing & Integration

**Goal**: Make it production-ready

1. **Add** comprehensive tests
2. **Integrate** with main SCALF compiler
3. **Add** `scalf build --native` command
4. **Document** everything
5. **Fix** bugs found by users

## Learning Resources

### Compilers
- [Crafting Interpreters](https://craftinginterpreters.com/) - Great intro
- [Engineering a Compiler](https://www.elsevier.com/books/engineering-a-compiler/cooper/978-0-12-088478-0)
- [LLVM Tutorial](https://llvm.org/docs/tutorial/) - Similar concepts

### Cranelift Specific
- [Cranelift Docs](https://cranelift.dev/)
- [Wasmtime Source](https://github.com/bytecodealliance/wasmtime) - Real usage
- [Cranelift Code Generation](https://github.com/bytecodealliance/wasmtime/blob/main/cranelift/docs/ir.md)

### IR Design
- [LLVM IR](https://llvm.org/docs/LangRef.html) - Industry standard
- [Cranelift IR](https://github.com/bytecodealliance/wasmtime/blob/main/cranelift/docs/ir.md)
- [QBE IL](https://c9x.me/compile/doc/il.html) - Simpler alternative

## Common Tasks

### Add a new IR instruction

1. Add to `ir::Instruction` enum
2. Add translation in `codegen/lowering.rs`
3. Add codegen in `backend/cranelift.rs`
4. Add tests

### Add an optimization pass

1. Create new file in `optimize/`
2. Implement `Pass` trait
3. Add to default optimization pipeline
4. Add benchmarks

### Debug IR

```rust
use sculk::ir::printer::print_ir;

let module = /* ... */;
print_ir(&module);  // Prints human-readable IR
```

## Tips

1. **Start simple** - Don't try to compile everything at once
2. **Test constantly** - Every feature needs tests
3. **Read Cranelift examples** - Learn from their tests
4. **Ask questions** - Open issues if stuck
5. **Document as you go** - Future you will thank you

## Roadmap Checklist

- [ ] IR supports all SCALF expressions
- [ ] IR supports all SCALF statements
- [ ] AST → IR lowering works for simple programs
- [ ] Cranelift backend compiles "hello world"
- [ ] Functions and calls work
- [ ] Control flow works (if/while/for)
- [ ] Variables and scoping work
- [ ] Collections (lists/maps) work
- [ ] Stdlib functions callable
- [ ] Basic optimizations implemented
- [ ] Performance matches bytecode
- [ ] Performance beats Python
- [ ] Integrated with `scalf build --native`
- [ ] Production-ready

## Current Status

- [x] Project structure created
- [x] IR design started
- [ ] AST lowering (YOU START HERE)
- [ ] Cranelift integration
- [ ] Everything else

## Need Help?

- Read the code comments
- Check `/sculk/tests` for examples
- Open an issue in main SCALF repo
- Read Cranelift docs

Good luck! You're building something awesome. 🚀
