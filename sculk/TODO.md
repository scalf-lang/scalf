# Sculk TODO List

## Phase 0: Foundation ✅
- [x] Project structure
- [x] Cargo.toml with dependencies
- [x] Basic IR definitions
- [x] README documentation
- [x] Getting started guide

## Phase 1: IR Design (Week 1-2)
- [ ] Extend IR with all SCALF types
- [ ] Add support for closures in IR
- [ ] Add support for pattern matching in IR
- [ ] Add error handling types
- [ ] Validate IR correctness
- [ ] Write IR builder tests
- [ ] Document IR design decisions

## Phase 2: AST Lowering (Week 3-6)
- [ ] Link to main SCALF AST definitions
- [ ] Lower expressions:
  - [ ] Literals (int, float, string, bool)
  - [ ] Binary operations
  - [ ] Unary operations
  - [ ] Variable references
  - [ ] Function calls
- [ ] Lower statements:
  - [ ] Variable declarations
  - [ ] Assignments
  - [ ] If/else
  - [ ] While loops
  - [ ] For loops
  - [ ] Return statements
- [ ] Lower functions:
  - [ ] Function definitions
  - [ ] Parameters
  - [ ] Closures (future)
- [ ] Lower collections:
  - [ ] List literals
  - [ ] Map literals
  - [ ] Indexing
  - [ ] Comprehensions
- [ ] Handle control flow:
  - [ ] Break statements
  - [ ] Continue statements
  - [ ] Early returns
- [ ] Test with real SCALF programs

## Phase 3: Cranelift Integration (Week 7-10)
- [ ] Add Cranelift dependencies
- [ ] Create Cranelift module context
- [ ] Translate IR types to Cranelift types
- [ ] Translate IR instructions to Cranelift IR:
  - [ ] Arithmetic operations
  - [ ] Comparisons
  - [ ] Function calls
  - [ ] Memory operations
- [ ] Translate control flow:
  - [ ] Branches
  - [ ] Conditional branches
  - [ ] Returns
- [ ] Compile simple function
- [ ] Generate executable binary
- [ ] Run "hello world" natively
- [ ] Debug symbols (DWARF)

## Phase 4: Runtime Library (Week 11-12)
- [ ] Reference counting implementation
- [ ] Memory allocation/deallocation
- [ ] String runtime functions
- [ ] List runtime functions
- [ ] Map runtime functions
- [ ] Built-in functions in native code:
  - [ ] print()
  - [ ] len()
  - [ ] Type conversions
- [ ] Stdlib integration (http, fs, json, etc.)

## Phase 5: Optimization (Week 13-16)
- [ ] Constant folding pass
- [ ] Dead code elimination
- [ ] Common subexpression elimination
- [ ] Function inlining
- [ ] Pass manager infrastructure
- [ ] Optimization benchmarks
- [ ] Tune based on real programs

## Phase 6: Testing & Polish (Week 17-20)
- [ ] Comprehensive test suite
- [ ] Test all SCALF language features
- [ ] Benchmark vs bytecode
- [ ] Benchmark vs Python/Lua/Ruby
- [ ] Fix correctness bugs
- [ ] Improve error messages
- [ ] Add safety checks

## Phase 7: Integration (Week 21-24)
- [ ] Add `--native` flag to main compiler
- [ ] Add `--backend` flag (cranelift/qbe/custom)
- [ ] Cross-compilation support
- [ ] CI/CD for multiple platforms
- [ ] Documentation for users
- [ ] Migration guide from bytecode

## Phase 8: Production Ready (Week 25+)
- [ ] Beta testing with real users
- [ ] Performance tuning
- [ ] Stability fixes
- [ ] Release as part of SCALF v0.9.0+

## Future Enhancements
- [ ] QBE backend (simpler alternative)
- [ ] Custom x86-64 backend (learning project)
- [ ] JIT compilation mode
- [ ] Profile-guided optimization
- [ ] Link-time optimization
- [ ] Parallel compilation

## Current Blockers
- None yet - just getting started!

## Notes
- Don't start Phase 2 until Phase 1 IR is solid
- Don't start Phase 3 until simple programs lower correctly
- Test constantly - every feature needs tests
- Benchmark often - don't guess at performance

---

**Last Updated**: 2026-02-12
**Status**: Phase 0 complete, starting Phase 1
