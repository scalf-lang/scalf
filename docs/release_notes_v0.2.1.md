# SCALF v0.2.1

This patch release improves script compatibility and runtime stability, especially for router-style and map-backed object patterns.

## Highlights

- Import behavior fixes
  - String imports now correctly support local files (non-HTTP specs) and resolve relative to the current source file.
  - Build feature detection no longer treats local string imports as network imports.

- Parser compatibility improvements
  - Added logical `and` operator support.
  - Map literal keys now accept keyword-style keys (for example: `use: ...`).
  - Member/index assignment forms are accepted and lowered internally.

- Runtime behavior fixes
  - Map-backed callable members now dispatch correctly before built-in map methods.
  - Improved method-call context for map-backed objects (`this` and captured state behavior).
  - Better compatibility with real router middleware/handler patterns.

- Typechecker improvements
  - Equality checks against `nil` are accepted in broader valid cases.
  - Improved inference/validation for boolean logic expressions (`and` and boolean `or` usage).

## Validation

Validated with targeted lexer/parser/typechecker/runtime tests and end-to-end router example execution.

