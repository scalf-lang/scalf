# SCALF v0.2.1

Release date: February 14, 2026

This is a patch release generated from `git log 0.2.0..HEAD` and `git diff 0.2.0..HEAD`.

## Summary

- Migrated error documentation links from `scalf-lang.dev` (didn't exist) to `scalf-lang.github.io`.
- Bumped crate version from `0.2.0` to `0.2.1`.

## Changes Since 0.2.0

- Error/reporting URL updates across:
  - CLI lex error output (`src/main.rs`)
  - Pretty parse/type/lint formatting (`src/errors/pretty.rs`)
  - REPL lex error output (`src/repl/mod.rs`)
  - Runtime error docs URL generation (`src/runtime/mod.rs`)

## Why This Release

- Ensures all surfaced diagnostics point to the current docs host.
- Keeps error links consistent across CLI, REPL, and runtime paths.
