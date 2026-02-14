# SCALF v0.1.1

This patch release improves standalone build minimization so generated binaries only compile the runtime features each script actually uses.

## Highlights

- Real per-script feature detection for `scalf build`
  - Build analysis now detects `net`, `json`, and `crypto` usage from AST nodes.
  - Scripts that do not use networking/JSON/crypto can now build with fewer compiled dependencies.
- Feature-split runtime gating
  - `json` and `crypto` are now independent optional features.
  - `net` depends on `json` and `crypto` where needed.
- Better runtime behavior when features are not compiled in
  - Clear runtime errors for unavailable APIs such as `json.parse`, `json.stringify`, and `crypto.sha256` in minimized builds.
- Expanded coverage tests
  - Added feature-detection tests for json-only, crypto-only, and mixed net/json/crypto scripts.

## Dependency / Feature Changes

- `serde_json` is now optional behind `json`.
- `sha2` is now optional behind `crypto`.
- Feature layout:
  - `default = ["net", "json", "crypto"]`
  - `net = ["dep:reqwest", "json", "crypto"]`
  - `json = ["dep:serde_json"]`
  - `crypto = ["dep:sha2"]`

## Validation

Verified build matrix:

- `cargo check`
- `cargo check --no-default-features`
- `cargo check --no-default-features --features json`
- `cargo check --no-default-features --features crypto`
- `cargo check --no-default-features --features net`

Plus targeted tests:

- `cargo test --bin scalf build_feature_detection`

## Notes

- `scalf build` now reports selected features in its output, making it easier to confirm minimization results per script.
