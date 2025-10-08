# Repository Guidelines

## Project Structure & Module Organization
- `n0-error/` — main Rust crate (library).
  - `src/` core implementation (`any.rs`, `error.rs`, `ext.rs`, `location.rs`, `lib.rs`).
  - `tests/` integration tests (e.g., `transparent.rs`).
  - `examples/` runnable examples (e.g., `simple.rs`).
  - `n0-error-macros/` procedural macro crate used by the library.
  - `Makefile.toml` tasks for formatting.

## Build, Test, and Development Commands
- Build: `cd n0-error && cargo build` — builds library and macros.
- Test: `cd n0-error && cargo test` — runs unit + integration tests.
- Example: `cd n0-error && cargo run --example simple` — runs the example.
- Format: `cd n0-error && cargo make format` — apply rustfmt with project settings.
- Format check: `cd n0-error && cargo make format-check` — verify formatting in CI.

## Coding Style & Naming Conventions
- Use Rust defaults: 4-space indent, `snake_case` for fns/modules, `UpperCamelCase` for types.
- Keep public APIs documented; prefer concise `Display` messages and informative `Debug`.
- Formatting via rustfmt; project supplies flags through `Makefile.toml` (imports grouping, doc comment formatting, reorder imports).

## Testing Guidelines
- Framework: Rust test harness with unit tests (in `src`) and integration tests (in `tests/`).
- Naming: test files under `tests/` end with `.rs`; functions annotated with `#[test]`.
- Run all tests with `cargo test`. Use `RUST_BACKTRACE=1` to see richer reports.

## Commit & Pull Request Guidelines
- Commits: short, imperative subject (e.g., "fix: clarify error display").
- PRs: include summary, rationale, and links to issues. Call out breaking changes.
- Add screenshots or example outputs only if they clarify behavior (e.g., formatted reports).

## Agent Notes
- Attribute migration: see `n0-error/CHANGELOG.md` for moving from legacy `#[from]`, `#[std]`, `#[transparent]` to `#[error(...)]`.
