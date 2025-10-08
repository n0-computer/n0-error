# Changelog

## Unreleased — Attribute System Overhaul

Breaking change: move from multiple attributes to a unified `#[error(...)]` syntax.

Attribute mapping
- `#[from]` → `#[error(from)]`
- `#[std]` → `#[error(std_err)]`
- `#[transparent]` → `#[error(transparent)]`
- Optional: mark any field as the source with `#[error(source)]` (defaults to a field named `source` if none is marked).

Top-level defaults (reduce per-variant noise)
- `#[error(from_sources)]` on the item/enum: derive `From<SourceType>` for each source field automatically.
- `#[error(std_sources)]` on the item/enum: treat all sources as `std::error::Error`.
  - Per-field overrides:
    - `#[error(stack_err)]` to use `StackError` for that field even when `std_sources` is set.
    - `#[error(std_err)]` to force std behavior for a specific field when `std_sources` is not set.

Before → After examples
```rust
#[derive(Error)]
enum In {
    #[transparent]
    Wrap { #[from] source: Leaf },
}
```
```rust
#[derive(Error)]
enum In {
    #[error(transparent)]
    Wrap { #[error(from)] source: Leaf },
}
```

Top-level defaults example
```rust
#[derive(Error)]
#[error(from_sources, std_sources)]
enum CopyError {
    #[display("read")]
    Read { source: std::io::Error }, // auto From<io::Error>, std source

    #[display("nested")]
    Nested { #[error(stack_err)] source: Other }, // stack source override
}
```

Notes
- Transparent variants must have exactly one source field.
- If no field is marked `#[error(source)]`, a field literally named `source` is used by default.
- Use `#[display("...")]` for messages; otherwise `Display` falls back to the variant/struct name.
