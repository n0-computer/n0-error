# n0-error

**Experimental / Work in progress**

A error library that supports tracking the call-site location of errors. Also features an anyhow-style `AnyError`.

```rust
use n0_error::{err, stack_error, StackError, Result, StackResultExt, StdResultExt};

/// The `stack_error` macro controls how to turn our enum into a `StackError`.
///
/// * `add_meta` adds a field to all variants to track the call-site error location
/// * `derive` adds `#[derive(StackError)]`
/// * `from_sources` creates `From` impls for the error sources
#[stack_error(derive, add_meta, from_sources)]
enum MyError {
    /// We can define the error message with the `error` attribute
    #[error("bad input ({count})")]
    BadInput { count: usize },
    /// Or we can define a variant as `transparent`, which forwards the Display impl to the error source
    #[error(transparent)]
    Io {
        /// For sources that do not implement `StackError`, we have to mark the source as `std_err`.
        #[error(std_err)]
        source: std::io::Error,
    },
}

// A function that returns a std::io::Error
fn fail_io() -> std::io::Result<()> {
    Err(std::io::Error::other("io failed"))
}

// An outer function returning our custom MyError
fn some_fn(count: usize) -> Result<(), MyError> {
    if count == 13 {
        // The `err` macro constructs a `StackError` while automatically adding the `meta` field.
        return Err(err!(MyError::BadInput { count }));
    }
    // We have a `From` impl for `std::io::Error` on our error.
    fail_io()?;
    // Without the From impl, we'd need to forward the error manually.
    // The `err` macro can assist here, so that we don't have to declare the `meta` field manually.
    fail_io().map_err(|source| err!(MyError::Io, source))?;
    Ok(())
}

// A main function that returns AnyError (via the crate's Result alias)
fn run() -> Result<()> {
    // We can add context to errors via the result extensions.
    // The `context` function adds context to any `StackError`.
    some_fn(13).context("failed at some_fn")?;
    // To add context to std errors, we have to use `std_context` from `StdResultExt`.
    fail_io().std_context("failed at fail_io")?;
    Ok(())
}

fn main() -> Result<()> {
    let err = run().unwrap_err();
    assert_eq!(
        format!("{err:#}"),
        "failed at some_fn: bad input (13)"
    );
    Ok(())
}

/// You can also use the macros with tuple structs or enums.
/// In this case the meta field will be added as the last field.
#[stack_error(derive, add_meta)]
#[error("tuple fail ({_0})")]
struct TupleStruct(u32);

#[stack_error(derive, add_meta)]
enum TupleEnum {
    #[error("io failed")]
    Io(#[error(source, std_err)] std::io::Error),
}

```
