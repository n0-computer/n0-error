# n0-error

**Experimental / Work in progress**

A error library that supports tracking the call-site location of errors. Also features an anyhow-style `AnyError`.

```rust
use n0_error::{e, add_meta, StackError, Result, StackResultExt, StdResultExt};

// Adds a `meta` field to all variants to track call-site error location.
#[add_meta]
// Derives the various impls for our error.
#[derive(StackError)]
// Automatically create From impls from the error sources
#[error(from_sources)]
enum MyError {
    // A custom validation error
    #[error("bad input: {count}")]
    BadInput { count: usize },
    // Wrap a std::io::Error as a source (std error)
    #[error("IO error")]
    Io {
        #[error(std_err)]
        source: std::io::Error,
    },
}

// A function that returns a std io::Error
fn fail_io() -> std::io::Result<()> {
    Err(std::io::Error::other("io failed"))
}

// An outer function returning our custom MyError
fn some_fn(count: usize) -> Result<(), MyError> {
    if count == 13 {
        return Err(e!(MyError::BadInput { count }));
    }
    // We have a From impl for std::io::Error on our error.
    fail_io()?;
    // Without the From impl, we'd need to forward the error manually.
    // The `e` macro can assist here, so that we don't have to declare the `meta` field manually.
    fail_io().map_err(|source| e!(MyError::Io, source))?;
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
    let res = run();
    if let Err(err) = run() {
        println!("{err}");
        // This prints:
        // Error: failed at some_fn
        // Caused by:
        //    0: bad input: 0
    }
    Ok(())
}

// You can also use the macros with tuple structs or enums.
// In this case the meta field will be added as the last field.

#[add_meta]
#[derive(StackError)]
#[error("tuple fail ({_0})")]
struct TupleStruct(u32);

#[add_meta]
#[derive(StackError)]
enum TupleEnum {
    #[error("io failed")]
    Io(#[error(source, std_err)] std::io::Error),
}

```
