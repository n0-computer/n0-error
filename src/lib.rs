#![doc = include_str!("../README.md")]
#![deny(missing_docs)]

pub use n0_error_macros::{StackError, stack_error};

extern crate self as n0_error;

mod any;
mod error;
mod ext;
mod macros;
mod meta;
#[cfg(test)]
mod tests;

pub use self::{any::*, error::*, ext::*, macros::*, meta::*};

/// `Result` type alias where the error type defaults to [`AnyError`].
pub type Result<T = (), E = AnyError> = std::result::Result<T, E>;

/// Returns a result with the error type set to [`AnyError`].
///
/// Equivalent to `Ok::<_, AnyError>(value)`.
#[allow(non_snake_case)]
pub fn Ok<T>(value: T) -> Result<T, AnyError> {
    std::result::Result::Ok(value)
}
