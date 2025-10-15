pub use n0_error_macros::{Error, add_meta};

extern crate self as n0_error;

mod any;
mod error;
mod ext;
mod macros;
mod meta;
#[cfg(test)]
mod tests;

pub use self::{any::*, error::*, ext::*, macros::*, meta::*};

pub type Result<T = (), E = AnyError> = std::result::Result<T, E>;

#[allow(non_snake_case)]
pub fn Ok<T>(value: T) -> Result<T, AnyError> {
    std::result::Result::Ok(value)
}
