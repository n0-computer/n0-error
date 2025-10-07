pub use n0_error_macros::{Error, add_location};

extern crate self as n0_error;

mod any;
mod error;
mod ext;
mod location;
#[cfg(test)]
mod tests;

pub use self::{any::*, error::*, ext::*, location::*};

pub type Result<T = (), E = AnyError> = std::result::Result<T, E>;

#[allow(non_snake_case)]
pub fn Ok<T>(value: T) -> Result<T, AnyError> {
    std::result::Result::Ok(value)
}
