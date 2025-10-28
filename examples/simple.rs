use std::io;

use n0_error::{StackError, e, meta};

use self::error::CopyError;
use crate::error::{InvalidArgsError, OperationError};
fn main() {
    println!("### Read");
    let err = operation().err().unwrap();
    print(err);

    println!("### BadRequest");
    // let err = e!(CopyError::BadRequest { missing: 32 });
    let err = CopyError::BadRequest {
        missing: 32,
        meta: meta(),
    };

    print(err);

    println!("### InvalidArgs");
    let err = e!(InvalidArgsError::FailedToParse);
    let err = e!(CopyError::InvalidArgs, err);
    // let err = e!(CopyError::InvalidArgs { source: err });
    // let err = CopyError!(InvalidArgs { source: err });
    let err = e!(OperationError::Copy { source: err });
    print(err);
}

fn _some_fn() -> Result<(), CopyError> {
    // Err! macro works like e! but wraps in Err
    Err(e!(CopyError::Read, io::Error::other("yada")))
}

fn operation() -> Result<(), OperationError> {
    let res = copy();
    res?;
    Ok(())
}

fn copy() -> Result<(), CopyError> {
    read().map_err(|err| e!(CopyError::Read { source: err }))?;
    Ok(())
}

fn read() -> io::Result<()> {
    let err = io::Error::new(io::ErrorKind::AddrInUse, "bad addr");
    Err(err)
}

fn print(err: impl StackError) {
    println!("== display == ");
    println!("{err}");
    println!("== display alt == ");
    println!("{err:#}");
    println!("== display report == ");
    println!("{}", err.report());
    println!("== display debug == ");
    println!("{err:?}");
    println!("== display debug alt == ");
    println!("{err:#?}");
}

pub mod error {
    use std::io;

    use n0_error::{StackError, stack_error};

    #[stack_error(derive, add_meta, from_sources)]
    pub enum OperationError {
        /// Failed to copy
        Copy { source: CopyError },
    }

    #[stack_error(derive, add_meta)]
    pub enum CopyError {
        /// Read error
        Read {
            // If sources only impl std::error::Error but not StackError, we need to mark them with `std`
            // This is needed unfortunately because we don't have specialization in rust,
            // otherwise we can't get both sources with locations (StackError) and foreign sources (std error)
            #[error(std_err)]
            source: io::Error,
        },
        /// Write error
        // Another io::Error, so can't use from, but can use the constructors
        Write {
            #[error(std_err)]
            source: io::Error,
        },
        #[error("Bad request - missing characters: {missing} {}", missing * 2)]
        BadRequest {
            missing: usize,
        },
        #[error(transparent)]
        InvalidArgs {
            #[error(from)]
            source: InvalidArgsError,
        },
        Foo,
    }

    #[stack_error(add_meta)]
    #[derive(StackError)]
    pub enum InvalidArgsError {
        /// Failed to parse arguments
        FailedToParse {},
    }
}
