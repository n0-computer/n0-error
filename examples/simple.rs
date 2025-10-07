use n0_error::StackError;
use std::io;

use self::error::CopyError;
use crate::error::{InvalidArgsError, OperationError};

fn main() {
    println!("### Read");
    let err = operation().err().unwrap();
    print(err);

    println!("### BadRequest");
    let err = CopyError::bad_request(32);
    print(err);

    println!("### InvalidArgs");
    let err = InvalidArgsError::failed_to_parse();
    let err = CopyError::invalid_args(err);
    let err = OperationError::copy(err);
    print(err);
}

fn operation() -> Result<(), OperationError> {
    let res = copy();
    res?;
    Ok(())
}

fn copy() -> Result<(), CopyError> {
    read().map_err(CopyError::read)?;
    // let res = read();
    // match res {
    //     Ok(()) => Ok(()),
    //     Err(err) => Err(CopyError::read(err)),
    // }
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

    #[n0_error::add_location]
    #[derive(n0_error::Error)]
    pub enum OperationError {
        /// Failed to copy
        Copy {
            #[from]
            source: CopyError,
        },
    }

    #[n0_error::add_location]
    #[derive(n0_error::Error)]
    pub enum CopyError {
        /// Read error
        Read {
            // If sources only impl std::error::Error but not StackError, we need to mark them with `std`
            // This is needed unfortunately because we don't have specialization in rust,
            // otherwise we can't get both sources with locations (StackError) and foreign sources (std error)
            #[std]
            source: io::Error,
        },
        /// Write error
        // Another io::Error, so can't use from, but can use the constructors
        Write {
            #[std]
            source: io::Error,
        },
        #[display("Bad request - missing characters: {missing} {}", missing * 2)]
        BadRequest { missing: usize },
        #[transparent]
        InvalidArgs {
            #[from]
            source: InvalidArgsError,
        },
    }

    #[n0_error::add_location]
    #[derive(n0_error::Error)]
    pub enum InvalidArgsError {
        /// Failed to parse arguments
        FailedToParse {},
    }
}
