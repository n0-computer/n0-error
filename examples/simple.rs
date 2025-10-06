mod error {
    use std::{fmt, io, panic::Location};

    #[n0_error::expand]
    pub enum OperationError {
        /// Failed to copy
        Copy { source: CopyError },
    }

    #[n0_error::expand]
    pub enum CopyError {
        /// Read error
        Read {
            #[from]
            source: io::Error,
        },
        /// Write error
        Write { source: io::Error },
        #[display("Bad request - missing characters: {missing}")]
        BadRequest { missing: usize },
        #[transparent]
        InvalidArgs { source: InvalidArgsError },
    }

    #[n0_error::expand]
    pub enum InvalidArgsError {
        /// Failed to parse arguments
        FailedToParse {},
    }
}

use std::io;

use n0_error::StackError;

use crate::error::{InvalidArgsError, OperationError};

use self::error::CopyError;

// fn main() {
//     println!("### InvalidArgs wrapped");
//     let inner = InvalidArgsError::failed_to_parse();
//     let err = CopyError::invalid_args(inner);
//     let err = OperationError::copy(err);
//     println!("== display debug == ");
//     println!("{err:?}");
// }
fn main() {
    println!("### Read");
    let inner = io::Error::new(io::ErrorKind::AddrInUse, "bad addr");
    let err = CopyError::read(inner);
    print(err);

    println!("### BadRequest");
    let err = CopyError::bad_request(32);
    print(err);

    println!("### InvalidArgs");
    let inner = InvalidArgsError::failed_to_parse();
    let err = CopyError::invalid_args(inner);
    print(err);

    println!("### InvalidArgs wrapped");
    let inner = InvalidArgsError::failed_to_parse();
    let err = CopyError::invalid_args(inner);
    let err = OperationError::copy(err);
    print(err);
}

fn print(err: impl StackError) {
    println!("== display == ");
    println!("{err}");
    println!("== display alt == ");
    println!("{err:#}");
    println!("== display debug == ");
    println!("{err:?}");
}
