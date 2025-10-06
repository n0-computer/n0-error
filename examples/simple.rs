mod error {
    use std::{fmt, io, panic::Location};
    enum CopyError {
        /// Read error
        Read { source: io::Error },
        /// Write error
        Write { source: io::Error },
    }

    pub mod expanded {
        use std::{fmt, io, panic::Location};

        use n0_error::{ErrorSource, SourceFormat, StackError};

        pub enum CopyError {
            /// Read error
            Read {
                source: io::Error,
                location: &'static Location<'static>,
            },
            /// Write error
            Write {
                source: io::Error,
                location: &'static Location<'static>,
            },
        }

        impl CopyError {
            #[track_caller]
            pub fn read(source: io::Error) -> Self {
                Self::Read {
                    source,
                    location: Location::caller(),
                }
            }

            #[track_caller]
            pub fn write(source: io::Error) -> Self {
                Self::Write {
                    source,
                    location: Location::caller(),
                }
            }
        }

        impl StackError for CopyError {
            fn location(&self) -> &'static Location<'static> {
                match self {
                    CopyError::Read { location, .. } => location,
                    CopyError::Write { location, .. } => location,
                }
            }

            fn source(&self) -> Option<n0_error::ErrorSource<'_>> {
                match self {
                    CopyError::Read { source, .. } => Some(ErrorSource::Std(source)),
                    CopyError::Write { source, .. } => Some(ErrorSource::Std(source)),
                }
            }

            fn display_plain(&self, f: &mut fmt::Formatter) -> fmt::Result {
                match self {
                    CopyError::Read { .. } => write!(f, "Read error"),
                    CopyError::Write { .. } => write!(f, "Write error"),
                }
            }
        }

        impl fmt::Display for CopyError {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                self.display_plain(f)?;
                if f.alternate() {
                    self.fmt_sources(f, SourceFormat::OneLine)?;
                }
                write!(f, "\n")
            }
        }

        impl fmt::Debug for CopyError {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                self.display_plain(f)?;
                write!(f, "\n  ")?;
                self.fmt_location(f)?;
                self.fmt_sources(f, SourceFormat::MultiLine)?;
                Ok(())
            }
        }
    }
}

use std::io;

use self::error::expanded::CopyError;

fn main() {
    let inner = io::Error::new(io::ErrorKind::AddrInUse, "bad addr");
    let err = CopyError::read(inner);
    println!("== display == ");
    println!("{err}");
    println!("== display alt == ");
    println!("{err:#}");
    println!("== display debug == ");
    println!("{err:?}");
}
