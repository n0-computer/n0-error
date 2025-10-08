use std::fmt::{self, Formatter};

use crate::{AnyError, Location, StdErr, StdWrapperRef, backtrace_enabled};
#[derive(Debug, Copy, Clone)]
pub enum SourceFormat {
    OneLine,
    MultiLine { location: bool },
}

pub trait StackError: fmt::Display + fmt::Debug + Send + Sync + 'static {
    fn as_std(&self) -> &(dyn StdErr);
    fn location(&self) -> Option<&Location>;
    fn set_location(&mut self, location: Location);
    fn source(&self) -> Option<ErrorRef<'_>>;
    fn is_transparent(&self) -> bool;
}

impl dyn StackError {
    pub fn stack(&self) -> impl Iterator<Item = ErrorRef<'_>> {
        Chain::new(Some(ErrorRef::Stack(self)))
    }

    pub fn sources(&self) -> impl Iterator<Item = ErrorRef<'_>> {
        self.stack().skip(1)
    }

    pub fn as_source(&self) -> ErrorRef<'_> {
        ErrorRef::Stack(self)
    }

    pub fn report(&self) -> Report<'_> {
        Report::new(self)
    }
}

pub trait StackErrorExt: StackError + Sized {
    #[track_caller]
    fn into_any(self) -> AnyError {
        AnyError::Stack(Box::new(self))
    }

    #[track_caller]
    fn context(self, context: impl fmt::Display) -> AnyError {
        self.into_any().context(context)
    }

    fn stack(&self) -> impl Iterator<Item = ErrorRef<'_>> {
        (self as &dyn StackError).stack()
    }

    fn sources(&self) -> impl Iterator<Item = ErrorRef<'_>> {
        (self as &dyn StackError).sources()
    }

    fn report(&self) -> Report<'_> {
        Report::new(self)
    }

    fn as_source(&self) -> ErrorRef<'_> {
        ErrorRef::Stack(self)
    }
}

impl<T: StackError + Sized> StackErrorExt for T {}

/// Reference to an error which can either be a std error or a stack error.
///
/// If it's a stack error, allows to access the inner fields.
#[derive(Copy, Clone, Debug)]
pub enum ErrorRef<'a> {
    /// Std error (no location info)
    Std(StdWrapperRef<'a>),
    /// StackError (has location info)
    Stack(&'a dyn StackError),
}

impl<'a> ErrorRef<'a> {
    /// Returns `true` if this error is transparent (i.e. directly forwards to its source).
    pub fn is_transparent(&self) -> bool {
        match self {
            ErrorRef::Std(_) => false,
            ErrorRef::Stack(error) => error.is_transparent(),
        }
    }

    /// Returns the error as a std error.
    pub fn as_std(&self) -> &dyn std::error::Error {
        match self {
            ErrorRef::Std(error) => error.as_std(),
            ErrorRef::Stack(error) => error.as_std(),
        }
    }

    /// Returns the next source in the source chain as a [`ErrorSource`].
    pub fn source(self) -> Option<ErrorRef<'a>> {
        match self {
            Self::Std(error) => StdWrapperRef::source(error),
            Self::Stack(error) => StackError::source(error),
        }
    }

    /// Returns the location where this error was created, if available.
    pub fn location(&self) -> Option<&Location> {
        match self {
            ErrorRef::Std(_) => None,
            ErrorRef::Stack(error) => error.location(),
        }
    }

    pub fn fmt_location(&self, f: &mut Formatter) -> fmt::Result {
        match &self {
            ErrorRef::Std(_) => Ok(()),
            ErrorRef::Stack(error) => error.report().fmt_location(f),
        }
    }
}

impl<'a> fmt::Display for ErrorRef<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Std(error) => write!(f, "{}", error.as_std()),
            Self::Stack(error) => write!(f, "{error}"),
        }
    }
}

/// A [`Report`] can be used to customize the display of an error.
pub struct Report<'a> {
    inner: &'a dyn StackError,
    location: bool,
    sources: Option<SourceFormat>,
}

impl<'a> Report<'a> {
    pub(crate) fn new(inner: &'a dyn StackError) -> Self {
        Self {
            inner,
            location: false,
            sources: None,
        }
    }

    /// Prints all available details.
    pub fn full(mut self) -> Self {
        let location = backtrace_enabled();
        self.location = location;
        self.sources = Some(SourceFormat::MultiLine { location });
        self
    }

    /// Sets if location info should be printed.
    ///
    /// Note that location info is only captured if environment variable
    /// `RUST_BACKTRACE` is set to `1` or `full`.
    pub fn location(mut self, value: bool) -> Self {
        self.location = value;
        self
    }

    /// Makes the report include sources.
    pub fn sources(mut self, format: SourceFormat) -> Self {
        self.sources = Some(format);
        self
    }

    /// Prints the report via a [`Formatter`].
    pub fn format(&self, f: &mut Formatter) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }

    /// Formats only the location.
    pub fn fmt_location(&self, f: &mut Formatter) -> fmt::Result {
        if let Some(location) = self.inner.location() {
            write!(f, " (at {})", location)?;
        }
        Ok(())
    }

    /// Formats only the sources.
    pub fn fmt_sources(&self, f: &mut Formatter, format: SourceFormat) -> fmt::Result {
        let mut chain = self
            .inner
            .sources()
            // We skip errors marked as transparent.
            .filter(|s| !s.is_transparent())
            .enumerate()
            .peekable();
        if chain.peek().is_some() && matches!(format, SourceFormat::MultiLine { .. }) {
            writeln!(f, "\nCaused by:")?;
        }
        while let Some((i, item)) = chain.next() {
            match format {
                SourceFormat::OneLine => {
                    write!(f, ": {item}")?;
                }
                SourceFormat::MultiLine { location } => {
                    write!(f, "    {i}: {item}")?;
                    if location {
                        item.fmt_location(f)?;
                    }
                    if chain.peek().is_some() {
                        writeln!(f)?;
                    }
                }
            }
        }
        Ok(())
    }
}

impl<'a> fmt::Display for Report<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.inner)?;
        if self.location {
            self.fmt_location(f)?;
        }
        if let Some(format) = self.sources {
            self.fmt_sources(f, format)?;
        }
        Ok(())
    }
}

struct Chain<'a> {
    current: Option<ErrorRef<'a>>,
}

impl<'a> Chain<'a> {
    fn new(item: Option<ErrorRef<'a>>) -> Self {
        Self { current: item }
    }
}

impl<'a> Iterator for Chain<'a> {
    type Item = ErrorRef<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        match self.current {
            None => None,
            Some(item) => {
                let out = self.current;
                self.current = item.source();
                out
            }
        }
    }
}
