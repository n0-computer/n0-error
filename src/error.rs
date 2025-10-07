use std::fmt::{self, Formatter};

use crate::{AnyError, Location, backtrace_enabled};
#[derive(Debug, Copy, Clone)]
pub enum SourceFormat {
    OneLine,
    MultiLine { location: bool },
}

#[derive(Default)]
pub struct DisplayOpts {
    pub location: bool,
    pub sources: Option<SourceFormat>,
    // pub color: bool,
}

impl DisplayOpts {
    pub fn location(mut self, value: bool) -> Self {
        self.location = value;
        self
    }

    pub fn sources(mut self, format: SourceFormat) -> Self {
        self.sources = Some(format);
        self
    }

    // pub fn with_color(mut self, value: bool) -> Self {
    //     self.color = value;
    //     self
    // }
}

pub trait StackError: std::fmt::Display + std::fmt::Debug + Send + Sync + 'static {
    fn as_std(&self) -> &(dyn ::std::error::Error + 'static);
    fn location(&self) -> Option<&Location>;
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

    pub fn fmt_location(&self, f: &mut Formatter) -> fmt::Result {
        if let Some(location) = self.location() {
            write!(f, " (at {})", location)?;
        }
        Ok(())
    }

    pub fn as_source(&self) -> ErrorRef<'_> {
        ErrorRef::Stack(self)
    }

    pub fn report(&self) -> impl fmt::Display {
        Report(self)
    }

    pub fn fmt_full(&self, f: &mut Formatter) -> fmt::Result {
        let location = backtrace_enabled();
        let opts = DisplayOpts::default()
            // TODO: When to enable color?
            // .with_color(false)
            .location(location)
            .sources(SourceFormat::MultiLine { location });
        self.fmt_with_opts(f, opts)
    }

    pub fn fmt_with_opts(&self, f: &mut Formatter, opts: DisplayOpts) -> fmt::Result {
        write!(f, "{self}")?;
        if opts.location {
            self.fmt_location(f)?;
        }
        if let Some(format) = opts.sources {
            self.fmt_sources(f, format)?;
        }
        Ok(())
    }

    pub fn fmt_sources(&self, f: &mut Formatter, format: SourceFormat) -> fmt::Result {
        let mut chain = self
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

    fn fmt_location(&self, f: &mut Formatter) -> fmt::Result {
        (self as &dyn StackError).fmt_location(f)
    }

    fn report(&self) -> impl fmt::Display {
        (self as &dyn StackError).report()
    }

    fn fmt_full(&self, f: &mut Formatter) -> fmt::Result {
        (self as &dyn StackError).fmt_full(f)
    }

    fn fmt_with_opts(&self, f: &mut Formatter, opts: DisplayOpts) -> fmt::Result {
        (self as &dyn StackError).fmt_with_opts(f, opts)
    }

    fn fmt_sources(&self, f: &mut Formatter, format: SourceFormat) -> fmt::Result {
        (self as &dyn StackError).fmt_sources(f, format)
    }
}

impl<T: StackError + Sized> StackErrorExt for T {}

/// Reference to an error which can either be a std error or a stack error.
///
/// If it's a stack error, allows to access the inner fields.
#[derive(Copy, Clone, Debug)]
pub enum ErrorRef<'a> {
    /// Std error (no location info)
    Std(&'a dyn std::error::Error),
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
            ErrorRef::Std(error) => error,
            ErrorRef::Stack(error) => error.as_std(),
        }
    }

    /// Returns the next source in the source chain as a [`ErrorSource`].
    pub fn next_source(self) -> Option<ErrorRef<'a>> {
        match self {
            Self::Std(error) => std::error::Error::source(error).map(Self::Std),
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
            ErrorRef::Stack(error) => error.fmt_location(f),
        }
    }
}

impl<'a> fmt::Display for ErrorRef<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Std(error) => write!(f, "{error}"),
            Self::Stack(error) => write!(f, "{error}"),
        }
    }
}

struct Report<'a>(&'a dyn StackError);

impl<'a> fmt::Display for Report<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.0.fmt_full(f)
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
                self.current = item.next_source();
                out
            }
        }
    }
}
