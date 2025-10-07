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
    pub fn with_location(mut self) -> Self {
        self.location = true;
        self
    }

    pub fn with_sources(mut self, format: SourceFormat) -> Self {
        self.sources = Some(format);
        self
    }

    // pub fn with_color(mut self, value: bool) -> Self {
    //     self.color = value;
    //     self
    // }
}

// pub trait StackError: std::error::Error {
pub trait StackError: std::fmt::Display + std::fmt::Debug + Send + Sync {
    fn as_std(&self) -> &(dyn ::std::error::Error + 'static);
    fn location(&self) -> Option<Location> {
        None
    }

    fn source(&self) -> Option<ErrorSource<'_>> {
        None
    }

    fn as_source(&self) -> ErrorSource<'_>
    where
        Self: Sized,
    {
        ErrorSource::Stack(self)
    }

    fn is_transparent(&self) -> bool {
        false
    }

    fn report(&self) -> impl fmt::Display
    where
        Self: Sized,
    {
        Report(self)
    }

    fn stack(&self) -> impl Iterator<Item = ErrorSource<'_>>
    where
        Self: Sized,
    {
        Chain::new(Some(ErrorSource::Stack(self)))
    }

    fn sources(&self) -> impl Iterator<Item = ErrorSource<'_>>
    where
        Self: Sized,
    {
        self.stack().skip(1)
    }

    fn fmt_full(&self, f: &mut Formatter) -> fmt::Result
    where
        Self: Sized,
    {
        // TODO: Is std::env::var expensive, should we cache that?
        let location = backtrace_enabled();
        let source_format = SourceFormat::MultiLine { location };
        let opts = DisplayOpts::default()
            // TODO: When to enable color?
            // .with_color(false)
            .with_location()
            .with_sources(source_format);
        self.fmt_with_opts(f, opts)
    }

    fn fmt_with_opts(&self, f: &mut Formatter, opts: DisplayOpts) -> fmt::Result
    where
        Self: Sized,
    {
        write!(f, "{self}")?;
        if opts.location && self.location().is_some() {
            write!(f, " ")?;
            self.fmt_location(f)?;
        }
        if let Some(format) = opts.sources {
            self.fmt_sources(f, format)?;
        }
        Ok(())
    }

    fn fmt_location(&self, f: &mut Formatter) -> fmt::Result {
        if let Some(location) = self.location() {
            let s = format!("(at {})", location);
            // write!(f, "{}", s.dim())?;
            write!(f, "{s}")?;
        }
        Ok(())
    }

    fn fmt_sources(&self, f: &mut Formatter, format: SourceFormat) -> fmt::Result
    where
        Self: Sized,
    {
        let mut chain = self
            .sources()
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
                    if location && item.location().is_some() {
                        write!(f, "  ")?;
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

    #[track_caller]
    fn into_any(self) -> AnyError
    where
        Self: Sized + 'static,
    {
        AnyError::Stack(Box::new(self))
    }

    #[track_caller]
    fn context(self, context: impl fmt::Display) -> AnyError
    where
        Self: Sized + 'static,
    {
        self.into_any().context(context)
    }
}

/// Wrapper around an error source.
#[derive(Copy, Clone, Debug)]
pub enum ErrorSource<'a> {
    /// Std error (no location info)
    Std(&'a dyn std::error::Error),
    /// StackError (has location info)
    Stack(&'a dyn StackError),
}

impl<'a> ErrorSource<'a> {
    /// Returns `true` if this error is transparent (i.e. directly forwards to its source).
    pub fn is_transparent(&self) -> bool {
        match self {
            ErrorSource::Std(_) => false,
            ErrorSource::Stack(error) => error.is_transparent(),
        }
    }

    /// Returns the error as a std error.
    pub fn as_std(&self) -> &dyn std::error::Error {
        match self {
            ErrorSource::Std(error) => error,
            ErrorSource::Stack(error) => error.as_std(),
        }
    }

    /// Returns the next source in the source chain as a [`ErrorSource`].
    pub fn next_source(self) -> Option<ErrorSource<'a>> {
        match self {
            Self::Std(error) => std::error::Error::source(error).map(Self::Std),
            Self::Stack(error) => StackError::source(error),
        }
    }

    /// Returns the location where this error was created, if available.
    pub fn location(&self) -> Option<Location> {
        match self {
            ErrorSource::Std(_) => None,
            ErrorSource::Stack(error) => error.location(),
        }
    }

    pub fn fmt_location(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            ErrorSource::Std(_) => Ok(()),
            ErrorSource::Stack(error) => error.fmt_location(f),
        }
    }
}

impl<'a> fmt::Display for ErrorSource<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Std(error) => write!(f, "{error}"),
            Self::Stack(error) => write!(f, "{error}"),
        }
    }
}

struct Report<'a, E>(&'a E);

impl<'a, E: StackError> fmt::Display for Report<'a, E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.0.fmt_full(f)
    }
}

struct Chain<'a> {
    current: Option<ErrorSource<'a>>,
}

impl<'a> Chain<'a> {
    fn new(item: Option<ErrorSource<'a>>) -> Self {
        Self { current: item }
    }
}

impl<'a> Iterator for Chain<'a> {
    type Item = ErrorSource<'a>;
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
