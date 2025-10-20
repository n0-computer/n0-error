use std::fmt::{self, Formatter};

use crate::{AnyError, Location, Meta, backtrace_enabled};

/// Output style for rendering error sources in a [`Report`].
#[derive(Debug, Copy, Clone)]
pub enum SourceFormat {
    /// Renders sources inline on a single line.
    OneLine,
    /// Renders sources on multiple lines
    MultiLine {
        /// If `true` include call-site info for each source.
        location: bool,
    },
}

/// Trait implemented by errors produced by this crate.
///
/// It extends `std::error::Error` semantics with optional error metadata,
/// and a `source` method where sources may *also* provide metadata.
pub trait StackError: fmt::Display + fmt::Debug + Send + Sync {
    /// Returns this error as a std error reference.
    fn as_std(&self) -> &(dyn std::error::Error + Send + Sync + 'static);

    /// Returns this error as a `dyn StackError`.
    fn as_dyn(&self) -> &(dyn StackError);

    /// Returns metadata captured at creation time, if available.
    fn meta(&self) -> Option<&Meta>;

    /// Returns the next source in the chain, if any.
    fn source(&self) -> Option<ErrorRef<'_>>;

    /// Returns whether this error is transparent and should be skipped in reports.
    fn is_transparent(&self) -> bool;

    /// Returns this error as an [`ErrorRef`].
    ///
    /// See [`ErrorRef`] for details.
    fn as_ref(&self) -> ErrorRef<'_> {
        ErrorRef::Stack(self.as_dyn())
    }

    /// Returns an iterator over this error followed by its sources.
    fn stack(&self) -> Chain<'_> {
        Chain::stack(self.as_ref())
    }

    /// Returns an iterator over sources of this error (skipping `self`).
    fn sources(&self) -> Chain<'_> {
        Chain::sources(self.as_ref())
    }

    /// Returns a [`Report`] to output the error with configurable formatting.
    fn report(&self) -> Report<'_> {
        Report::new(self.as_dyn())
    }
}

/// Extension methods for [`StackError`]s that are [`Sized`].
pub trait StackErrorExt: StackError + Sized + 'static {
    /// Converts the error into [`AnyError`].
    ///
    /// This preserves the error's location metadata.
    #[track_caller]
    fn into_any(self) -> AnyError {
        AnyError::from_stack(self)
    }

    /// Converts the error into [`AnyError`], and adds additional error context.
    #[track_caller]
    fn context(self, context: impl fmt::Display) -> AnyError {
        self.into_any().context(context)
    }
}

impl<T: StackError + Sized + 'static> StackErrorExt for T {}

/// Reference to an error which can either be a std error or a stack error.
///
/// This provides a unified interface to either a std or a stack error. If it's a
/// stack error it allows to access the error metadata captured at the call site.
#[derive(Copy, Clone, Debug)]
pub enum ErrorRef<'a> {
    /// Std error (no location info).
    Std(&'a (dyn std::error::Error), Option<&'a Meta>),
    /// StackError (has location info).
    Stack(&'a dyn StackError),
}

impl<'a> ErrorRef<'a> {
    /// Creates a [`ErrorRef`] for a std error.
    pub fn std(err: &dyn std::error::Error) -> ErrorRef<'_> {
        ErrorRef::Std(err, None)
    }

    ///
    pub fn std_with_meta(err: &'a dyn std::error::Error, meta: &'a Meta) -> ErrorRef<'a> {
        ErrorRef::Std(err, Some(meta))
    }

    /// Creates a [`ErrorRef`] for a `StackError`.
    pub fn stack(err: &dyn StackError) -> ErrorRef<'_> {
        ErrorRef::Stack(err)
    }

    /// Returns `true` if this error is transparent (i.e. directly forwards to its source).
    pub fn is_transparent(&self) -> bool {
        match self {
            ErrorRef::Std(_, _) => false,
            ErrorRef::Stack(error) => error.is_transparent(),
        }
    }

    /// Returns the error as a std error.
    pub fn as_std(&self) -> &dyn std::error::Error {
        match self {
            ErrorRef::Std(error, _) => error,
            ErrorRef::Stack(error) => error.as_std(),
        }
    }

    /// Returns the next source in the source chain as a [`ErrorRef`].
    pub fn source(self) -> Option<ErrorRef<'a>> {
        match self {
            Self::Std(error, _) => error.source().map(ErrorRef::std),
            Self::Stack(error) => StackError::source(error),
        }
    }

    /// Returns the location where this error was created, if available.
    pub fn meta(&self) -> Option<&Meta> {
        match self {
            ErrorRef::Std(_, meta) => *meta,
            ErrorRef::Stack(error) => error.meta(),
        }
    }

    /// Formats the captured location, if present.
    pub(crate) fn fmt_location(&self, f: &mut Formatter) -> fmt::Result {
        fmt_location(self.meta().and_then(|m| m.location()), f)
    }
}

impl<'a> fmt::Display for ErrorRef<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Std(error, _) => write!(f, "{error}"),
            Self::Stack(error) => write!(f, "{error}"),
        }
    }
}

/// A [`Report`] customizes how an error is displayed.
#[derive(Clone, Copy)]
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

    /// Enables all available details.
    pub fn full(mut self) -> Self {
        let location = backtrace_enabled();
        self.location = location;
        self.sources = Some(SourceFormat::MultiLine { location });
        self
    }

    /// Sets whether location info is printed.
    ///
    /// Note that location info is only captured if environment variable
    /// `RUST_BACKTRACE` is set to `1` or `full`.
    pub fn location(mut self, value: bool) -> Self {
        self.location = value;
        self
    }

    /// Prints the error's sources.
    pub fn sources(mut self, format: SourceFormat) -> Self {
        self.sources = Some(format);
        self
    }

    /// Formats the report via a [`Formatter`].
    pub fn format(&self, f: &mut Formatter) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }

    /// Formats only the location.
    pub fn fmt_location(&'a self, f: &mut Formatter) -> fmt::Result {
        match self.inner.as_dyn().meta().and_then(|m| m.location()) {
            None => Ok(()),
            Some(location) => {
                write!(f, " (at {})", location)
            }
        }
    }

    /// Formats only the sources.
    pub fn fmt_sources(&'a self, f: &mut Formatter, format: SourceFormat) -> fmt::Result {
        let chain = self.inner.as_dyn().sources();
        let mut chain = chain
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

fn fmt_location(location: Option<&Location>, f: &mut Formatter) -> fmt::Result {
    if let Some(location) = location {
        write!(f, " (at {})", location)?;
    }
    Ok(())
}

/// Iterator over the sources of an error.
pub struct Chain<'a> {
    item: Option<ErrorRef<'a>>,
    skip: bool,
}

impl<'a> Chain<'a> {
    /// Creates a chain over `self` and its sources.
    fn stack(item: ErrorRef<'a>) -> Self {
        Self {
            item: Some(item),
            skip: false,
        }
    }

    /// Creates a chain over only the sources, skipping `self`.
    fn sources(item: ErrorRef<'a>) -> Self {
        Self {
            item: Some(item),
            skip: true,
        }
    }
}

impl<'a> Iterator for Chain<'a> {
    type Item = ErrorRef<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let item = self.item?;
            self.item = item.source();
            if self.skip {
                self.skip = false;
            } else {
                return Some(item);
            }
        }
    }
}
