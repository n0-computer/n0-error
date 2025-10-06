use std::{
    fmt::{self, Formatter},
    sync::OnceLock,
};

use yansi::Paint;

pub use n0_error_macros::{Error, add_location};

#[derive(Debug, Copy, Clone)]
pub enum SourceFormat {
    OneLine,
    MultiLine,
    MultiLineWithLocation,
}

#[derive(Default)]
pub struct DisplayOpts {
    pub location: bool,
    pub sources: Option<SourceFormat>,
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
}

pub trait StackError: std::error::Error {
    fn location(&self) -> Option<Location> {
        None
    }

    fn source(&self) -> Option<ErrorSource<'_>> {
        None
    }

    fn is_transparent(&self) -> bool {
        false
    }

    fn report<'a>(&'a self) -> impl fmt::Display
    where
        Self: Sized,
    {
        Report(self)
    }

    fn fmt_full(&self, f: &mut Formatter) -> fmt::Result
    where
        Self: Sized,
    {
        // TODO: Is std::env::var expensive, should we cache that?
        static SOURCE_FORMAT: OnceLock<SourceFormat> = OnceLock::new();
        let source_format =
            SOURCE_FORMAT.get_or_init(|| match std::env::var("RUST_BACKTRACE").as_deref() {
                Ok("1") | Ok("full") => SourceFormat::MultiLineWithLocation,
                _ => SourceFormat::MultiLine,
            });
        let opts = DisplayOpts::default()
            .with_location()
            .with_sources(*source_format);
        self.fmt_with_opts(f, opts)
    }

    fn fmt_with_opts(&self, f: &mut Formatter, opts: DisplayOpts) -> fmt::Result
    where
        Self: Sized,
    {
        write!(f, "{self}")?;
        // self.fmt_message(f)?;
        if opts.location {
            write!(f, "  ")?;
            self.fmt_location(f)?;
            write!(f, "\n")?;
        }
        if let Some(format) = opts.sources {
            self.fmt_sources(f, format)?;
        }
        Ok(())
    }

    fn fmt_location(&self, f: &mut Formatter) -> fmt::Result {
        if let Some(location) = self.location() {
            let s = format!("(at {})", location);
            write!(f, "{}", s.dim())?;
        }
        Ok(())
    }

    fn fmt_sources(&self, f: &mut Formatter, format: SourceFormat) -> fmt::Result
    where
        Self: Sized,
    {
        let mut chain = Chain::new(ErrorSource::Stack(self))
            .filter(|f| !f.is_transparent())
            .peekable();
        if chain.peek().is_some()
            && let SourceFormat::MultiLine = format
        {
            writeln!(f, "\nCaused by:")?;
        }
        for (i, item) in chain.enumerate() {
            match format {
                SourceFormat::OneLine => {
                    write!(f, ": {item}")?;
                }
                SourceFormat::MultiLine => {
                    write!(f, "    {i}: {item}\n")?;
                }
                SourceFormat::MultiLineWithLocation => {
                    write!(f, "    {i}: {item}")?;
                    if let Some(location) = item.location() {
                        let loc = format!("(at {location})");
                        write!(f, " {}", loc.dim())?;
                    }
                    write!(f, "\n")?;
                }
            }
        }
        Ok(())
    }
}

/// Wrapper around an error source.
#[derive(Copy, Clone)]
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
            ErrorSource::Stack(error) => error,
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

    pub fn fmt_location(&self, f: &mut Formatter, newline: bool) -> fmt::Result {
        if let Some(location) = self.location() {
            write!(f, " (at {})", location)?;
            if newline {
                write!(f, "\n")?;
            }
        }
        Ok(())
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

pub struct Chain<'a> {
    current: Option<ErrorSource<'a>>,
}

impl<'a> Chain<'a> {
    pub fn new(item: ErrorSource<'a>) -> Self {
        Self {
            current: Some(item),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.current
            .map(|s| s.next_source().is_none())
            .unwrap_or(true)
    }
}

impl<'a> Iterator for Chain<'a> {
    type Item = ErrorSource<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        match self.current {
            None => None,
            Some(item) => {
                self.current = item.next_source();
                self.current
            }
        }
    }
}

pub type Location = &'static std::panic::Location<'static>;

#[doc(hidden)]
pub fn backtrace_enabled() -> bool {
    static BACKTRACE_ENABLED: OnceLock<bool> = OnceLock::new();
    *(BACKTRACE_ENABLED.get_or_init(|| match std::env::var("RUST_BACKTRACE").as_deref() {
        Ok("1") | Ok("full") => true,
        _ => false,
    }))
}

#[doc(hidden)]
#[track_caller]
pub fn location() -> Option<Location> {
    if backtrace_enabled() {
        Some(std::panic::Location::caller())
    } else {
        None
    }
}
