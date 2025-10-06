use std::{
    fmt::{self, Formatter},
    panic::Location,
};

use yansi::Paint;

pub use n0_error_macros::expand;

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
    fn display_plain(&self, f: &mut Formatter) -> fmt::Result;

    fn location(&self) -> &'static Location<'static>;

    fn source(&self) -> Option<ErrorSource<'_>> {
        None
    }

    fn is_transparent(&self) -> bool {
        false
    }

    fn fmt_with_opts(&self, f: &mut Formatter, opts: DisplayOpts) -> fmt::Result
    where
        Self: Sized,
    {
        self.display_plain(f)?;
        if opts.location {
            write!(f, "\n  ")?;
            self.fmt_location(f)?;
        }
        if let Some(format) = opts.sources {
            self.fmt_sources(f, format)?;
        }
        Ok(())
    }

    fn fmt_location(&self, f: &mut Formatter) -> fmt::Result {
        let s = format!("(at {})", self.location());
        write!(f, "{}", s.dim())
    }

    fn fmt_sources(&self, f: &mut Formatter, format: SourceFormat) -> fmt::Result
    where
        Self: Sized,
    {
        // println!("FMT SOURCES self transparent {}", self.is_transparent());
        let chain = Chain::new(ErrorSource::Stack(self));
        // let skip = if self.is_transparent() { 1 } else { 0 };
        // let mut chain = chain.skip(skip).peekable();
        let mut chain = chain.filter(|f| !f.is_transparent()).peekable();
        // let mut chain = chain.peekable();
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
                        write!(f, "\n    {}", loc.dim())?;
                    }
                    write!(f, "\n")?;
                }
            }
        }
        Ok(())
    }
}

#[derive(Copy, Clone)]
pub enum ErrorSource<'a> {
    Std(&'a dyn std::error::Error),
    Stack(&'a dyn StackError),
}

impl<'a> ErrorSource<'a> {
    pub fn is_transparent(&self) -> bool {
        match self {
            ErrorSource::Std(_) => false,
            ErrorSource::Stack(error) => error.is_transparent(),
        }
    }
    pub fn as_std(&self) -> &dyn std::error::Error {
        match self {
            ErrorSource::Std(error) => error,
            ErrorSource::Stack(error) => error,
        }
    }
    pub fn next_source(self) -> Option<ErrorSource<'a>> {
        match self {
            Self::Std(error) => std::error::Error::source(error).map(Self::Std),
            Self::Stack(error) => StackError::source(error),
        }
    }

    pub fn location(&self) -> Option<&'static Location<'static>> {
        match self {
            ErrorSource::Std(_) => None,
            ErrorSource::Stack(error) => Some(error.location()),
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

pub struct Chain<'a> {
    current: Option<ErrorSource<'a>>,
    current_is_transparent: bool,
}

impl<'a> Chain<'a> {
    pub fn new(item: ErrorSource<'a>) -> Self {
        Self {
            current: Some(item),
            current_is_transparent: item.is_transparent(),
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
                let next = item.next_source();
                // let out = !self.current_is_transparent;
                // self.current_is_transparent =
                //     next.as_ref().map(|s| s.is_transparent()).unwrap_or(false);
                self.current = next;
                next
            }
        }
        // loop {
        //     match self.current {
        //         None => return None,
        //         Some(item) => {
        //             let next = item.next_source();
        //             let out = !self.current_is_transparent;
        //             self.current_is_transparent =
        //                 next.as_ref().map(|s| s.is_transparent()).unwrap_or(false);
        //             self.current = next;
        //             if out {
        //                 return next;
        //             }
        //         }
        //     }
        // }
    }
}
