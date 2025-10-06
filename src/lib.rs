use std::{
    fmt::{self, Formatter},
    panic::Location,
};

use yansi::Paint;

pub enum SourceFormat {
    OneLine,
    MultiLine,
}

pub trait StackError: fmt::Display {
    fn display_plain(&self, f: &mut Formatter) -> fmt::Result;
    fn location(&self) -> &'static Location<'static>;
    fn source(&self) -> Option<ErrorSource<'_>>;

    fn fmt_location(&self, f: &mut Formatter) -> fmt::Result {
        let s = format!("at {}", self.location());
        write!(f, " {}", s.dim())
    }

    fn fmt_sources(&self, f: &mut Formatter, format: SourceFormat) -> fmt::Result
    where
        Self: Sized,
    {
        let chain = Chain::new(ErrorSource::Stack(self));
        if !chain.is_empty()
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
                    write!(f, "    {i}: {item}")?;
                    write!(f, "\n      ")?;
                    item.fmt_location(f)?;
                    write!(f, "\n")?;
                }
            }
        }
        // if let Some(source) = self.source() {
        //     let chain = Chain::new(source);
        //     if let SourceFormat::MultiLine = format {
        //         writeln!(f, "\nCaused by:")?;
        //     }
        //     for (i, item) in chain.enumerate() {
        //         println!("NEXT {item}");
        //         match format {
        //             SourceFormat::OneLine => {
        //                 write!(f, ": {item}")?;
        //             }
        //             SourceFormat::MultiLine => {
        //                 write!(f, "    {i}: {item}")?;
        //                 write!(f, "\n      ")?;
        //                 item.fmt_location(f)?;
        //                 write!(f, "\n")?;
        //             }
        //         }
        //     }
        // }
        Ok(())
    }
}

#[derive(Copy, Clone)]
pub enum ErrorSource<'a> {
    Std(&'a dyn std::error::Error),
    Stack(&'a dyn StackError),
}

impl<'a> ErrorSource<'a> {
    pub fn next_source(self) -> Option<ErrorSource<'a>> {
        match self {
            Self::Std(error) => error.source().map(Self::Std),
            Self::Stack(error) => error.source(),
        }
    }

    pub fn location(&self) -> Option<&'static Location<'static>> {
        match self {
            ErrorSource::Std(_) => None,
            ErrorSource::Stack(error) => Some(error.location()),
        }
    }

    pub fn fmt_location(&self, f: &mut Formatter) -> fmt::Result {
        if let Some(location) = self.location() {
            write!(f, " (at {})", location)?;
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

pub struct Chain<'a>(Option<ErrorSource<'a>>);

impl<'a> Chain<'a> {
    pub fn new(item: ErrorSource<'a>) -> Self {
        Self(Some(item))
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_none()
    }
}

impl<'a> Iterator for Chain<'a> {
    type Item = ErrorSource<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        match self.0 {
            None => None,
            Some(item) => {
                let next = item.next_source();
                self.0 = next;
                next
            }
        }
    }
}
