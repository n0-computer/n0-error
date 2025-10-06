use std::{
    fmt::{self, Formatter},
    ops::Deref,
};

use crate::{ErrorSource, FromString, Location, SourceFormat, StackError};

pub enum AnyError {
    Stack(Box<dyn StackError>),
    Std(Box<dyn std::error::Error + Send + Sync>),
}

impl AnyError {
    #[track_caller]
    pub fn std(err: impl std::error::Error + Send + Sync + 'static) -> Self {
        Self::Std(Box::new(err))
    }

    #[track_caller]
    pub fn stack(err: impl StackError + 'static) -> Self {
        Self::Stack(Box::new(err))
    }

    #[track_caller]
    pub fn context(self, context: impl fmt::Display) -> AnyError {
        FromString::with_source(context.to_string(), self).into_any()
    }

    pub fn as_source<'a>(&'a self) -> ErrorSource<'a> {
        match self {
            AnyError::Stack(error) => ErrorSource::Stack(error.deref()),
            AnyError::Std(error) => ErrorSource::Std(error.deref()),
        }
    }
}

impl fmt::Display for AnyError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            AnyError::Stack(error) => write!(f, "{error}")?,
            AnyError::Std(error) => write!(f, "{error}")?,
        }
        if f.alternate() {
            self.fmt_sources(f, SourceFormat::OneLine)?;
        }
        Ok(())
    }
}

impl fmt::Debug for AnyError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            match self {
                AnyError::Stack(error) => {
                    write!(f, "Stack({error:#?})")
                }
                AnyError::Std(error) => {
                    write!(f, "Std({error:#?})")
                }
            }
        } else {
            self.fmt_full(f)
        }
    }
}

// TODO: Maybe remove this impl
impl StackError for AnyError {
    fn as_std(&self) -> &(dyn ::std::error::Error + 'static) {
        match self {
            AnyError::Stack(error) => error.as_std(),
            AnyError::Std(error) => error.deref(),
        }
    }

    fn location(&self) -> Option<Location> {
        self.as_source().location()
    }

    fn source(&self) -> Option<ErrorSource<'_>> {
        self.as_source().next_source()
    }

    fn as_source(&self) -> ErrorSource<'_>
    where
        Self: Sized,
    {
        ErrorSource::Stack(self)
    }
}

#[cfg(feature = "anyhow")]
impl From<anyhow::Error> for AnyError {
    fn from(value: anyhow::Error) -> Self {
        Self::Std(value.into_boxed_dyn_error())
    }
}

impl From<&str> for AnyError {
    fn from(value: &str) -> Self {
        crate::FromString::without_source(value.to_string()).into_any()
    }
}

impl From<String> for AnyError {
    fn from(value: String) -> Self {
        crate::FromString::without_source(value).into_any()
    }
}
