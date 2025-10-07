use std::{
    fmt::{self, Formatter},
    ops::Deref,
};

use crate::{ErrorRef, FromString, Location, SourceFormat, StackError, StackErrorExt};

pub enum AnyError {
    Stack(Box<dyn StackError>),
    Std(Box<dyn std::error::Error + Send + Sync>),
}

impl AnyError {
    #[track_caller]
    pub fn from_std(err: impl std::error::Error + Send + Sync + 'static) -> Self {
        Self::Std(Box::new(err))
    }

    #[track_caller]
    pub fn from_stack(err: impl StackError + 'static) -> Self {
        Self::Stack(Box::new(err))
    }

    #[track_caller]
    pub fn context(self, context: impl fmt::Display) -> AnyError {
        FromString::with_source(context.to_string(), self).into_any()
    }

    pub fn as_source<'a>(&'a self) -> ErrorRef<'a> {
        match self {
            AnyError::Stack(error) => ErrorRef::Stack(error.deref()),
            AnyError::Std(error) => ErrorRef::Std(error.deref()),
        }
    }

    pub fn into_boxed_dyn_error(self) -> Box<dyn std::error::Error + Send + Sync + 'static> {
        Box::new(StdWrapper(self))
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

    fn location(&self) -> Option<&Location> {
        match self {
            AnyError::Stack(error) => error.location(),
            AnyError::Std(_) => None,
        }
    }

    fn source(&self) -> Option<ErrorRef<'_>> {
        self.as_source().next_source()
    }

    fn is_transparent(&self) -> bool {
        match self {
            AnyError::Stack(error) => error.is_transparent(),
            AnyError::Std(_error) => false,
        }
    }
}

#[derive(derive_more::Debug, derive_more::Display)]
struct StdWrapper(AnyError);

impl std::error::Error for StdWrapper {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.0.as_std().source()
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
