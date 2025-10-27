use std::fmt;

use crate::{AnyError, StackError, StackErrorExt, add_meta, meta};

/// Provides extension methods to add context to [`StackError`]s.
pub trait StackResultExt<T, E> {
    /// Wraps the result's error value with additional context.
    #[track_caller]
    fn context(self, context: impl fmt::Display) -> Result<T, AnyError>;

    /// Wraps the result's error value with lazily-evaluated additional context.
    ///
    /// The `context` closure is only invoked if an error occurs.
    #[track_caller]
    fn with_context<F, C>(self, context: F) -> Result<T, AnyError>
    where
        F: FnOnce(&E) -> C,
        C: fmt::Display;
}

impl<T, E: StackError + 'static> StackResultExt<T, E> for Result<T, E> {
    fn context(self, context: impl fmt::Display) -> Result<T, AnyError> {
        match self {
            Ok(v) => Ok(v),
            Err(e) => Err(e.into_any().context(context)),
        }
    }

    fn with_context<F, C>(self, context: F) -> Result<T, AnyError>
    where
        F: FnOnce(&E) -> C,
        C: fmt::Display,
    {
        match self {
            Ok(v) => Ok(v),
            Err(e) => {
                let context = context(&e);
                Err(e.into_any().context(context))
            }
        }
    }
}

/// Provides extension methods to add context to std errors.
pub trait StdResultExt<T, E> {
    /// Wraps the result's error value with additional context.
    #[track_caller]
    fn std_context(self, context: impl fmt::Display) -> Result<T, AnyError>;

    /// Wraps the result's error value with lazily-evaluated additional context.
    ///
    /// The `context` closure is only invoked if an error occurs.
    #[track_caller]
    fn with_std_context<F, C>(self, context: F) -> Result<T, AnyError>
    where
        F: FnOnce(&E) -> C,
        C: fmt::Display;

    /// Converts the result's error into [`AnyError`].
    #[track_caller]
    fn e(self) -> Result<T, AnyError>;
}

impl<T, E: std::error::Error + Send + Sync + 'static> StdResultExt<T, E> for Result<T, E> {
    fn std_context(self, context: impl fmt::Display) -> Result<T, AnyError> {
        match self {
            Ok(v) => Ok(v),
            Err(e) => Err(AnyError::from_std(e).context(context)),
        }
    }

    fn with_std_context<F, C>(self, context: F) -> Result<T, AnyError>
    where
        F: FnOnce(&E) -> C,
        C: fmt::Display,
    {
        match self {
            Ok(v) => Ok(v),
            Err(e) => {
                let context = context(&e);
                Err(AnyError::from_std(e).context(context))
            }
        }
    }

    fn e(self) -> Result<T, AnyError> {
        match self {
            Ok(v) => Ok(v),
            Err(err) => Err(AnyError::from_std(err)),
        }
    }
}

impl<T> StdResultExt<T, NoneError> for Option<T> {
    fn std_context(self, context: impl fmt::Display) -> Result<T, AnyError> {
        match self {
            Some(v) => Ok(v),
            None => Err(NoneError { meta: meta() }.into_any().context(context)),
        }
    }

    fn with_std_context<F, C>(self, context: F) -> Result<T, AnyError>
    where
        F: FnOnce(&NoneError) -> C,
        C: fmt::Display,
    {
        match self {
            Some(v) => Ok(v),
            None => {
                let err = NoneError { meta: meta() };
                let context = context(&err);
                Err(err.into_any().context(context))
            }
        }
    }

    fn e(self) -> Result<T, AnyError> {
        match self {
            Some(v) => Ok(v),
            None => Err(NoneError { meta: meta() }.into_any()),
        }
    }
}

impl<T> StackResultExt<T, NoneError> for Option<T> {
    fn context(self, context: impl fmt::Display) -> Result<T, AnyError> {
        match self {
            Some(v) => Ok(v),
            None => Err(NoneError { meta: meta() }.into_any().context(context)),
        }
    }

    fn with_context<F, C>(self, context: F) -> Result<T, AnyError>
    where
        F: FnOnce(&NoneError) -> C,
        C: fmt::Display,
    {
        match self {
            Some(v) => Ok(v),
            None => {
                let err = NoneError { meta: meta() };
                let context = context(&err);
                Err(err.into_any().context(context))
            }
        }
    }
}

/// Error returned when converting [`Option`]s to an error.
#[add_meta]
#[derive(crate::StackError)]
#[error("Expected some, found none")]
pub struct NoneError {}

/// A simple string error, providing a message and optionally a source.
#[add_meta]
#[derive(crate::StackError)]
pub(crate) enum FromString {
    #[error("{message}")]
    WithSource { message: String, source: AnyError },
    #[error("{message}")]
    WithoutSource { message: String },
}
