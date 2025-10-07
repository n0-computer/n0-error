use std::fmt;

use crate::{AnyError, StackError, add_location};

#[macro_export]
macro_rules! whatever {
    ($fmt:literal$(, $($arg:expr),* $(,)?)?) => {
        return core::result::Result::Err({
            $crate::format_err!($fmt$(, $($arg),*)*)
        });
    };
    ($source:expr, $fmt:literal$(, $($arg:expr),* $(,)?)*) => {
        match $source {
            core::result::Result::Ok(v) => v,
            core::result::Result::Err(e) => {
                return core::result::Result::Err({
                    $crate::FromString::with_source(
                        format!($fmt$(, $($arg),*)*),
                        core::convert::Into::into(e),
                    ).into_any()
                });
            }
        }
    };
}

#[macro_export]
macro_rules! format_err {
    ($fmt:literal$(, $($arg:expr),* $(,)?)?) => {
        {
            $crate::FromString::without_source(
                format!($fmt$(, $($arg),*)*),
            ).into_any()
        }
    };
}

#[macro_export]
macro_rules! ensure {
    ($predicate:expr, $err:expr $(,)?) => {
        if !$predicate {
            return Err(::core::convert::Into::into($err));
        }
    };
}

#[add_location]
#[derive(crate::Error)]
#[display("Expected some, found none")]
pub struct NoneError {}

#[add_location]
#[derive(crate::Error)]
pub enum FromString {
    #[display("{message}")]
    WithSource { message: String, source: AnyError },
    #[display("{message}")]
    WithoutSource { message: String },
}

pub trait ResultExt<T> {
    #[track_caller]
    fn context(self, context: impl fmt::Display) -> Result<T, AnyError>;

    #[track_caller]
    fn with_context<F>(self, context: F) -> Result<T, AnyError>
    where
        F: FnOnce() -> String;

    /// Quickly convert a std error into a `Error`, without having to write a `context` message.
    #[track_caller]
    fn e(self) -> Result<T, AnyError>;
}

impl<T, E> ResultExt<T> for Result<T, E>
where
    E: std::error::Error + Sync + Send + 'static,
{
    #[track_caller]
    fn context(self, context: impl fmt::Display) -> Result<T, AnyError> {
        match self {
            Ok(v) => Ok(v),
            Err(err) => Err(AnyError::std(err).context(context)),
        }
    }

    #[track_caller]
    fn e(self) -> Result<T, AnyError> {
        match self {
            Ok(v) => Ok(v),
            Err(err) => Err(AnyError::std(err)),
        }
    }

    #[track_caller]
    fn with_context<F>(self, context: F) -> Result<T, AnyError>
    where
        F: FnOnce() -> String,
    {
        match self {
            Ok(v) => Ok(v),
            Err(err) => Err(AnyError::std(err).context(context())),
        }
    }
}

impl<T> ResultExt<T> for Option<T> {
    #[track_caller]
    fn context(self, context: impl fmt::Display) -> Result<T, AnyError> {
        match self {
            Some(v) => Ok(v),
            None => Err(NoneError::new().context(context)),
        }
    }

    #[track_caller]
    fn e(self) -> Result<T, AnyError> {
        match self {
            Some(v) => Ok(v),
            None => Err(NoneError::new().into_any()),
        }
    }

    #[track_caller]
    fn with_context<F>(self, context: F) -> Result<T, AnyError>
    where
        F: FnOnce() -> String,
    {
        match self {
            Some(v) => Ok(v),
            None => Err(NoneError::new().context(context())),
        }
    }
}

impl<T> ResultExt<T> for Result<T, AnyError> {
    #[track_caller]
    fn context(self, context: impl fmt::Display) -> Result<T, AnyError> {
        match self {
            Ok(v) => Ok(v),
            Err(err) => Err(err.context(context)),
        }
    }

    #[track_caller]
    fn e(self) -> Result<T, AnyError> {
        match self {
            Ok(v) => Ok(v),
            Err(err) => Err(err),
        }
    }

    #[track_caller]
    fn with_context<F>(self, context: F) -> Result<T, AnyError>
    where
        F: FnOnce() -> String,
    {
        match self {
            Ok(v) => Ok(v),
            Err(err) => Err(err.context(context())),
        }
    }
}
