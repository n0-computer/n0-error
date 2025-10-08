use crate::{AnyError, StackError, StackErrorExt, add_location};

#[macro_export]
macro_rules! whatever {
    ($fmt:literal$(, $($arg:expr),* $(,)?)?) => {
        return core::result::Result::Err({
            $crate::format_err!($fmt$(, $($arg),*)*)
        });
    };
    ($source:expr, $fmt:literal$(, $($arg:expr),* $(,)?)*) => {
        use crate::StackErrorExt;
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
            use crate::StackErrorExt;
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

#[macro_export]
macro_rules! ensure_any {
    ($cond:expr, $fmt:literal) => {
        if !$cond{
            return Err($crate::anyerr!($fmt));
        }
    };

    ($cond:expr, $fmt:literal$(, $($arg:expr),* $(,)?)?) => {
        if !$cond{
            return Err($crate::anyerr!($fmt, $($arg),*));
        }
    };

    ($cond:expr, $err:expr) => {
        if !$cond{
            return Err($crate::anyerr!($err));
        }
    }
}

#[doc(hidden)]
pub use spez as __spez;

#[macro_export]
macro_rules! anyerr {
    ($fmt:literal) => {
        $crate::format_err!($fmt)
    };

    ($fmt:literal$(, $($arg:expr),* $(,)?)?) => {
        $crate::format_err!($fmt$(, $($arg),*)*)
    };

    ($err:expr) => {
        $crate::__spez::spez! {
            for err = $err;
            match $crate::AnyError -> $crate::AnyError {
                err
            }
            match<T: $crate::StackError> T -> $crate::AnyError {
                $crate::AnyError::from_stack(err)
            }
            match<T: $crate::StdErr + Send + Sync + 'static> T -> $crate::AnyError {
                $crate::AnyError::from_std(err)
            }
            match <T: ::std::fmt::Display> T -> $crate::AnyError {
                {
                    use $crate::StackErrorExt;
                    $crate::FromString::without_source(err.to_string()).into_any()
                }
            }
        }
    };


}

/// Error returned when converting [`Option`]s to an error.
#[add_location]
#[derive(crate::Error)]
#[display("Expected some, found none")]
pub(crate) struct NoneError {}

/// A simple string error, providing a message and optionally a source.
#[add_location]
#[derive(crate::Error)]
pub enum FromString {
    #[display("{message}")]
    WithSource { message: String, source: AnyError },
    #[display("{message}")]
    WithoutSource { message: String },
}

/// Extension methods for results to provide additional context to errors.
pub trait ResultExt<T, E> {
    /// Wraps the result's error value with lazily-evaluated additional context.
    ///
    /// The `context` closure is only invoked if an error occurs.
    #[track_caller]
    fn with_context<F, C, E2>(self, context: F) -> Result<T, E2>
    where
        F: FnOnce() -> C,
        C: ElevateErr<E, E2>,
        E2: StackError;

    /// Quickly convert a std error into [`AnyError`] without a `context` message.
    #[track_caller]
    fn e(self) -> Result<T, AnyError>;

    #[track_caller]
    fn context<C, E2>(self, context: C) -> Result<T, E2>
    where
        C: ElevateErr<E, E2>,
        E2: StackError;
}

pub trait ElevateErr<Source, Target: StackError> {
    #[track_caller]
    fn elevate(self, source: Source) -> Target;

    #[track_caller]
    fn elevate_tracked(self, source: Source) -> Target
    where
        Self: Sized,
    {
        let mut err = self.elevate(source);
        if let Some(location) = crate::location() {
            err.set_location(location);
        }
        err
    }
}

pub use std::error::Error as StdErr;
// pub trait StdErr: std::error::Error + Send + Sync + 'static {}
// impl<T> StdErr for T where T: std::error::Error + Send + Sync + 'static {}

impl<E: StdErr + Send + Sync + 'static> ElevateErr<E, AnyError> for &str {
    #[track_caller]
    fn elevate(self, source: E) -> AnyError {
        FromString::with_source(self.to_string(), AnyError::from_std_untracked(source)).into_any()
    }
}

impl ElevateErr<AnyError, AnyError> for &str {
    #[track_caller]
    fn elevate(self, source: AnyError) -> AnyError {
        FromString::with_source(self.to_string(), source).into_any()
    }
}

impl<E: StdErr + Send + Sync + 'static> ElevateErr<E, AnyError> for String {
    #[track_caller]
    fn elevate(self, source: E) -> AnyError {
        FromString::with_source(self, AnyError::from_std_untracked(source)).into_any()
    }
}

impl ElevateErr<AnyError, AnyError> for String {
    #[track_caller]
    fn elevate(self, source: AnyError) -> AnyError {
        FromString::with_source(self, source).into_any()
    }
}

impl<E1, E2: StackError, F> ElevateErr<E1, E2> for F
where
    F: Fn(E1) -> E2,
{
    #[track_caller]
    fn elevate(self, source: E1) -> E2 {
        (self)(source)
    }
}
impl<T, E: StdErr + Send + Sync + 'static> ResultExt<T, E> for Result<T, E> {
    #[track_caller]
    fn e(self) -> Result<T, AnyError> {
        match self {
            Ok(v) => Ok(v),
            Err(err) => Err(AnyError::from_std(err)),
        }
    }

    #[track_caller]
    fn with_context<F, C, E2>(self, context: F) -> Result<T, E2>
    where
        F: FnOnce() -> C,
        C: ElevateErr<E, E2>,
        E2: StackError,
    {
        match self {
            Ok(v) => Ok(v),
            Err(err) => Err(context().elevate_tracked(err)),
        }
    }

    #[track_caller]
    fn context<C, E2>(self, context: C) -> Result<T, E2>
    where
        C: ElevateErr<E, E2>,
        E2: StackError,
    {
        match self {
            Ok(v) => Ok(v),
            Err(err) => Err(context.elevate_tracked(err)),
        }
    }
}

impl<T> ResultExt<T, NoneError> for Option<T> {
    #[track_caller]
    fn e(self) -> Result<T, AnyError> {
        match self {
            Some(v) => Ok(v),
            None => Err(NoneError::new().into_any()),
        }
    }

    #[track_caller]
    fn with_context<F, C, E2>(self, context: F) -> Result<T, E2>
    where
        F: FnOnce() -> C,
        C: ElevateErr<NoneError, E2>,
        E2: StackError,
    {
        match self {
            Some(v) => Ok(v),
            None => Err(context().elevate_tracked(NoneError::new())),
        }
    }

    #[track_caller]
    fn context<C, E2>(self, context: C) -> Result<T, E2>
    where
        C: ElevateErr<NoneError, E2>,
        E2: StackError,
    {
        match self {
            Some(v) => Ok(v),
            None => Err(context.elevate_tracked(NoneError::new())),
        }
    }
}

impl<T> ResultExt<T, AnyError> for Result<T, AnyError> {
    #[track_caller]
    fn e(self) -> Result<T, AnyError> {
        match self {
            Ok(v) => Ok(v),
            Err(err) => Err(err),
        }
    }

    #[track_caller]
    fn with_context<F, C, E2>(self, context: F) -> Result<T, E2>
    where
        F: FnOnce() -> C,
        C: ElevateErr<AnyError, E2>,
        E2: StackError,
    {
        match self {
            Ok(v) => Ok(v),
            Err(err) => Err(context().elevate_tracked(err)),
        }
    }

    #[track_caller]
    fn context<C, E2>(self, context: C) -> Result<T, E2>
    where
        C: ElevateErr<AnyError, E2>,
        E2: StackError,
    {
        match self {
            Ok(v) => Ok(v),
            Err(err) => Err(context.elevate_tracked(err)),
        }
    }
}

// impl<T> ResultExt<T, Box<dyn StdErr + Send + Sync + 'static>> for Result<T, Box<dyn StdErr>> {
//     #[track_caller]
//     fn e(self) -> Result<T, AnyError> {
//         match self {
//             Ok(v) => Ok(v),
//             Err(err) => Err(AnyError::from_std_box(err)),
//         }
//     }

//     #[track_caller]
//     fn with_context<F, C, E2>(self, context: F) -> Result<T, E2>
//     where
//         F: FnOnce() -> C,
//         C: ElevateErr<Box<dyn StdErr>, E2>,
//         E2: StackError,
//     {
//         match self {
//             Ok(v) => Ok(v),
//             Err(err) => Err(context().elevate_tracked(err)),
//         }
//     }

//     #[track_caller]
//     fn context<C, E2>(self, context: C) -> Result<T, E2>
//     where
//         C: ElevateErr<Box<dyn StdErr>, E2>,
//         E2: StackError,
//     {
//         match self {
//             Ok(v) => Ok(v),
//             Err(err) => Err(context.elevate_tracked(err)),
//         }
//     }
// }
