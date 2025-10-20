/// Constructs an error enum/struct value while automatically filling `meta: Meta`.
///
/// - `e!(MyError::Variant)` constructs `MyError::Variant { meta: Meta::default() }`.
/// - `e!(MyError::Variant, source)` constructs `MyError::Variant { source, meta: Meta::default() }`.
/// - `e!(MyError::Variant { field: value, other })` constructs `MyError::Variant { field: value, other, meta: Meta::default() }`
#[macro_export]
macro_rules! e {
    // No fields
    ($($err:tt)::+) => {
        $($err)::+ { meta: ::n0_error::Meta::default() }
    };

    // Single expression: treat as source
    ($($err:tt)::+ , $source:expr) => {
        $($err)::+ { source: $source, meta: ::n0_error::Meta::default() }
    };

    // Fields and values
    ($($err:tt)::+ { $($body:tt)* }) => {
        $($err)::+ { meta: ::n0_error::Meta::default(), $($body)* }
    };
}

/// Constructs an error enum/struct value and wraps it in `Err(err)`.
///
/// See [`e`] for supported syntax.
#[macro_export]
macro_rules! Err {
    ($($tt:tt)*) => {
        Err(e!($($tt)*))
    }
}

/// Propagates an error, adding formatted context.
///
/// - `whatever!("msg")` returns `Err(format_err!(...))`.
/// - `whatever!(source, "msg {x}", x)` unwraps `source` or returns with context.
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
                let context = format!($fmt$(, $($arg),*)*);
                return core::result::Result::Err(
                    $crate::anyerr!(e).context(context)
                );
            }
        }
    };
}

/// Formats a message into an [`AnyError`].
#[macro_export]
macro_rules! format_err {
    ($fmt:literal$(, $($arg:expr),* $(,)?)?) => {
        $crate::AnyError::from_display(
            format!($fmt$(, $($arg),*)*),
        )
    };
}

/// Ensures a condition, otherwise returns the given error.
#[macro_export]
macro_rules! ensure {
    ($predicate:expr, $err:expr $(,)?) => {
        if !$predicate {
            return Err(::core::convert::Into::into($err));
        }
    };
}

/// Ensures a condition, otherwise returns an [`AnyError`].
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

/// Converts a value into [`AnyError`], or formats a message.
///
/// - `anyerr!("msg")` formats a message.
/// - `anyerr!(value)` converts `StackError`, std error, or `Display` in [`AnyError`].
///
/// This uses *autoref specialization* to use the most details available: if given a [`StackError`]
/// it uses [`AnyError::from_stack`], if given a std error, uses [`AnyError::from_std`], if given a value
/// that impls `Display` it uses [`AnyError::from_display`] - in this order.
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
            match<T: $crate::StackError + 'static> T -> $crate::AnyError {
                $crate::AnyError::from_stack(err)
            }
            match<T: ::std::error::Error + Send + Sync + 'static> T -> $crate::AnyError {
                $crate::AnyError::from_std(err)
            }
            match <T: ::std::fmt::Display> T -> $crate::AnyError {
                $crate::AnyError::from_display(err)
            }
        }
    };
}
