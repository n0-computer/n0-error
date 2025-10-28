/// Constructs an error enum/struct value while automatically filling `meta: Meta`.
///
/// - `e!(MyError::Variant)` constructs `MyError::Variant { meta: Meta::default() }`.
/// - `e!(MyError::Variant, source)` constructs `MyError::Variant { source, meta: Meta::default() }`.
/// - `e!(MyError::Variant { field: value, other })` constructs `MyError::Variant { field: value, other, meta: Meta::default() }`
#[macro_export]
macro_rules! e {
    // No fields
    ($($err:tt)::+) => {
        $($err)::+ { meta: $crate::Meta::default() }
    };

    // Single expression: treat as source
    ($($err:tt)::+ , $source:expr) => {
        $($err)::+ { source: $source, meta: $crate::Meta::default() }
    };

    // Fields and values plus source
    ($($err:tt)::+ { $($body:tt)* }, $source:expr) => {
        $($err)::+ { meta: $crate::Meta::default(), source: $source, $($body)* }
    };

    // Fields and values
    ($($err:tt)::+ { $($body:tt)* }) => {
        $($err)::+ { meta: $crate::Meta::default(), $($body)* }
    };
}

/// Unwraps a result, returning in the error case while converting the error.
///
/// If the result is the error variant, this will construct a new error with [`e`]
/// that takes the result's error as its source.
#[macro_export]
macro_rules! try_or {
    ($result:expr, $($tt:tt)*) => {
        match $result {
            ::core::result::Result::Ok(v) => v,
            ::core::result::Result::Err(e) => {
                return ::core::result::Result::Err($crate::e!($($tt)*, e));
            }
        }
    };
}

/// Unwraps a result, returning in the error case while adding context to the error.
///
/// If the result is the error variant, this will construct a new error with [`anyerr`]
/// from the result's error while providing additional context.
#[macro_export]
macro_rules! try_or_any {
    ($result:expr, $($context:tt)*) => {
        match $result {
            ::core::result::Result::Ok(v) => v,
            ::core::result::Result::Err(e) => {
                return ::core::result::Result::Err($crate::anyerr!(e, $($context)*));
            }
        }
    };
}

/// Formats a message into an [`AnyError`].
#[macro_export]
macro_rules! format_err {
    ($fmt:literal$(, $($arg:expr),* $(,)?)?) => {
        $crate::AnyError::from_display(
            ::std::format!($fmt$(, $($arg),*)*),
        )
    };
}

/// Ensures a condition, otherwise returns the error constructed with [`e`] from the remaining args.
///
/// This macro takes an expression as its first argument. If the expression evaluates
/// to `false`, the macro expands to returning an error result.
///
/// The error will be constructed by passing all remaining arguments to [`e`].
/// See its docs for details on accepted forms.
#[macro_export]
macro_rules! ensure {
    ($predicate:expr, $($tt:tt)*) => {
        if !$predicate {
            $crate::bail_e!($($tt)*)
        }
    };
}

/// Ensures a condition, otherwise returns an [`AnyError`].
///
/// This macro takes an expression as its first argument. If the expression evaluates
/// to `false`, the macro expands to returning an error result. The error will be constructed
/// by passing the remaining arguments after the expression to [`anyerr`]. See its docs for
/// supported forms.
#[macro_export]
macro_rules! ensure_any {
    ($cond:expr, $($tt:tt)*) => {
        if !$cond {
            $crate::bail!($($tt)*)
        }
    };
}

/// Returns an error result by constructing an error with [`e`].
///
/// This macro accepts the same forms as [`e`], but wraps the error into `Err` and
/// expands to returning the result from the current function.
#[macro_export]
macro_rules! bail_e {
    ($($tt:tt)*) => {
        return ::core::result::Result::Err($crate::e!($($tt)*).into())
    }
}

/// Returns an error result by constructing an error with [`anyerr`].
///
/// This macro accepts the same forms as [`anyerr`], but wraps the error into `Err` and
/// expands to returning the result from the current function.
#[macro_export]
macro_rules! bail {
    ($($tt:tt)*) => {
        return core::result::Result::Err($crate::anyerr!($($tt)*))
    }
}

/// Reexport `spez` for use in the [`anyerr`] macro.
#[doc(hidden)]
pub use spez as __spez;

/// Converts a value into [`AnyError`].
///
/// - `anyerr!("msg")` creates an error with just a message.
/// - `anyerr!("this failed at {a} with {}", b)` creates an error with a formatted message
/// - `anyerr!(value)` converts any `impl StackError`, `impl std::error::Error`, or `impl Display` into [`AnyError`].
/// - `anyerr!(value, "context string") works as above, but adds "context string" as [`context`](Anyerr::context).
/// - `anyerr!(value, "context {}", foo) works as above, but with a formatted string as context
///
/// The forms that take `value` use *autoref specialization* to keep the most details possible:
/// if given a [`StackError`] it uses [`AnyError::from_stack`], if given a std error, uses [`AnyError::from_std`],
/// if given a value that impls `Display` it uses [`AnyError::from_display`] - in this order.
#[macro_export]
macro_rules! anyerr {
    ($fmt:literal$(, $($arg:expr),* $(,)?)?) => {
        $crate::format_err!($fmt$(, $($arg),*)*)
    };

    ($err:expr, $fmt:literal$(, $($arg:expr),* $(,)?)?) => {
        $crate::anyerr!($err).context(format!($fmt$(, $($arg),*)*))
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

    ($($err:tt)::+) => {
        $($err)::+ { meta: $crate::Meta::default() }
    };

    // Single expression: treat as source
    ($($err:tt)::+ , $source:expr) => {
        $($err)::+ { source: $source, meta: $crate::Meta::default() }
    };

    // Fields and values plus source
    ($($err:tt)::+ { $($body:tt)* }, $source:expr) => {
        $($err)::+ { meta: $crate::Meta::default(), source: $source, $($body)* }
    };

    // Fields and values
    ($($err:tt)::+ { $($body:tt)* }) => {
        $($err)::+ { meta: $crate::Meta::default(), $($body)* }
    };

}
