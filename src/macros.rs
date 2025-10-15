#[macro_export]
macro_rules! e {
    // No fields
    ($($err:tt)::+ $(,)?) => {
        $($err)::+ { meta: ::n0_error::Meta::default() }
    };

    ($($err:tt)::+ , $source:expr) => {
        $($err)::+ { source: $source, meta: ::n0_error::Meta::default() }
    };

    // // Fields with a trailing comma
    // ($($err:tt)::+ { $($body:tt)* , }) => {
    //     $($err)::+ { $($body)* meta: ::n0_error::Meta::default() }
    // };

    // Fields without a trailing comma
    ($($err:tt)::+ { $($body:tt)* }) => {
        $($err)::+ { $($body)*, meta: ::n0_error::Meta::default() }
    };
}

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

#[macro_export]
macro_rules! format_err {
    ($fmt:literal$(, $($arg:expr),* $(,)?)?) => {
        $crate::AnyError::from_str(
            format!($fmt$(, $($arg),*)*),
        )
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
            match<T: $crate::StackError + 'static> T -> $crate::AnyError {
                $crate::AnyError::from_stack(err)
            }
            match<T: ::std::error::Error + Send + Sync + 'static> T -> $crate::AnyError {
                $crate::AnyError::from_std(err)
            }
            match <T: ::std::fmt::Display> T -> $crate::AnyError {
                $crate::AnyError::from_str(err)
            }
        }
    };


}
