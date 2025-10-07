use std::sync::OnceLock;

pub type Location = &'static std::panic::Location<'static>;

#[cfg(test)]
static BACKTRACE_ENABLED: OnceLock<std::sync::RwLock<bool>> = OnceLock::new();

#[cfg(not(test))]
static BACKTRACE_ENABLED: OnceLock<bool> = OnceLock::new();

#[doc(hidden)]
pub fn backtrace_enabled() -> bool {
    let from_env = || {
        matches!(
            std::env::var("RUST_BACKTRACE").as_deref(),
            Ok("1") | Ok("full")
        )
    };
    #[cfg(test)]
    return *(BACKTRACE_ENABLED
        .get_or_init(|| std::sync::RwLock::new(from_env()))
        .read()
        .unwrap());

    #[cfg(not(test))]
    return *(BACKTRACE_ENABLED.get_or_init(from_env));
}

#[doc(hidden)]
#[cfg(test)]
pub fn set_backtrace_enabled(value: bool) {
    let mut inner = BACKTRACE_ENABLED
        .get_or_init(Default::default)
        .write()
        .unwrap();
    *inner = value;
}

#[doc(hidden)]
#[track_caller]
pub fn location() -> Option<Location> {
    if backtrace_enabled() {
        Some(std::panic::Location::caller())
    } else {
        None
    }
}
