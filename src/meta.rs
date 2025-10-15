use std::sync::OnceLock;

#[derive(derive_more::Debug, derive_more::Display, Clone, Copy)]
#[debug("{_0:?}")]
pub struct Location(&'static std::panic::Location<'static>);

#[derive(Debug)]
pub struct Meta {
    location: Location,
}

#[track_caller]
pub fn meta() -> Meta {
    Meta::default()
}

impl Default for Meta {
    #[track_caller]
    fn default() -> Self {
        Self {
            location: Location(std::panic::Location::caller()),
        }
    }
}

impl Meta {
    #[track_caller]
    pub fn new() -> Self {
        Self::default()
    }
    pub fn location(&self) -> &Location {
        &self.location
    }
}

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
        Some(Location(std::panic::Location::caller()))
    } else {
        None
    }
}
