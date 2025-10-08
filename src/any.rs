use std::{
    fmt::{self, Formatter},
    ops::Deref,
};

use crate::{
    ErrorRef, FromString, Location, SourceFormat, StackError, StackErrorExt, StdErr, location,
};

pub(crate) struct StdWrapper {
    inner: Box<dyn std::error::Error + Send + Sync + 'static>,
    location: Option<Location>,
}

impl fmt::Display for StdWrapper {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.inner)?;
        if f.alternate() {
            self.report().fmt_sources(f, SourceFormat::OneLine)?;
        }
        Ok(())
    }
}

impl fmt::Debug for StdWrapper {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            f.debug_struct("StdWrapper")
                .field("inner", &self.inner)
                .field("location", &self.location)
                .finish()
        } else {
            self.report().full().format(f)
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub struct StdWrapperRef<'a> {
    inner: &'a (dyn std::error::Error),
    location: Option<&'a Location>,
}

impl<'a> StdWrapperRef<'a> {
    pub fn new(inner: &dyn std::error::Error) -> StdWrapperRef<'_> {
        StdWrapperRef {
            inner,
            location: None,
        }
    }
    pub fn as_std(&self) -> &(dyn std::error::Error) {
        self.inner
    }

    pub fn source(self) -> Option<ErrorRef<'a>> {
        self.inner.source().map(|s| ErrorRef::Std(Self::new(s)))
    }
    pub fn location(&self) -> Option<&Location> {
        self.location
    }
}

impl StdWrapper {
    #[track_caller]
    fn new(inner: Box<dyn std::error::Error + Send + Sync + 'static>) -> StdWrapper {
        StdWrapper {
            inner,
            location: location(),
        }
    }
    fn new_untracked(inner: Box<dyn StdErr + Send + Sync + 'static>) -> StdWrapper {
        Self {
            inner,
            location: None,
        }
    }
    fn as_ref(&self) -> StdWrapperRef<'_> {
        StdWrapperRef {
            inner: self.inner.as_ref(),
            location: self.location.as_ref(),
        }
    }
}

impl StackError for StdWrapper {
    fn as_std(&self) -> &(dyn StdErr + Send + Sync + 'static) {
        self.inner.as_ref()
    }

    fn location(&self) -> Option<&Location> {
        self.location.as_ref()
    }

    fn set_location(&mut self, location: Location) {
        self.location = Some(location);
    }

    fn source(&self) -> Option<ErrorRef<'_>> {
        self.as_ref().source()
    }

    fn is_transparent(&self) -> bool {
        false
    }
}

pub struct AnyError(Inner);

enum Inner {
    Stack(Box<dyn StackError>),
    Std(StdWrapper),
}

impl AnyError {
    #[track_caller]
    pub fn from_std(err: impl std::error::Error + Send + Sync + 'static) -> Self {
        Self::from_std_box(Box::new(err))
    }

    #[cfg(feature = "anyhow")]
    pub fn from_anyhow(err: anyhow::Error) -> Self {
        Self::from_std_box(err.into_boxed_dyn_error())
    }

    #[track_caller]
    pub(crate) fn from_std_untracked(err: impl std::error::Error + Send + Sync + 'static) -> Self {
        Self(Inner::Std(StdWrapper::new_untracked(Box::new(err))))
    }

    #[track_caller]
    pub fn from_std_box(err: Box<dyn std::error::Error + Send + Sync + 'static>) -> Self {
        Self(Inner::Std(StdWrapper::new(err)))
    }

    #[track_caller]
    pub fn from_str(s: impl fmt::Display) -> Self {
        FromString::without_source(s.to_string()).into_any()
    }

    fn inner(&self) -> &dyn StackError {
        match &self.0 {
            Inner::Stack(err) => err.deref(),
            Inner::Std(err) => err,
        }
    }

    fn inner_mut(&mut self) -> &mut dyn StackError {
        match &mut self.0 {
            Inner::Stack(err) => err.as_mut(),
            Inner::Std(err) => err,
        }
    }

    #[track_caller]
    pub fn from_stack(err: impl StackError + 'static) -> Self {
        Self(Inner::Stack(Box::new(err)))
    }

    #[track_caller]
    pub fn context(self, context: impl fmt::Display) -> AnyError {
        FromString::with_source(context.to_string(), self).into_any()
    }

    pub fn as_ref<'a>(&'a self) -> ErrorRef<'a> {
        match &self.0 {
            Inner::Stack(error) => ErrorRef::Stack(error.deref()),
            Inner::Std(error) => ErrorRef::Std(error.as_ref()),
        }
    }

    pub fn into_boxed_dyn_error(self) -> Box<dyn std::error::Error + Send + Sync + 'static> {
        Box::new(AnyErrorAsStd(self))
    }
}

impl fmt::Display for AnyError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.inner())?;
        if f.alternate() {
            self.report().fmt_sources(f, SourceFormat::OneLine)?;
        }
        Ok(())
    }
}

impl fmt::Debug for AnyError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            match &self.0 {
                Inner::Stack(error) => {
                    write!(f, "Stack({error:#?})")
                }
                Inner::Std(error) => {
                    write!(f, "Std({error:#?})")
                }
            }
        } else {
            self.report().full().format(f)
        }
    }
}

// TODO: Maybe remove this impl
impl StackError for AnyError {
    fn as_std(&self) -> &(dyn StdErr + Send + Sync + 'static) {
        self.inner().as_std()
    }

    fn location(&self) -> Option<&Location> {
        self.inner().location()
    }

    fn set_location(&mut self, location: Location) {
        self.inner_mut().set_location(location)
    }

    fn source(&self) -> Option<ErrorRef<'_>> {
        self.inner().source()
    }

    fn is_transparent(&self) -> bool {
        self.inner().is_transparent()
    }
}

#[derive(derive_more::Debug, derive_more::Display)]
struct AnyErrorAsStd(AnyError);

impl std::error::Error for AnyErrorAsStd {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.0.as_std().source()
    }
}

// #[cfg(feature = "anyhow")]
// impl From<anyhow::Error> for AnyError {
//     fn from(value: anyhow::Error) -> Self {
//         Self::Std(value.into_boxed_dyn_error())
//     }
// }

// impl From<&str> for AnyError {
//     fn from(value: &str) -> Self {
//         crate::FromString::without_source(value.to_string()).into_any()
//     }
// }

// impl From<String> for AnyError {
//     fn from(value: String) -> Self {
//         crate::FromString::without_source(value).into_any()
//     }
// }

impl<E> From<E> for AnyError
where
    E: StdErr + Send + Sync + 'static,
{
    fn from(value: E) -> Self {
        Self::from_std(value)
    }
}

// impl From<Box<dyn std::error::Error + Send + Sync + 'static>> for AnyError {
//     fn from(value: Box<dyn std::error::Error + Send + Sync + 'static>) -> Self {
//         Self::Std(value)
//     }
// }
