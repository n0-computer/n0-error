use std::{
    convert::Infallible,
    fmt::{self, Formatter},
    ops::Deref,
};

use crate::{ErrorRef, FromString, Meta, SourceFormat, StackError, StackErrorExt};

pub struct AnyError(Inner);

enum Inner {
    Stack(Box<dyn StackError>),
    Std(Box<dyn std::error::Error + Send + Sync>, Meta),
}

impl AnyError {
    #[track_caller]
    pub fn from_std(err: impl std::error::Error + Send + Sync + 'static) -> Self {
        Self::from_std_box(Box::new(err))
    }

    #[track_caller]
    #[cfg(feature = "anyhow")]
    pub fn from_anyhow(err: anyhow::Error) -> Self {
        Self::from_std_box(err.into_boxed_dyn_error())
    }

    #[track_caller]
    pub fn from_std_box(err: Box<dyn std::error::Error + Send + Sync + 'static>) -> Self {
        Self(Inner::Std(err, Meta::default()))
    }

    #[track_caller]
    pub fn from_display(s: impl fmt::Display) -> Self {
        Self::from_string(s.to_string())
    }

    #[track_caller]
    pub fn from_string(message: String) -> Self {
        FromString::WithoutSource {
            message,
            meta: Meta::default(),
        }
        .into_any()
    }

    #[track_caller]
    pub fn from_stack(err: impl StackError + 'static) -> Self {
        Self::from_stack_box(Box::new(err))
    }

    #[track_caller]
    pub fn from_stack_box(err: Box<dyn StackError>) -> Self {
        Self(Inner::Stack(err))
    }

    #[track_caller]
    pub fn context(self, context: impl fmt::Display) -> AnyError {
        FromString::WithSource {
            message: context.to_string(),
            source: self,
            meta: Meta::default(),
        }
        .into_any()
    }

    pub fn as_ref<'a>(&'a self) -> ErrorRef<'a> {
        match &self.0 {
            Inner::Stack(error) => ErrorRef::Stack(error.deref()),
            Inner::Std(error, _) => ErrorRef::std(error.as_ref()),
        }
    }

    pub fn into_boxed_dyn_error(self) -> Box<dyn std::error::Error + Send + Sync + 'static> {
        Box::new(AnyErrorAsStd(self))
    }
}

impl fmt::Display for AnyError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_ref())?;
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
                Inner::Std(error, meta) => {
                    write!(f, "Std({error:#?}, {meta:?})")
                }
            }
        } else {
            self.report().full().format(f)
        }
    }
}

impl StackError for AnyError {
    fn as_dyn(&self) -> &(dyn StackError) {
        self
    }

    fn as_std(&self) -> &(dyn std::error::Error + Send + Sync + 'static) {
        match &self.0 {
            Inner::Std(err, _) => err.as_ref(),
            Inner::Stack(err) => err.as_std(),
        }
    }

    fn meta(&self) -> Option<&Meta> {
        match &self.0 {
            Inner::Std(_, meta) => Some(meta),
            Inner::Stack(err) => err.meta(),
        }
    }

    fn source(&self) -> Option<ErrorRef<'_>> {
        self.as_ref().source()
    }

    fn is_transparent(&self) -> bool {
        self.as_ref().is_transparent()
    }
}

#[derive(derive_more::Debug, derive_more::Display)]
struct AnyErrorAsStd(AnyError);

impl std::error::Error for AnyErrorAsStd {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.0.as_std().source()
    }
}

// #[derive(derive_more::Display, derive_more::Debug)]
// struct AnyErrorAsStack<'a>(&'a AnyError);

pub trait IntoAnyError {
    #[track_caller]
    fn into_any_error(self) -> AnyError;
}

impl From<String> for AnyError {
    #[track_caller]
    fn from(value: String) -> Self {
        Self::from_display(value)
    }
}

impl From<&str> for AnyError {
    #[track_caller]
    fn from(value: &str) -> Self {
        Self::from_display(value)
    }
}

impl From<Box<dyn std::error::Error + Send + Sync>> for AnyError {
    #[track_caller]
    fn from(value: Box<dyn std::error::Error + Send + Sync>) -> Self {
        Self::from_std_box(value)
    }
}

#[cfg(feature = "anyhow")]
impl From<anyhow::Error> for AnyError {
    #[track_caller]
    fn from(value: anyhow::Error) -> Self {
        Self::from_anyhow(value)
    }
}

impl std::str::FromStr for AnyError {
    type Err = Infallible;

    #[track_caller]
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self::from_display(s))
    }
}

impl<T: IntoAnyError> From<T> for AnyError {
    #[track_caller]
    fn from(value: T) -> Self {
        value.into_any_error()
    }
}
