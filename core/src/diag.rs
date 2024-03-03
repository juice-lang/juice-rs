use std::{borrow::Cow, fmt::Display, sync::Arc};

use ariadne::{Color, Fmt as _};

mod private {
    pub trait Sealed {}
}

pub trait DiagnosticArg: private::Sealed {
    fn with_color(self, color: impl Into<Option<Color>>) -> impl Display;
}

macro_rules! diagnostic_arg {
    ($($type: ty),*) => {
        $(
            impl private::Sealed for $type {}
            impl DiagnosticArg for $type {
                fn with_color(self, _color: impl Into<Option<Color>>) -> impl Display {
                    self
                }
            }
        )*
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Colored<T>(T);

impl<T: Display> private::Sealed for Colored<T> {}

impl<T: Display> DiagnosticArg for Colored<T> {
    fn with_color(self, color: impl Into<Option<Color>>) -> impl Display {
        self.0.fg(color)
    }
}

impl<T: Display> From<T> for Colored<T> {
    fn from(value: T) -> Self {
        Self(value)
    }
}

diagnostic_arg!(
    u8,
    i8,
    u16,
    i16,
    u32,
    i32,
    u64,
    i64,
    u128,
    i128,
    usize,
    isize,
    f32,
    f64,
    char,
    bool,
    &str,
    String,
    Arc<str>,
    Cow<'_, str>
);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticKind {
    Error,
    Warning,
}
