use std::{
    borrow::Cow,
    fmt::{Display, Formatter, Result as FmtResult},
    marker::PhantomData,
    sync::{Arc, Mutex, PoisonError},
};

use ariadne::{Color, Fmt as _, ReportKind};

#[derive(Clone, Default)]
pub struct ColorGenerator {
    colors: Arc<Mutex<(ariadne::ColorGenerator, Vec<Color>)>>,
    current: usize,
}

impl ColorGenerator {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn from_generator(generator: ariadne::ColorGenerator) -> Self {
        Self {
            colors: Arc::new(Mutex::new((generator, Vec::new()))),
            current: 0,
        }
    }

    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> Color {
        let mut colors = self.colors.lock().unwrap_or_else(PoisonError::into_inner);
        while self.current >= colors.1.len() {
            let color = colors.0.next();
            colors.1.push(color);
        }

        let color = colors.1[self.current];
        self.current += 1;
        color
    }
}

pub trait ColorExt {
    fn error_color() -> Self;
    fn warning_color() -> Self;
    fn note_color() -> Self;
}

impl ColorExt for Color {
    fn error_color() -> Self {
        Color::Red
    }

    fn warning_color() -> Self {
        Color::Yellow
    }

    fn note_color() -> Self {
        Color::Fixed(115)
    }
}

pub trait DiagnosticArg {
    fn with_color(self, color: impl Into<Option<Color>>) -> impl Display;
}

macro_rules! diagnostic_arg {
    ($($type: ty),*) => {
        $(
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PrefixedWithArticle<T>(T);

impl<T: Display> DiagnosticArg for PrefixedWithArticle<T> {
    fn with_color(self, _color: impl Into<Option<Color>>) -> impl Display {
        let string = self.0.to_string();
        let article = in_definite::get_a_or_an(&string);
        format!("{} {}", article, string)
    }
}

impl<T: Display> From<T> for PrefixedWithArticle<T> {
    fn from(value: T) -> Self {
        Self(value)
    }
}

impl<T: Display> From<T> for Colored<PrefixedWithArticle<T>> {
    fn from(value: T) -> Self {
        Self(PrefixedWithArticle(value))
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

impl<T> DiagnosticArg for PhantomData<T> {
    fn with_color(self, _color: impl Into<Option<Color>>) -> impl Display {
        "PhantomData"
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticKind {
    Error,
    Warning,
}

impl DiagnosticKind {
    pub fn is_error(self) -> bool {
        matches!(self, Self::Error)
    }

    pub fn is_warning(self) -> bool {
        matches!(self, Self::Warning)
    }
}

impl Display for DiagnosticKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            DiagnosticKind::Error => write!(f, "Error"),
            DiagnosticKind::Warning => write!(f, "Warning"),
        }
    }
}

impl From<DiagnosticKind> for ReportKind<'_> {
    fn from(kind: DiagnosticKind) -> Self {
        match kind {
            DiagnosticKind::Error => ReportKind::Error,
            DiagnosticKind::Warning => ReportKind::Warning,
        }
    }
}

impl From<DiagnosticKind> for Option<Color> {
    fn from(kind: DiagnosticKind) -> Self {
        Some(match kind {
            DiagnosticKind::Error => Color::error_color(),
            DiagnosticKind::Warning => Color::warning_color(),
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DiagnosticCode {
    diagnostic_kind: DiagnosticKind,
    error_code: u32,
}

impl DiagnosticCode {
    pub fn new(diagnostic_kind: DiagnosticKind, error_code: u32) -> Self {
        Self {
            diagnostic_kind,
            error_code,
        }
    }
}

impl Display for DiagnosticCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        let kind = match self.diagnostic_kind {
            DiagnosticKind::Error => 'E',
            DiagnosticKind::Warning => 'W',
        };

        write!(f, "{}{:04}", kind, self.error_code)
    }
}
