use std::{borrow::Cow, convert::Infallible, marker::PhantomData};

use ariadne::Color;
use juice_core::diag::{Colored, DiagnosticCode, DiagnosticKind};
use juice_macros::Diagnostic;

#[derive(Debug, Clone, Diagnostic)]
pub enum Diagnostic<'a, 'b> {
    #[diag(error = "This is a simple error")]
    SimpleError,
    #[diag(warning = "This is a simple warning")]
    SimpleWarning,
    #[diag(error = "This is an error with an argument: {}")]
    ErrorWithArg(u32),
    #[diag(warning = "This is a warning with an argument: {}")]
    WarningWithArg(&'static str),
    #[diag(error = "This is an error with many arguments: {}, {}, {}, {}")]
    ErrorWithManyArgs { a: u32, b: i32, c: f32, d: &'a str },
    #[diag(warning = "This is a warning with many arguments: {}, {}, {}, {}")]
    WarningWithManyArgs { a: u32, b: i32, c: f32, d: &'a str },
    #[diag(error = "This is an error with an into argument: {}")]
    ErrorWithIntoArg(#[diag(into)] String),
    #[diag(warning = "This is a warning with an into argument: {}")]
    WarningWithIntoArg(#[diag(into)] String),
    #[diag(error = "This is an error with a colored argument: {}")]
    ErrorWithColoredArg(#[diag(into)] Colored<u32>),
    #[diag(warning = "This is a warning with a colored argument: {}")]
    WarningWithColoredArg(#[diag(into)] Colored<u32>),
    #[diag(error = "This is an error with a default argument: {}")]
    ErrorWithDefaultArg {
        #[diag(default = "hello")]
        string: &'static str,
    },
    #[diag(warning = "This is a warning with a default argument: {}")]
    WarningWithDefaultArg {
        #[diag(default = "hello")]
        string: &'static str,
    },
    #[diag(error = "This is an error with a default into argument: {}")]
    ErrorWithDefaultIntoArg {
        #[diag(into, default = "hello")]
        string: Colored<&'static str>,
    },
    #[diag(warning = "This is a warning with a default into argument: {}")]
    WarningWithDefaultIntoArg {
        #[diag(into, default = "hello")]
        string: Colored<&'static str>,
    },
    #[diag(unreachable)]
    _Unreachable(Infallible, PhantomData<&'b ()>),
}

#[derive(Debug, Clone, Diagnostic)]
#[diag(offset = 14)]
pub enum StaticDiagnostic<'a> {
    #[diag(error = "This is a static error")]
    StaticError,
    #[diag(warning = "This is a static warning")]
    StaticWarning,
    #[diag(error = "This is a static error with an argument: {}")]
    StaticErrorWithArg(u32),
    #[diag(warning = "This is a static warning with an argument: {}")]
    StaticWarningWithArg(&'static str),
    #[diag(error = "This is a static error with many arguments: {}, {}, {}, {}")]
    StaticErrorWithManyArgs { a: u32, b: i32, c: f32, d: &'a str },
    #[diag(warning = "This is a static warning with many arguments: {}, {}, {}, {}")]
    StaticWarningWithManyArgs { a: u32, b: i32, c: f32, d: &'a str },
    #[diag(error = "This is a static error with an into argument: {}")]
    StaticErrorWithIntoArg(#[diag(into)] String),
    #[diag(warning = "This is a static warning with an into argument: {}")]
    StaticWarningWithIntoArg(#[diag(into)] String),
    #[diag(error = "This is a static error with a colored argument: {}")]
    StaticErrorWithColoredArg(#[diag(into)] Colored<u32>),
    #[diag(warning = "This is a static warning with a colored argument: {}")]
    StaticWarningWithColoredArg(#[diag(into)] Colored<u32>),
    #[diag(error = "This is a static error with a default argument: {}")]
    StaticErrorWithDefaultArg {
        #[diag(default = "hello")]
        string: &'static str,
    },
    #[diag(warning = "This is a static warning with a default argument: {}")]
    StaticWarningWithDefaultArg {
        #[diag(default = "hello")]
        string: &'static str,
    },
    #[diag(error = "This is a static error with a default into argument: {}")]
    StaticErrorWithDefaultIntoArg {
        #[diag(into, default = "hello")]
        string: Colored<&'static str>,
    },
    #[diag(warning = "This is a static warning with a default into argument: {}")]
    StaticWarningWithDefaultIntoArg {
        #[diag(into, default = "hello")]
        string: Colored<&'static str>,
    },
}

#[allow(clippy::approx_constant)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_error() {
        let diagnostic = Diagnostic::simple_error();
        assert_eq!(diagnostic.get_code(), DiagnosticCode::new(DiagnosticKind::Error, 0));
        assert_eq!(diagnostic.get_kind(), DiagnosticKind::Error);
        assert_eq!(
            diagnostic.clone().into_formatted_message(None),
            "This is a simple error"
        );
        assert_eq!(
            diagnostic.into_formatted_message(Some(Color::Red)),
            "This is a simple error"
        );
    }

    #[test]
    fn test_simple_warning() {
        let diagnostic = Diagnostic::simple_warning();
        assert_eq!(diagnostic.get_code(), DiagnosticCode::new(DiagnosticKind::Warning, 1));
        assert_eq!(diagnostic.get_kind(), DiagnosticKind::Warning);
        assert_eq!(
            diagnostic.clone().into_formatted_message(None),
            "This is a simple warning"
        );
        assert_eq!(
            diagnostic.into_formatted_message(Some(Color::Yellow)),
            "This is a simple warning"
        );
    }

    #[test]
    fn test_error_with_arg() {
        let diagnostic = Diagnostic::error_with_arg(42);
        assert_eq!(diagnostic.get_code(), DiagnosticCode::new(DiagnosticKind::Error, 2));
        assert_eq!(diagnostic.get_kind(), DiagnosticKind::Error);
        assert_eq!(
            diagnostic.clone().into_formatted_message(None),
            "This is an error with an argument: 42"
        );
        assert_eq!(
            diagnostic.into_formatted_message(Some(Color::Red)),
            "This is an error with an argument: 42"
        );
    }

    #[test]
    fn test_warning_with_arg() {
        let diagnostic = Diagnostic::warning_with_arg("hello");
        assert_eq!(diagnostic.get_code(), DiagnosticCode::new(DiagnosticKind::Warning, 3));
        assert_eq!(diagnostic.get_kind(), DiagnosticKind::Warning);
        assert_eq!(
            diagnostic.clone().into_formatted_message(None),
            "This is a warning with an argument: hello"
        );
        assert_eq!(
            diagnostic.into_formatted_message(Some(Color::Yellow)),
            "This is a warning with an argument: hello"
        );
    }

    #[test]
    fn test_error_with_many_args() {
        let diagnostic = Diagnostic::error_with_many_args(42, -42, 3.14, "hello");
        assert_eq!(diagnostic.get_code(), DiagnosticCode::new(DiagnosticKind::Error, 4));
        assert_eq!(diagnostic.get_kind(), DiagnosticKind::Error);
        assert_eq!(
            diagnostic.clone().into_formatted_message(None),
            "This is an error with many arguments: 42, -42, 3.14, hello"
        );
        assert_eq!(
            diagnostic.into_formatted_message(Some(Color::Red)),
            "This is an error with many arguments: 42, -42, 3.14, hello"
        );
    }

    #[test]
    fn test_warning_with_many_args() {
        let diagnostic = Diagnostic::warning_with_many_args(42, -42, 3.14, "hello");
        assert_eq!(diagnostic.get_code(), DiagnosticCode::new(DiagnosticKind::Warning, 5));
        assert_eq!(diagnostic.get_kind(), DiagnosticKind::Warning);
        assert_eq!(
            diagnostic.clone().into_formatted_message(None),
            "This is a warning with many arguments: 42, -42, 3.14, hello"
        );
        assert_eq!(
            diagnostic.into_formatted_message(Some(Color::Yellow)),
            "This is a warning with many arguments: 42, -42, 3.14, hello"
        );
    }

    #[test]
    fn test_error_with_into_arg() {
        let string = "hello";
        let diagnostic = Diagnostic::error_with_into_arg(string);
        assert_eq!(diagnostic.get_code(), DiagnosticCode::new(DiagnosticKind::Error, 6));
        assert_eq!(diagnostic.get_kind(), DiagnosticKind::Error);
        assert_eq!(
            diagnostic.clone().into_formatted_message(None),
            "This is an error with an into argument: hello"
        );
        assert_eq!(
            diagnostic.into_formatted_message(Some(Color::Red)),
            "This is an error with an into argument: hello"
        );

        let string = String::from("hello");
        let diagnostic = Diagnostic::error_with_into_arg(string);
        assert_eq!(diagnostic.get_code(), DiagnosticCode::new(DiagnosticKind::Error, 6));
        assert_eq!(diagnostic.get_kind(), DiagnosticKind::Error);
        assert_eq!(
            diagnostic.clone().into_formatted_message(None),
            "This is an error with an into argument: hello"
        );
        assert_eq!(
            diagnostic.into_formatted_message(Some(Color::Red)),
            "This is an error with an into argument: hello"
        );

        let string = Cow::Borrowed("hello");
        let diagnostic = Diagnostic::error_with_into_arg(string);
        assert_eq!(diagnostic.get_code(), DiagnosticCode::new(DiagnosticKind::Error, 6));
        assert_eq!(diagnostic.get_kind(), DiagnosticKind::Error);
        assert_eq!(
            diagnostic.clone().into_formatted_message(None),
            "This is an error with an into argument: hello"
        );
        assert_eq!(
            diagnostic.into_formatted_message(Some(Color::Red)),
            "This is an error with an into argument: hello"
        );
    }

    #[test]
    fn test_warning_with_into_arg() {
        let string = "hello";
        let diagnostic = Diagnostic::warning_with_into_arg(string);
        assert_eq!(diagnostic.get_code(), DiagnosticCode::new(DiagnosticKind::Warning, 7));
        assert_eq!(diagnostic.get_kind(), DiagnosticKind::Warning);
        assert_eq!(
            diagnostic.clone().into_formatted_message(None),
            "This is a warning with an into argument: hello"
        );
        assert_eq!(
            diagnostic.into_formatted_message(Some(Color::Yellow)),
            "This is a warning with an into argument: hello"
        );

        let string = String::from("hello");
        let diagnostic = Diagnostic::warning_with_into_arg(string);
        assert_eq!(diagnostic.get_code(), DiagnosticCode::new(DiagnosticKind::Warning, 7));
        assert_eq!(diagnostic.get_kind(), DiagnosticKind::Warning);
        assert_eq!(
            diagnostic.clone().into_formatted_message(None),
            "This is a warning with an into argument: hello"
        );
        assert_eq!(
            diagnostic.into_formatted_message(Some(Color::Yellow)),
            "This is a warning with an into argument: hello"
        );

        let string = Cow::Borrowed("hello");
        let diagnostic = Diagnostic::warning_with_into_arg(string);
        assert_eq!(diagnostic.get_code(), DiagnosticCode::new(DiagnosticKind::Warning, 7));
        assert_eq!(diagnostic.get_kind(), DiagnosticKind::Warning);
        assert_eq!(
            diagnostic.clone().into_formatted_message(None),
            "This is a warning with an into argument: hello"
        );
        assert_eq!(
            diagnostic.into_formatted_message(Some(Color::Yellow)),
            "This is a warning with an into argument: hello"
        );
    }

    #[test]
    fn test_error_with_colored_arg() {
        let diagnostic = Diagnostic::error_with_colored_arg(42);
        assert_eq!(diagnostic.get_code(), DiagnosticCode::new(DiagnosticKind::Error, 8));
        assert_eq!(diagnostic.get_kind(), DiagnosticKind::Error);
        assert_eq!(
            diagnostic.clone().into_formatted_message(None),
            "This is an error with a colored argument: 42"
        );
        assert_eq!(
            diagnostic.into_formatted_message(Some(Color::Red)),
            "This is an error with a colored argument: \u{1b}[31m42\u{1b}[0m"
        );
    }

    #[test]
    fn test_warning_with_colored_arg() {
        let diagnostic = Diagnostic::warning_with_colored_arg(42);
        assert_eq!(diagnostic.get_code(), DiagnosticCode::new(DiagnosticKind::Warning, 9));
        assert_eq!(diagnostic.get_kind(), DiagnosticKind::Warning);
        assert_eq!(
            diagnostic.clone().into_formatted_message(None),
            "This is a warning with a colored argument: 42"
        );
        assert_eq!(
            diagnostic.into_formatted_message(Some(Color::Yellow)),
            "This is a warning with a colored argument: \u{1b}[33m42\u{1b}[0m"
        );
    }

    #[test]
    fn test_error_with_default_arg() {
        let diagnostic = Diagnostic::error_with_default_arg();
        assert_eq!(diagnostic.get_code(), DiagnosticCode::new(DiagnosticKind::Error, 10));
        assert_eq!(diagnostic.get_kind(), DiagnosticKind::Error);
        assert_eq!(
            diagnostic.clone().into_formatted_message(None),
            "This is an error with a default argument: hello"
        );
        assert_eq!(
            diagnostic.into_formatted_message(Some(Color::Red)),
            "This is an error with a default argument: hello"
        );
    }

    #[test]
    fn test_warning_with_default_arg() {
        let diagnostic = Diagnostic::warning_with_default_arg();
        assert_eq!(diagnostic.get_code(), DiagnosticCode::new(DiagnosticKind::Warning, 11));
        assert_eq!(diagnostic.get_kind(), DiagnosticKind::Warning);
        assert_eq!(
            diagnostic.clone().into_formatted_message(None),
            "This is a warning with a default argument: hello"
        );
        assert_eq!(
            diagnostic.into_formatted_message(Some(Color::Red)),
            "This is a warning with a default argument: hello"
        );
    }

    #[test]
    fn test_error_with_default_into_arg() {
        let diagnostic = Diagnostic::error_with_default_into_arg();
        assert_eq!(diagnostic.get_code(), DiagnosticCode::new(DiagnosticKind::Error, 12));
        assert_eq!(diagnostic.get_kind(), DiagnosticKind::Error);
        assert_eq!(
            diagnostic.clone().into_formatted_message(None),
            "This is an error with a default into argument: hello"
        );
        assert_eq!(
            diagnostic.into_formatted_message(Some(Color::Red)),
            "This is an error with a default into argument: \u{1b}[31mhello\u{1b}[0m"
        );
    }

    #[test]
    fn test_warning_with_default_into_arg() {
        let diagnostic = Diagnostic::warning_with_default_into_arg();
        assert_eq!(diagnostic.get_code(), DiagnosticCode::new(DiagnosticKind::Warning, 13));
        assert_eq!(diagnostic.get_kind(), DiagnosticKind::Warning);
        assert_eq!(
            diagnostic.clone().into_formatted_message(None),
            "This is a warning with a default into argument: hello"
        );
        assert_eq!(
            diagnostic.into_formatted_message(Some(Color::Yellow)),
            "This is a warning with a default into argument: \u{1b}[33mhello\u{1b}[0m"
        );
    }

    #[test]
    fn test_static_error() {
        let diagnostic = StaticDiagnostic::static_error();
        assert_eq!(diagnostic.get_code(), DiagnosticCode::new(DiagnosticKind::Error, 14));
        assert_eq!(diagnostic.get_kind(), DiagnosticKind::Error);
        assert_eq!(
            diagnostic.clone().into_formatted_message(None),
            "This is a static error"
        );
        assert_eq!(
            diagnostic.into_formatted_message(Some(Color::Red)),
            "This is a static error"
        );
    }

    #[test]
    fn test_static_warning() {
        let diagnostic = StaticDiagnostic::static_warning();
        assert_eq!(diagnostic.get_code(), DiagnosticCode::new(DiagnosticKind::Warning, 15));
        assert_eq!(diagnostic.get_kind(), DiagnosticKind::Warning);
        assert_eq!(
            diagnostic.clone().into_formatted_message(None),
            "This is a static warning"
        );
        assert_eq!(
            diagnostic.into_formatted_message(Some(Color::Yellow)),
            "This is a static warning"
        );
    }

    #[test]
    fn test_static_error_with_arg() {
        let diagnostic = StaticDiagnostic::static_error_with_arg(42);
        assert_eq!(diagnostic.get_code(), DiagnosticCode::new(DiagnosticKind::Error, 16));
        assert_eq!(diagnostic.get_kind(), DiagnosticKind::Error);
        assert_eq!(
            diagnostic.clone().into_formatted_message(None),
            "This is a static error with an argument: 42"
        );
        assert_eq!(
            diagnostic.into_formatted_message(Some(Color::Red)),
            "This is a static error with an argument: 42"
        );
    }

    #[test]
    fn test_static_warning_with_arg() {
        let diagnostic = StaticDiagnostic::static_warning_with_arg("hello");
        assert_eq!(diagnostic.get_code(), DiagnosticCode::new(DiagnosticKind::Warning, 17));
        assert_eq!(diagnostic.get_kind(), DiagnosticKind::Warning);
        assert_eq!(
            diagnostic.clone().into_formatted_message(None),
            "This is a static warning with an argument: hello"
        );
        assert_eq!(
            diagnostic.into_formatted_message(Some(Color::Yellow)),
            "This is a static warning with an argument: hello"
        );
    }

    #[test]
    fn test_static_error_with_many_args() {
        let diagnostic = StaticDiagnostic::static_error_with_many_args(42, -42, 3.14, "hello");
        assert_eq!(diagnostic.get_code(), DiagnosticCode::new(DiagnosticKind::Error, 18));
        assert_eq!(diagnostic.get_kind(), DiagnosticKind::Error);
        assert_eq!(
            diagnostic.clone().into_formatted_message(None),
            "This is a static error with many arguments: 42, -42, 3.14, hello"
        );
        assert_eq!(
            diagnostic.into_formatted_message(Some(Color::Red)),
            "This is a static error with many arguments: 42, -42, 3.14, hello"
        );
    }

    #[test]
    fn test_static_warning_with_many_args() {
        let diagnostic = StaticDiagnostic::static_warning_with_many_args(42, -42, 3.14, "hello");
        assert_eq!(diagnostic.get_code(), DiagnosticCode::new(DiagnosticKind::Warning, 19));
        assert_eq!(diagnostic.get_kind(), DiagnosticKind::Warning);
        assert_eq!(
            diagnostic.clone().into_formatted_message(None),
            "This is a static warning with many arguments: 42, -42, 3.14, hello"
        );
        assert_eq!(
            diagnostic.into_formatted_message(Some(Color::Yellow)),
            "This is a static warning with many arguments: 42, -42, 3.14, hello"
        );
    }

    #[test]
    fn test_static_error_with_into_arg() {
        let string = "hello";
        let diagnostic = StaticDiagnostic::static_error_with_into_arg(string);
        assert_eq!(diagnostic.get_code(), DiagnosticCode::new(DiagnosticKind::Error, 20));
        assert_eq!(diagnostic.get_kind(), DiagnosticKind::Error);
        assert_eq!(
            diagnostic.clone().into_formatted_message(None),
            "This is a static error with an into argument: hello"
        );
        assert_eq!(
            diagnostic.into_formatted_message(Some(Color::Red)),
            "This is a static error with an into argument: hello"
        );

        let string = String::from("hello");
        let diagnostic = StaticDiagnostic::static_error_with_into_arg(string);
        assert_eq!(diagnostic.get_code(), DiagnosticCode::new(DiagnosticKind::Error, 20));
        assert_eq!(diagnostic.get_kind(), DiagnosticKind::Error);
        assert_eq!(
            diagnostic.clone().into_formatted_message(None),
            "This is a static error with an into argument: hello"
        );
        assert_eq!(
            diagnostic.into_formatted_message(Some(Color::Red)),
            "This is a static error with an into argument: hello"
        );

        let string = Cow::Borrowed("hello");
        let diagnostic = StaticDiagnostic::static_error_with_into_arg(string);
        assert_eq!(diagnostic.get_code(), DiagnosticCode::new(DiagnosticKind::Error, 20));
        assert_eq!(diagnostic.get_kind(), DiagnosticKind::Error);
        assert_eq!(
            diagnostic.clone().into_formatted_message(None),
            "This is a static error with an into argument: hello"
        );
        assert_eq!(
            diagnostic.into_formatted_message(Some(Color::Red)),
            "This is a static error with an into argument: hello"
        );
    }

    #[test]
    fn test_static_warning_with_into_arg() {
        let string = "hello";
        let diagnostic = StaticDiagnostic::static_warning_with_into_arg(string);
        assert_eq!(diagnostic.get_code(), DiagnosticCode::new(DiagnosticKind::Warning, 21));
        assert_eq!(diagnostic.get_kind(), DiagnosticKind::Warning);
        assert_eq!(
            diagnostic.clone().into_formatted_message(None),
            "This is a static warning with an into argument: hello"
        );
        assert_eq!(
            diagnostic.into_formatted_message(Some(Color::Yellow)),
            "This is a static warning with an into argument: hello"
        );

        let string = String::from("hello");
        let diagnostic = StaticDiagnostic::static_warning_with_into_arg(string);
        assert_eq!(diagnostic.get_code(), DiagnosticCode::new(DiagnosticKind::Warning, 21));
        assert_eq!(diagnostic.get_kind(), DiagnosticKind::Warning);
        assert_eq!(
            diagnostic.clone().into_formatted_message(None),
            "This is a static warning with an into argument: hello"
        );
        assert_eq!(
            diagnostic.into_formatted_message(Some(Color::Yellow)),
            "This is a static warning with an into argument: hello"
        );

        let string = Cow::Borrowed("hello");
        let diagnostic = StaticDiagnostic::static_warning_with_into_arg(string);
        assert_eq!(diagnostic.get_code(), DiagnosticCode::new(DiagnosticKind::Warning, 21));
        assert_eq!(diagnostic.get_kind(), DiagnosticKind::Warning);
        assert_eq!(
            diagnostic.clone().into_formatted_message(None),
            "This is a static warning with an into argument: hello"
        );
        assert_eq!(
            diagnostic.into_formatted_message(Some(Color::Yellow)),
            "This is a static warning with an into argument: hello"
        );
    }

    #[test]
    fn test_static_error_with_colored_arg() {
        let diagnostic = StaticDiagnostic::static_error_with_colored_arg(42);
        assert_eq!(diagnostic.get_code(), DiagnosticCode::new(DiagnosticKind::Error, 22));
        assert_eq!(diagnostic.get_kind(), DiagnosticKind::Error);
        assert_eq!(
            diagnostic.clone().into_formatted_message(None),
            "This is a static error with a colored argument: 42"
        );
        assert_eq!(
            diagnostic.into_formatted_message(Some(Color::Red)),
            "This is a static error with a colored argument: \u{1b}[31m42\u{1b}[0m"
        );
    }

    #[test]
    fn test_static_warning_with_colored_arg() {
        let diagnostic = StaticDiagnostic::static_warning_with_colored_arg(42);
        assert_eq!(diagnostic.get_code(), DiagnosticCode::new(DiagnosticKind::Warning, 23));
        assert_eq!(diagnostic.get_kind(), DiagnosticKind::Warning);
        assert_eq!(
            diagnostic.clone().into_formatted_message(None),
            "This is a static warning with a colored argument: 42"
        );
        assert_eq!(
            diagnostic.into_formatted_message(Some(Color::Yellow)),
            "This is a static warning with a colored argument: \u{1b}[33m42\u{1b}[0m"
        );
    }

    #[test]
    fn test_static_error_with_default_arg() {
        let diagnostic = StaticDiagnostic::static_error_with_default_arg();
        assert_eq!(diagnostic.get_code(), DiagnosticCode::new(DiagnosticKind::Error, 24));
        assert_eq!(diagnostic.get_kind(), DiagnosticKind::Error);
        assert_eq!(
            diagnostic.clone().into_formatted_message(None),
            "This is a static error with a default argument: hello"
        );
        assert_eq!(
            diagnostic.into_formatted_message(Some(Color::Red)),
            "This is a static error with a default argument: hello"
        );
    }

    #[test]
    fn test_static_warning_with_default_arg() {
        let diagnostic = StaticDiagnostic::static_warning_with_default_arg();
        assert_eq!(diagnostic.get_code(), DiagnosticCode::new(DiagnosticKind::Warning, 25));
        assert_eq!(diagnostic.get_kind(), DiagnosticKind::Warning);
        assert_eq!(
            diagnostic.clone().into_formatted_message(None),
            "This is a static warning with a default argument: hello"
        );
        assert_eq!(
            diagnostic.into_formatted_message(Some(Color::Yellow)),
            "This is a static warning with a default argument: hello"
        );
    }

    #[test]
    fn test_static_error_with_default_into_arg() {
        let diagnostic = StaticDiagnostic::static_error_with_default_into_arg();
        assert_eq!(diagnostic.get_code(), DiagnosticCode::new(DiagnosticKind::Error, 26));
        assert_eq!(diagnostic.get_kind(), DiagnosticKind::Error);
        assert_eq!(
            diagnostic.clone().into_formatted_message(None),
            "This is a static error with a default into argument: hello"
        );
        assert_eq!(
            diagnostic.into_formatted_message(Some(Color::Red)),
            "This is a static error with a default into argument: \u{1b}[31mhello\u{1b}[0m"
        );
    }

    #[test]
    fn test_static_warning_with_default_into_arg() {
        let diagnostic = StaticDiagnostic::static_warning_with_default_into_arg();
        assert_eq!(diagnostic.get_code(), DiagnosticCode::new(DiagnosticKind::Warning, 27));
        assert_eq!(diagnostic.get_kind(), DiagnosticKind::Warning);
        assert_eq!(
            diagnostic.clone().into_formatted_message(None),
            "This is a static warning with a default into argument: hello"
        );
        assert_eq!(
            diagnostic.into_formatted_message(Some(Color::Yellow)),
            "This is a static warning with a default into argument: \u{1b}[33mhello\u{1b}[0m"
        );
    }
}
