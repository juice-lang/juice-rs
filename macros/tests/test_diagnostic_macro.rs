use std::borrow::Cow;

use ariadne::Color;
use juice_core::diag::{Colored, DiagnosticCode, DiagnosticKind};
use juice_macros::diagnostic;

diagnostic!(
    pub enum Diagnostic<'a> {
        [error] SimpleError => "This is a simple error",
        [warning] SimpleWarning => "This is a simple warning",
        [error] ErrorWithArg(number: u32) => "This is an error with an argument: {}",
        [warning] WarningWithArg(string: &'static str) => "This is a warning with an argument: {}",
        [error] ErrorWithManyArgs(a: u32, b: i32, c: f32, d: &'a str) =>
            "This is an error with many arguments: {}, {}, {}, {}",
        [warning] WarningWithManyArgs(a: u32, b: i32, c: f32, d: &'a str) =>
            "This is a warning with many arguments: {}, {}, {}, {}",
        [error] ErrorWithIntoArg(string: into String) => "This is an error with an into argument: {}",
        [warning] WarningWithIntoArg(string: into String) => "This is a warning with an into argument: {}",
        [error] ErrorWithColoredArg(colored: into Colored<u32>) => "This is an error with a colored argument: {}",
        [warning] WarningWithColoredArg(colored: into Colored<u32>) => "This is a warning with a colored argument: {}",
    }

    pub enum StaticDiagnostic<'a> {
        [error] StaticError => "This is a static error",
        [warning] StaticWarning => "This is a static warning",
        [error] StaticErrorWithArg(number: u32) => "This is a static error with an argument: {}",
        [warning] StaticWarningWithArg(string: &'static str) => "This is a static warning with an argument: {}",
        [error] StaticErrorWithManyArgs(a: u32, b: i32, c: f32, d: &'a str) =>
            "This is a static error with many arguments: {}, {}, {}, {}",
        [warning] StaticWarningWithManyArgs(a: u32, b: i32, c: f32, d: &'a str) =>
            "This is a static warning with many arguments: {}, {}, {}, {}",
        [error] StaticErrorWithIntoArg(string: into String) => "This is a static error with an into argument: {}",
        [warning] StaticWarningWithIntoArg(string: into String) => "This is a static warning with an into argument: {}",
        [error] StaticErrorWithColoredArg(colored: into Colored<u32>) =>
            "This is a static error with a colored argument: {}",
        [warning] StaticWarningWithColoredArg(colored: into Colored<u32>) =>
            "This is a static warning with a colored argument: {}",
    }
);

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
    fn test_static_error() {
        let diagnostic = StaticDiagnostic::static_error();
        assert_eq!(diagnostic.get_code(), DiagnosticCode::new(DiagnosticKind::Error, 10));
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
        assert_eq!(diagnostic.get_code(), DiagnosticCode::new(DiagnosticKind::Warning, 11));
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
        assert_eq!(diagnostic.get_code(), DiagnosticCode::new(DiagnosticKind::Error, 12));
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
        assert_eq!(diagnostic.get_code(), DiagnosticCode::new(DiagnosticKind::Warning, 13));
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
        assert_eq!(diagnostic.get_code(), DiagnosticCode::new(DiagnosticKind::Error, 14));
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
        assert_eq!(diagnostic.get_code(), DiagnosticCode::new(DiagnosticKind::Warning, 15));
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
        assert_eq!(diagnostic.get_code(), DiagnosticCode::new(DiagnosticKind::Error, 16));
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
        assert_eq!(diagnostic.get_code(), DiagnosticCode::new(DiagnosticKind::Error, 16));
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
        assert_eq!(diagnostic.get_code(), DiagnosticCode::new(DiagnosticKind::Error, 16));
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
        assert_eq!(diagnostic.get_code(), DiagnosticCode::new(DiagnosticKind::Warning, 17));
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
        assert_eq!(diagnostic.get_code(), DiagnosticCode::new(DiagnosticKind::Warning, 17));
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
        assert_eq!(diagnostic.get_code(), DiagnosticCode::new(DiagnosticKind::Warning, 17));
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
        assert_eq!(diagnostic.get_code(), DiagnosticCode::new(DiagnosticKind::Error, 18));
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
        assert_eq!(diagnostic.get_code(), DiagnosticCode::new(DiagnosticKind::Warning, 19));
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
}
