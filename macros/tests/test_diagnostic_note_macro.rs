use std::borrow::Cow;

use ariadne::Color;
use juice_core::diag::Colored;
use juice_macros::diagnostic_note;

diagnostic_note!(
    DiagnosticNote;
    SimpleNote => "This is a simple note",
    NoteWithArg(number: u32) => "This is a note with an argument: {}",
    NoteWithManyArgs(a: u32, b: i32, c: f32, d: &'static str) => "This is a note with many arguments: {}, {}, {}, {}",
    NoteWithIntoArg(string: into String) => "This is a note with an into argument: {}",
    NoteWithColoredArg(colored: into Colored<u32>) => "This is a note with a colored argument: {}",
);

#[allow(clippy::approx_constant)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_note() {
        let diagnostic = DiagnosticNote::simple_note();
        assert_eq!(diagnostic.clone().into_formatted_message(None), "This is a simple note");
        assert_eq!(
            diagnostic.into_formatted_message(Some(Color::Red)),
            "This is a simple note"
        );
    }

    #[test]
    fn test_note_with_arg() {
        let diagnostic = DiagnosticNote::note_with_arg(42);
        assert_eq!(
            diagnostic.clone().into_formatted_message(None),
            "This is a note with an argument: 42"
        );
        assert_eq!(
            diagnostic.into_formatted_message(Some(Color::Red)),
            "This is a note with an argument: 42"
        );
    }

    #[test]
    fn test_note_with_many_args() {
        let diagnostic = DiagnosticNote::note_with_many_args(42, -42, 3.14, "hello");
        assert_eq!(
            diagnostic.clone().into_formatted_message(None),
            "This is a note with many arguments: 42, -42, 3.14, hello"
        );
        assert_eq!(
            diagnostic.into_formatted_message(Some(Color::Red)),
            "This is a note with many arguments: 42, -42, 3.14, hello"
        );
    }

    #[test]
    fn test_note_with_into_arg() {
        let string = "hello";
        let diagnostic = DiagnosticNote::note_with_into_arg(string);
        assert_eq!(
            diagnostic.clone().into_formatted_message(None),
            "This is a note with an into argument: hello"
        );
        assert_eq!(
            diagnostic.into_formatted_message(Some(Color::Red)),
            "This is a note with an into argument: hello"
        );

        let string = String::from("hello");
        let diagnostic = DiagnosticNote::note_with_into_arg(string);
        assert_eq!(
            diagnostic.clone().into_formatted_message(None),
            "This is a note with an into argument: hello"
        );
        assert_eq!(
            diagnostic.into_formatted_message(Some(Color::Red)),
            "This is a note with an into argument: hello"
        );

        let string = Cow::Borrowed("hello");
        let diagnostic = DiagnosticNote::note_with_into_arg(string);
        assert_eq!(
            diagnostic.clone().into_formatted_message(None),
            "This is a note with an into argument: hello"
        );
        assert_eq!(
            diagnostic.into_formatted_message(Some(Color::Red)),
            "This is a note with an into argument: hello"
        );
    }

    #[test]
    fn test_note_with_colored_arg() {
        let diagnostic = DiagnosticNote::note_with_colored_arg(42);
        assert_eq!(
            diagnostic.clone().into_formatted_message(None),
            "This is a note with a colored argument: 42"
        );
        assert_eq!(
            diagnostic.into_formatted_message(Some(Color::Blue)),
            "This is a note with a colored argument: \u{1b}[34m42\u{1b}[0m"
        );
    }
}
