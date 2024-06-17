use std::{borrow::Cow, convert::Infallible, marker::PhantomData};

use ariadne::Color;
use juice_core::diag::Colored;
use juice_macros::DiagnosticNote;

#[derive(Debug, Clone, DiagnosticNote)]
pub enum DiagnosticNote<'a, 'b> {
    #[diag(note = "This is a simple note")]
    SimpleNote,
    #[diag(note = "This is a note with an argument: {}")]
    NoteWithArg(u32),
    #[diag(note = "This is a note with many arguments: {}, {}, {}, {}")]
    NoteWithManyArgs { a: u32, b: i32, c: f32, d: &'a str },
    #[diag(note = "This is a note with an into argument: {}")]
    NoteWithIntoArg(#[diag(into)] String),
    #[diag(note = "This is a note with a colored argument: {}")]
    NoteWithColoredArg(#[diag(into)] Colored<u32>),
    #[diag(note = "This is a note with a default argument: {}")]
    NoteWithDefaultArg {
        #[diag(default = 42)]
        a: u32,
    },
    #[diag(note = "This is a note with a default into argument: {}")]
    NoteWithDefaultIntoArg {
        #[diag(into, default = 42)]
        colored: Colored<u32>,
    },
    #[diag(unreachable)]
    _Unreachable(Infallible, PhantomData<&'b ()>),
}

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

    #[test]
    fn test_note_with_default_arg() {
        let diagnostic = DiagnosticNote::note_with_default_arg();
        assert_eq!(
            diagnostic.clone().into_formatted_message(None),
            "This is a note with a default argument: 42"
        );
        assert_eq!(
            diagnostic.into_formatted_message(Some(Color::Red)),
            "This is a note with a default argument: 42"
        );
    }

    #[test]
    fn test_note_with_default_into_arg() {
        let diagnostic = DiagnosticNote::note_with_default_into_arg();
        assert_eq!(
            diagnostic.clone().into_formatted_message(None),
            "This is a note with a default into argument: 42"
        );
        assert_eq!(
            diagnostic.into_formatted_message(Some(Color::Blue)),
            "This is a note with a default into argument: \u{1b}[34m42\u{1b}[0m"
        );
    }
}
