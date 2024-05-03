use std::borrow::Cow;

use juice_core::diag::Colored;
use juice_macros::diagnostic;

diagnostic!(
    #[derive(Debug, Clone)]
    pub enum Diagnostic<'src> {
        [error] InvalidCharacter(c: into Colored<char>) => "Invalid character `{}` in source file",
        [error] UnterminatedComment => "Unterminated block comment",
        [error] UnexpectedCommentTerminator => "Unexpected block comment terminator",
        [error] InvalidDigit(digit_name: &'static str, c: into Colored<char>, literal_name: &'static str) =>
            "Invalid {} `{}` in {} literal",
        [error] MissingDigit(digit_name: &'static str, literal_name: &'static str) => "Missing {} in {} literal",
        [error] ExpectedLiteralTerminator(terminator: into Colored<Cow<'static, str>>, literal_name: &'static str) =>
            "Expected `{}` to terminate {} literal",
        [error] EmptyLiteral(literal_name: &'static str) => "Empty {} literal",
        [error] StringInCharLiteral => "Character literal may only contain one codepoint",
        [error] NewlineInLiteral(literal_name: &'static str) => "Newline in {} literal",
        [error] ExpectedEscapeSequence(c: into Colored<char> = '\\', literal_name: &'static str) =>
            "Expected escape sequence after `{}` in {} literal",
        [error] InvalidUnicodeScalar(hex: into Colored<&'src str>, literal_name: &'static str) =>
            "Invalid Unicode scalar value `{}` in {} literal",
        [error] InvalidEscapeSequence(c: into Colored<char>, literal_name: &'static str) =>
            "Invalid escape sequence `{}` in {} literal",
        [error] ExpectedUnicodeEscapeBrace(c: into Colored<char>, purpose: &'static str, literal_name: &'static str) =>
            "Expected `{}` to {} Unicode escape in {} literal",
        [error] InvalidUnicodeEscapeDigit(c: into Colored<char>, literal_name: &'static str) =>
            "Invalid Unicode escape digit `{}` in {} literal",
        [error] MissingUnicodeEscape(literal_name: &'static str) => "Missing Unicode escape sequence in {} literal",
        [error] OverlongUnicodeEscape(literal_name: &'static str) =>
            "Unicode escape sequence in {} literal is too long",
        [error] InsufficientIndentation => "Insufficient indentation in multiline string literal",
        [error] ExpectedInterpolationEnd(c: into Colored<char> = '}') =>
            "Expected `{}` to end interpolation in string literal",
        [error] NewlineInInterpolation => "Newline in string interpolation",
        [error] MultilineStringInInterpolation => "Multiline string literal in string interpolation",
    }

    #[derive(Debug, Clone)]
    pub enum StaticDiagnostic<'src> {
        [error] IoError(message: into Colored<String>) => "Error while doing IO: {}",
    }
);
