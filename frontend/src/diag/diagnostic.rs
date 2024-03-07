use juice_core::diag::Colored;
use juice_macros::diagnostic;

diagnostic!(
    #[derive(Debug, Clone)]
    pub enum Diagnostic<'a> {
        [error] InvalidCharacter(c: into Colored<char>) => "Invalid character `{}` in source file",
        [error] UnterminatedComment => "Unterminated block comment",
        [error] UnexpectedCommentTerminator => "Unexpected block comment terminator",
        [error] InvalidDigit(digit_name: &'static str, c: into Colored<char>, literal_name: &'static str) =>
            "Invalid {} `{}` in {} literal",
        [error] MissingDigit(digit_name: &'static str, literal_name: &'static str) => "Missing {} in {} literal",
        [error] InvalidUnicodeScalar(hex: into Colored<&'a str>) => "Invalid Unicode scalar value `{}`",
        [error] InsufficientIndentation => "Insufficient indentation in multiline string literal",
    }

    #[derive(Debug, Clone)]
    pub enum StaticDiagnostic<'a> {
        [error] IoError(message: into Colored<String>) => "Error while doing IO: {}",
    }
);
