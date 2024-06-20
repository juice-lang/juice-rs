use std::borrow::Cow;

use juice_core::diag::Colored;
use juice_macros::Diagnostic;

#[derive(Debug, Clone, Diagnostic)]
#[diag(offset = 1000)]
pub enum StaticDiagnostic {
    #[diag(error = "Error while doing IO: {}")]
    IoError(#[diag(into)] Colored<String>),
    #[diag(error = "Error while serializing JSON: {}")]
    JsonError(#[diag(into)] Colored<String>),
}

#[derive(Debug, Clone, Diagnostic)]
pub enum Diagnostic<'src> {
    #[diag(error = "Invalid character `{}` in source file")]
    InvalidCharacter(#[diag(into)] Colored<char>),
    #[diag(error = "Unterminated block comment")]
    UnterminatedComment,
    #[diag(error = "Unexpected block comment terminator")]
    UnexpectedCommentTerminator,
    #[diag(error = "Invalid {} `{}` in {} literal")]
    InvalidDigit {
        digit_name: &'static str,
        #[diag(into)]
        c: Colored<char>,
        literal_name: &'static str,
    },
    #[diag(error = "Missing {} in {} literal")]
    MissingDigit {
        digit_name: &'static str,
        literal_name: &'static str,
    },
    #[diag(error = "Expected `{}` to terminate {} literal")]
    ExpectedLiteralTerminator {
        #[diag(into)]
        terminator: Colored<Cow<'static, str>>,
        literal_name: &'static str,
    },
    #[diag(error = "Empty {} literal")]
    EmptyLiteral(&'static str),
    #[diag(error = "Character literal may only contain one codepoint")]
    StringInCharLiteral,
    #[diag(error = "Newline in {} literal")]
    NewlineInLiteral(&'static str),
    #[diag(error = "Expected escape sequence after `{}` in {} literal")]
    ExpectedEscapeSequence {
        #[diag(into, default = '\\')]
        c: Colored<char>,
        literal_name: &'static str,
    },
    #[diag(error = "Invalid Unicode scalar value `{}` in {} literal")]
    InvalidUnicodeScalar {
        #[diag(into)]
        hex: Colored<&'src str>,
        literal_name: &'static str,
    },
    #[diag(error = "Invalid escape sequence `{}` in {} literal")]
    InvalidEscapeSequence {
        #[diag(into)]
        c: Colored<char>,
        literal_name: &'static str,
    },
    #[diag(error = "Expected `{}` to {} Unicode escape in {} literal")]
    ExpectedUnicodeEscapeBrace {
        #[diag(into)]
        c: Colored<char>,
        purpose: &'static str,
        literal_name: &'static str,
    },
    #[diag(error = "Invalid Unicode escape digit `{}` in {} literal")]
    InvalidUnicodeEscapeDigit {
        #[diag(into)]
        c: Colored<char>,
        literal_name: &'static str,
    },
    #[diag(error = "Missing Unicode escape sequence in {} literal")]
    MissingUnicodeEscape(&'static str),
    #[diag(error = "Unicode escape sequence in {} literal is too long")]
    OverlongUnicodeEscape(&'static str),
    #[diag(error = "Insufficient indentation in multiline string literal")]
    InsufficientIndentation,
    #[diag(error = "Expected `{}` to end interpolation in string literal")]
    ExpectedInterpolationEnd {
        #[diag(into, default = '}')]
        c: Colored<char>,
    },
    #[diag(error = "Newline in string interpolation")]
    NewlineInInterpolation,
    #[diag(error = "Multiline string literal in string interpolation")]
    MultilineStringInInterpolation,
    #[diag(error = "Unexpected parser error")]
    UnexpectedParserError,
    #[diag(error = "Expected expression {}")]
    ExpectedExpression(&'static str),
    #[diag(error = "Unexpected binary operator")]
    UnexpectedBinaryOperator,
    #[diag(error = "Expected statement {}")]
    ExpectedStatement(&'static str),
    #[diag(error = "Expected newline or `{}` to separate statements, but found `{}`")]
    UnseparatedStatements {
        #[diag(into, default = ';')]
        separator: Colored<char>,
        #[diag(into)]
        found: Colored<&'src str>,
    },
    #[diag(error = "Expected variable name in declaration")]
    ExpectedVarDeclName,
}
