use std::{convert::Infallible, marker::PhantomData};

use juice_core::diag::{Colored, PrefixedWithArticle};
use juice_macros::DiagnosticNote;

use super::arg::{TokenKindArg, TokenKindListArg};

#[derive(Debug, Clone, DiagnosticNote)]
pub enum DiagnosticContextNote<'src> {
    #[diag(note = "Invalid character is here")]
    InvalidCharacterLocation,
    #[diag(note = "Invalid digit is here")]
    InvalidDigitLocation,
    #[diag(note = "Invalid scalar is here")]
    InvalidUnicodeScalarLocation,
    #[diag(note = "Line starts here")]
    LineStartLocation,
    #[diag(note = "Should match indentation here")]
    IndentationLocation,
    #[diag(note = "In this interpolation")]
    InterpolationLocation,
    #[diag(note = "Interpolation starts here")]
    InterpolationStartLocation,
    #[diag(note = "Comment is here")]
    CommentLocation,
    #[diag(note = "Comment terminator is here")]
    CommentTerminatorLocation,
    #[diag(note = "In this operator")]
    ContainingOperatorLocation,
    #[diag(note = "Literal is here")]
    LiteralLocation,
    #[diag(note = "In this literal")]
    ContainingLiteralLocation,
    #[diag(note = "This literal is unterminated")]
    UnterminatedLiteralLocation,
    #[diag(note = "Escape sequence is here")]
    EscapeSequenceLocation,
    #[diag(note = "Unicode escape is here")]
    UnicodeEscapeLocation,
    #[diag(note = "Parser error generated here")]
    UnexpectedParserErrorLocation,
    #[diag(note = "Expression expected here")]
    ExpectedExpressionLocation,
    #[diag(note = "Identifier expected here")]
    ExpectedIdentifierLocation,
    #[diag(note = "Operator is here")]
    OperatorLocation,
    #[diag(note = "Did you mean to write a prefix operator with this operand?")]
    MaybePrefixOperandLocation,
    #[diag(note = "Statement expected here")]
    ExpectedStatementLocation,
    #[diag(note = "Statement separator expected here")]
    ExpectedStatementSeparatorLocation,
    #[diag(note = "In this variable declaration")]
    ContainingVarDeclLocation,
    #[diag(note = "Variable initializer expected after this `{}`")]
    VarDeclEqualsLocation {
        #[diag(into, default = '=')]
        c: Colored<char>,
    },
    #[diag(unreachable)]
    _Unreachable(Infallible, PhantomData<&'src ()>),
}

#[derive(Debug, Clone, DiagnosticNote)]
pub enum DiagnosticNote<'src> {
    #[diag(note = "Missing trailing `{}` to terminate the block comment")]
    MissingBlockCommentEnd {
        #[diag(into, default = "*/")]
        symbols: Colored<&'static str>,
    },
    #[diag(note = "This should be {} ({})")]
    ExpectedDigit {
        #[diag(into)]
        digit_name: PrefixedWithArticle<&'static str>,
        #[diag(into)]
        digit_hint: Colored<&'static str>,
    },
    #[diag(
        note = "The string `{}` is always interpreted as a block comment terminator, even if it is part of an operator"
    )]
    CommentTerminatorInOperator {
        #[diag(into, default = "*/")]
        symbols: Colored<&'static str>,
    },
    #[diag(note = "Unicode escape sequences must be between 1 and 8 hexadecimal digits long")]
    UnicodeEscapeLength,
    #[diag(note = "If you meant to write a string literal, use double quotes (`{}`) instead")]
    StringInCharLiteral {
        #[diag(into, default = '"')]
        del: Colored<char>,
    },
    #[diag(note = "Only multiline string literals (delimited by `{}`) can contain newlines, use `{}` instead")]
    NewlineInLiteral {
        #[diag(into, default = r#"""""#)]
        del: Colored<&'static str>,
        #[diag(into, default = r#"\n"#)]
        escape: Colored<&'static str>,
    },
    #[diag(note = "Consider introducing a variable for complex interpolated expressions")]
    NewlineInInterpolation,
    #[diag(note = "The indentation of the last line gets stripped from all other lines in multiline string literals")]
    InsufficientIndentation,
    #[diag(note = "Expected one of the following tokens: {}, but found {}")]
    UnexpectedParserError {
        #[diag(into)]
        expected: TokenKindListArg,
        #[diag(into)]
        found: TokenKindArg,
    },
    #[diag(note = "Prefix operators cannot be separated from their operand")]
    ExpectedPrefixOperator,
    #[diag(note = "Consecutive statements on the same line must be separated by a `{}`")]
    UnseparatedStatements {
        #[diag(into, default = ';')]
        separator: Colored<char>,
    },
    #[diag(unreachable)]
    _Unreachable(Infallible, PhantomData<&'src ()>),
}
