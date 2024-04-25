use juice_core::diag::{Colored, PrefixedWithArticle};
use juice_macros::diagnostic_note;

diagnostic_note!(
    #[derive(Debug, Clone)]
    pub enum DiagnosticContextNote<'a> {
        InvalidCharacterLocation => "Invalid character is here",
        InvalidDigitLocation => "Invalid digit is here",
        InvalidUnicodeScalarLocation => "Invalid scalar is here",
        LineStartLocation => "Line starts here",
        IndentationLocation => "Should match indentation here",
        InterpolationLocation => "In this interpolation",
        CommentLocation => "Comment is here",
        CommentTerminatorLocation => "Comment terminator is here",
        LiteralLocation => "Literal is here",
        ContainingLiteralLocation => "In this literal",
        UnterminatedLiteralLocation => "This literal is unterminated",
        EscapeSequenceLocation => "Escape sequence is here",
        UnicodeEscapeLocation => "Unicode escape is here",
    }
);

diagnostic_note!(
    #[derive(Debug, Clone)]
    pub enum DiagnosticNote<'a> {
        MissingBlockCommentEnd(symbols: into Colored<&str> = "*/") =>
            "Missing trailing `{}` to terminate the block comment",
        ExpectedDigit(digit_name: into PrefixedWithArticle<&'static str>, digit_hint: into Colored<&'static str>) =>
            "This should be {} ({})",
        CommentTerminatorInOperator(symbols: into Colored<&str> = "*/") =>
            "The string `{}` is always interpreted as a block comment terminator, even if it is part of an operator",
        UnicodeEscapeLength => "Unicode escape sequences must be between 1 and 8 hexadecimal digits long",
        StringInCharLiteral(del: into Colored<&'static str> = r#"""#) =>
            "If you meant to write a string literal, use double quotes (`{}`) instead",
        NewlineInLiteral(del: into Colored<&'static str> = r#"""""#, escape: into Colored<&'static str> = r#"\n"#) =>
            "Only multiline string literals (delimited by `{}`) can contain newlines, use `{}` instead",
        NewlineInInterpolation => "Consider introducing a variable for complex interpolated expressions",
        InsufficientIndentation =>
            "The indentation of the last line gets stripped from all other lines in multiline string literals"
    }
);
