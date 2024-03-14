use juice_core::diag::Colored;
use juice_macros::diagnostic_note;

diagnostic_note!(
    #[derive(Debug, Clone)]
    pub enum DiagnosticContextNote<'a> {
        InvalidCharacterLocation => "Invalid character is here",
        InvalidDigitLocation => "Invalid digit is here",
        InvalidUnicodeScalarLocation => "Invalid scalar is here",
        LineStartLocation => "Line starts here",
        IndentationLocation => "Should match indentation here",
        InterpolationLocation => "Interpolation is here",
        CommentLocation => "Comment is here",
        CommentTerminatorLocation => "Comment terminator is here",
        LiteralLocation => "In this literal",
    }
);

diagnostic_note!(
    #[derive(Debug, Clone)]
    pub enum DiagnosticNote<'a> {
        MissingBlockCommentEnd(symbols: into Colored<&str> = "*/") =>
            "Missing trailing `{}` to terminate the block comment",
        ExpectedDigit(article: &'static str, digit_name: &'static str, digit_hint: into Colored<&'static str>) =>
            "This should be {} {} ({})",
        CommentTerminatorInOperator(symbols: into Colored<&str> = "*/") =>
            "The string `{}` is always interpreted as a block comment terminator, even if it is part of an operator",
        NewlineInInterpolation => "String interpolations cannot contain newlines",
        InsufficientIndentation =>
            "The indentation of the last line gets stripped from all other lines in multiline string literals"
    }
);
