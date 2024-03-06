use juice_core::diag::Colored;
use juice_macros::diagnostic_note;

diagnostic_note!(
    #[derive(Debug, Clone)]
    pub enum DiagnosticContextNote<'a> {
        InvalidCharacterLocation => "Invalid character is here",
        CommentLocation => "Comment is here",
    }
);

diagnostic_note!(
    #[derive(Debug, Clone)]
    pub enum DiagnosticNote<'a> {
        MissingBlockCommentEnd(symbols: into Colored<&str> = "*/") =>
            "Missing trailing `{}` to terminate the block comment",
    }
);
