use super::TokenKind;
use crate::source_loc::SourceRange;

#[derive(Debug, Clone)]
pub struct Token<'a> {
    pub kind: TokenKind<'a>,
    pub source_range: SourceRange<'a>,
    pub leading_whitespace_range: SourceRange<'a>,
    pub has_trailing_whitespace: bool,
}

impl<'a> Token<'a> {
    pub fn new(
        kind: TokenKind<'a>,
        source_range: SourceRange<'a>,
        leading_whitespace_range: SourceRange<'a>,
        has_trailing_whitespace: bool,
    ) -> Self {
        Self {
            kind,
            source_range,
            leading_whitespace_range,
            has_trailing_whitespace,
        }
    }
}
