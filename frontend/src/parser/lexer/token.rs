use std::fmt::{Debug, Formatter, Result as FmtResult};

use super::TokenKind;
use crate::source_loc::SourceRange;

#[derive(Clone)]
pub struct Token<'a> {
    pub kind: TokenKind<'a>,
    pub source_range: SourceRange<'a>,
    pub leading_whitespace_range: SourceRange<'a>,
    pub has_leading_whitespace: bool,
    pub has_trailing_whitespace: bool,
}

impl<'a> Token<'a> {
    pub fn new(
        kind: TokenKind<'a>,
        source_range: SourceRange<'a>,
        leading_whitespace_range: SourceRange<'a>,
        has_leading_whitespace: bool,
        has_trailing_whitespace: bool,
    ) -> Self {
        Self {
            kind,
            source_range,
            leading_whitespace_range,
            has_leading_whitespace,
            has_trailing_whitespace,
        }
    }
}

impl<'a> Debug for Token<'a> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f.debug_struct("Token")
            .field("kind", &self.kind)
            .field("text", &self.source_range.get_str())
            .field("has_leading_whitespace", &self.has_leading_whitespace)
            .field("has_trailing_whitespace", &self.has_trailing_whitespace)
            .finish()
    }
}
