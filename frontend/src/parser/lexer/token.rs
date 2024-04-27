use std::fmt::{Debug, Formatter, Result as FmtResult};

use super::TokenKind;
use crate::{source_loc::SourceRange, source_manager::SourceManager};

#[derive(Clone)]
pub struct Token<'a, M: SourceManager> {
    pub kind: TokenKind<'a, M>,
    pub source_range: SourceRange<'a, M>,
    pub leading_whitespace_range: SourceRange<'a, M>,
    pub has_leading_whitespace: bool,
    pub has_trailing_whitespace: bool,
}

impl<'a, M: SourceManager> Token<'a, M> {
    pub fn new(
        kind: TokenKind<'a, M>,
        source_range: SourceRange<'a, M>,
        leading_whitespace_range: SourceRange<'a, M>,
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

impl<M: SourceManager> Debug for Token<'_, M> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f.debug_struct("Token")
            .field("kind", &self.kind)
            .field("text", &self.source_range.get_str())
            .field("has_leading_whitespace", &self.has_leading_whitespace)
            .field("has_trailing_whitespace", &self.has_trailing_whitespace)
            .finish()
    }
}
