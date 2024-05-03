use std::fmt::{Debug, Formatter, Result as FmtResult};

use derive_where::derive_where;

use super::TokenKind;
use crate::{source_loc::SourceRange, source_manager::SourceManager};

#[derive_where(Clone)]
pub struct Token<'src, M: SourceManager> {
    pub kind: TokenKind<'src, M>,
    pub source_range: SourceRange<'src, M>,
    pub leading_whitespace_range: SourceRange<'src, M>,
    pub has_leading_whitespace: bool,
    pub has_trailing_whitespace: bool,
}

impl<'src, M: SourceManager> Token<'src, M> {
    pub fn new(
        kind: TokenKind<'src, M>,
        source_range: SourceRange<'src, M>,
        leading_whitespace_range: SourceRange<'src, M>,
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

    pub fn is_surrounded_by_whitespace(&self) -> bool {
        self.has_leading_whitespace || self.has_trailing_whitespace
    }

    pub fn has_no_whitespace(&self) -> bool {
        !self.has_leading_whitespace && !self.has_trailing_whitespace
    }

    pub fn has_only_leading_whitespace(&self) -> bool {
        self.has_leading_whitespace && !self.has_trailing_whitespace
    }

    pub fn has_only_trailing_whitespace(&self) -> bool {
        !self.has_leading_whitespace && self.has_trailing_whitespace
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
