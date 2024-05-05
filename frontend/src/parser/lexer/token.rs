use std::fmt::{Debug, Formatter, Result as FmtResult};

use derive_where::derive_where;

use super::TokenKind;
use crate::{source_loc::SourceRange, source_manager::SourceManager};

#[derive_where(Clone)]
pub struct Token<'src, M: 'src + SourceManager> {
    pub kind: TokenKind<'src, M>,
    pub source_range: SourceRange<'src, M>,
    pub leading_whitespace_range: SourceRange<'src, M>,
}

impl<'src, M: 'src + SourceManager> Token<'src, M> {
    pub fn new(
        kind: TokenKind<'src, M>,
        source_range: SourceRange<'src, M>,
        leading_whitespace_range: SourceRange<'src, M>,
    ) -> Self {
        Self {
            kind,
            source_range,
            leading_whitespace_range,
        }
    }
}

impl<M: SourceManager> Debug for Token<'_, M> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f.debug_struct("Token")
            .field("kind", &self.kind)
            .field("text", &self.source_range.get_str())
            .finish()
    }
}
