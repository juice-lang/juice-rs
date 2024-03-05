use juice_macros::string_enum;

use crate::source_loc::SourceRange;

string_enum! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    enum KeywordKind {}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {}

#[derive(Debug)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub source_range: SourceRange<'a>,
}
