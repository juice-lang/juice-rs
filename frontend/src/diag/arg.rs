use std::fmt::{Display, Formatter, Result as FmtResult};

use ariadne::{Color, Fmt as _};
use itertools::Itertools as _;
use juice_core::diag::DiagnosticArg;

use crate::{
    parser::lexer::{LiteralKind, TokenKind},
    source_manager::SourceManager,
};

#[derive(Debug, Clone)]
pub struct TokenKindArg {
    string: String,
}

impl TokenKindArg {
    pub fn new<M: SourceManager>(kind: TokenKind<'_, M>) -> Self {
        let string = match kind {
            TokenKind::Keyword(keyword) => format!("keyword.{keyword}"),
            TokenKind::Punctuation(punctuation) => format!("punctuation.{punctuation}"),
            TokenKind::Literal(LiteralKind::Int(_, _)) | TokenKind::Literal(LiteralKind::BigInt(_, _)) => {
                "literal.int".to_string()
            }
            TokenKind::Literal(LiteralKind::Float(_)) => "literal.float".to_string(),
            TokenKind::Literal(LiteralKind::Char(_)) => "literal.char".to_string(),
            TokenKind::Literal(LiteralKind::String(_)) => "literal.string".to_string(),
            TokenKind::Literal(LiteralKind::StringInterpolation(_)) => "literal.string_interpolation".to_string(),
            TokenKind::Literal(LiteralKind::InvalidInt) => "literal.invalid_int".to_string(),
            TokenKind::Literal(LiteralKind::InvalidFloat) => "literal.invalid_float".to_string(),
            TokenKind::Literal(LiteralKind::InvalidChar) => "literal.invalid_char".to_string(),
            TokenKind::Literal(LiteralKind::InvalidString) => "literal.invalid_string".to_string(),
            TokenKind::Identifier(ident) => format!("identifier.{ident}"),
            TokenKind::PrefixOperator(op) => format!("prefix_operator.{op}"),
            TokenKind::PostfixOperator(op) => format!("postfix_operator.{op}"),
            TokenKind::BinaryOperator(op) => format!("binary_operator.{op}"),
            TokenKind::Unknown => "unknown".to_string(),
        };

        Self { string }
    }
}

impl<M: SourceManager> From<TokenKind<'_, M>> for TokenKindArg {
    fn from(kind: TokenKind<'_, M>) -> Self {
        Self::new(kind)
    }
}

impl<M: SourceManager> From<Option<TokenKind<'_, M>>> for TokenKindArg {
    fn from(kind: Option<TokenKind<'_, M>>) -> Self {
        kind.map_or_else(
            || Self {
                string: "eof".to_string(),
            },
            Self::new,
        )
    }
}

impl Display for TokenKindArg {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "{}", self.string)
    }
}

impl DiagnosticArg for TokenKindArg {
    fn with_color(self, color: impl Into<Option<Color>>) -> impl Display {
        self.fg(color)
    }
}

#[derive(Debug, Clone)]
pub struct TokenKindListArg {
    token_kinds: Vec<TokenKindArg>,
}

impl TokenKindListArg {
    pub fn new<'src, M: 'src + SourceManager>(token_kinds: impl Iterator<Item = Option<TokenKind<'src, M>>>) -> Self {
        let token_kinds = token_kinds.map(TokenKindArg::from).collect();
        Self { token_kinds }
    }
}

impl<'src, M: 'src + SourceManager, I: IntoIterator<Item = Option<TokenKind<'src, M>>>> From<I> for TokenKindListArg {
    fn from(iter: I) -> Self {
        Self::new(iter.into_iter())
    }
}

impl Display for TokenKindListArg {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "[{}]", self.token_kinds.iter().format(", "))
    }
}

impl DiagnosticArg for TokenKindListArg {
    fn with_color(self, color: impl Into<Option<Color>>) -> impl Display {
        let color = color.into();

        format!(
            "[{}]",
            self.token_kinds
                .into_iter()
                .format_with(", ", |arg, f| f(&arg.with_color(color)))
        )
    }
}
