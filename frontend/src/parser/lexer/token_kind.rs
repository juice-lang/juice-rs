use derive_where::derive_where;
use juice_macros::string_enum;

use super::literal::LiteralKind;
use crate::source_manager::SourceManager;

string_enum! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum KeywordKind {
        Else = "else",
        False = "false",
        If = "if",
        Let = "let",
        True = "true",
        Var = "var",
        While = "while",
    }
}

string_enum! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum PunctuationKind {
        Newline = "\n",
        Backtick = "`",
        LeftParen = "(",
        RightParen = ")",
        LeftBracket = "[",
        RightBracket = "]",
        LeftBrace = "{",
        RightBrace = "}",
        Comma = ",",
        Colon = ":",
        Semicolon = ";",
        At = "@",
        QuestionMark = "?",
        Dot = ".",
        Equals = "=",
        FatArrow = "=>",
        Arrow = "->",
        Ampersand = "&",
        AmpersandW = "&w",
        NumberSign = "#",
    }
}

#[derive_where(Debug, Clone, PartialEq)]
pub enum TokenKind<'src, M: 'src + SourceManager> {
    Keyword(KeywordKind),
    Punctuation(PunctuationKind),
    Literal(LiteralKind<'src, M>),
    Identifier,
    PrefixOperator,
    PostfixOperator,
    BinaryOperator,
    Unknown,
}

macro_rules! keyword_kind {
    ($kind:ident) => {
        $crate::parser::lexer::TokenKind::Keyword($crate::parser::lexer::KeywordKind::$kind)
    };
}

macro_rules! punctuation_kind {
    ($kind:ident) => {
        $crate::parser::lexer::TokenKind::Punctuation($crate::parser::lexer::PunctuationKind::$kind)
    };
}

macro_rules! literal_kind {
    ($kind:pat_param) => {
        $crate::parser::lexer::TokenKind::Literal($kind)
    };
}

macro_rules! Tok {
    (else) => {
        $crate::parser::lexer::token_kind::keyword_kind!(Else)
    };
    (false) => {
        $crate::parser::lexer::token_kind::keyword_kind!(False)
    };
    (if) => {
        $crate::parser::lexer::token_kind::keyword_kind!(If)
    };
    (let) => {
        $crate::parser::lexer::token_kind::keyword_kind!(Let)
    };
    (true) => {
        $crate::parser::lexer::token_kind::keyword_kind!(True)
    };
    (var) => {
        $crate::parser::lexer::token_kind::keyword_kind!(Var)
    };
    (while) => {
        $crate::parser::lexer::token_kind::keyword_kind!(While)
    };
    (Newline) => {
        $crate::parser::lexer::token_kind::punctuation_kind!(Newline)
    };
    (Backtick) => {
        $crate::parser::lexer::token_kind::punctuation_kind!(Backtick)
    };
    (LeftParen) => {
        $crate::parser::lexer::token_kind::punctuation_kind!(LeftParen)
    };
    (RightParen) => {
        $crate::parser::lexer::token_kind::punctuation_kind!(RightParen)
    };
    (LeftBracket) => {
        $crate::parser::lexer::token_kind::punctuation_kind!(LeftBracket)
    };
    (RightBracket) => {
        $crate::parser::lexer::token_kind::punctuation_kind!(RightBracket)
    };
    (LeftBrace) => {
        $crate::parser::lexer::token_kind::punctuation_kind!(LeftBrace)
    };
    (RightBrace) => {
        $crate::parser::lexer::token_kind::punctuation_kind!(RightBrace)
    };
    (,) => {
        $crate::parser::lexer::token_kind::punctuation_kind!(Comma)
    };
    (:) => {
        $crate::parser::lexer::token_kind::punctuation_kind!(Colon)
    };
    (;) => {
        $crate::parser::lexer::token_kind::punctuation_kind!(Semicolon)
    };
    (@) => {
        $crate::parser::lexer::token_kind::punctuation_kind!(At)
    };
    (?) => {
        $crate::parser::lexer::token_kind::punctuation_kind!(QuestionMark)
    };
    (.) => {
        $crate::parser::lexer::token_kind::punctuation_kind!(Dot)
    };
    (=) => {
        $crate::parser::lexer::token_kind::punctuation_kind!(Equals)
    };
    (=>) => {
        $crate::parser::lexer::token_kind::punctuation_kind!(FatArrow)
    };
    (->) => {
        $crate::parser::lexer::token_kind::punctuation_kind!(Arrow)
    };
    (&) => {
        $crate::parser::lexer::token_kind::punctuation_kind!(Ampersand)
    };
    (&w) => {
        $crate::parser::lexer::token_kind::punctuation_kind!(AmpersandW)
    };
    (#) => {
        $crate::parser::lexer::token_kind::punctuation_kind!(NumberSign)
    };
    (Int($pat:pat_param, $radix:pat_param)) => {
        $crate::parser::lexer::token_kind::literal_kind!($crate::parser::lexer::LiteralKind::Int($pat, $radix))
    };
    (Int($pat:pat_param)) => {
        $crate::parser::lexer::token_kind::literal_kind!($crate::parser::lexer::LiteralKind::Int($pat, _))
    };
    (BigInt($pat:pat_param, $radix:pat_param)) => {
        $crate::parser::lexer::token_kind::literal_kind!($crate::parser::lexer::LiteralKind::BigInt($pat, $radix))
    };
    (BigInt($pat:pat_param)) => {
        $crate::parser::lexer::token_kind::literal_kind!($crate::parser::lexer::LiteralKind::BigInt($pat, _))
    };
    (Float($pat:pat_param)) => {
        $crate::parser::lexer::token_kind::literal_kind!($crate::parser::lexer::LiteralKind::Float($pat))
    };
    (Char($pat:pat_param)) => {
        $crate::parser::lexer::token_kind::literal_kind!($crate::parser::lexer::LiteralKind::Char($pat))
    };
    (String($pat:pat_param)) => {
        $crate::parser::lexer::token_kind::literal_kind!($crate::parser::lexer::LiteralKind::String($pat))
    };
    (Interpolation($pat:pat_param)) => {
        $crate::parser::lexer::token_kind::literal_kind!($crate::parser::lexer::LiteralKind::StringInterpolation($pat))
    };
    (Ident) => {
        $crate::parser::lexer::TokenKind::Identifier
    };
    (PrefixOp) => {
        $crate::parser::lexer::TokenKind::PrefixOperator
    };
    (PostfixOp) => {
        $crate::parser::lexer::TokenKind::PostfixOperator
    };
    (BinOp) => {
        $crate::parser::lexer::TokenKind::BinaryOperator
    };
    (Unknown) => {
        $crate::parser::lexer::TokenKind::Unknown
    };
}

pub(crate) use keyword_kind;
pub(crate) use literal_kind;
pub(crate) use punctuation_kind;
pub(crate) use Tok;
