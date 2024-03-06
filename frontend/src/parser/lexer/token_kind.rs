use juice_macros::string_enum;

string_enum! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum KeywordKind {}
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
        Pound = "#",
        At = "@",
        QuestionMark = "?",
        Dot = ".",
        Equals = "=",
        FatArrow = "=>",
        Arrow = "->",
        Ampersand = "&",
        AmpersandW = "&w",
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LiteralKind {
    Int,
    Float,
    Char,
    String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    Keyword(KeywordKind),
    Punctuation(PunctuationKind),
    Literal(LiteralKind),
    Identifier,
    Operator,
}

#[macro_export]
macro_rules! keyword_kind {
    ($kind:ident) => {
        $crate::parser::lexer::TokenKind::Keyword($crate::parser::lexer::token_kind::KeywordKind::$kind)
    };
}

#[macro_export]
macro_rules! punctuation_kind {
    ($kind:ident) => {
        $crate::parser::lexer::TokenKind::Punctuation($crate::parser::lexer::token_kind::PunctuationKind::$kind)
    };
}

#[macro_export]
macro_rules! literal_kind {
    ($kind:ident) => {
        $crate::parser::lexer::TokenKind::Literal($crate::parser::lexer::token_kind::LiteralKind::$kind)
    };
}

#[macro_export]
macro_rules! Tok {
    (Newline) => {
        $crate::punctuation_kind!(Newline)
    };
    (Backtick) => {
        $crate::punctuation_kind!(Backtick)
    };
    (LeftParen) => {
        $crate::punctuation_kind!(LeftParen)
    };
    (RightParen) => {
        $crate::punctuation_kind!(RightParen)
    };
    (LeftBracket) => {
        $crate::punctuation_kind!(LeftBracket)
    };
    (RightBracket) => {
        $crate::punctuation_kind!(RightBracket)
    };
    (LeftBrace) => {
        $crate::punctuation_kind!(LeftBrace)
    };
    (RightBrace) => {
        $crate::punctuation_kind!(RightBrace)
    };
    (,) => {
        $crate::punctuation_kind!(Comma)
    };
    (:) => {
        $crate::punctuation_kind!(Colon)
    };
    (;) => {
        $crate::punctuation_kind!(Semicolon)
    };
    (#) => {
        $crate::punctuation_kind!(Pound)
    };
    (@) => {
        $crate::punctuation_kind!(At)
    };
    (?) => {
        $crate::punctuation_kind!(QuestionMark)
    };
    (.) => {
        $crate::punctuation_kind!(Dot)
    };
    (=) => {
        $crate::punctuation_kind!(Equals)
    };
    (=>) => {
        $crate::punctuation_kind!(FatArrow)
    };
    (->) => {
        $crate::punctuation_kind!(Arrow)
    };
    (&) => {
        $crate::punctuation_kind!(Ampersand)
    };
    (&w) => {
        $crate::punctuation_kind!(AmpersandW)
    };
    (Int) => {
        $crate::literal_kind!(Int)
    };
    (Float) => {
        $crate::literal_kind!(Float)
    };
    (Char) => {
        $crate::literal_kind!(Char)
    };
    (String) => {
        $crate::literal_kind!(String)
    };
    (Ident) => {
        $crate::parser::lexer::TokenKind::Identifier
    };
    (Op) => {
        $crate::parser::lexer::TokenKind::Operator
    };
}
