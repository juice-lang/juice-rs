use std::sync::Arc;

use num_bigint::BigUint;

use super::{
    fsm::{Fsm as _, NumberFsm, NumberFsmState},
    Error, Lexer, Token,
};
use crate::{
    diag::{Diagnostic, DiagnosticContextNote, DiagnosticNote},
    Result,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Radix {
    Binary,
    Octal,
    Decimal,
    Hexadecimal,
}

impl Radix {
    pub fn radix_value(self) -> u32 {
        match self {
            Self::Binary => 2,
            Self::Octal => 8,
            Self::Decimal => 10,
            Self::Hexadecimal => 16,
        }
    }
}

#[derive(Debug, Clone)]
pub enum InterpolationPart<'a> {
    String(Arc<str>),
    Interpolation(Vec<Token<'a>>),
}

#[derive(Debug, Clone)]
pub enum LiteralKind<'a> {
    Int(Vec<u64>, Radix),
    Float(f64),
    Char(char),
    String(Arc<str>),
    StringInterpolation(Vec<InterpolationPart<'a>>),
}

#[allow(clippy::result_large_err)]
impl<'a> LiteralKind<'a> {
    pub fn parse_number(lexer: &mut Lexer<'a>, start: char) -> Result<Self, Error<'a>> {
        use NumberFsmState::*;

        let start_state = if start == '0' { ZeroStart } else { Integer };

        Ok(match NumberFsm::run(lexer, start_state) {
            BinaryInteger => Self::parse_int(lexer, Radix::Binary),
            OctalInteger => Self::parse_int(lexer, Radix::Octal),
            HexInteger => Self::parse_int(lexer, Radix::Hexadecimal),
            Integer => Self::parse_int(lexer, Radix::Decimal),
            Float | Exponent => Self::parse_float(lexer),
            error_state => {
                let literal_name = match error_state {
                    InvalidDigit | InvalidBinaryDigit | MissingBinaryDigit | InvalidOctalDigit | MissingOctalDigit
                    | InvalidHexDigit | MissingHexDigit => "integer",
                    InvalidFloatDigit | InvalidExponentDigit | MissingExponentDigit => "floating-point",
                    _ => unreachable!("Invalid end state"),
                };

                let (article, digit_name, digit_hint) = match error_state {
                    InvalidDigit | InvalidFloatDigit | InvalidExponentDigit | MissingExponentDigit => {
                        ("a", "digit", None)
                    }
                    InvalidBinaryDigit | MissingBinaryDigit => ("a", "binary digit", Some("0 or 1")),
                    InvalidOctalDigit | MissingOctalDigit => ("an", "octal digit", Some("0-7")),
                    InvalidHexDigit | MissingHexDigit => ("a", "hexadecimal digit", Some("0-9, a-f, or A-F")),
                    _ => unreachable!("Invalid end state"),
                };

                let mut error = match error_state {
                    InvalidDigit | InvalidBinaryDigit | InvalidOctalDigit | InvalidHexDigit | InvalidFloatDigit
                    | InvalidExponentDigit => lexer.error_at_next_character(
                        Diagnostic::invalid_digit(digit_name, lexer.peek().unwrap(), literal_name),
                        DiagnosticContextNote::invalid_digit_location(),
                    ),
                    MissingBinaryDigit | MissingOctalDigit | MissingHexDigit | MissingExponentDigit => lexer
                        .error_at_end(
                            Diagnostic::missing_digit(digit_name, literal_name),
                            DiagnosticContextNote::literal_location(),
                        ),
                    _ => unreachable!("Invalid end state"),
                };

                if let Some(hint) = digit_hint {
                    error = error.with_note(DiagnosticNote::expected_digit(article, digit_name, hint));
                }

                return Err(error);
            }
        })
    }

    pub fn parse_char(lexer: &mut Lexer<'a>) -> Result<Self, Error<'a>> {
        todo!()
    }

    pub fn parse_string(lexer: &mut Lexer<'a>, start: char) -> Result<Self, Error<'a>> {
        todo!()
    }

    fn parse_int(lexer: &Lexer, radix: Radix) -> Self {
        let mut string = lexer.get_current_range().get_text();

        if radix != Radix::Decimal {
            string = &string[2..];
        }

        let radix_digits = string
            .bytes()
            .filter_map(|b| match b {
                b'0'..=b'9' => Some(b - b'0'),
                b'a'..=b'f' => Some(b - b'a' + 10),
                b'A'..=b'F' => Some(b - b'A' + 10),
                b'_' => None,
                _ => unreachable!("Invalid digit"),
            })
            .collect::<Vec<_>>();

        let bigint = BigUint::from_radix_be(&radix_digits, radix.radix_value()).expect("Digits should be valid here");
        let words = bigint.to_u64_digits();

        Self::Int(words, radix)
    }

    fn parse_float(lexer: &Lexer) -> Self {
        let string = lexer.get_current_range().get_text();
        todo!()
    }
}
