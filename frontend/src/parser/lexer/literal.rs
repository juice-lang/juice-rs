use std::{iter::Peekable, sync::Arc};

use num_bigint::BigUint;

use super::{
    fsm::{Fsm as _, NumberFsm, NumberFsmState, StringFsm, StringFsmState, StringFsmStateKind},
    Error, Lexer, Token,
};
use crate::{
    diag::{Diagnostic, DiagnosticContextNote, DiagnosticNote},
    source_loc::SourceRange,
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

    fn parse_int(lexer: &Lexer, radix: Radix) -> Self {
        let mut string = lexer.get_current_range().get_str();

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
        let string = lexer.get_current_range().get_str();
        todo!()
    }

    pub fn parse_char(lexer: &mut Lexer<'a>) -> Result<Self, Error<'a>> {
        todo!()
    }

    pub fn parse_string(lexer: &mut Lexer<'a>, start: char) -> Result<Self, Error<'a>> {
        use StringFsmStateKind::*;

        if start == '#' {
            return Self::parse_raw_string(lexer);
        }

        let state = StringFsm::run(lexer, StringFsmState::new(StringStart(1), false));

        Ok(match (state.kind, state.multiline) {
            (StringEnd(1), false) => {
                let source_range = lexer.source.get_range(lexer.start + 1, lexer.current - 1);
                let string = Self::parse_string_part(source_range)?;
                Self::String(string)
            }
            (StringEnd(3), true) => {
                let source_range = lexer.source.get_range(lexer.start + 3, lexer.current - 3);

                let string = source_range.get_str();
                if string.is_empty() {
                    Self::String(Arc::from(""))
                } else {
                    let indentation = Self::get_multiline_indentation(source_range);
                    let string = Self::parse_multiline_string_part(lexer, source_range, indentation, true, true)?;
                    Self::String(string)
                }
            }
            (Interpolation, multiline) => {
                let first_part_range = lexer
                    .source
                    .get_range(lexer.start + if multiline { 3 } else { 1 }, lexer.current - 2);
                Self::parse_string_interpolation(lexer, first_part_range, multiline)?
            }
            _ => todo!(),
        })
    }

    fn parse_string_interpolation(
        lexer: &mut Lexer<'a>,
        first_part_range: SourceRange<'a>,
        multiline: bool,
    ) -> Result<Self, Error<'a>> {
        use StringFsmStateKind::*;

        enum Part<'a> {
            String(SourceRange<'a>),
            Interpolation(Vec<Token<'a>>),
        }

        let mut parts = vec![Part::String(first_part_range)];

        loop {
            let interpolation_start = lexer.current - 2;

            let tokens = lexer.with_interpolation(|nested_lexer| nested_lexer.collect::<Result<_, _>>())?;

            lexer.expect_char_eq('}', |l| {
                Error::new(
                    l.source.get_range(interpolation_start, l.current),
                    Diagnostic::expected_interpolation_end(),
                    DiagnosticContextNote::interpolation_location(),
                    true,
                )
                .with_context_note(l.get_current_range(), DiagnosticContextNote::literal_location())
            })?;

            parts.push(Part::Interpolation(tokens));

            let start = lexer.current;

            let state = StringFsm::run(lexer, StringFsmState::new(String, multiline));

            match (state.kind, state.multiline) {
                (StringEnd(1), false) => {
                    let source_range = lexer.source.get_range(start, lexer.current - 1);
                    parts.push(Part::String(source_range));
                    break;
                }
                (StringEnd(3), true) => {
                    let source_range = lexer.source.get_range(start, lexer.current - 3);
                    parts.push(Part::String(source_range));
                    break;
                }
                (Interpolation, _) => {
                    let source_range = lexer.source.get_range(start, lexer.current - 2);
                    parts.push(Part::String(source_range));
                }
                _ => todo!(),
            }
        }

        if multiline {
            let last_part = parts.last().expect("Parts should not be empty");
            if let Part::String(source_range) = last_part {
                let indentation = Self::get_multiline_indentation(*source_range);

                let part_count = parts.len();

                parts
                    .into_iter()
                    .enumerate()
                    .map(|(i, part)| match part {
                        Part::String(source_range) => Self::parse_multiline_string_part(
                            lexer,
                            source_range,
                            indentation,
                            i == 0,
                            i == part_count - 1,
                        )
                        .map(InterpolationPart::String),
                        Part::Interpolation(tokens) => Ok(InterpolationPart::Interpolation(tokens)),
                    })
                    .collect::<Result<_, _>>()
            } else {
                unreachable!("Last part should be a string")
            }
        } else {
            parts
                .into_iter()
                .map(|part| match part {
                    Part::String(source_range) => Self::parse_string_part(source_range).map(InterpolationPart::String),
                    Part::Interpolation(tokens) => Ok(InterpolationPart::Interpolation(tokens)),
                })
                .collect::<Result<_, _>>()
        }
        .map(Self::StringInterpolation)
    }

    fn parse_string_part(source_range: SourceRange<'a>) -> Result<Arc<str>, Error<'a>> {
        let mut parsed = String::new();

        let mut chars = source_range.get_str().chars().enumerate().peekable();
        while let Some((_, c)) = chars.next() {
            if c == '\\' {
                let escape_sequence = Self::consume_escape_sequence(source_range, &mut chars)?;
                parsed.push(escape_sequence);
            } else {
                parsed.push(c);
            }
        }

        Ok(Arc::from(parsed))
    }

    fn parse_multiline_string_part(
        lexer: &Lexer<'a>,
        source_range: SourceRange<'a>,
        indentation: Option<SourceRange<'a>>,
        is_first: bool,
        is_last: bool,
    ) -> Result<Arc<str>, Error<'a>> {
        let mut parsed = String::new();

        let mut chars = source_range.get_str().chars().enumerate().peekable();
        while let Some((_, c)) = chars.next() {
            match c {
                '\r' => {
                    chars.next_if(|(_, c)| *c == '\n');

                    parsed.push('\n');
                }
                '\n' => {
                    parsed.push('\n');
                }
                '\\' => {
                    if chars.next_if(|(_, c)| *c == '\r').is_some() {
                        chars.next_if(|(_, c)| *c == '\n');
                    } else if chars.next_if(|(_, c)| *c == '\n').is_none() {
                        let escape_sequence = Self::consume_escape_sequence(source_range, &mut chars)?;
                        parsed.push(escape_sequence);

                        continue;
                    }
                }
                _ => {
                    parsed.push(c);
                    continue;
                }
            }

            if let Some(indentation) = indentation {
                for indent in indentation.get_str().chars() {
                    if chars.next_if(|(_, c)| *c == indent).is_none() {
                        let i = chars.peek().expect("Indentation should be valid here").0;
                        return Err(Error::new(
                            SourceRange::new(source_range.source, source_range.start + i, source_range.start + i + 1),
                            Diagnostic::insufficient_indentation(),
                            DiagnosticContextNote::line_start_location(),
                            false,
                        )
                        .with_context_note(indentation, DiagnosticContextNote::indentation_location())
                        .with_context_note(lexer.get_current_range(), DiagnosticContextNote::literal_location())
                        .with_note(DiagnosticNote::insufficient_indentation()));
                    }
                }
            }
        }

        let mut parsed = parsed.as_str();

        if is_first {
            if let Some(stripped) = parsed.strip_prefix('\n') {
                parsed = stripped;
            }
        }

        if is_last {
            if let Some(stripped) = parsed.strip_suffix('\n') {
                parsed = stripped;
            }
        }

        Ok(Arc::from(parsed))
    }

    fn get_multiline_indentation(source_range: SourceRange<'a>) -> Option<SourceRange<'a>> {
        let string = source_range.get_str();
        string
            .rfind(|c| !matches!(c, ' ' | '\t'))
            .and_then(|indentation_start| {
                let string = &string[indentation_start..];

                if string.starts_with(|c| matches!(c, '\n' | '\r')) {
                    Some(SourceRange::new(
                        source_range.source,
                        source_range.start + indentation_start + 1,
                        source_range.end,
                    ))
                } else {
                    None
                }
            })
    }

    fn parse_raw_string(lexer: &mut Lexer<'a>) -> Result<Self, Error<'a>> {
        todo!()
    }

    fn consume_escape_sequence(
        literal_range: SourceRange<'a>,
        chars: &mut Peekable<impl Iterator<Item = (usize, char)>>,
    ) -> Result<char, Error<'a>> {
        Ok(match chars.next().expect("Escape sequence should be valid here").1 {
            '0' => '\0',
            '\\' => '\\',
            't' => '\t',
            'n' => '\n',
            'r' => '\r',
            '"' => '"',
            '\'' => '\'',
            '$' => '$',
            'u' => {
                let open = chars.next().expect("Escape sequence should be valid here");
                assert_eq!(open.1, '{');

                let mut code_point = 0;
                for _ in 0..8 {
                    let Some((_, c)) = chars.next_if(|(_, c)| *c != '}') else {
                        break;
                    };

                    let digit = c.to_digit(16).expect("Escape sequence should be valid here");
                    code_point = (code_point << 4) | digit;
                }

                let close = chars.next().expect("Escape sequence should be valid here");
                assert_eq!(close.1, '}');

                char::from_u32(code_point).ok_or_else(|| {
                    let code_point_range = SourceRange::new(
                        literal_range.source,
                        literal_range.start + open.0 + 1,
                        literal_range.start + close.0,
                    );

                    Error::new(
                        code_point_range,
                        Diagnostic::invalid_unicode_scalar(code_point_range.get_str()),
                        DiagnosticContextNote::invalid_unicode_scalar_location(),
                        false,
                    )
                })?
            }
            _ => unreachable!("Invalid escape sequence"),
        })
    }
}
