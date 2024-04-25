use std::sync::Arc;

use juice_core::{CharExt, OptionExt};
use num_bigint::BigUint;

use super::{Lexer, Token};
use crate::{
    diag::{Diagnostic, DiagnosticContextNote, DiagnosticNote},
    source_loc::{SourceLoc, SourceRange},
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

    pub fn from_prefix(prefix: char) -> Option<Self> {
        match prefix {
            'b' => Some(Self::Binary),
            'o' => Some(Self::Octal),
            'x' => Some(Self::Hexadecimal),
            _ => None,
        }
    }

    pub fn matches_digit(self, c: char) -> bool {
        (match self {
            Self::Binary => CharExt::is_binary_digit,
            Self::Octal => CharExt::is_octal_digit,
            Self::Decimal => CharExt::is_decimal_digit,
            Self::Hexadecimal => CharExt::is_hex_digit,
        })(c)
    }

    pub fn digit_name(self) -> &'static str {
        match self {
            Self::Binary => "binary digit",
            Self::Octal => "octal digit",
            Self::Decimal => "digit",
            Self::Hexadecimal => "hexadecimal digit",
        }
    }

    pub fn digit_hint(self) -> Option<&'static str> {
        match self {
            Self::Binary => Some("0 or 1"),
            Self::Octal => Some("0-7"),
            Self::Hexadecimal => Some("0-9, a-f, or A-F"),
            _ => None,
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
    Int(u64, Radix),
    BigInt(Vec<u64>, Radix),
    Float(f64),
    Char(char),
    String(Arc<str>),
    StringInterpolation(Vec<InterpolationPart<'a>>),
    InvalidInt,
    InvalidFloat,
    InvalidChar,
    InvalidString,
}

#[allow(clippy::result_large_err)]
impl<'a> LiteralKind<'a> {
    pub fn lex_number(lexer: &mut Lexer<'a>, start: char) -> Self {
        let (radix, mut is_first) = (start == '0')
            .then(|| lexer.match_char_map(Radix::from_prefix))
            .flatten()
            .map_or((Radix::Decimal, false), |r| (r, true));

        loop {
            let Some(c) = lexer.peek() else {
                return if is_first {
                    lexer
                        .error_at_end(
                            Diagnostic::missing_digit(radix.digit_name(), "integer"),
                            DiagnosticContextNote::containing_literal_location(),
                        )
                        .record();

                    Self::InvalidInt
                } else {
                    Self::parse_int(lexer.get_current_range().get_str(), radix)
                };
            };

            if !is_first && c == '_' {
                lexer.advance();
                continue;
            }

            if radix == Radix::Decimal
                && (matches!(c, 'e' | 'E') || (c == '.' && lexer.peek2().is_some_and(CharExt::is_decimal_digit)))
            {
                break;
            }

            if !radix.matches_digit(c) {
                if c.is_number_end() && !is_first {
                    return Self::parse_int(lexer.get_current_range().get_str(), radix);
                }

                let range = lexer.get_next_character_range(c);

                let mut error = lexer
                    .error_at_token(
                        lexer.get_current_loc(),
                        Diagnostic::invalid_digit(radix.digit_name(), lexer.peek().unwrap(), "integer"),
                        DiagnosticContextNote::containing_literal_location(),
                    )
                    .with_context_note(range, DiagnosticContextNote::invalid_digit_location());

                if let Some(hint) = radix.digit_hint() {
                    error = error.with_note(DiagnosticNote::expected_digit(radix.digit_name(), hint));
                }

                error.record();

                while lexer.match_char(|c| !c.is_number_end()) {}

                return Self::InvalidInt;
            }

            is_first = false;
            lexer.advance();
        }

        assert_eq!(radix, Radix::Decimal);

        if lexer.match_char_eq('.') {
            lexer.expect_char(CharExt::is_decimal_digit, |_| {
                unreachable!("There should be a digit after the dot")
            });

            while lexer.match_char(|c| CharExt::is_decimal_digit(c) || c == '_') {}
        }

        if lexer.match_char(|c| matches!(c, 'e' | 'E')) {
            lexer.advance_if(|c| matches!(c, '+' | '-'));

            if !lexer.match_char(CharExt::is_decimal_digit) {
                lexer
                    .error_at_end(
                        Diagnostic::missing_digit("digit", "floating-point"),
                        DiagnosticContextNote::containing_literal_location(),
                    )
                    .record();

                return Self::InvalidFloat;
            }

            while lexer.match_char(|c| CharExt::is_decimal_digit(c) || c == '_') {}
        }

        let c = lexer.peek();

        if c.is_none_or(CharExt::is_number_end) {
            return Self::parse_float(lexer.get_current_range().get_str());
        }

        let range = lexer.get_next_character_range(c.unwrap());

        lexer
            .error_at_token(
                lexer.get_current_loc(),
                Diagnostic::invalid_digit("digit", lexer.peek().unwrap(), "floating-point"),
                DiagnosticContextNote::containing_literal_location(),
            )
            .with_context_note(range, DiagnosticContextNote::invalid_digit_location())
            .record();

        while lexer.match_char(|c| !c.is_number_end()) {}

        Self::InvalidFloat
    }

    fn parse_int(mut string: &str, radix: Radix) -> Self {
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

        if words.len() == 1 {
            Self::Int(words[0], radix)
        } else {
            Self::BigInt(words, radix)
        }
    }

    fn parse_float(string: &str) -> Self {
        let string = string.replace('_', "");

        let float = string.parse().expect("Digits should be valid here");

        Self::Float(float)
    }

    pub fn lex_char(lexer: &mut Lexer<'a>) -> Self {
        todo!()
    }

    pub fn lex_string(lexer: &mut Lexer<'a>) -> Self {
        enum Part<'a> {
            String {
                content: String,
                newline_locations: Vec<(usize, SourceLoc<'a>)>,
            },
            Interpolation(Vec<Token<'a>>),
        }

        let is_multiline = lexer.match_str(r#""""#); // One quote is already consumed by the lexer

        if is_multiline && lexer.in_interpolation {
            lexer
                .error_at_token(
                    lexer.get_current_loc() - 3,
                    Diagnostic::multiline_string_in_interpolation(),
                    DiagnosticContextNote::literal_location(),
                )
                .with_note(DiagnosticNote::newline_in_interpolation())
                .record();
        }

        let mut parts = Vec::new();
        let mut current_content = String::new();
        let mut current_newline_locations = Vec::new();

        loop {
            let Some(c) = lexer.advance() else {
                let terminator = if is_multiline { r#"""""# } else { r#"""# };

                lexer
                    .error_at_end(
                        Diagnostic::expected_string_literal_terminator(terminator),
                        DiagnosticContextNote::unterminated_literal_location(),
                    )
                    .record();

                return Self::InvalidString;
            };

            match c {
                '"' => {
                    if !is_multiline || lexer.match_str(r#""""#) {
                        break;
                    } else {
                        current_content.push('"');
                    }
                }
                '\r' | '\n' => {
                    let start = lexer.current - 1;

                    if c == '\r' {
                        lexer.match_char_eq('\n');
                    }

                    if !is_multiline {
                        lexer
                            .error_at_token(
                                lexer.source.get_loc(start),
                                Diagnostic::newline_in_literal("string"),
                                DiagnosticContextNote::containing_literal_location(),
                            )
                            .with_note(DiagnosticNote::newline_in_literal())
                            .record();
                    }

                    current_content.push('\n');
                    current_newline_locations.push((current_content.len(), lexer.get_current_loc()));
                }
                '\\' => {
                    let (escaped, was_newline) = Self::consume_escape_sequence(lexer, is_multiline, "string");
                    if let Some(escaped) = escaped {
                        current_content.push(escaped);
                    }

                    if was_newline {
                        current_newline_locations.push((current_content.len(), lexer.get_current_loc()));
                    }
                }
                '$' => {
                    if lexer.match_char_eq('{') {
                        if !current_content.is_empty() {
                            let content = std::mem::take(&mut current_content);
                            let newline_locations = std::mem::take(&mut current_newline_locations);
                            parts.push(Part::String {
                                content,
                                newline_locations,
                            });
                        }

                        let interpolation_start = lexer.current;

                        let tokens = lexer.with_interpolation(|nested_lexer| nested_lexer.collect());

                        parts.push(Part::Interpolation(tokens));
                    }
                }
                _ => {
                    current_content.push(c);
                }
            }
        }

        if is_multiline {
            let indentation = current_newline_locations.last().and_then(|(index, loc)| {
                let last_line = &current_content[*index..];
                if !last_line.is_empty() && last_line.chars().all(|c| matches!(c, ' ' | '\t')) {
                    Some(loc.to_range(last_line.len()))
                } else {
                    None
                }
            });

            parts.push(Part::String {
                content: current_content,
                newline_locations: current_newline_locations,
            });

            let parts: Vec<_> = if let Some(indentation) = indentation {
                let part_count = parts.len();

                parts
                    .into_iter()
                    .enumerate()
                    .map(|(i, part)| match part {
                        Part::String {
                            content,
                            newline_locations,
                        } => {
                            let content = Self::remove_multiline_indentation(
                                lexer,
                                content,
                                newline_locations,
                                indentation,
                                i == 0,
                                i == part_count - 1,
                            );

                            InterpolationPart::String(content)
                        }
                        Part::Interpolation(tokens) => InterpolationPart::Interpolation(tokens),
                    })
                    .collect()
            } else {
                parts
                    .into_iter()
                    .map(|part| match part {
                        Part::String { content, .. } => InterpolationPart::String(Arc::from(content)),
                        Part::Interpolation(tokens) => InterpolationPart::Interpolation(tokens),
                    })
                    .collect()
            };

            if let [InterpolationPart::String(content)] = parts.as_slice() {
                Self::String(content.clone())
            } else {
                Self::StringInterpolation(parts)
            }
        } else {
            let last_content = Arc::from(current_content);

            if parts.is_empty() {
                Self::String(last_content)
            } else {
                let parts = parts
                    .into_iter()
                    .map(|part| match part {
                        Part::String { content, .. } => InterpolationPart::String(Arc::from(content)),
                        Part::Interpolation(tokens) => InterpolationPart::Interpolation(tokens),
                    })
                    .chain(Some(InterpolationPart::String(last_content)))
                    .collect();
                Self::StringInterpolation(parts)
            }
        }
    }

    fn remove_multiline_indentation(
        lexer: &mut Lexer<'a>,
        mut string: String,
        newline_locations: Vec<(usize, SourceLoc<'a>)>,
        indentation: SourceRange<'a>,
        is_first: bool,
        is_last: bool,
    ) -> Arc<str> {
        for (index, loc) in newline_locations.into_iter().rev() {
            if let Some(((i, _), _)) = string[index..]
                .char_indices()
                .zip(indentation.get_str().char_indices())
                .find(|((_, a), (_, b))| a != b)
            {
                lexer
                    .error_at_token(
                        loc + i,
                        Diagnostic::insufficient_indentation(),
                        DiagnosticContextNote::containing_literal_location(),
                    )
                    .with_context_note(loc.to_range(1), DiagnosticContextNote::line_start_location())
                    .with_context_note(indentation, DiagnosticContextNote::indentation_location())
                    .with_note(DiagnosticNote::insufficient_indentation())
                    .record();
            } else {
                string.replace_range(index..index + indentation.len(), "");
            }
        }

        let mut string = string.as_str();

        if is_first {
            if let Some(stripped) = string.strip_prefix('\n') {
                string = stripped;
            }
        }

        if is_last {
            if let Some(stripped) = string.strip_suffix('\n') {
                string = stripped;
            }
        }

        Arc::from(string)
    }

    pub fn lex_raw_string(lexer: &mut Lexer<'a>) -> Self {
        todo!()
    }

    fn consume_escape_sequence(
        lexer: &mut Lexer<'a>,
        is_multiline: bool,
        literal_name: &'static str,
    ) -> (Option<char>, bool) {
        let escaped = 'b: {
            if let Some(c) = lexer.advance() {
                Some(match c {
                    '\r' | '\n' => {
                        let start = lexer.current - 1;

                        if c == '\r' {
                            lexer.match_char_eq('\n');
                        }

                        if !is_multiline {
                            lexer
                                .error_at_token(
                                    lexer.source.get_loc(start),
                                    Diagnostic::newline_in_literal("string"),
                                    DiagnosticContextNote::containing_literal_location(),
                                )
                                .with_note(DiagnosticNote::newline_in_literal())
                                .record();
                        }

                        return (None, true);
                    }
                    '0' => '\0',
                    '\\' => '\\',
                    't' => '\t',
                    'n' => '\n',
                    'r' => '\r',
                    '"' => '"',
                    '\'' => '\'',
                    '$' => '$',
                    'u' => {
                        if !lexer.match_char_eq('{') {
                            let range = lexer.get_current_character_range(c);

                            lexer
                                .error_at_token(
                                    range.end_loc(),
                                    Diagnostic::expected_unicode_escape_brace(literal_name),
                                    DiagnosticContextNote::containing_literal_location(),
                                )
                                .with_context_note(range, DiagnosticContextNote::unicode_escape_location())
                                .record();

                            break 'b None;
                        }

                        let mut code_point = 0;
                        let mut i = 0;
                        let mut invalid_code_point = false;
                        loop {
                            let Some(c) = lexer.advance_if(|c| c != '}') else {
                                break;
                            };

                            let Some(digit) = c.to_digit(16) else {
                                let range = lexer.get_current_character_range(c);

                                lexer
                                    .error_at_token(
                                        range.start_loc(),
                                        Diagnostic::invalid_unicode_escape_digit(c, literal_name),
                                        DiagnosticContextNote::containing_literal_location(),
                                    )
                                    .with_context_note(range, DiagnosticContextNote::invalid_digit_location())
                                    .with_note(DiagnosticNote::expected_digit("hexadecimal digit", "0-9, a-f, or A-F"))
                                    .record();

                                invalid_code_point = true;

                                continue;
                            };

                            code_point = (code_point << 4) | digit;
                            i += 1;
                        }

                        if !lexer.match_char_eq('}') {
                            let range = lexer.source.get_range(lexer.current - i - 2, lexer.current);

                            lexer
                                .error_at_token(
                                    range.end_loc(),
                                    Diagnostic::expected_unicode_escape_brace(literal_name),
                                    DiagnosticContextNote::containing_literal_location(),
                                )
                                .with_context_note(range, DiagnosticContextNote::unicode_escape_location())
                                .record();

                            break 'b None;
                        }

                        if i == 0 {
                            let range = lexer.source.get_range(lexer.current - 3, lexer.current);

                            lexer
                                .error_at_token(
                                    lexer.source.get_loc(lexer.current - 1),
                                    Diagnostic::missing_unicode_escape(literal_name),
                                    DiagnosticContextNote::containing_literal_location(),
                                )
                                .with_context_note(range, DiagnosticContextNote::unicode_escape_location())
                                .with_note(DiagnosticNote::unicode_escape_length())
                                .record();

                            break 'b None;
                        } else if i > 8 {
                            let range = lexer.source.get_range(lexer.current - i - 3, lexer.current);

                            lexer
                                .error_at_token(
                                    lexer.source.get_loc(lexer.current - i - 1),
                                    Diagnostic::overlong_unicode_escape(literal_name),
                                    DiagnosticContextNote::containing_literal_location(),
                                )
                                .with_context_note(range, DiagnosticContextNote::unicode_escape_location())
                                .with_note(DiagnosticNote::unicode_escape_length())
                                .record();

                            break 'b None;
                        } else if invalid_code_point {
                            break 'b None;
                        }

                        let Some(c) = char::from_u32(code_point) else {
                            let range = lexer.source.get_range(lexer.current - i - 1, lexer.current - 1);

                            lexer
                                .error_at_token(
                                    range.start_loc(),
                                    Diagnostic::invalid_unicode_scalar(range.get_str()),
                                    DiagnosticContextNote::containing_literal_location(),
                                )
                                .with_context_note(range, DiagnosticContextNote::invalid_unicode_scalar_location())
                                .record();

                            break 'b None;
                        };

                        c
                    }
                    c => {
                        let range = lexer.get_current_character_range(c);

                        lexer
                            .error_at_token(
                                range.start_loc(),
                                Diagnostic::invalid_escape_sequence(c, literal_name),
                                DiagnosticContextNote::containing_literal_location(),
                            )
                            .with_context_note(range, DiagnosticContextNote::escape_sequence_location())
                            .record();

                        break 'b None;
                    }
                })
            } else {
                lexer
                    .error_at_end(
                        Diagnostic::expected_escape_sequence(literal_name),
                        DiagnosticContextNote::containing_literal_location(),
                    )
                    .record();

                None
            }
        };

        (escaped, false)
    }
}
