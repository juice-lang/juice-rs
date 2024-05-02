use std::{borrow::Cow, sync::Arc};

use juice_core::CharExt;
use num_bigint::BigUint;

use super::{Lexer, Token};
use crate::{
    diag::{Diagnostic, DiagnosticContextNote, DiagnosticNote},
    source_loc::{SourceLoc, SourceRange},
    source_manager::SourceManager,
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
pub enum InterpolationPart<'a, M: SourceManager> {
    String(Arc<str>),
    Interpolation(Vec<Token<'a, M>>),
}

#[derive(Debug, Clone)]
pub enum LiteralKind<'a, M: SourceManager> {
    Int(u64, Radix),
    BigInt(Vec<u64>, Radix),
    Float(f64),
    Char(char),
    String(Arc<str>),
    StringInterpolation(Vec<InterpolationPart<'a, M>>),
    InvalidInt,
    InvalidFloat,
    InvalidChar,
    InvalidString,
}

impl<'a, M: SourceManager> LiteralKind<'a, M> {
    pub fn lex_number(lexer: &mut Lexer<'a, M>, start: char) -> Self {
        let (radix, mut is_first) = (start == '0')
            .then(|| lexer.match_char_map(Radix::from_prefix))
            .flatten()
            .map_or((Radix::Decimal, false), |r| (r, true));

        loop {
            let Some(c) = lexer.peek() else {
                return if is_first {
                    let mut error = lexer.error_at_end(
                        Diagnostic::missing_digit(radix.digit_name(), "integer"),
                        DiagnosticContextNote::containing_literal_location(),
                    );

                    if let Some(hint) = radix.digit_hint() {
                        error = error.with_note(DiagnosticNote::expected_digit(radix.digit_name(), hint));
                    }

                    error.record();

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
                && (matches!(c, 'e' | 'E')
                    || (c == '.' && !lexer.last_was_dot && lexer.peek2().is_some_and(CharExt::is_decimal_digit)))
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
                if let Some(c) = lexer.peek() {
                    let range = lexer.get_next_character_range(c);

                    lexer
                        .error_at_token(
                            lexer.get_current_loc(),
                            Diagnostic::invalid_digit("digit", c, "floating-point"),
                            DiagnosticContextNote::containing_literal_location(),
                        )
                        .with_context_note(range, DiagnosticContextNote::invalid_digit_location())
                        .record();
                } else {
                    lexer
                        .error_at_end(
                            Diagnostic::missing_digit("digit", "floating-point"),
                            DiagnosticContextNote::containing_literal_location(),
                        )
                        .record();
                }

                while lexer.match_char(|c| !c.is_number_end()) {}

                return Self::InvalidFloat;
            }

            while lexer.match_char(|c| CharExt::is_decimal_digit(c) || c == '_') {}
        }

        let Some(c) = lexer.peek().filter(|c| c.is_identifier_char()) else {
            return Self::parse_float(lexer.get_current_range().get_str());
        };

        let range = lexer.get_next_character_range(c);

        lexer
            .error_at_token(
                lexer.get_current_loc(),
                Diagnostic::invalid_digit("digit", c, "floating-point"),
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

        match words.len() {
            0 => Self::Int(0, radix),
            1 => Self::Int(words[0], radix),
            _ => Self::BigInt(words, radix),
        }
    }

    fn parse_float(string: &str) -> Self {
        let string = string.replace('_', "");

        let float = string.parse().expect("Digits should be valid here");

        Self::Float(float)
    }

    pub fn lex_char(lexer: &mut Lexer<'a, M>) -> Self {
        let start_loc = lexer.get_current_loc() - 1;

        let terminator = Cow::Borrowed("'");

        let Some(c) = lexer.peek() else {
            lexer
                .error_at_end(
                    Diagnostic::expected_literal_terminator(terminator, "character"),
                    DiagnosticContextNote::unterminated_literal_location(),
                )
                .record();

            return Self::InvalidChar;
        };

        let content = match c {
            '\'' => {
                lexer
                    .error_at_token(
                        start_loc,
                        Diagnostic::empty_literal("character"),
                        DiagnosticContextNote::literal_location(),
                    )
                    .record();

                lexer.advance();

                return Self::InvalidChar;
            }
            '\r' | '\n' => {
                let mut count = 1;

                if c == '\r' && lexer.peek2() == Some('\n') {
                    count += 1;
                }

                if lexer.peek_n(count) == Some('\'') {
                    lexer
                        .error_at_token(
                            lexer.get_current_loc(),
                            Diagnostic::newline_in_literal("character"),
                            DiagnosticContextNote::containing_literal_location(),
                        )
                        .with_note(DiagnosticNote::newline_in_literal())
                        .record();

                    lexer
                        .advance_by(count)
                        .expect("Should be able to advance by newline count");

                    None
                } else {
                    lexer
                        .error_at_token(
                            lexer.get_current_loc(),
                            Diagnostic::expected_literal_terminator(terminator, "character"),
                            DiagnosticContextNote::unterminated_literal_location(),
                        )
                        .record();

                    return Self::InvalidChar;
                }
            }
            '\\' => {
                lexer.advance();

                let (escaped, was_newline) = Self::consume_escape_sequence(lexer, false, '\'', "character");

                if was_newline {
                    let mut count = 1;

                    if lexer.peek() == Some('\r') && lexer.peek2() == Some('\n') {
                        count += 1;
                    }

                    if lexer.peek_n(count) == Some('\'') {
                        lexer
                            .error_at_token(
                                lexer.get_current_loc(),
                                Diagnostic::newline_in_literal("character"),
                                DiagnosticContextNote::containing_literal_location(),
                            )
                            .with_note(DiagnosticNote::newline_in_literal())
                            .record();

                        lexer
                            .advance_by(count)
                            .expect("Should be able to advance by newline count");

                        None
                    } else {
                        lexer
                            .error_at_token(
                                lexer.get_current_loc(),
                                Diagnostic::expected_literal_terminator(terminator, "character"),
                                DiagnosticContextNote::unterminated_literal_location(),
                            )
                            .record();

                        return Self::InvalidChar;
                    }
                } else {
                    escaped
                }
            }
            _ => {
                lexer.advance();

                Some(c)
            }
        };

        if !lexer.match_char_eq('\'') {
            if !matches!(
                Self::lex_string_impl(lexer, start_loc, terminator, None, false, true, "character"),
                Self::InvalidString
            ) {
                lexer
                    .error_at_token(
                        start_loc,
                        Diagnostic::string_in_char_literal(),
                        DiagnosticContextNote::literal_location(),
                    )
                    .with_note(DiagnosticNote::string_in_char_literal())
                    .record();
            }

            Self::InvalidChar
        } else if let Some(content) = content {
            Self::Char(content)
        } else {
            Self::InvalidChar
        }
    }

    pub fn lex_string(lexer: &mut Lexer<'a, M>) -> Self {
        let start_loc = lexer.get_current_loc() - 1;

        let is_multiline = lexer.match_str(r#""""#); // One quote is already consumed by the lexer
        let terminator = Cow::Borrowed(if is_multiline { r#"""""# } else { r#"""# });

        Self::lex_string_impl(lexer, start_loc, terminator, None, is_multiline, false, "string")
    }

    pub fn lex_raw_string(lexer: &mut Lexer<'a, M>) -> Self {
        let start_loc = lexer.get_current_loc() - 1;
        let mut raw_level = 1;

        while lexer.match_char_eq('#') {
            raw_level += 1;
        }

        let is_multiline = if lexer.match_str(r#"""""#) {
            true
        } else {
            assert_eq!(lexer.advance(), Some('"'), "Raw string should start with `\"`");
            false
        };

        let mut terminator = String::from(if is_multiline { r#"""""# } else { r#"""# });
        terminator += &"#".repeat(raw_level);

        let terminator = Cow::from(terminator);

        Self::lex_string_impl(
            lexer,
            start_loc,
            terminator,
            Some(raw_level),
            is_multiline,
            false,
            "raw string",
        )
    }

    fn lex_string_impl(
        lexer: &mut Lexer<'a, M>,
        start_loc: SourceLoc<'a, M>,
        terminator: Cow<'static, str>,
        raw_level: Option<usize>,
        is_multiline: bool,
        recovering_character: bool,
        literal_name: &'static str,
    ) -> Self {
        enum Part<'a, M: SourceManager> {
            String {
                content: String,
                newline_locations: Vec<(usize, SourceLoc<'a, M>)>,
            },
            Interpolation(Vec<Token<'a, M>>),
        }

        if is_multiline && lexer.in_interpolation {
            lexer
                .error_at_token(
                    start_loc,
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
            if lexer.match_str(&terminator) {
                break;
            }

            let Some(c) = lexer.peek() else {
                lexer
                    .error_at_end(
                        Diagnostic::expected_literal_terminator(terminator, literal_name),
                        DiagnosticContextNote::unterminated_literal_location(),
                    )
                    .record();

                return Self::InvalidString;
            };

            match c {
                '\r' | '\n' => {
                    let mut count = 1;

                    if c == '\r' && lexer.peek2() == Some('\n') {
                        count += 1;
                    }

                    if recovering_character {
                        lexer
                            .error_at_token(
                                lexer.get_current_loc(),
                                Diagnostic::expected_literal_terminator(terminator, literal_name),
                                DiagnosticContextNote::unterminated_literal_location(),
                            )
                            .record();

                        return Self::InvalidString;
                    }

                    if !is_multiline {
                        lexer
                            .error_at_token(
                                lexer.get_current_loc(),
                                Diagnostic::newline_in_literal(literal_name),
                                DiagnosticContextNote::containing_literal_location(),
                            )
                            .with_note(DiagnosticNote::newline_in_literal())
                            .record();
                    }

                    lexer
                        .advance_by(count)
                        .expect("Should be able to advance by newline count");

                    current_content.push('\n');
                    current_newline_locations.push((current_content.len(), lexer.get_current_loc()));
                }
                '\\' => {
                    lexer.advance();

                    if let Some(raw_level) = raw_level {
                        if !lexer.match_str(&"#".repeat(raw_level)) {
                            current_content.push(c);
                            continue;
                        }
                    }

                    let (escaped, was_newline) = Self::consume_escape_sequence(
                        lexer,
                        is_multiline,
                        terminator.chars().next().unwrap(),
                        literal_name,
                    );
                    if let Some(escaped) = escaped {
                        current_content.push(escaped);
                    }

                    if was_newline {
                        let mut count = 1;

                        if lexer.peek() == Some('\r') && lexer.peek2() == Some('\n') {
                            count += 1;
                        }

                        if recovering_character {
                            lexer
                                .error_at_token(
                                    lexer.get_current_loc(),
                                    Diagnostic::expected_literal_terminator(terminator, literal_name),
                                    DiagnosticContextNote::unterminated_literal_location(),
                                )
                                .record();

                            return Self::InvalidString;
                        }

                        if !is_multiline {
                            lexer
                                .error_at_token(
                                    lexer.get_current_loc(),
                                    Diagnostic::newline_in_literal(literal_name),
                                    DiagnosticContextNote::containing_literal_location(),
                                )
                                .with_note(DiagnosticNote::newline_in_literal())
                                .record();

                            lexer
                                .advance_by(count)
                                .expect("Should be able to advance by newline count");
                        }

                        current_newline_locations.push((current_content.len(), lexer.get_current_loc()));
                    }
                }
                '$' if raw_level.is_none() && !recovering_character && lexer.peek2() == Some('{') => {
                    lexer.advance_by(2).expect("Should be able to advance by 2");

                    let content = std::mem::take(&mut current_content);
                    let newline_locations = std::mem::take(&mut current_newline_locations);
                    parts.push(Part::String {
                        content,
                        newline_locations,
                    });

                    let tokens = lexer.with_interpolation(|nested_lexer| nested_lexer.collect());

                    parts.push(Part::Interpolation(tokens));
                }
                _ => {
                    lexer.advance();
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
                Self::StringInterpolation(
                    parts
                        .into_iter()
                        .filter_map(|part| {
                            if let InterpolationPart::String(content) = &part {
                                if content.is_empty() {
                                    return None;
                                }
                            }

                            Some(part)
                        })
                        .collect(),
                )
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
                    .filter_map(|part| {
                        if let InterpolationPart::String(content) = &part {
                            if content.is_empty() {
                                return None;
                            }
                        }

                        Some(part)
                    })
                    .collect();
                Self::StringInterpolation(parts)
            }
        }
    }

    fn remove_multiline_indentation(
        lexer: &mut Lexer<'a, M>,
        mut string: String,
        newline_locations: Vec<(usize, SourceLoc<'a, M>)>,
        indentation: SourceRange<'a, M>,
        is_first: bool,
        is_last: bool,
    ) -> Arc<str> {
        let mut errors = Vec::new();
        for (index, loc) in newline_locations.into_iter().rev() {
            if let Some(((i, _), _)) = string[index..]
                .char_indices()
                .zip(indentation.get_str().char_indices())
                .find(|((_, a), (_, b))| a != b)
            {
                errors.push((loc, i));
            } else {
                string.replace_range(index..index + indentation.len(), "");
            }
        }

        for (loc, i) in errors.into_iter().rev() {
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

    fn consume_escape_sequence(
        lexer: &mut Lexer<'a, M>,
        is_multiline: bool,
        terminator: char,
        literal_name: &'static str,
    ) -> (Option<char>, bool) {
        let escaped = 'b: {
            if let Some(c) = lexer.peek() {
                if matches!(c, '\r' | '\n') {
                    if is_multiline {
                        lexer.advance();

                        if c == '\r' {
                            lexer.match_char_eq('\n');
                        }
                    }

                    return (None, true);
                }

                lexer.advance();

                Some(match c {
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
                                    Diagnostic::expected_unicode_escape_brace('{', "start", literal_name),
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
                            let Some(c) = lexer.advance_if(|c| c != '}' && c != terminator) else {
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
                                    Diagnostic::expected_unicode_escape_brace('}', "end", literal_name),
                                    DiagnosticContextNote::containing_literal_location(),
                                )
                                .with_context_note(range, DiagnosticContextNote::unicode_escape_location())
                                .record();

                            break 'b None;
                        }

                        if invalid_code_point {
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
                        }

                        if i > 8 {
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
                        }

                        let Some(c) = char::from_u32(code_point) else {
                            let range = lexer.source.get_range(lexer.current - i - 1, lexer.current - 1);

                            lexer
                                .error_at_token(
                                    range.start_loc(),
                                    Diagnostic::invalid_unicode_scalar(range.get_str(), literal_name),
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

#[cfg(test)]
mod tests {
    use super::{InterpolationPart, LiteralKind::*, Radix::*};
    use crate::{
        diag::{Diagnostic, DiagnosticContextNote, DiagnosticNote},
        parser::lexer::{
            test::{assert_all_reports, assert_all_tokens, run_lexer},
            token_kind::{PunctuationKind::*, TokenKind::*},
        },
        source_manager::test::SourceManager,
    };

    #[test]
    fn test_integer_literal() {
        static SOURCE_MANAGER: SourceManager = SourceManager::new(
            "
            0 0x0 0b0 0o0
            10 0x10 0b10 0o10
            1_000_000
            0x1_0000_0000_0000_0000
            0.foo
            foo.0.0
            ",
        );

        let tokens = run_lexer(&SOURCE_MANAGER).unwrap();

        assert_all_tokens!(
            tokens;
            Punctuation(Newline), 0;
            Literal(Int(0, Decimal)), 13;
            Literal(Int(0, Hexadecimal)), 15..18;
            Literal(Int(0, Binary)), 19..22;
            Literal(Int(0, Octal)), 23..26;
            Punctuation(Newline), 26;
            Literal(Int(10, Decimal)), 39..41;
            Literal(Int(16, Hexadecimal)), 42..46;
            Literal(Int(2, Binary)), 47..51;
            Literal(Int(8, Octal)), 52..56;
            Punctuation(Newline), 56;
            Literal(Int(1_000_000, Decimal)), 69..78;
            Punctuation(Newline), 78;
            Literal(BigInt(v, Hexadecimal)) if v.len() == 2 && v[0] == 0 && v[1] == 1, 91..114;
            Punctuation(Newline), 114;
            Literal(Int(0, Decimal)), 127;
            Punctuation(Dot), 128;
            Identifier, 129, "foo";
            Punctuation(Newline), 132;
            Identifier, 145, "foo";
            Punctuation(Dot), 148;
            Literal(Int(0, Decimal)), 149;
            Punctuation(Dot), 150;
            Literal(Int(0, Decimal)), 151;
            Punctuation(Newline), 152;
        );
    }

    #[test]
    fn test_integer_literal_error() {
        static SOURCE_MANAGER: SourceManager = SourceManager::new(
            "0r0r 0x
            0b",
        );

        let reports = run_lexer(&SOURCE_MANAGER).unwrap_err();

        assert_all_reports!(
            reports;
            Diagnostic::InvalidDigit { .. }, 1,
                <DiagnosticContextNote::InvalidDigitLocation, 1..2>,
                <DiagnosticContextNote::ContainingLiteralLocation, 0..4>;
            Diagnostic::InvalidDigit { .. }, 7,
                <DiagnosticContextNote::InvalidDigitLocation, 7..8>,
                <DiagnosticContextNote::ContainingLiteralLocation, 5..7>,
                (DiagnosticNote::ExpectedDigit { .. });
            Diagnostic::MissingDigit { .. }, 21,
                <DiagnosticContextNote::ContainingLiteralLocation, 20..22>,
                (DiagnosticNote::ExpectedDigit { .. });
        );
    }

    #[test]
    fn test_float_literal() {
        static SOURCE_MANAGER: SourceManager = SourceManager::new(
            "
            0.0
            1.0 1.0e0 1.0e+1 1.0e-1 1E0 1E+1 1E-1
            1_000.0 1.000_1
            0.0.foo
            ",
        );

        let tokens = run_lexer(&SOURCE_MANAGER).unwrap();

        assert_all_tokens!(
            tokens;
            Punctuation(Newline), 0;
            Literal(Float(0.0)), 13..16;
            Punctuation(Newline), 16;
            Literal(Float(1.0)), 29..32;
            Literal(Float(1.0)), 33..38;
            Literal(Float(10.0)), 39..45;
            Literal(Float(0.1)), 46..52;
            Literal(Float(1.0)), 53..56;
            Literal(Float(10.0)), 57..61;
            Literal(Float(0.1)), 62..66;
            Punctuation(Newline), 66;
            Literal(Float(1_000.0)), 79..86;
            Literal(Float(1.000_1)), 87..94;
            Punctuation(Newline), 94;
            Literal(Float(0.0)), 107..110;
            Punctuation(Dot), 110;
            Identifier, 111, "foo";
            Punctuation(Newline), 114;
        );
    }

    #[test]
    fn test_float_literal_error() {
        static SOURCE_MANAGER: SourceManager = SourceManager::new("0.0e+x0x 0.0x0x 0e");

        let reports = run_lexer(&SOURCE_MANAGER).unwrap_err();

        assert_all_reports!(
            reports;
            Diagnostic::InvalidDigit { .. }, 5,
                <DiagnosticContextNote::InvalidDigitLocation, 5..6>,
                <DiagnosticContextNote::ContainingLiteralLocation, 0..8>;
            Diagnostic::InvalidDigit { .. }, 12,
                <DiagnosticContextNote::InvalidDigitLocation, 12..13>,
                <DiagnosticContextNote::ContainingLiteralLocation, 9..15>;
            Diagnostic::MissingDigit { .. }, 17,
                <DiagnosticContextNote::ContainingLiteralLocation, 16..18>;
        );
    }

    #[test]
    fn test_char_literal() {
        static SOURCE_MANAGER: SourceManager =
            SourceManager::new(r#"'a' '0' ' ' '"' '\0' '\\' '\t' '\n' '\r' '\"' '\'' '\u{20}' '\u{1F600}' '😀'"#);

        let tokens = run_lexer(&SOURCE_MANAGER).unwrap();

        assert_all_tokens!(
            tokens;
            Literal(Char('a')), 0..3;
            Literal(Char('0')), 4..7;
            Literal(Char(' ')), 8..11;
            Literal(Char('"')), 12..15;
            Literal(Char('\0')), 16..20;
            Literal(Char('\\')), 21..25;
            Literal(Char('\t')), 26..30;
            Literal(Char('\n')), 31..35;
            Literal(Char('\r')), 36..40;
            Literal(Char('"')), 41..45;
            Literal(Char('\'')), 46..50;
            Literal(Char(' ')), 51..59;
            Literal(Char('\u{1F600}')), 60..71;
            Literal(Char('\u{1F600}')), 72..78;
        );
    }

    #[test]
    fn test_char_literal_error() {
        static SOURCE_MANAGER: SourceManager = SourceManager::new(
            "'' '\r\n' '\n
            '\\\r\n' '\\\n
            '\\u' '\\u{z}' '\\u{123' '\\u{}' '\\u{123456789}' '\\u{DEADBEEF}'
            '\\q' 'abc' '",
        );

        let reports = run_lexer(&SOURCE_MANAGER).unwrap_err();

        assert_all_reports!(
            reports;
            Diagnostic::EmptyLiteral { .. }, 0,
                <DiagnosticContextNote::LiteralLocation, 0..2>;
            Diagnostic::NewlineInLiteral { .. }, 4,
                <DiagnosticContextNote::ContainingLiteralLocation, 3..7>,
                (DiagnosticNote::NewlineInLiteral);
            Diagnostic::ExpectedLiteralTerminator { .. }, 9,
                <DiagnosticContextNote::UnterminatedLiteralLocation, 8..9>;
            Diagnostic::NewlineInLiteral { .. }, 25,
                <DiagnosticContextNote::ContainingLiteralLocation, 23..28>,
                (DiagnosticNote::NewlineInLiteral);
            Diagnostic::ExpectedLiteralTerminator { .. }, 31,
                <DiagnosticContextNote::UnterminatedLiteralLocation, 29..31>;
            Diagnostic::ExpectedUnicodeEscapeBrace { purpose: "start", .. }, 48,
                <DiagnosticContextNote::UnicodeEscapeLocation, 47..48>,
                <DiagnosticContextNote::ContainingLiteralLocation, 45..49>;
            Diagnostic::InvalidUnicodeEscapeDigit { .. }, 54,
                <DiagnosticContextNote::InvalidDigitLocation, 54..55>,
                <DiagnosticContextNote::ContainingLiteralLocation, 50..57>,
                (DiagnosticNote::ExpectedDigit { .. });
            Diagnostic::ExpectedUnicodeEscapeBrace { purpose: "end", .. }, 65,
                <DiagnosticContextNote::UnicodeEscapeLocation, 60..65>,
                <DiagnosticContextNote::ContainingLiteralLocation, 58..66>;
            Diagnostic::MissingUnicodeEscape { .. }, 71,
                <DiagnosticContextNote::UnicodeEscapeLocation, 69..72>,
                <DiagnosticContextNote::ContainingLiteralLocation, 67..73>,
                (DiagnosticNote::UnicodeEscapeLength);
            Diagnostic::OverlongUnicodeEscape { .. }, 78,
                <DiagnosticContextNote::UnicodeEscapeLocation, 76..88>,
                <DiagnosticContextNote::ContainingLiteralLocation, 74..89>,
                (DiagnosticNote::UnicodeEscapeLength);
            Diagnostic::InvalidUnicodeScalar { .. }, 94,
                <DiagnosticContextNote::InvalidUnicodeScalarLocation, 94..102>,
                <DiagnosticContextNote::ContainingLiteralLocation, 90..104>;
            Diagnostic::InvalidEscapeSequence { .. }, 119,
                <DiagnosticContextNote::EscapeSequenceLocation, 119..120>,
                <DiagnosticContextNote::ContainingLiteralLocation, 117..121>;
            Diagnostic::StringInCharLiteral, 122,
                <DiagnosticContextNote::LiteralLocation, 122..127>,
                (DiagnosticNote::StringInCharLiteral);
            Diagnostic::ExpectedLiteralTerminator { .. }, 128,
                <DiagnosticContextNote::UnterminatedLiteralLocation, 128..129>;
        );
    }

    #[test]
    fn test_string_literal() {
        static SOURCE_MANAGER: SourceManager = SourceManager::new(
            r###"
            "" "hello" "'\0\\\t\n\r\"\'\$\u{20}"
            #""# #"hello"# #"\n\#n"# ##"\n\#n\##n"##
            """""" """hello""" """\n"""
            """
            hello
            """
            """
            hello, \
            world
            """
            """
              hello
              """
            #"""
            hello\
            world\#
            !
            """#
            "###,
        );

        let tokens = run_lexer(&SOURCE_MANAGER).unwrap();

        assert_all_tokens!(
            tokens;
            Punctuation(Newline), 0;
            Literal(String(s)) if s.as_ref() == "", 13..15;
            Literal(String(s)) if s.as_ref() == "hello", 16..23;
            Literal(String(s)) if s.as_ref() == "'\0\\\t\n\r\"\'$ ", 24..49;
            Punctuation(Newline), 49;
            Literal(String(s)) if s.as_ref() == "", 62..66;
            Literal(String(s)) if s.as_ref() == "hello", 67..76;
            Literal(String(s)) if s.as_ref() == "\\n\n", 77..86;
            Literal(String(s)) if s.as_ref() == "\\n\\#n\n", 87..102;
            Punctuation(Newline), 102;
            Literal(String(s)) if s.as_ref() == "", 115..121;
            Literal(String(s)) if s.as_ref() == "hello", 122..133;
            Literal(String(s)) if s.as_ref() == "\n", 134..142;
            Punctuation(Newline), 142;
            Literal(String(s)) if s.as_ref() == "hello", 155..192;
            Punctuation(Newline), 192;
            Literal(String(s)) if s.as_ref() == "hello, world", 205..263;
            Punctuation(Newline), 263;
            Literal(String(s)) if s.as_ref() == "hello", 276..317;
            Punctuation(Newline), 317;
            Literal(String(s)) if s.as_ref() == "hello\\\nworld!", 330..404;
            Punctuation(Newline), 404;
        );
    }

    #[test]
    fn test_string_literal_error() {
        static SOURCE_MANAGER: SourceManager = SourceManager::new(
            r###"
            "
            \
            \u{}"
            """
            \u{DEADBEEF}
            hello
                """
            #"\#q\#u{"#
            ##"""
            """#
            "###,
        );

        let reports = run_lexer(&SOURCE_MANAGER).unwrap_err();

        assert_all_reports!(
            reports;
            Diagnostic::NewlineInLiteral { .. }, 14,
                <DiagnosticContextNote::ContainingLiteralLocation, 13..46>,
                (DiagnosticNote::NewlineInLiteral);
            Diagnostic::NewlineInLiteral { .. }, 28,
                <DiagnosticContextNote::ContainingLiteralLocation, 13..46>,
                (DiagnosticNote::NewlineInLiteral);
            Diagnostic::MissingUnicodeEscape { .. }, 44,
                <DiagnosticContextNote::UnicodeEscapeLocation, 42..45>,
                <DiagnosticContextNote::ContainingLiteralLocation, 13..46>,
                (DiagnosticNote::UnicodeEscapeLength);
            Diagnostic::InvalidUnicodeScalar { .. }, 78,
                <DiagnosticContextNote::InvalidUnicodeScalarLocation, 78..86>,
                <DiagnosticContextNote::ContainingLiteralLocation, 59..125>;
            Diagnostic::InsufficientIndentation, 75,
                <DiagnosticContextNote::LineStartLocation, 63..64>,
                <DiagnosticContextNote::IndentationLocation, 106..122>,
                <DiagnosticContextNote::ContainingLiteralLocation, 59..125>,
                (DiagnosticNote::InsufficientIndentation);
            Diagnostic::InsufficientIndentation, 100,
                <DiagnosticContextNote::LineStartLocation, 88..89>,
                <DiagnosticContextNote::IndentationLocation, 106..122>,
                <DiagnosticContextNote::ContainingLiteralLocation, 59..125>,
                (DiagnosticNote::InsufficientIndentation);
            Diagnostic::InvalidEscapeSequence { .. }, 142,
                <DiagnosticContextNote::EscapeSequenceLocation, 142..143>,
                <DiagnosticContextNote::ContainingLiteralLocation, 138..149>;
            Diagnostic::ExpectedUnicodeEscapeBrace { purpose: "end", .. }, 147,
                <DiagnosticContextNote::UnicodeEscapeLocation, 145..147>,
                <DiagnosticContextNote::ContainingLiteralLocation, 138..149>;
            Diagnostic::ExpectedLiteralTerminator { .. }, 196,
                <DiagnosticContextNote::UnterminatedLiteralLocation, 162..197>;
        );
    }

    #[test]
    fn test_interpolation() {
        static SOURCE_MANAGER: SourceManager = SourceManager::new(
            r#"
            "hello, ${world}" "${a + b}${c}"
            "${"${a}"}"
            """
            hello$, ${world}
            """
            "#,
        );

        let tokens = run_lexer(&SOURCE_MANAGER).unwrap();

        assert_all_tokens!(
            tokens;
            Punctuation(Newline), 0;
            Literal(StringInterpolation(parts)) if matches!(
                parts.as_slice(),
                [
                    InterpolationPart::String(s),
                    InterpolationPart::Interpolation(inner_tokens),
                ] if {
                    assert_all_tokens!(
                        inner_tokens;
                        Identifier, 23, "world";
                    );
                    s.as_ref() == "hello, "
                }
            ), 13..30;
            Literal(StringInterpolation(parts)) if matches!(
                parts.as_slice(),
                [
                    InterpolationPart::Interpolation(inner_tokens_1),
                    InterpolationPart::Interpolation(inner_tokens_2),
                ] if {
                    assert_all_tokens!(
                        inner_tokens_1;
                        Identifier, 34, "a";
                        Operator, 36, "+";
                        Identifier, 38, "b";
                    );
                    assert_all_tokens!(
                        inner_tokens_2;
                        Identifier, 42, "c";
                    );
                    true
                }
            ), 31..45;
            Punctuation(Newline), 45;
            Literal(StringInterpolation(parts)) if matches!(
                parts.as_slice(),
                [InterpolationPart::Interpolation(inner_tokens)] if {
                    assert_all_tokens!(
                        inner_tokens;
                        Literal(StringInterpolation(parts)) if matches!(
                            parts.as_slice(),
                            [InterpolationPart::Interpolation(inner_inner_tokens)] if {
                                assert_all_tokens!(
                                    inner_inner_tokens;
                                    Identifier, 64, "a";
                                );
                                true
                            }
                        ), 61..67;
                    );
                    true
                }
            ), 58..69;
            Punctuation(Newline), 69;
            Literal(StringInterpolation(parts)) if matches!(
                parts.as_slice(),
                [
                    InterpolationPart::String(s),
                    InterpolationPart::Interpolation(inner_tokens),
                ] if {
                    assert_all_tokens!(
                        inner_tokens;
                        Identifier, 108, "world";
                    );
                    s.as_ref() == "hello$, "
                }
            ), 82..130;
            Punctuation(Newline), 130;
        );
    }

    #[test]
    fn test_interpolation_error() {
        static SOURCE_MANAGER: SourceManager = SourceManager::new(
            r#""hello, ${
            world /*
            */ }"
            "${"${π}"}"
            """
            ${world """"#,
        );

        let reports = run_lexer(&SOURCE_MANAGER).unwrap_err();

        assert_all_reports!(
            reports;
            Diagnostic::NewlineInInterpolation, 10,
                <DiagnosticContextNote::InterpolationLocation, 8..48>,
                <DiagnosticContextNote::ContainingLiteralLocation, 0..49>,
                (DiagnosticNote::NewlineInInterpolation);
            Diagnostic::NewlineInInterpolation, 31,
                <DiagnosticContextNote::InterpolationLocation, 8..48>,
                <DiagnosticContextNote::ContainingLiteralLocation, 0..49>,
                (DiagnosticNote::NewlineInInterpolation);
            Diagnostic::InvalidCharacter { .. }, 68,
                <DiagnosticContextNote::InvalidCharacterLocation, 68..70>, // `π` is two bytes long
                <DiagnosticContextNote::InterpolationLocation, 66..71>,
                <DiagnosticContextNote::ContainingLiteralLocation, 65..72>,
                <DiagnosticContextNote::InterpolationLocation, 63..73>,
                <DiagnosticContextNote::ContainingLiteralLocation, 62..74>;
            Diagnostic::ExpectedLiteralTerminator { .. }, 113,
                <DiagnosticContextNote::UnterminatedLiteralLocation, 87..114>;
            Diagnostic::ExpectedInterpolationEnd, 113,
                <DiagnosticContextNote::InterpolationStartLocation, 103..105>,
                <DiagnosticContextNote::ContainingLiteralLocation, 87..114>;
            Diagnostic::ExpectedLiteralTerminator { .. }, 113,
                <DiagnosticContextNote::UnterminatedLiteralLocation, 111..114>,
                <DiagnosticContextNote::InterpolationLocation, 103..114>,
                <DiagnosticContextNote::ContainingLiteralLocation, 87..114>;
            Diagnostic::MultilineStringInInterpolation, 111,
                <DiagnosticContextNote::LiteralLocation, 111..114>,
                <DiagnosticContextNote::InterpolationLocation, 103..114>,
                <DiagnosticContextNote::ContainingLiteralLocation, 87..114>,
                (DiagnosticNote::NewlineInInterpolation);
        );
    }
}
