mod fsm;
mod literal;
mod token;
mod token_kind;

use std::num::NonZero;

use juice_core::{CharExt, OptionExt as _, PeekableChars};

use self::literal::LiteralKind;
pub use self::{token::Token, token_kind::TokenKind};
use crate::{
    diag::{Diagnostic, DiagnosticConsumer, DiagnosticContextNote, DiagnosticEngine, DiagnosticNote},
    source_loc::{SourceLoc, SourceRange},
    source_manager::Source,
    Result, Tok,
};

pub struct Error<'a> {
    pub source_loc: SourceLoc<'a>,
    pub diagnostic: Diagnostic<'a>,
    pub context_notes: Vec<(SourceRange<'a>, DiagnosticContextNote<'a>)>,
    pub note: Option<DiagnosticNote<'a>>,
}

impl<'a> Error<'a> {
    pub fn new(
        source_range: SourceRange<'a>,
        diagnostic: Diagnostic<'a>,
        context_note: DiagnosticContextNote<'a>,
        at_end: bool,
    ) -> Self {
        let source_loc = if at_end && !source_range.is_empty() {
            source_range.source.get_loc(source_range.end - 1)
        } else {
            source_range.start_loc()
        };

        Self {
            source_loc,
            diagnostic,
            context_notes: vec![(source_range, context_note)],
            note: None,
        }
    }

    pub fn with_context_note<'b>(
        self,
        source_range: SourceRange<'b>,
        context_note: DiagnosticContextNote<'b>,
    ) -> Error<'b>
    where
        'a: 'b,
    {
        let mut context_notes = self.context_notes;
        context_notes.push((source_range, context_note));

        Error { context_notes, ..self }
    }

    pub fn with_note<'b>(self, note: DiagnosticNote<'b>) -> Error<'b>
    where
        'a: 'b,
    {
        Error {
            note: Some(note),
            ..self
        }
    }

    pub fn diagnose<'b, C: DiagnosticConsumer>(self, diagnostics: &DiagnosticEngine<'b, C>) -> Result<()>
    where
        'a: 'b,
    {
        let mut report = diagnostics.report(self.source_loc, self.diagnostic);

        for (source_range, context_note) in self.context_notes {
            report = report.with_context_note(source_range, context_note);
        }

        if let Some(note) = self.note {
            report = report.with_note(note);
        }

        report.diagnose()
    }
}

pub type LexerResult<'a> = Result<Token<'a>, Error<'a>>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct InterpolationInfo {
    start: usize,
    literal_start: usize,
}

#[derive(Debug)]
pub struct Lexer<'a> {
    source: Source<'a>,
    chars: PeekableChars<'a>,
    start: usize,
    current: usize,
    leading_whitespace_start: usize,
    interpolation_info: Option<InterpolationInfo>,
    brace_depth: isize,
}

#[allow(clippy::result_large_err)]
impl<'a> Lexer<'a> {
    pub fn new(source: Source<'a>) -> Self {
        let chars = source.get_contents().into();
        Self {
            source,
            chars,
            start: 0,
            current: 0,
            leading_whitespace_start: 0,
            interpolation_info: None,
            brace_depth: 0,
        }
    }

    pub fn with_interpolation<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        let start = self.start;
        let leading_whitespace_start = self.leading_whitespace_start;
        let interpolation_info = self.interpolation_info;
        let brace_depth = self.brace_depth;

        self.start = self.current;
        self.leading_whitespace_start = self.current;
        self.interpolation_info = Some(InterpolationInfo {
            start: self.current - 2,
            literal_start: self.start,
        });
        self.brace_depth = 0;

        let res = f(self);

        self.start = start;
        self.leading_whitespace_start = leading_whitespace_start;
        self.interpolation_info = interpolation_info;
        self.brace_depth = brace_depth;

        res
    }

    fn next_token(&mut self) -> Option<LexerResult<'a>> {
        self.start = self.current;
        self.leading_whitespace_start = self.current;

        if self.interpolation_info.is_some() && self.brace_depth == 0 && self.peek() == Some('}') {
            return None;
        }

        self.advance().and_then(|mut c| {
            loop {
                if c.is_insignificant_whitespace() {
                    while self.match_char(CharExt::is_insignificant_whitespace) {}
                } else if c == '/' {
                    if self.match_char_eq('/') {
                        while self.match_char_neq('\n') {}
                    } else if self.match_char_eq('*') {
                        if let Some(error) = self.skip_block_comment() {
                            return Some(Err(error));
                        }
                    } else {
                        break;
                    }
                } else {
                    break;
                }

                self.start = self.current;

                c = self.advance()?;
            }

            Some(self.next_token_impl(c))
        })
    }

    fn next_token_impl(&mut self, c: char) -> LexerResult<'a> {
        let token_kind = match c {
            '\n' => {
                if let Some(info) = self.interpolation_info {
                    return Err(Error::new(
                        self.source.get_range(info.start, self.current),
                        Diagnostic::expected_interpolation_end(),
                        DiagnosticContextNote::interpolation_location(),
                        true,
                    )
                    .with_context_note(
                        self.source.get_range(info.literal_start, self.current),
                        DiagnosticContextNote::literal_location(),
                    )
                    .with_note(DiagnosticNote::newline_in_interpolation()));
                } else {
                    Tok![Newline]
                }
            }
            '`' => Tok![Backtick],
            '(' => Tok![LeftParen],
            ')' => Tok![RightParen],
            '[' => Tok![LeftBracket],
            ']' => Tok![RightBracket],
            '{' => {
                self.brace_depth += 1;

                Tok![LeftBrace]
            }
            '}' => {
                self.brace_depth -= 1;

                Tok![RightBrace]
            }
            ',' => Tok![,],
            ':' => Tok![:],
            ';' => Tok![;],
            '@' => Tok![@],
            '?' => Tok![?],
            '.' => {
                if self.match_char(CharExt::is_dot_operator) {
                    self.try_consume_operator(true)?
                } else {
                    Tok![.]
                }
            }
            '=' => {
                if self.match_char_eq('>') {
                    if self.peek().is_some_and(CharExt::is_operator) {
                        self.try_consume_operator(false)?
                    } else {
                        Tok![=>]
                    }
                } else if self.peek().is_some_and(CharExt::is_operator) {
                    self.try_consume_operator(false)?
                } else {
                    Tok![=]
                }
            }
            '-' => {
                if self.match_char_eq('>') {
                    if self.peek().is_some_and(CharExt::is_operator) {
                        self.try_consume_operator(false)?
                    } else {
                        Tok![->]
                    }
                } else {
                    self.try_consume_operator(false)?
                }
            }
            '&' => {
                if self.peek() == Some('w') && self.peek2().is_none_or(|c| !c.is_identifier_char()) {
                    Tok![&w]
                } else if self.peek().is_some_and(CharExt::is_operator) {
                    self.try_consume_operator(false)?
                } else {
                    Tok![&]
                }
            }
            '#' => {
                if self.chars.peek_first_after_eq('#') == Some('"') {
                    self.try_consume_string_literal(c)?
                } else {
                    Tok![#]
                }
            }
            '*' => {
                if self.match_char_eq('/') {
                    return Err(self.error(
                        Diagnostic::unexpected_comment_terminator(),
                        DiagnosticContextNote::comment_terminator_location(),
                    ));
                } else {
                    self.try_consume_operator(false)?
                }
            }
            '\'' => self.try_consume_char_literal()?,
            '"' => self.try_consume_string_literal(c)?,
            _ if c.is_identifier_start() => self.consume_identifier(),
            _ if c.is_operator() => self.try_consume_operator(false)?,
            _ if c.is_ascii_digit() => self.try_consume_number_literal(c)?,
            _ => {
                return Err(self.error(
                    Diagnostic::invalid_character(c),
                    DiagnosticContextNote::invalid_character_location(),
                ))
            }
        };

        self.make_token(token_kind)
    }

    fn consume_identifier(&mut self) -> TokenKind<'a> {
        while self.match_char(CharExt::is_identifier_char) {}

        self.get_current_range()
            .get_str()
            .parse()
            .map_or(Tok![Ident], TokenKind::Keyword)
    }

    fn try_consume_operator(&mut self, allow_dot: bool) -> Result<TokenKind<'a>, Error<'a>> {
        let func = if allow_dot {
            char::is_dot_operator
        } else {
            char::is_operator
        };

        loop {
            match self.peek() {
                Some('/') => {
                    if matches!(self.peek2(), Some('/') | Some('*')) {
                        break;
                    }
                }
                Some('*') => {
                    if self.peek2() == Some('/') {
                        return Err(Error::new(
                            self.source.get_range(self.current, self.current + 2),
                            Diagnostic::unexpected_comment_terminator(),
                            DiagnosticContextNote::comment_terminator_location(),
                            false,
                        )
                        .with_note(DiagnosticNote::comment_terminator_in_operator()));
                    }
                }
                _ => {}
            }

            if !self.match_char(func) {
                break;
            }
        }

        Ok(Tok![Op])
    }

    fn try_consume_number_literal(&mut self, start: char) -> Result<TokenKind<'a>, Error<'a>> {
        LiteralKind::parse_number(self, start).map(TokenKind::Literal)
    }

    fn try_consume_char_literal(&mut self) -> Result<TokenKind<'a>, Error<'a>> {
        LiteralKind::parse_char(self).map(TokenKind::Literal)
    }

    fn try_consume_string_literal(&mut self, start: char) -> Result<TokenKind<'a>, Error<'a>> {
        LiteralKind::parse_string(self, start).map(TokenKind::Literal)
    }

    #[must_use]
    fn skip_block_comment(&mut self) -> Option<Error<'a>> {
        let mut depth = 1;
        while depth > 0 {
            let Some(c) = self.advance() else {
                return Some(
                    self.error(
                        Diagnostic::unterminated_comment(),
                        DiagnosticContextNote::comment_location(),
                    )
                    .with_note(DiagnosticNote::missing_block_comment_end()),
                );
            };

            if c == '/' && self.match_char_eq('*') {
                depth += 1;
            } else if c == '*' && self.match_char_eq('/') {
                depth -= 1;
            }
        }

        None
    }

    fn make_token(&mut self, kind: TokenKind<'a>) -> LexerResult<'a> {
        let has_trailing_whitespace = self.peek().is_some_and(|c| {
            CharExt::is_whitespace_or_newline(c) || (c == '/' && matches!(self.peek2(), Some('/') | Some('*')))
        });

        Ok(Token::new(
            kind,
            self.get_current_range(),
            self.get_leading_whitespace_range(),
            has_trailing_whitespace,
        ))
    }

    fn error(&self, diagnostic: Diagnostic<'a>, context_note: DiagnosticContextNote<'a>) -> Error<'a> {
        Error::new(self.get_current_range(), diagnostic, context_note, false)
    }

    fn error_at_end(&self, diagnostic: Diagnostic<'a>, context_note: DiagnosticContextNote<'a>) -> Error<'a> {
        Error::new(self.get_current_range(), diagnostic, context_note, true)
    }

    fn error_at_next_character(
        &self,
        diagnostic: Diagnostic<'a>,
        context_note: DiagnosticContextNote<'a>,
    ) -> Error<'a> {
        Error::new(self.get_next_character_range(), diagnostic, context_note, false)
    }

    fn get_current_range(&self) -> SourceRange<'a> {
        self.source.get_range(self.start, self.current)
    }

    fn get_next_character_range(&self) -> SourceRange<'a> {
        self.source.get_range(self.current, self.current + 1)
    }

    fn get_leading_whitespace_range(&self) -> SourceRange<'a> {
        self.source.get_range(self.leading_whitespace_start, self.start)
    }

    fn expect_char_eq(&mut self, expected: char, error: impl FnOnce(&Self) -> Error<'a>) -> Result<char, Error<'a>> {
        self.expect_char(|c| c == expected, error)
    }

    fn expect_char(
        &mut self,
        func: impl FnOnce(char) -> bool,
        error: impl FnOnce(&Self) -> Error<'a>,
    ) -> Result<char, Error<'a>> {
        if let Some(c) = self.advance() {
            if func(c) {
                Ok(c)
            } else {
                Err(error(self))
            }
        } else {
            Err(error(self))
        }
    }

    fn match_str(&mut self, expected: &str) -> bool {
        if self.chars.as_str().starts_with(expected) {
            self.advance_by(expected.len()).unwrap();
            true
        } else {
            false
        }
    }

    fn match_char_eq(&mut self, expected: char) -> bool {
        self.match_char(|c| c == expected)
    }

    fn match_char_neq(&mut self, expected: char) -> bool {
        self.match_char(|c| c != expected)
    }

    fn match_char(&mut self, func: impl FnOnce(char) -> bool) -> bool {
        self.advance_if(func).is_some()
    }

    fn advance_by(&mut self, n: usize) -> Result<(), NonZero<usize>> {
        self.chars
            .advance_by(n)
            .inspect(|_| self.current += n)
            .inspect_err(|k| self.current += n - k.get())
    }

    fn advance_if(&mut self, func: impl FnOnce(char) -> bool) -> Option<char> {
        self.chars.next_if(func).inspect(|_| self.current += 1)
    }

    fn advance(&mut self) -> Option<char> {
        self.chars.next().inspect(|_| self.current += 1)
    }

    fn peek(&self) -> Option<char> {
        self.chars.peek()
    }

    fn peek2(&self) -> Option<char> {
        self.chars.peek2()
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = LexerResult<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}
