mod fsm;
mod literal;
mod token;
mod token_kind;

use std::{mem::MaybeUninit, num::NonZero};

use juice_core::{CharExt, OptionExt as _, PeekableChars};

use self::literal::LiteralKind;
pub use self::{token::Token, token_kind::TokenKind};
use crate::{
    diag::{Diagnostic, DiagnosticConsumer, DiagnosticContextNote, DiagnosticEngine, DiagnosticNote},
    source_loc::SourceRange,
    source_manager::Source,
    Result, Tok,
};

pub struct Error<'a> {
    pub source_range: SourceRange<'a>,
    pub diagnostic: Diagnostic<'a>,
    pub context_note: DiagnosticContextNote<'a>,
    pub note: Option<DiagnosticNote<'a>>,
    pub at_end: bool,
}

impl<'a> Error<'a> {
    pub fn new(
        source_range: SourceRange<'a>,
        diagnostic: Diagnostic<'a>,
        context_note: DiagnosticContextNote<'a>,
        at_end: bool,
    ) -> Self {
        Self {
            source_range,
            diagnostic,
            context_note,
            note: None,
            at_end,
        }
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
        let source_loc = if self.at_end {
            self.source_range.end_loc()
        } else {
            self.source_range.start_loc()
        };

        let mut report = diagnostics
            .report(source_loc, self.diagnostic)
            .with_context_note(self.source_range, self.context_note);

        if let Some(note) = self.note {
            report = report.with_note(note);
        }

        report.diagnose()
    }
}

pub type LexerResult<'a> = Result<Token<'a>, Error<'a>>;

#[derive(Debug)]
pub struct Lexer<'a> {
    source: Source<'a>,
    chars: PeekableChars<'a>,
    start: usize,
    current: usize,
    leading_whitespace_start: usize,
    is_nested: bool,
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
            is_nested: false,
            brace_depth: 0,
        }
    }

    pub fn with_nested<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        let mut res = MaybeUninit::uninit();
        take_mut::take(self, |mut lexer| {
            let mut nested_lexer = Lexer {
                source: lexer.source,
                chars: lexer.chars,
                start: lexer.current,
                current: lexer.current,
                leading_whitespace_start: lexer.current,
                is_nested: true,
                brace_depth: 0,
            };

            res.write(f(&mut nested_lexer));

            let Lexer {
                source, chars, current, ..
            } = nested_lexer;

            lexer.source = source;
            lexer.chars = chars;
            lexer.current = current;

            lexer
        });

        unsafe { res.assume_init() }
    }

    fn next_token(&mut self) -> Option<LexerResult<'a>> {
        self.start = self.current;
        self.leading_whitespace_start = self.current;

        if self.is_nested && self.brace_depth == 0 && self.peek() == Some('}') {
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
            '\n' => Tok![Newline],
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
            .get_text()
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

    fn error(&mut self, diagnostic: Diagnostic<'a>, context_note: DiagnosticContextNote<'a>) -> Error<'a> {
        Error::new(self.get_current_range(), diagnostic, context_note, false)
    }

    fn error_at_end(&mut self, diagnostic: Diagnostic<'a>, context_note: DiagnosticContextNote<'a>) -> Error<'a> {
        Error::new(self.get_current_range(), diagnostic, context_note, true)
    }

    fn error_at_next_character(
        &mut self,
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
