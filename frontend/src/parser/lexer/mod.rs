mod token;
mod token_kind;

use std::num::NonZero;

use juice_core::{CharExt, OptionExt as _, PeekableChars};

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
}

impl<'a> Error<'a> {
    pub fn new(
        source_range: SourceRange<'a>,
        diagnostic: Diagnostic<'a>,
        context_note: DiagnosticContextNote<'a>,
    ) -> Self {
        Self {
            source_range,
            diagnostic,
            context_note,
            note: None,
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
        let mut report = diagnostics
            .report(self.source_range.start_loc(), self.diagnostic)
            .with_context_note(self.source_range, self.context_note);

        if let Some(note) = self.note {
            report = report.with_note(note);
        }

        report.diagnose()
    }
}

pub type LexerResult<'a> = Result<Token<'a>, Error<'a>>;

pub struct Lexer<'a> {
    source: Source<'a>,
    chars: PeekableChars<'a>,
    start: usize,
    current: usize,
    leading_whitespace_start: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: Source<'a>) -> Self {
        let chars = source.get_contents().into();
        Self {
            source,
            chars,
            start: 0,
            current: 0,
            leading_whitespace_start: 0,
        }
    }

    fn next_token(&mut self) -> Option<LexerResult<'a>> {
        self.start = self.current;
        self.leading_whitespace_start = self.current;

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
            '{' => Tok![LeftBrace],
            '}' => Tok![RightBrace],
            ',' => Tok![,],
            ':' => Tok![:],
            ';' => Tok![;],
            '#' => Tok![#],
            '@' => Tok![@],
            '?' => Tok![?],
            '.' => {
                if self.match_char(CharExt::is_dot_operator) {
                    while self.match_char(CharExt::is_dot_operator) {}
                    return self.make_token(Tok![Op]);
                }

                Tok![.]
            }
            '=' => {
                if self.match_char_eq('>') {
                    if self.match_char(CharExt::is_operator) {
                        self.consume_operator()
                    } else {
                        Tok![=>]
                    }
                } else if self.match_char(CharExt::is_operator) {
                    self.consume_operator()
                } else {
                    Tok![=]
                }
            }
            '-' => {
                if self.match_char_eq('>') {
                    if self.match_char(CharExt::is_operator) {
                        self.consume_operator()
                    } else {
                        Tok![->]
                    }
                } else {
                    self.consume_operator()
                }
            }
            '&' => {
                if self.peek() == Some('w') && self.peek2().is_none_or(|c| !c.is_identifier_char()) {
                    Tok![&w]
                } else if self.match_char(CharExt::is_operator) {
                    self.consume_operator()
                } else {
                    Tok![&]
                }
            }
            '"' => self.try_consume_string_literal()?,
            '\'' => self.try_consume_char_literal()?,
            _ if c.is_operator() => self.consume_operator(),
            _ if c.is_identifier_start() => self.consume_identifier(),
            _ if c.is_ascii_digit() => self.try_consume_number_literal()?,
            _ => {
                return Err(self.error(
                    Diagnostic::invalid_character(c),
                    DiagnosticContextNote::invalid_character_location(),
                ))
            }
        };

        self.make_token(token_kind)
    }

    fn consume_operator(&mut self) -> TokenKind {
        while self.match_char(CharExt::is_operator) {}

        Tok![Op]
    }

    fn consume_identifier(&mut self) -> TokenKind {
        while self.match_char(CharExt::is_identifier_char) {}

        self.get_current_range()
            .get_text()
            .parse()
            .map_or(Tok![Ident], TokenKind::Keyword)
    }

    fn try_consume_number_literal(&mut self) -> Result<TokenKind, Error<'a>> {
        todo!()
    }

    fn try_consume_string_literal(&mut self) -> Result<TokenKind, Error<'a>> {
        todo!()
    }

    fn try_consume_char_literal(&mut self) -> Result<TokenKind, Error<'a>> {
        todo!()
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

    fn make_token(&mut self, kind: TokenKind) -> LexerResult<'a> {
        let has_trailing_whitespace = self.peek().is_some_and(|c| {
            CharExt::is_whitespace_or_newline(c) || (c == '/' && matches!(self.peek2(), Some('/') | Some('*')))
        });

        let leading_whitespace_range = self.source.get_range(self.leading_whitespace_start, self.start);
        Ok(Token::new(
            kind,
            self.get_current_range(),
            leading_whitespace_range,
            has_trailing_whitespace,
        ))
    }

    fn error(&mut self, diagnostic: Diagnostic<'a>, context_note: DiagnosticContextNote<'a>) -> Error<'a> {
        Error::new(self.get_current_range(), diagnostic, context_note)
    }

    fn get_current_range(&self) -> SourceRange<'a> {
        self.source.get_range(self.start, self.current)
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
        self.chars.next_if_eq(expected).inspect(|_| self.current += 1).is_some()
    }

    fn match_char_neq(&mut self, expected: char) -> bool {
        self.chars
            .next_if(|c| c != expected)
            .inspect(|_| self.current += 1)
            .is_some()
    }

    fn match_char(&mut self, func: impl FnOnce(char) -> bool) -> bool {
        self.chars.next_if(func).inspect(|_| self.current += 1).is_some()
    }

    fn advance_by(&mut self, n: usize) -> Result<(), NonZero<usize>> {
        self.chars
            .advance_by(n)
            .inspect(|_| self.current += n)
            .inspect_err(|k| self.current += n - k.get())
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
