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

#[derive(Debug, Clone)]
pub struct Error<'a> {
    source_loc: SourceLoc<'a>,
    diagnostic: Diagnostic<'a>,
    context_notes: Vec<(SourceRange<'a>, DiagnosticContextNote<'a>)>,
    note: Option<DiagnosticNote<'a>>,
}

impl<'a> Error<'a> {
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

#[derive(Debug, Clone)]
struct PendingError<'a> {
    source_loc: SourceLoc<'a>,
    diagnostic: Diagnostic<'a>,
    initial_context_note: DiagnosticContextNote<'a>,
    context_notes: Vec<(SourceRange<'a>, DiagnosticContextNote<'a>)>,
    note: Option<DiagnosticNote<'a>>,
}

#[must_use = "Errors must be recorded to be diagnosed"]
struct ErrorBuilder<'a, 'b>
where
    'a: 'b,
{
    source_loc: Result<(SourceRange<'a>, bool), SourceLoc<'a>>,
    diagnostic: Diagnostic<'a>,
    initial_context_note: DiagnosticContextNote<'a>,
    context_notes: Vec<(SourceRange<'a>, DiagnosticContextNote<'a>)>,
    note: Option<DiagnosticNote<'a>>,
    lexer: &'b mut Lexer<'a>,
}

impl<'a, 'b> ErrorBuilder<'a, 'b> {
    fn new_with_range(
        source_range: SourceRange<'a>,
        at_end: bool,
        diagnostic: Diagnostic<'a>,
        context_note: DiagnosticContextNote<'a>,
        lexer: &'b mut Lexer<'a>,
    ) -> Self {
        Self {
            source_loc: Ok((source_range, at_end)),
            diagnostic,
            initial_context_note: context_note,
            context_notes: Vec::new(),
            note: None,
            lexer,
        }
    }

    fn new_with_loc(
        source_loc: SourceLoc<'a>,
        diagnostic: Diagnostic<'a>,
        context_note: DiagnosticContextNote<'a>,
        lexer: &'b mut Lexer<'a>,
    ) -> Self {
        Self {
            source_loc: Err(source_loc),
            diagnostic,
            initial_context_note: context_note,
            context_notes: Vec::new(),
            note: None,
            lexer,
        }
    }

    fn with_context_note(mut self, source_range: SourceRange<'a>, context_note: DiagnosticContextNote<'a>) -> Self {
        self.context_notes.push((source_range, context_note));
        self
    }

    fn with_note(mut self, note: DiagnosticNote<'a>) -> Self {
        self.note = Some(note);
        self
    }

    fn record(self) {
        let Self {
            source_loc,
            diagnostic,
            initial_context_note,
            context_notes,
            note,
            lexer,
        } = self;

        match source_loc {
            Ok((source_range, at_end)) => {
                let source_loc = if at_end {
                    if let Some(c) = source_range.get_str().chars().last() {
                        source_range.source.get_loc(source_range.end - c.len_utf8())
                    } else {
                        source_range.start_loc()
                    }
                } else {
                    source_range.start_loc()
                };

                let mut context_notes = context_notes;
                context_notes.push((source_range, initial_context_note));

                let error = Error {
                    source_loc,
                    diagnostic,
                    context_notes,
                    note,
                };

                lexer.errors.push(error);
            }
            Err(source_loc) => lexer.pending_errors.push(PendingError {
                source_loc,
                diagnostic,
                initial_context_note,
                context_notes,
                note,
            }),
        }
    }
}

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
    last_considered_leading_whitespace: bool,
    in_interpolation: bool,
    brace_depth: isize,
    errors: Vec<Error<'a>>,
    pending_errors: Vec<PendingError<'a>>,
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
            last_considered_leading_whitespace: true,
            in_interpolation: false,
            brace_depth: 0,
            errors: Vec::new(),
            pending_errors: Vec::new(),
        }
    }

    pub fn with_interpolation<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        let interpolation_start = self.current - 2;

        let outer_start = self.start;
        let outer_leading_whitespace_start = self.leading_whitespace_start;
        let outer_last_considered_leading_whitespace = self.last_considered_leading_whitespace;
        let outer_in_interpolation = self.in_interpolation;
        let outer_brace_depth = self.brace_depth;
        let outer_errors = std::mem::take(&mut self.errors);
        let mut outer_pending_errors = std::mem::take(&mut self.pending_errors);

        self.start = self.current;
        self.leading_whitespace_start = self.current;
        self.last_considered_leading_whitespace = true;
        self.in_interpolation = true;
        self.brace_depth = 0;

        let res = f(self);

        assert!(self.pending_errors.is_empty());

        self.start = outer_start;
        self.leading_whitespace_start = outer_leading_whitespace_start;
        self.last_considered_leading_whitespace = outer_last_considered_leading_whitespace;
        self.in_interpolation = outer_in_interpolation;
        self.brace_depth = outer_brace_depth;

        self.expect_char_eq('}', |l| {
            l.errors.push(Error {
                source_loc: l.get_current_loc(),
                diagnostic: Diagnostic::expected_interpolation_end(),
                context_notes: Vec::new(),
                note: None,
            });
        });

        for error in std::mem::replace(&mut self.errors, outer_errors) {
            let Error {
                source_loc,
                diagnostic,
                mut context_notes,
                note,
            } = error;

            context_notes.push((
                self.source.get_range(interpolation_start, self.current),
                DiagnosticContextNote::interpolation_location(),
            ));

            outer_pending_errors.push(PendingError {
                source_loc,
                diagnostic,
                initial_context_note: DiagnosticContextNote::containing_literal_location(),
                context_notes,
                note,
            });
        }

        self.pending_errors = outer_pending_errors;

        res
    }

    pub fn diagnose_errors<'b, C: DiagnosticConsumer>(&mut self, diagnostics: &DiagnosticEngine<'b, C>) -> Result<()>
    where
        'a: 'b,
    {
        for error in self.errors.drain(..) {
            error.diagnose(diagnostics)?;
        }

        Ok(())
    }

    fn next_token(&mut self) -> Option<Token<'a>> {
        self.start = self.current;
        self.leading_whitespace_start = self.current;

        if self.in_interpolation && self.brace_depth == 0 && self.peek() == Some('}') {
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
                        self.skip_block_comment()
                    } else {
                        break;
                    }
                } else {
                    break;
                }

                self.start = self.current;

                if self.in_interpolation && self.brace_depth == 0 && self.peek() == Some('}') {
                    return None;
                }

                c = self.advance()?;
            }

            Some(self.next_token_impl(c))
        })
    }

    fn next_token_impl(&mut self, c: char) -> Token<'a> {
        let token_kind = match c {
            '\n' => {
                if self.in_interpolation {
                    self.errors.push(Error {
                        source_loc: self.get_current_loc(),
                        diagnostic: Diagnostic::newline_in_interpolation(),
                        context_notes: Vec::new(),
                        note: Some(DiagnosticNote::newline_in_interpolation()),
                    });
                }

                Tok![Newline]
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
                    self.consume_operator(true)
                } else {
                    Tok![.]
                }
            }
            '=' => {
                if self.match_char_eq('>') {
                    if self.peek().is_some_and(CharExt::is_operator) {
                        self.consume_operator(false)
                    } else {
                        Tok![=>]
                    }
                } else if self.peek().is_some_and(CharExt::is_operator) {
                    self.consume_operator(false)
                } else {
                    Tok![=]
                }
            }
            '-' => {
                if self.match_char_eq('>') {
                    if self.peek().is_some_and(CharExt::is_operator) {
                        self.consume_operator(false)
                    } else {
                        Tok![->]
                    }
                } else {
                    self.consume_operator(false)
                }
            }
            '&' => {
                if self.peek() == Some('w') && self.peek2().is_none_or(|c| !c.is_identifier_char()) {
                    Tok![&w]
                } else if self.peek().is_some_and(CharExt::is_operator) {
                    self.consume_operator(false)
                } else {
                    Tok![&]
                }
            }
            '#' => {
                if self.chars.peek_first_after_eq('#') == Some('"') {
                    self.consume_string_literal(c)
                } else {
                    Tok![#]
                }
            }
            '*' => {
                if self.match_char_eq('/') {
                    self.error(
                        Diagnostic::unexpected_comment_terminator(),
                        DiagnosticContextNote::comment_terminator_location(),
                    )
                    .record()
                }

                self.consume_operator(false)
            }
            '\'' => self.consume_char_literal(),
            '"' => self.consume_string_literal(c),
            _ if c.is_identifier_start() => self.consume_identifier(),
            _ if c.is_operator() => self.consume_operator(false),
            _ if c.is_ascii_digit() => self.consume_number_literal(c),
            _ => {
                self.error(
                    Diagnostic::invalid_character(c),
                    DiagnosticContextNote::invalid_character_location(),
                )
                .record();

                Tok![Unknown]
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

    fn consume_operator(&mut self, allow_dot: bool) -> TokenKind<'a> {
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
                        self.error_with_range(
                            self.source.get_range(self.current, self.current + 2),
                            false,
                            Diagnostic::unexpected_comment_terminator(),
                            DiagnosticContextNote::comment_terminator_location(),
                        )
                        .with_note(DiagnosticNote::comment_terminator_in_operator())
                        .record()
                    }
                }
                _ => {}
            }

            if !self.match_char(func) {
                break;
            }
        }

        Tok![Op]
    }

    fn consume_number_literal(&mut self, start: char) -> TokenKind<'a> {
        TokenKind::Literal(LiteralKind::lex_number(self, start))
    }

    fn consume_char_literal(&mut self) -> TokenKind<'a> {
        TokenKind::Literal(LiteralKind::lex_char(self))
    }

    fn consume_string_literal(&mut self, start: char) -> TokenKind<'a> {
        TokenKind::Literal(if start == '#' {
            LiteralKind::lex_raw_string(self)
        } else {
            LiteralKind::lex_string(self)
        })
    }

    fn skip_block_comment(&mut self) {
        let mut depth = 1;
        while depth > 0 {
            let Some(c) = self.advance() else {
                return self
                    .error(
                        Diagnostic::unterminated_comment(),
                        DiagnosticContextNote::comment_location(),
                    )
                    .with_note(DiagnosticNote::missing_block_comment_end())
                    .record();
            };

            if c == '\n' && self.in_interpolation {
                self.errors.push(Error {
                    source_loc: self.get_current_loc(),
                    diagnostic: Diagnostic::newline_in_interpolation(),
                    context_notes: Vec::new(),
                    note: Some(DiagnosticNote::newline_in_interpolation()),
                });
            }

            if c == '/' && self.match_char_eq('*') {
                depth += 1;
            } else if c == '*' && self.match_char_eq('/') {
                depth -= 1;
            }
        }
    }

    fn make_token(&mut self, kind: TokenKind<'a>) -> Token<'a> {
        let current_range = self.get_current_range();

        for pending in self.pending_errors.drain(..) {
            let mut context_notes = pending.context_notes;
            context_notes.push((current_range, pending.initial_context_note));

            let error = Error {
                source_loc: pending.source_loc,
                diagnostic: pending.diagnostic,
                context_notes,
                note: pending.note,
            };

            self.errors.push(error);
        }

        let has_leading_whitespace =
            self.last_considered_leading_whitespace || !self.get_leading_whitespace_range().is_empty();

        let has_trailing_whitespace = self
            .peek()
            .is_none_or(|c| c.is_trailing_whitespace() || (c == '/' && matches!(self.peek2(), Some('/') | Some('*'))));

        if let Some(c) = current_range.get_str().chars().last() {
            self.last_considered_leading_whitespace = c.is_leading_whitespace();
        }

        Token::new(
            kind,
            current_range,
            self.get_leading_whitespace_range(),
            has_leading_whitespace,
            has_trailing_whitespace,
        )
    }

    fn error<'b>(
        &'b mut self,
        diagnostic: Diagnostic<'a>,
        context_note: DiagnosticContextNote<'a>,
    ) -> ErrorBuilder<'a, 'b> {
        self.error_with_range(self.get_current_range(), false, diagnostic, context_note)
    }

    fn error_at_end<'b>(
        &'b mut self,
        diagnostic: Diagnostic<'a>,
        context_note: DiagnosticContextNote<'a>,
    ) -> ErrorBuilder<'a, 'b> {
        self.error_with_range(self.get_current_range(), true, diagnostic, context_note)
    }

    fn error_with_range<'b>(
        &'b mut self,
        source_range: SourceRange<'a>,
        at_end: bool,
        diagnostic: Diagnostic<'a>,
        context_note: DiagnosticContextNote<'a>,
    ) -> ErrorBuilder<'a, 'b> {
        ErrorBuilder::new_with_range(source_range, at_end, diagnostic, context_note, self)
    }

    fn error_at_token<'b>(
        &'b mut self,
        source_loc: SourceLoc<'a>,
        diagnostic: Diagnostic<'a>,
        context_note: DiagnosticContextNote<'a>,
    ) -> ErrorBuilder<'a, 'b> {
        ErrorBuilder::new_with_loc(source_loc, diagnostic, context_note, self)
    }

    fn get_current_range(&self) -> SourceRange<'a> {
        self.source.get_range(self.start, self.current)
    }

    fn get_current_character_range(&self, c: char) -> SourceRange<'a> {
        self.source.get_range(self.current - c.len_utf8(), self.current)
    }

    fn get_next_character_range(&self, c: char) -> SourceRange<'a> {
        self.source.get_range(self.current, self.current + c.len_utf8())
    }

    fn get_leading_whitespace_range(&self) -> SourceRange<'a> {
        self.source.get_range(self.leading_whitespace_start, self.start)
    }

    fn get_current_loc(&self) -> SourceLoc<'a> {
        self.source.get_loc(self.current)
    }

    fn expect_char_eq(&mut self, expected: char, error: impl FnOnce(&mut Self)) -> Option<char> {
        self.expect_char(|c| c == expected, error)
    }

    fn expect_char(&mut self, func: impl FnOnce(char) -> bool, error: impl FnOnce(&mut Self)) -> Option<char> {
        if let Some(c) = self.advance_if(func) {
            Some(c)
        } else {
            error(self);
            None
        }
    }

    fn match_char_map<T>(&mut self, func: impl FnOnce(char) -> Option<T>) -> Option<T> {
        let next = self.peek()?;

        func(next).inspect(|_| {
            self.advance();
        })
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
        self.chars.next_if(func).inspect(|c| self.current += c.len_utf8())
    }

    fn advance(&mut self) -> Option<char> {
        self.chars.next().inspect(|c| self.current += c.len_utf8())
    }

    fn peek(&self) -> Option<char> {
        self.chars.peek()
    }

    fn peek2(&self) -> Option<char> {
        self.chars.peek2()
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

impl<'a> Drop for Lexer<'a> {
    fn drop(&mut self) {
        assert!(self.errors.is_empty(), "Lexer dropped with errors");
        assert!(self.pending_errors.is_empty());
    }
}
