use chumsky::{error::Error as ChumskyError, util::MaybeRef};
use derive_where::derive_where;

use super::lexer::TokenKind;
use crate::{
    diag::{Diagnostic, DiagnosticConsumer, DiagnosticContextNote, DiagnosticEngine, DiagnosticNote},
    source_loc::{SourceLoc, SourceRange},
    source_manager::SourceManager,
};

#[derive_where(Debug, Clone)]
pub struct Error<'src, M: 'src + SourceManager> {
    pub source_loc: SourceLoc<'src, M>,
    pub diagnostic: Diagnostic<'src>,
    pub context_notes: Vec<(SourceRange<'src, M>, DiagnosticContextNote<'src>)>,
    pub note: Option<DiagnosticNote<'src>>,
}

impl<'src, M: 'src + SourceManager> Error<'src, M> {
    pub fn new(
        source_loc: SourceLoc<'src, M>,
        diagnostic: Diagnostic<'src>,
        context_notes: Vec<(SourceRange<'src, M>, DiagnosticContextNote<'src>)>,
        note: Option<DiagnosticNote<'src>>,
    ) -> Self {
        Self {
            source_loc,
            diagnostic,
            context_notes,
            note,
        }
    }

    pub fn diagnose<C: DiagnosticConsumer<'src, M>>(
        self,
        diagnostics: &DiagnosticEngine<'src, M, C>,
    ) -> Result<(), C::Error> {
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

#[derive_where(Debug, Clone)]
pub enum ParserErrorKind<'src, M: 'src + SourceManager> {
    ExpectedFound {
        source_range: SourceRange<'src, M>,
        expected: Vec<Option<TokenKind<'src, M>>>,
        found: Option<TokenKind<'src, M>>,
    },
    Other(Error<'src, M>),
}

pub struct ParserError<'src, M: 'src + SourceManager> {
    kind: ParserErrorKind<'src, M>,
    context_notes: Vec<(SourceRange<'src, M>, DiagnosticContextNote<'src>)>,
}

impl<'src, M: 'src + SourceManager> ParserError<'src, M> {
    pub fn new(kind: ParserErrorKind<'src, M>) -> Self {
        Self {
            kind,
            context_notes: Vec::new(),
        }
    }

    pub fn with_context_note(
        mut self,
        source_range: SourceRange<'src, M>,
        context_note: DiagnosticContextNote<'src>,
    ) -> Self {
        self.context_notes.push((source_range, context_note));
        self
    }

    pub fn diagnose<C: DiagnosticConsumer<'src, M>>(
        mut self,
        diagnostics: &DiagnosticEngine<'src, M, C>,
    ) -> Result<(), C::Error> {
        let mut error = match self.kind {
            ParserErrorKind::ExpectedFound {
                source_range,
                expected,
                found,
            } => Error::new(
                source_range.start_loc(),
                Diagnostic::unexpected_parser_error(),
                vec![(source_range, DiagnosticContextNote::unexpected_parser_error_location())],
                Some(DiagnosticNote::unexpected_parser_error(expected, found)),
            ),
            ParserErrorKind::Other(error) => error,
        };

        error.context_notes.append(&mut self.context_notes);

        error.diagnose(diagnostics)
    }
}

impl<'src, M: 'src + SourceManager> From<Error<'src, M>> for ParserError<'src, M> {
    fn from(err: Error<'src, M>) -> Self {
        Self::new(ParserErrorKind::Other(err))
    }
}

impl<'src, 'lex, M: 'src + SourceManager> ChumskyError<'lex, super::ParserInput<'src, 'lex, M>> for ParserError<'src, M>
where
    'src: 'lex,
{
    fn expected_found<Iter: IntoIterator<Item = Option<MaybeRef<'lex, TokenKind<'src, M>>>>>(
        expected: Iter,
        found: Option<MaybeRef<'lex, TokenKind<'src, M>>>,
        span: SourceRange<'src, M>,
    ) -> Self {
        Self::new(ParserErrorKind::ExpectedFound {
            source_range: span,
            expected: expected.into_iter().map(|e| e.as_deref().cloned()).collect(),
            found: found.as_deref().cloned(),
        })
    }

    fn merge(mut self, mut other: Self) -> Self {
        if let (
            ParserErrorKind::ExpectedFound { expected, .. },
            ParserErrorKind::ExpectedFound {
                expected: other_expected,
                ..
            },
        ) = (&mut self.kind, &mut other.kind)
        {
            expected.append(other_expected);
        }

        self
    }
}
