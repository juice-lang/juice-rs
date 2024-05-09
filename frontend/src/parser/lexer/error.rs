use super::Lexer;
use crate::{
    diag::{Diagnostic, DiagnosticContextNote, DiagnosticNote},
    parser::Error,
    source_loc::{SourceLoc, SourceRange},
    source_manager::SourceManager,
    Result,
};

#[derive(Debug, Clone)]
pub struct PendingError<'src, M: 'src + SourceManager> {
    pub source_loc: SourceLoc<'src, M>,
    pub diagnostic: Diagnostic<'src>,
    pub initial_context_note: DiagnosticContextNote<'src>,
    pub context_notes: Vec<(SourceRange<'src, M>, DiagnosticContextNote<'src>)>,
    pub note: Option<DiagnosticNote<'src>>,
}

#[must_use = "Errors must be recorded to be diagnosed"]
pub struct ErrorBuilder<'src, 'lex, M: 'src + SourceManager>
where
    'src: 'lex,
{
    source_loc: Result<(SourceRange<'src, M>, bool), SourceLoc<'src, M>>,
    diagnostic: Diagnostic<'src>,
    initial_context_note: DiagnosticContextNote<'src>,
    context_notes: Vec<(SourceRange<'src, M>, DiagnosticContextNote<'src>)>,
    note: Option<DiagnosticNote<'src>>,
    lexer: &'lex mut Lexer<'src, M>,
}

impl<'src, 'lex, M: 'src + SourceManager> ErrorBuilder<'src, 'lex, M> {
    pub fn new_with_range(
        source_range: SourceRange<'src, M>,
        at_end: bool,
        diagnostic: Diagnostic<'src>,
        context_note: DiagnosticContextNote<'src>,
        lexer: &'lex mut Lexer<'src, M>,
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

    pub fn new_with_loc(
        source_loc: SourceLoc<'src, M>,
        diagnostic: Diagnostic<'src>,
        context_note: DiagnosticContextNote<'src>,
        lexer: &'lex mut Lexer<'src, M>,
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

    pub fn with_context_note(
        mut self,
        source_range: SourceRange<'src, M>,
        context_note: DiagnosticContextNote<'src>,
    ) -> Self {
        self.context_notes.push((source_range, context_note));
        self
    }

    pub fn with_note(mut self, note: DiagnosticNote<'src>) -> Self {
        self.note = Some(note);
        self
    }

    pub fn record(self) {
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

                let error = Error::new(source_loc, diagnostic, context_notes, note);

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
