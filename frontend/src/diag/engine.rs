use std::sync::atomic::{AtomicBool, Ordering as AtomicOrdering};

use super::{DefaultDiagnosticConsumer, Diagnostic, DiagnosticConsumer, DiagnosticContextNote, DiagnosticNote};
use crate::{
    source_loc::{SourceLoc, SourceRange},
    source_manager::SourceManager,
    Result,
};

pub struct Engine<'a, C: DiagnosticConsumer> {
    source_manager: &'a SourceManager,
    consumer: C,
    had_error: AtomicBool,
}

impl<'a, C: DiagnosticConsumer> Engine<'a, C> {
    pub fn new_with_consumer(source_manager: &'a SourceManager, consumer: C) -> Self {
        Self {
            source_manager,
            consumer,
            had_error: false.into(),
        }
    }

    pub fn get_source_manager(&self) -> &SourceManager {
        self.source_manager
    }

    pub fn get_consumer(&self) -> &C {
        &self.consumer
    }

    pub fn had_error(&self) -> bool {
        self.had_error.load(AtomicOrdering::Acquire)
    }

    pub(self) fn record_error(&self) {
        self.had_error.store(true, AtomicOrdering::Release);
    }

    pub fn report<'b>(&'a self, source_loc: SourceLoc<'a>, diagnostic: Diagnostic<'b>) -> Report<'b, C>
    where
        'a: 'b,
    {
        Report::new(source_loc, diagnostic, self)
    }
}

impl<'a> Engine<'a, DefaultDiagnosticConsumer> {
    pub fn new(source_manager: &'a SourceManager) -> Self {
        Self::new_with_consumer(source_manager, DefaultDiagnosticConsumer)
    }
}

#[must_use = "report does nothing unless diagnosed"]
pub struct Report<'a, C: DiagnosticConsumer> {
    pub source_loc: SourceLoc<'a>,
    pub diagnostic: Diagnostic<'a>,
    pub context_notes: Vec<(SourceRange<'a>, DiagnosticContextNote<'a>)>,
    pub note: Option<DiagnosticNote<'a>>,
    engine: &'a Engine<'a, C>,
}

impl<'a, C: DiagnosticConsumer> Report<'a, C> {
    pub(self) fn new(source_loc: SourceLoc<'a>, diagnostic: Diagnostic<'a>, engine: &'a Engine<'a, C>) -> Self {
        Self {
            source_loc,
            diagnostic,
            context_notes: Vec::new(),
            note: None,
            engine,
        }
    }

    pub fn with_context_note<'b>(
        self,
        source_range: SourceRange<'b>,
        context_note: DiagnosticContextNote<'b>,
    ) -> Report<'b, C>
    where
        'a: 'b,
    {
        let mut context_notes = self.context_notes;
        context_notes.push((source_range, context_note));

        Report { context_notes, ..self }
    }

    pub fn with_note<'b>(self, note: DiagnosticNote<'b>) -> Report<'b, C>
    where
        'a: 'b,
    {
        Report {
            note: Some(note),
            ..self
        }
    }

    pub fn diagnose(self) -> Result<()> {
        let engine = self.engine;

        if self.diagnostic.get_kind().is_error() {
            engine.record_error();
        }

        engine.get_consumer().consume(self, engine)
    }
}
