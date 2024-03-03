use super::{Diagnostic, DiagnosticConsumer, DiagnosticContextNote, DiagnosticNote};
use crate::{
    source_loc::{SourceLoc, SourceRange},
    source_manager::SourceManager,
    Result,
};

pub struct Engine<'a, C: DiagnosticConsumer> {
    source_manager: &'a SourceManager,
    consumer: C,
}

impl<'a, C: DiagnosticConsumer> Engine<'a, C> {
    pub fn new(source_manager: &'a SourceManager, consumer: C) -> Self {
        Self {
            source_manager,
            consumer,
        }
    }

    pub fn get_source_manager(&self) -> &SourceManager {
        self.source_manager
    }

    pub fn get_consumer(&self) -> &C {
        &self.consumer
    }

    pub fn report(&self, source_loc: SourceLoc, diagnostic: Diagnostic) -> Report<C> {
        Report::new(source_loc, diagnostic, self)
    }
}

pub struct Report<'a, C: DiagnosticConsumer> {
    pub source_loc: SourceLoc<'a>,
    pub diagnostic: Diagnostic,
    pub context_notes: Vec<(SourceRange<'a>, DiagnosticContextNote)>,
    pub note: Option<DiagnosticNote>,
    engine: &'a Engine<'a, C>,
}

impl<'a, C: DiagnosticConsumer> Report<'a, C> {
    pub(self) fn new(source_loc: SourceLoc<'a>, diagnostic: Diagnostic, engine: &'a Engine<'a, C>) -> Self {
        Self {
            source_loc,
            diagnostic,
            context_notes: Vec::new(),
            note: None,
            engine,
        }
    }

    pub fn with_context_note(mut self, source_range: SourceRange<'a>, context_note: DiagnosticContextNote) -> Self {
        self.context_notes.push((source_range, context_note));
        self
    }

    pub fn with_note(mut self, note: DiagnosticNote) -> Self {
        self.note = Some(note);
        self
    }

    pub fn diagnose(self) -> Result<()> {
        let engine = self.engine;
        self.engine.get_consumer().consume(self, engine)
    }
}
