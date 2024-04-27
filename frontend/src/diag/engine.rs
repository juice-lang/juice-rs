use std::sync::atomic::{AtomicBool, Ordering as AtomicOrdering};

use super::{DefaultDiagnosticConsumer, Diagnostic, DiagnosticConsumer, DiagnosticContextNote, DiagnosticNote};
use crate::{
    source_loc::{SourceLoc, SourceRange},
    source_manager::{DefaultSourceManager, SourceManager},
    Result,
};

pub struct Engine<'a, M, C> {
    source_manager: &'a M,
    consumer: C,
    had_error: AtomicBool,
}

impl<'a, M: SourceManager, C: DiagnosticConsumer<'a, M>> Engine<'a, M, C> {
    pub fn new_with_consumer(source_manager: &'a M, consumer: C) -> Self {
        Self {
            source_manager,
            consumer,
            had_error: false.into(),
        }
    }

    pub fn get_source_manager(&self) -> &M {
        self.source_manager
    }

    pub fn get_consumer(&self) -> &C {
        &self.consumer
    }

    pub fn into_consumer(self) -> C {
        self.consumer
    }

    pub fn had_error(&self) -> bool {
        self.had_error.load(AtomicOrdering::Acquire)
    }

    pub(self) fn record_error(&self) {
        self.had_error.store(true, AtomicOrdering::Release);
    }

    pub fn report<'b>(&'b self, source_loc: SourceLoc<'a, M>, diagnostic: Diagnostic<'a>) -> Report<'a, 'b, M, C>
    where
        'a: 'b,
    {
        Report::new(source_loc, diagnostic, self)
    }
}

impl<'a> Engine<'a, DefaultSourceManager, DefaultDiagnosticConsumer> {
    pub fn new(source_manager: &'a DefaultSourceManager) -> Self {
        Self::new_with_consumer(source_manager, DefaultDiagnosticConsumer)
    }
}

#[must_use = "report does nothing unless diagnosed"]
pub struct Report<'a, 'b, M: SourceManager, C> {
    pub source_loc: SourceLoc<'a, M>,
    pub diagnostic: Diagnostic<'a>,
    pub context_notes: Vec<(SourceRange<'a, M>, DiagnosticContextNote<'a>)>,
    pub note: Option<DiagnosticNote<'a>>,
    engine: &'b Engine<'a, M, C>,
}

impl<'a, 'b, M: SourceManager, C: DiagnosticConsumer<'a, M>> Report<'a, 'b, M, C>
where
    'a: 'b,
{
    pub(self) fn new(source_loc: SourceLoc<'a, M>, diagnostic: Diagnostic<'a>, engine: &'b Engine<'a, M, C>) -> Self {
        Self {
            source_loc,
            diagnostic,
            context_notes: Vec::new(),
            note: None,
            engine,
        }
    }

    pub fn with_context_note(
        mut self,
        source_range: SourceRange<'a, M>,
        context_note: DiagnosticContextNote<'a>,
    ) -> Self {
        self.context_notes.push((source_range, context_note));
        self
    }

    pub fn with_note(mut self, note: DiagnosticNote<'a>) -> Self {
        self.note = Some(note);
        self
    }

    pub fn diagnose(self) -> C::Output {
        let engine = self.engine;

        if self.diagnostic.get_kind().is_error() {
            engine.record_error();
        }

        engine.get_consumer().consume(self, engine)
    }
}

#[cfg(test)]
pub(crate) mod test {
    use crate::{
        diag::{consumer::test::Consumer as TestConsumer, Diagnostic, DiagnosticContextNote, DiagnosticNote},
        source_loc::{SourceLoc, SourceRange},
        source_manager::test::SourceManager as TestSourceManager,
    };

    #[derive(Debug, Clone)]
    pub(crate) struct Report {
        pub source_loc: SourceLoc<'static, TestSourceManager>,
        pub diagnostic: Diagnostic<'static>,
        pub context_notes: Vec<(SourceRange<'static, TestSourceManager>, DiagnosticContextNote<'static>)>,
        pub note: Option<DiagnosticNote<'static>>,
    }

    impl<'a> From<super::Report<'static, 'a, TestSourceManager, TestConsumer>> for Report {
        fn from(report: super::Report<'static, 'a, TestSourceManager, TestConsumer>) -> Self {
            Self {
                source_loc: report.source_loc,
                diagnostic: report.diagnostic,
                context_notes: report.context_notes,
                note: report.note,
            }
        }
    }
}
