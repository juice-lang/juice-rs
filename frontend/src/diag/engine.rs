use std::sync::atomic::{AtomicBool, Ordering as AtomicOrdering};

use super::{DefaultDiagnosticConsumer, Diagnostic, DiagnosticConsumer, DiagnosticContextNote, DiagnosticNote};
use crate::{
    source_loc::{SourceLoc, SourceRange},
    source_manager::{DefaultSourceManager, SourceManager},
};

pub struct Engine<'src, M: 'src, C> {
    source_manager: &'src M,
    consumer: C,
    had_error: AtomicBool,
}

impl<'src, M: 'src + SourceManager, C: DiagnosticConsumer<'src, M>> Engine<'src, M, C> {
    pub fn new_with_consumer(source_manager: &'src M, consumer: C) -> Self {
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

    #[allow(dead_code)]
    pub fn into_consumer(self) -> C {
        self.consumer
    }

    pub fn had_error(&self) -> bool {
        self.had_error.load(AtomicOrdering::Acquire)
    }

    pub(self) fn record_error(&self) {
        self.had_error.store(true, AtomicOrdering::Release);
    }

    pub fn report<'diag>(
        &'diag self,
        source_loc: SourceLoc<'src, M>,
        diagnostic: Diagnostic<'src>,
    ) -> Report<'src, 'diag, M, C>
    where
        'src: 'diag,
    {
        Report::new(source_loc, diagnostic, self)
    }
}

impl<'src> Engine<'src, DefaultSourceManager, DefaultDiagnosticConsumer> {
    pub fn new(source_manager: &'src DefaultSourceManager) -> Self {
        Self::new_with_consumer(source_manager, DefaultDiagnosticConsumer)
    }
}

#[must_use = "report does nothing unless diagnosed"]
pub struct Report<'src, 'diag, M: 'src + SourceManager, C> {
    pub source_loc: SourceLoc<'src, M>,
    pub diagnostic: Diagnostic<'src>,
    pub context_notes: Vec<(SourceRange<'src, M>, DiagnosticContextNote<'src>)>,
    pub note: Option<DiagnosticNote<'src>>,
    engine: &'diag Engine<'src, M, C>,
}

impl<'src, 'diag, M: 'src + SourceManager, C: DiagnosticConsumer<'src, M>> Report<'src, 'diag, M, C>
where
    'src: 'diag,
{
    pub(self) fn new(
        source_loc: SourceLoc<'src, M>,
        diagnostic: Diagnostic<'src>,
        engine: &'diag Engine<'src, M, C>,
    ) -> Self {
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

    pub fn diagnose(self) -> Result<(), C::Error> {
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

    impl<'diag> From<super::Report<'static, 'diag, TestSourceManager, TestConsumer>> for Report {
        fn from(report: super::Report<'static, 'diag, TestSourceManager, TestConsumer>) -> Self {
            Self {
                source_loc: report.source_loc,
                diagnostic: report.diagnostic,
                context_notes: report.context_notes,
                note: report.note,
            }
        }
    }
}
