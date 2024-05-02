use std::ops::Try;

use ariadne::{Color, ColorGenerator, Config, IndexType, Label, Report};
use juice_core::diag::ColorExt as _;

use super::{DiagnosticEngine, DiagnosticReport};
use crate::{
    source_loc::SourceRange,
    source_manager::{AriadneSourceManager, SourceManager},
    Result,
};

pub trait Consumer<'a, M: SourceManager>: Sized {
    type Output: Try<Output = ()>;

    fn consume<'b>(
        &self,
        report: DiagnosticReport<'a, 'b, M, Self>,
        engine: &'b DiagnosticEngine<'a, M, Self>,
    ) -> Self::Output;
}

#[derive(Debug, Default, Clone, Copy)]
pub struct DefaultConsumer;

impl DefaultConsumer {
    fn build_ariadne_report<'a, M: AriadneSourceManager>(
        &self,
        report: DiagnosticReport<'a, '_, M, Self>,
    ) -> Report<'a, SourceRange<'a, M>> {
        let DiagnosticReport {
            source_loc,
            diagnostic,
            context_notes,
            note,
            ..
        } = report;

        let kind = diagnostic.get_kind();

        let mut builder = Report::build(kind.into(), source_loc.source, source_loc.offset)
            .with_config(Config::default().with_index_type(IndexType::Byte))
            .with_code(diagnostic.get_code())
            .with_message(diagnostic.into_formatted_message(kind));

        let mut colors = ColorGenerator::new();

        for (source_range, context_note) in context_notes {
            let color = colors.next();

            builder = builder.with_label(
                Label::new(source_range)
                    .with_message(context_note.into_formatted_message(color))
                    .with_color(color),
            );
        }

        if let Some(note) = note {
            builder = builder.with_note(note.into_formatted_message(Color::note_color()));
        }

        builder.finish()
    }
}

impl<'a, M: AriadneSourceManager> Consumer<'a, M> for DefaultConsumer {
    type Output = Result<()>;

    fn consume<'b>(
        &self,
        report: DiagnosticReport<'a, 'b, M, Self>,
        engine: &'b DiagnosticEngine<'a, M, Self>,
    ) -> Result<()> {
        self.build_ariadne_report(report)
            .eprint(engine.get_source_manager().get_cache())
            .map_err(Into::into)
    }
}

#[cfg(test)]
pub(crate) mod test {
    use std::sync::{Mutex, PoisonError};

    use juice_core::Unit;

    use crate::{
        diag::{engine::test::Report, DiagnosticEngine, DiagnosticReport},
        source_manager::test::SourceManager as TestSourceManager,
    };

    #[derive(Debug, Default)]
    pub(crate) struct Consumer {
        reports: Mutex<Vec<Report>>,
    }

    impl Consumer {
        pub fn new() -> Self {
            Self {
                reports: Mutex::default(),
            }
        }

        pub fn into_reports(self) -> Vec<Report> {
            self.reports.into_inner().unwrap_or_else(PoisonError::into_inner)
        }
    }

    impl super::Consumer<'static, TestSourceManager> for Consumer {
        type Output = Unit;

        fn consume<'a>(
            &self,
            report: DiagnosticReport<'static, 'a, TestSourceManager, Self>,
            _: &'a DiagnosticEngine<'static, TestSourceManager, Self>,
        ) -> Unit {
            self.reports
                .lock()
                .unwrap_or_else(PoisonError::into_inner)
                .push(report.into());

            Unit
        }
    }
}
