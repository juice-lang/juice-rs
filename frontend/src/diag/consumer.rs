use ariadne::{Color, Config, IndexType, Label, Report};
use juice_core::diag::{ColorExt as _, ColorGenerator};

use super::{DiagnosticEngine, DiagnosticReport};
use crate::{
    source_loc::SourceRange,
    source_manager::{AriadneSourceManager, SourceManager},
    Error, Result,
};

pub trait Consumer<'src, M: 'src + SourceManager>: Sized {
    type Error;

    fn consume<'diag>(
        &self,
        report: DiagnosticReport<'src, 'diag, M, Self>,
        engine: &'diag DiagnosticEngine<'src, M, Self>,
    ) -> Result<(), Self::Error>;
}

#[derive(Debug, Default, Clone, Copy)]
pub struct DefaultConsumer;

impl DefaultConsumer {
    fn build_ariadne_report<'src, M: 'src + AriadneSourceManager>(
        &self,
        report: DiagnosticReport<'src, '_, M, Self>,
    ) -> Report<'src, SourceRange<'src, M>> {
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

impl<'src, M: 'src + AriadneSourceManager> Consumer<'src, M> for DefaultConsumer {
    type Error = Error;

    fn consume<'diag>(
        &self,
        report: DiagnosticReport<'src, 'diag, M, Self>,
        engine: &'diag DiagnosticEngine<'src, M, Self>,
    ) -> Result<()> {
        self.build_ariadne_report(report)
            .eprint(engine.get_source_manager().get_cache())
            .map_err(Into::into)
    }
}

#[cfg(test)]
pub(crate) mod test {
    use std::{
        convert::Infallible,
        sync::{Mutex, PoisonError},
    };

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
        type Error = Infallible;

        fn consume<'diag>(
            &self,
            report: DiagnosticReport<'static, 'diag, TestSourceManager, Self>,
            _: &'diag DiagnosticEngine<'static, TestSourceManager, Self>,
        ) -> Result<(), Infallible> {
            self.reports
                .lock()
                .unwrap_or_else(PoisonError::into_inner)
                .push(report.into());

            Ok(())
        }
    }
}
