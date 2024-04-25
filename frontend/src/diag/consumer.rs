use ariadne::{Color, ColorGenerator, Config, IndexType, Label, Report};
use juice_core::diag::ColorExt as _;

use super::{DiagnosticEngine, DiagnosticReport};
use crate::{source_loc::SourceRange, Result};

pub trait Consumer: Sized {
    fn consume<'a>(&self, report: DiagnosticReport<'a, Self>, engine: &'a DiagnosticEngine<'a, Self>) -> Result<()>;
}

#[derive(Debug, Default, Clone, Copy)]
pub struct DefaultConsumer;

impl DefaultConsumer {
    fn build_ariadne_report<'a>(&self, report: DiagnosticReport<'a, Self>) -> Report<'a, SourceRange<'a>> {
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

impl Consumer for DefaultConsumer {
    fn consume<'a>(&self, report: DiagnosticReport<'a, Self>, engine: &'a DiagnosticEngine<'a, Self>) -> Result<()> {
        self.build_ariadne_report(report)
            .eprint(engine.get_source_manager().get_cache())
            .map_err(Into::into)
    }
}
