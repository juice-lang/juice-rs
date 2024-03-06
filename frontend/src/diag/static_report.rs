use ariadne::{Color, Fmt as _};
use juice_core::diag::ColorExt as _;

use super::{DiagnosticNote, StaticDiagnostic};

#[must_use = "report does nothing unless diagnosed"]
pub struct StaticReport<'a> {
    diagnostic: StaticDiagnostic<'a>,
    note: Option<DiagnosticNote<'a>>,
}

impl<'a> StaticReport<'a> {
    pub fn new(diagnostic: StaticDiagnostic<'a>) -> Self {
        Self { diagnostic, note: None }
    }

    pub fn with_note<'b>(self, note: DiagnosticNote<'b>) -> StaticReport<'b>
    where
        'a: 'b,
    {
        StaticReport {
            note: Some(note),
            ..self
        }
    }

    pub fn diagnose(self) {
        let kind = self.diagnostic.get_kind();
        let id = format!("[{}] {}:", self.diagnostic.get_code(), kind);
        let id_len = id.len();

        eprintln!("{} {}", id.fg(kind), self.diagnostic.into_formatted_message(kind));

        if let Some(note) = self.note {
            eprintln!(
                "\n{}{}: {}\n",
                " ".repeat(id_len - 5),
                "Note".fg(Color::note_color()),
                note.into_formatted_message(Color::note_color())
            );
        }
    }
}
