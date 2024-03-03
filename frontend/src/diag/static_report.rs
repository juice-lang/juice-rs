use ariadne::{Color, Fmt as _};

use super::{DiagnosticNote, StaticDiagnostic};

#[must_use = "report does nothing unless diagnosed"]
pub struct StaticReport {
    diagnostic: StaticDiagnostic,
    note: Option<DiagnosticNote>,
}

impl StaticReport {
    pub fn new(diagnostic: StaticDiagnostic) -> Self {
        Self { diagnostic, note: None }
    }

    pub fn with_note(self, note: DiagnosticNote) -> Self {
        StaticReport {
            diagnostic: self.diagnostic,
            note: Some(note),
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
                "Note".fg(Color::Fixed(115)),
                note.into_formatted_message(kind)
            );
        }
    }
}
