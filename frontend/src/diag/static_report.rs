use ariadne::{Color, Fmt as _};

use super::{DiagnosticNote, StaticDiagnostic};

pub struct StaticReport {
    diagnostic: StaticDiagnostic,
    note: Option<DiagnosticNote>,
}

impl StaticReport {
    pub fn new(diagnostic: StaticDiagnostic) -> Self {
        Self { diagnostic, note: None }
    }

    pub fn with_note(mut self, note: DiagnosticNote) -> Self {
        self.note = Some(note);
        self
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
