use std::io;

use derive_more::From;

use crate::diag::{StaticDiagnostic, StaticDiagnosticReport};

#[derive(Debug, From)]
pub enum Error {
    #[from]
    Io(io::Error),
}

impl Error {
    pub fn diagnose(self) {
        let diagnostic = match self {
            Self::Io(err) => StaticDiagnostic::io_error(err.to_string()),
        };

        StaticDiagnosticReport::new(diagnostic).diagnose();
    }
}

pub type Result<T, E = Error> = std::result::Result<T, E>;
