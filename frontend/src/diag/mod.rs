mod consumer;
mod diagnostic;
mod diagnostic_note;
mod engine;
mod static_report;

pub use self::{
    consumer::{Consumer as DiagnosticConsumer, DefaultConsumer as DefaultDiagnosticConsumer},
    diagnostic::{Diagnostic, StaticDiagnostic},
    diagnostic_note::{DiagnosticContextNote, DiagnosticNote},
    engine::{Engine as DiagnosticEngine, Report as DiagnosticReport},
    static_report::StaticReport as StaticDiagnosticReport,
};
