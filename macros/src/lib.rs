#![feature(array_windows)]

mod diagnostic_variants;
mod enum_def;
mod string_enum;

use diagnostic_variants::Diagnostic;
use proc_macro::TokenStream;
use quote::ToTokens as _;
use string_enum::StringEnum;
use syn::parse_macro_input;

use crate::diagnostic_variants::DiagnosticNote;

#[derive(Debug, thiserror::Error)]
enum Error {
    #[error("Can only derive diagnostics on an enum")]
    NotAnEnum,
    #[error("Unknown attribute '{0}'")]
    UnknownAttribute(String),
    #[error("Duplicate 'error' or 'warning' attribute")]
    DuplicateDiagnosticKindAttr,
    #[error("Expected an 'error' or 'warning' attribute")]
    ExpectedDiagnosticKindAttr,
    #[error("Duplicate '{0}' attribute")]
    DuplicateAttr(&'static str),
    #[error("Expected a 'note' attribute")]
    ExpectedNoteAttr,
    #[error("DiagnosticNote cannot have attribute 'offset'")]
    DiagnosticNoteOffsetAttr,
    #[error("Variant cannot be unreachable and have a format string")]
    UnreachableWithFormat,
    #[error("Diagnostic fields cannot be named 'color'")]
    FieldNamedColor,
    #[error("Format string contains a placeholder but no fields were provided")]
    NoFieldsWithFormat,
    #[error("Enum cannot have a lifetime")]
    EnumCannotHaveLifetime,
    #[error("Duplicate variant string")]
    DuplicateVariantString,
}

#[proc_macro_derive(Diagnostic, attributes(diag))]
pub fn diagnostic(input: TokenStream) -> TokenStream {
    let diagnostic: Diagnostic = parse_macro_input!(input);
    diagnostic.into_token_stream().into()
}

#[proc_macro_derive(DiagnosticNote, attributes(diag))]
pub fn diagnostic_note(input: TokenStream) -> TokenStream {
    let diagnostic_note: DiagnosticNote = parse_macro_input!(input);
    diagnostic_note.into_token_stream().into()
}

#[proc_macro]
pub fn string_enum(input: TokenStream) -> TokenStream {
    let string_enum: StringEnum = parse_macro_input!(input);
    string_enum.into_token_stream().into()
}
