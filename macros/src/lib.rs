#![feature(array_windows)]

mod diagnostic_variants;
mod enum_def;
mod string_enum;

use std::fmt::{Display, Formatter, Result as FmtResult};

use proc_macro::TokenStream;
use quote::ToTokens as _;
use string_enum::StringEnum;
use syn::parse_macro_input;

use crate::diagnostic_variants::{DiagnosticNote, Diagnostics};

macro_rules! error {
    ($($name: ident => $message: literal,)*) => {
        enum Error {
            $(
                $name,
            )*
        }

        impl Display for Error {
            fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
                let message = match self {
                    $(
                        Self::$name => $message,
                    )*
                };

                write!(f, "{}", message)
            }
        }
    }
}

error! {
    ExpectedDiagnosticKind => "Expected either 'error' or 'warning'",
    FieldNamedColor => "Diagnostic fields cannot be named 'color'",
    NoFieldsWithFormat => "Format string contains a placeholder but no fields were provided",
    EnumCannotHaveLifetime => "Enum cannot have a lifetime",
    DuplicateVariantString => "Duplicate variant string",
}

#[proc_macro]
pub fn diagnostic(input: TokenStream) -> TokenStream {
    let diagnostics: Diagnostics = parse_macro_input!(input);
    diagnostics.into_token_stream().into()
}

#[proc_macro]
pub fn diagnostic_note(input: TokenStream) -> TokenStream {
    let diagnostic_note: DiagnosticNote = parse_macro_input!(input);
    diagnostic_note.into_token_stream().into()
}

#[proc_macro]
pub fn string_enum(input: TokenStream) -> TokenStream {
    let string_enum: StringEnum = parse_macro_input!(input);
    string_enum.into_token_stream().into()
}
